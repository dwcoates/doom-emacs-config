package harness

import (
	"bufio"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"
	"time"

	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"
)

// LogRecord is one JSONL record from a daemon log sink, per
// logging-contract.md.
type LogRecord struct {
	Timestamp string         `json:"timestamp"`
	Runtime   string         `json:"runtime"`
	Level     string         `json:"level"`
	PID       int            `json:"pid"`
	Operation string         `json:"operation"`
	Message   string         `json:"message"`
	Context   map[string]any `json:"context"`

	// THE PROMOTED IDENTITY FIELDS (internal/dlog/record.go's reservedKeys).
	// The contract requires every identifier to live in its OWN top-level
	// field and never only inside context, so a test that joins two runtimes'
	// records on one session reads them here rather than out of Context,
	// where dlog deliberately does not leave them.
	WorkspaceDir       string `json:"workspace_dir"`
	WorkspaceID        string `json:"workspace_id"`
	AgentReplSessionID string `json:"agent_repl_session_id"`
	ClaudeSessionID    string `json:"claude_session_id"`
	RequestID          string `json:"request_id"`

	// Raw is the line as written, for the assertions that care about shape.
	Raw string `json:"-"`
}

// TimestampPattern is the timestamp shape every record carries.
var TimestampPattern = regexp.MustCompile(`^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?(Z|[+-]\d{2}:\d{2})$`)

// RunLogPath is the restart-scoped run log.
func (d *Daemon) RunLogPath() string {
	return filepath.Join(d.StateDir, "logs", "daemon.run.log")
}

// WorkspaceLogPath is one of a workspace's per-workspace log sinks
// ("daemon", "shim", "webapp", "sidecar").
func WorkspaceLogPath(workspaceDir, sink string) string {
	return filepath.Join(workspaceDir, ".claude", "emacs", sink+".log")
}

// RunLog reads every record from the run log. A missing log reads as no
// records, so a test can assert on an empty log without special-casing it.
func (d *Daemon) RunLog() []LogRecord {
	d.t.Helper()
	return readLog(d.t, d.RunLogPath())
}

// WorkspaceLog reads a workspace's own log sink.
func (d *Daemon) WorkspaceLog(workspaceDir, sink string) []LogRecord {
	d.t.Helper()
	return readLog(d.t, WorkspaceLogPath(workspaceDir, sink))
}

// AwaitLogRecord waits for a record satisfying the predicate in a log file.
func (d *Daemon) AwaitLogRecord(path string, what string, pred func(LogRecord) bool) LogRecord {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		for _, r := range readLog(d.t, path) {
			if pred(r) {
				return r
			}
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %s in %s: %v", what, path, wait.Err())
		}
	}
}

// AwaitRunLogOperation waits for a run-log record with an exact operation.
func (d *Daemon) AwaitRunLogOperation(operation string) LogRecord {
	d.t.Helper()
	return d.AwaitLogRecord(d.RunLogPath(), "operation "+operation, func(r LogRecord) bool {
		return r.Operation == operation
	})
}

// AwaitWorkspaceLogOperation waits for a workspace-log record by operation.
func (d *Daemon) AwaitWorkspaceLogOperation(workspaceDir, operation string) LogRecord {
	d.t.Helper()
	return d.AwaitLogRecord(WorkspaceLogPath(workspaceDir, "daemon"), "operation "+operation, func(r LogRecord) bool {
		return r.Operation == operation
	})
}

// ReadLog reads every record from an arbitrary log file. It exists for the one
// test that must read a PREVIOUS runtime's own log target: the canonical run
// log is a symlink each runtime relinks onto its own file, so an incumbent's
// records are reachable only through the target resolved before its successor
// booted.
func ReadLog(t *testing.T, path string) []LogRecord { return readLog(t, path) }

func readLog(t *testing.T, path string) []LogRecord {
	t.Helper()
	f, err := os.Open(path)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		t.Fatalf("harness: open %s: %v", path, err)
	}
	defer f.Close()

	var out []LogRecord
	scanner := bufio.NewScanner(f)
	scanner.Buffer(make([]byte, 0, 64*1024), 16*1024*1024)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if line == "" {
			continue
		}
		var rec LogRecord
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("harness: %s holds a non-JSONL line %q: %v", path, line, err)
		}
		rec.Raw = line
		out = append(out, rec)
	}
	if err := scanner.Err(); err != nil {
		t.Fatalf("harness: scan %s: %v", path, err)
	}
	return out
}

// Warning levels the log discipline treats as failures unless expected.
var warningLevels = map[string]bool{"warn": true, "warning": true, "error": true, "fatal": true}

// ExpectWarnings declares the operations whose WARN or ERROR records this test
// intends to produce. It WIDENS the sweep StartDaemon already armed; it does
// not arm it. Anything not declared fails the test at cleanup, which is what
// drives the daemon's warning count to zero.
func (d *Daemon) ExpectWarnings(operations ...string) {
	d.t.Helper()
	d.mu.Lock()
	for _, op := range operations {
		d.expected[op] = true
	}
	d.mu.Unlock()
}

// UnexpectedWarnings is the sweep's own material: every WARN or worse this
// daemon recorded whose operation no ExpectWarnings declared. The cleanup sweep
// fails the test on it; a test ABOUT the sweep reads it.
func (d *Daemon) UnexpectedWarnings() []LogRecord {
	d.mu.Lock()
	expected := make(map[string]bool, len(d.expected))
	for k, v := range d.expected {
		expected[k] = v
	}
	d.mu.Unlock()
	return d.unexpectedWarnings(expected)
}

// unexpectedWarnings sweeps the run log and every per-workspace daemon sink
// this daemon wrote.
func (d *Daemon) unexpectedWarnings(expected map[string]bool) []LogRecord {
	return append(unexpectedWarnings(d.RunLog(), expected),
		unexpectedWarnings(d.WorkspaceLogRecords(), expected)...)
}

func (d *Daemon) assertNoUnexpectedWarnings() {
	d.mu.Lock()
	expected := make(map[string]bool, len(d.expected))
	for k, v := range d.expected {
		expected[k] = v
	}
	d.mu.Unlock()

	unexpected := d.unexpectedWarnings(expected)
	if len(unexpected) == 0 {
		return
	}
	var b strings.Builder
	for _, r := range unexpected {
		b.WriteString("\n  " + r.Level + " " + r.Operation + ": " + r.Message + contextSuffix(r))
	}
	d.t.Errorf("the daemon produced %d unexpected warning records; declare them with ExpectWarnings if they are intended:%s", len(unexpected), b.String())
}

// WorkspaceLogTargets are the per-workspace daemon sinks minted under THIS
// run's state root — `<state>/logs/agent-repl-<workspace>-daemon-*.log`, the
// names dlog's createTarget gives them.
//
// THE SWEEP READS THEM RATHER THAN THE WORKSPACES' OWN daemon.log, and that is
// not an optimization. `<workspace>/.claude/emacs/daemon.log` is a SYMLINK, and
// a merge takes the worktree — symlink and all — with `git worktree remove`. A
// sweep that read through the link found nothing for exactly the workspaces
// whose merge is the subject, so every warning a merged workspace logged went
// unswept. The target under the state root outlives the worktree, because the
// state root owns the daemon's durable logs.
func (d *Daemon) WorkspaceLogTargets() []string {
	d.t.Helper()
	pattern := filepath.Join(d.StateDir, "logs", "agent-repl-*-daemon-*.log")
	matches, err := filepath.Glob(pattern)
	if err != nil {
		d.t.Fatalf("harness: glob %s: %v", pattern, err)
	}
	sort.Strings(matches)
	return matches
}

// WorkspaceLogRecords is every record THIS daemon wrote to a per-workspace
// daemon sink.
//
// The pid filter is what keeps one state root's targets attributable: a
// restart, a successor and an incumbent share the root and each mints its own
// targets, and a successor must not be failed for the records its predecessor
// wrote (whose ExpectWarnings were declared on a different Daemon).
func (d *Daemon) WorkspaceLogRecords() []LogRecord {
	d.t.Helper()
	var out []LogRecord
	for _, target := range d.WorkspaceLogTargets() {
		for _, r := range readLog(d.t, target) {
			if r.PID == d.PID() {
				out = append(out, r)
			}
		}
	}
	return out
}

// AwaitWorkspaceLogRecordInState waits for one of this daemon's per-workspace
// records, read from the state root's targets. It is what a test whose
// workspace directory is GONE — a merged one — waits on.
func (d *Daemon) AwaitWorkspaceLogRecordInState(what string, pred func(LogRecord) bool) LogRecord {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		for _, r := range d.WorkspaceLogRecords() {
			if pred(r) {
				return r
			}
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %s in this daemon's workspace log targets: %v", what, wait.Err())
		}
	}
}

// ShimLoggedRequest recovers the LAST request the fake shim recorded for a verb
// from its durable log sink, and reports whether one was found.
//
// It exists for the verbs that END the shim: the fake's in-memory recorder dies
// with the process, so a forced KillSession can only be asserted from something
// that outlived it.
func ShimLoggedRequest(t *testing.T, workspaceDir, rpc string, into proto.Message) bool {
	t.Helper()
	found := false
	for _, r := range readLog(t, WorkspaceLogPath(workspaceDir, "shim")) {
		if r.Operation != "shim.fake."+rpc {
			continue
		}
		encoded, ok := r.Context["request"].(string)
		if !ok {
			continue
		}
		raw, err := base64.StdEncoding.DecodeString(encoded)
		if err != nil {
			t.Fatalf("harness: the shim log's %s request is not base64: %v", rpc, err)
		}
		if err := proto.Unmarshal(raw, into); err != nil {
			t.Fatalf("harness: the shim log's %s request does not decode: %v", rpc, err)
		}
		found = true
	}
	return found
}

// AwaitShimLoggedRequestMatching waits for the LAST request the fake shim
// recorded for a verb to satisfy the predicate, leaving it decoded in `into`.
//
// It exists for the verbs a workspace sees TWICE — a session started, parked,
// and started again — where "a request was logged" is already true of the
// EARLIER one, and only the request's own content tells the two apart. Waiting
// on the durable log rather than on a live control socket is also what makes
// such a wait survive a shim whose second life the daemon may end at any
// moment.
func (d *Daemon) AwaitShimLoggedRequestMatching(workspaceDir, rpc, what string, into proto.Message, pred func() bool) {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if ShimLoggedRequest(d.t, workspaceDir, rpc, into) && pred() {
			return
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %s (the shim's last logged %s request): %v", what, rpc, wait.Err())
		}
	}
}

// AwaitShimLoggedRequest waits for the fake shim to have logged one request for
// a verb, and decodes the last one.
func (d *Daemon) AwaitShimLoggedRequest(workspaceDir, rpc string, into proto.Message) {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if ShimLoggedRequest(d.t, workspaceDir, rpc, into) {
			return
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for the shim to log a %s request: %v", rpc, wait.Err())
		}
	}
}

// ClientLogPath is the sink ClientLog persists a webview's records to.
func ClientLogPath(ws *workspacev1.WorkspaceRef) string {
	return WorkspaceLogPath(ws.GetDir(), "webapp")
}

// AwaitWorkspaceLogRecord waits for a record satisfying the predicate in one
// workspace's own daemon sink. It exists for the assertions that key on a
// record's CONTEXT rather than only on its operation.
func (d *Daemon) AwaitWorkspaceLogRecord(workspaceDir, what string, pred func(LogRecord) bool) LogRecord {
	d.t.Helper()
	return d.AwaitLogRecord(WorkspaceLogPath(workspaceDir, "daemon"), what, pred)
}

// AwaitWorkspaceLogOperationCount waits until a workspace's own log sink holds
// at least `n` records under `operation`.
//
// It is how a test synchronizes on a daemon-side step it cannot observe on the
// wire. The fake shim records an rpc when the request ARRIVES, so a test that
// acts the moment it sees one is racing the daemon's handling of that rpc's
// ANSWER — pushing a turn's terminal frame before the daemon has opened the
// turn, for one, which loses the terminal and hangs whatever was waiting on it.
func (d *Daemon) AwaitWorkspaceLogOperationCount(workspaceDir, operation string, n int) {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	path := WorkspaceLogPath(workspaceDir, "daemon")
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		seen := 0
		for _, r := range readLog(d.t, path) {
			if r.Operation == operation {
				seen++
			}
		}
		if seen >= n {
			return
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %d records under %s in %s (saw %d): %v", n, operation, path, seen, wait.Err())
		}
	}
}

// WorkspaceLogOperationCount answers how many records a workspace's log sink
// already holds under an operation, for a test that needs a baseline.
func (d *Daemon) WorkspaceLogOperationCount(workspaceDir, operation string) int {
	d.t.Helper()
	seen := 0
	for _, r := range readLog(d.t, WorkspaceLogPath(workspaceDir, "daemon")) {
		if r.Operation == operation {
			seen++
		}
	}
	return seen
}

// WorkspaceLogTargets names every daemon-runtime target file behind one
// workspace sink, oldest first.
//
// The canonical <workspace>/.claude/emacs/<sink>.log symlink names only the
// CURRENT runtime's target: internal/dlog/sink.go mints a fresh target per
// runtime ("A restart never trusts the previous run's destination") and
// atomically re-points the link at it. A test that spans a crash and a cold
// boot must therefore read every target the runtimes minted for that sink,
// never the link alone, which answers only the successor's own records.
func WorkspaceLogTargets(t *testing.T, workspaceDir, sink string) []string {
	t.Helper()
	link := WorkspaceLogPath(workspaceDir, sink)
	current, err := filepath.EvalSymlinks(link)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		t.Fatalf("harness: resolve the %s sink link %s: %v", sink, link, err)
	}
	base := filepath.Base(current)
	marker := "-" + sink + "-"
	cut := strings.LastIndex(base, marker)
	if cut < 0 {
		t.Fatalf("harness: log target %q does not name the %q sink", current, sink)
	}
	pattern := filepath.Join(filepath.Dir(current), base[:cut+len(marker)]+"*.log")
	targets, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("harness: glob the %s sink targets %s: %v", sink, pattern, err)
	}
	mod := make(map[string]time.Time, len(targets))
	for _, target := range targets {
		info, err := os.Stat(target)
		if err != nil {
			t.Fatalf("harness: stat the %s sink target %s: %v", sink, target, err)
		}
		mod[target] = info.ModTime()
	}
	sort.SliceStable(targets, func(i, j int) bool {
		if mod[targets[i]].Equal(mod[targets[j]]) {
			return targets[i] < targets[j]
		}
		return mod[targets[i]].Before(mod[targets[j]])
	})
	return targets
}

// ReadCumulativeWorkspaceLog reads one workspace sink's records across EVERY
// daemon runtime that wrote it, oldest runtime first.
func ReadCumulativeWorkspaceLog(t *testing.T, workspaceDir, sink string) []LogRecord {
	t.Helper()
	var out []LogRecord
	for _, target := range WorkspaceLogTargets(t, workspaceDir, sink) {
		out = append(out, readLog(t, target)...)
	}
	return out
}

// CumulativeWorkspaceLogOperationCount answers how many records a workspace's
// daemon sink holds under an operation across every runtime that wrote it.
func (d *Daemon) CumulativeWorkspaceLogOperationCount(workspaceDir, operation string) int {
	d.t.Helper()
	seen := 0
	for _, r := range ReadCumulativeWorkspaceLog(d.t, workspaceDir, "daemon") {
		if r.Operation == operation {
			seen++
		}
	}
	return seen
}

// AwaitCumulativeWorkspaceLogOperationCount waits until a workspace's daemon
// sink holds at least `n` records under `operation` across every runtime that
// wrote it — the cross-restart counterpart of
// AwaitWorkspaceLogOperationCount, for a test that crashes a daemon and cold
// boots its successor.
func (d *Daemon) AwaitCumulativeWorkspaceLogOperationCount(workspaceDir, operation string, n int) {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		seen := d.CumulativeWorkspaceLogOperationCount(workspaceDir, operation)
		if seen >= n {
			return
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for %d cumulative records under %s for workspace %s (saw %d): %v", n, operation, workspaceDir, seen, wait.Err())
		}
	}
}

// OpTurnOpened is the record the session watcher writes once a turn is OPEN on
// it — the point after which that turn's terminal frame will be attributed.
const OpTurnOpened = "daemon.sessionwatcher.turn_opened"

// ShimVerbOrder answers the shim.v1 verbs one workspace's fake shim received,
// in arrival order.
//
// It is the suite's ORDERING PROOF. The fake's in-memory recorder answers per
// verb, so a test asking "did Hibernate arrive before KillSession" has nothing
// to compare; the durable shim sink records every verb on one timeline, and
// this reads that timeline back.
func ShimVerbOrder(t *testing.T, workspaceDir string) []string {
	t.Helper()
	var out []string
	for _, r := range readLog(t, WorkspaceLogPath(workspaceDir, "shim")) {
		if verb, ok := strings.CutPrefix(r.Operation, "shim.fake."); ok {
			out = append(out, verb)
		}
	}
	return out
}

// AwaitShimVerbOrder waits until a workspace's shim sink holds both verbs and
// answers their order, failing if the second never arrives.
func (d *Daemon) AwaitShimVerbOrder(workspaceDir string, verbs ...string) []string {
	d.t.Helper()
	wait, cancelWait := d.waitCtx()
	defer cancelWait()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		order := ShimVerbOrder(d.t, workspaceDir)
		if containsAll(order, verbs) {
			return order
		}
		select {
		case <-ticker.C:
		case <-wait.Done():
			d.t.Fatalf("waiting for the shim to receive %v (saw %v): %v", verbs, order, wait.Err())
		}
	}
}

// containsAll reports whether every verb appears at least once in the order.
func containsAll(order []string, verbs []string) bool {
	for _, want := range verbs {
		found := false
		for _, got := range order {
			if got == want {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

// contextSuffix renders a record's own structured context onto the sweep's
// failure line, keys sorted so two runs of the same fault read the same.
//
// The message alone is not enough to act on. "shim call failed" and "the
// session kill did not answer" both carry their CAUSE in the context and
// nowhere else, and a sweep failure that names neither the workspace nor the
// cause sends the next reader back to a log directory the test already
// deleted.
func contextSuffix(r LogRecord) string {
	if len(r.Context) == 0 {
		return ""
	}
	keys := make([]string, 0, len(r.Context))
	for k := range r.Context {
		keys = append(keys, k)
	}
	sort.Strings(keys)
	parts := make([]string, 0, len(keys))
	for _, k := range keys {
		parts = append(parts, fmt.Sprintf("%s=%v", k, r.Context[k]))
	}
	return " {" + strings.Join(parts, " ") + "}"
}

// unexpectedWarnings answers the records at a warning level whose operation the
// test did not declare. An EMPTY expected set flags every one of them, which is
// what makes the sweep StartDaemon arms an assertion rather than a no-op.
func unexpectedWarnings(records []LogRecord, expected map[string]bool) []LogRecord {
	var out []LogRecord
	for _, r := range records {
		if warningLevels[strings.ToLower(r.Level)] && !expected[r.Operation] {
			out = append(out, r)
		}
	}
	return out
}
