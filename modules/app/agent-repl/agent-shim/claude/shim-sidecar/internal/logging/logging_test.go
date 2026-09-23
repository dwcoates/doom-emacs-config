package logging

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"strings"
	"sync"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

// sinks builds a logger over in-memory sinks with a fixed clock and pid, so a
// record's bytes are entirely determined by the call under test.
func sinks(t *testing.T, verbose bool) (*Logger, *bytes.Buffer, *bytes.Buffer) {
	t.Helper()
	stderr, file := &bytes.Buffer{}, &bytes.Buffer{}
	level := sharedlogging.LevelInfo
	if verbose {
		level = sharedlogging.LevelDebug
	}
	l := NewAtLevel(stderr, file, level)
	l.now = func() time.Time { return time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC) }
	l.pid = func() int { return 4242 }
	return l, stderr, file
}

type recordingForwarder struct {
	address string
	err     error
	errors  []error
	// notReadyFor is how many leading Ready probes report the daemon is not yet
	// serving; zero (the default) means the daemon is serving from the first
	// probe, so a forward that fails is a genuine outage.
	notReadyFor int
	mu          sync.Mutex
	records     []ForwardRecord
	readyProbes int
}

func (f *recordingForwarder) Forward(record ForwardRecord) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.records = append(f.records, record)
	if index := len(f.records) - 1; index < len(f.errors) {
		return f.address, f.errors[index]
	}
	return f.address, f.err
}

func (f *recordingForwarder) Ready() (string, bool) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.readyProbes++
	return f.address, f.readyProbes > f.notReadyFor
}

func (f *recordingForwarder) Records() []ForwardRecord {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]ForwardRecord(nil), f.records...)
}

type blockingForwarder struct {
	started chan struct{}
	release chan struct{}
	once    sync.Once
}

func (f *blockingForwarder) Ready() (string, bool) { return "127.0.0.1:8123", true }

func (f *blockingForwarder) Forward(ForwardRecord) (string, error) {
	f.once.Do(func() { close(f.started) })
	<-f.release
	return "127.0.0.1:8123", nil
}

func forwardingSinks(t *testing.T, verbose bool, forwarder Forwarder) (*Logger, *bytes.Buffer, *bytes.Buffer) {
	t.Helper()
	terminal, global := &bytes.Buffer{}, &bytes.Buffer{}
	level := sharedlogging.LevelInfo
	if verbose {
		level = sharedlogging.LevelDebug
	}
	l := NewForwardingDurableOnlyAtLevel(terminal, global, level, forwarder)
	l.now = func() time.Time { return time.Date(2026, 8, 29, 12, 0, 0, 123456000, time.UTC) }
	l.pid = func() int { return 4242 }
	// THE LADDER IS EXERCISED, NOT WAITED ON. Its inter-attempt delay is the
	// one thing in it measured in real seconds, so the suite replaces the wait
	// itself rather than shortening it — no test here ever sleeps, and none
	// races a timer.
	l.forwardWait = func(time.Duration) bool { return true }
	return l, terminal, global
}

// failures builds a retry ladder's worth of the same error, so a subject can
// say "this record's every attempt failed" without restating the count.
func failures(n int, err error) []error {
	out := make([]error, n)
	for i := range out {
		out[i] = err
	}
	return out
}

func decode(t *testing.T, raw string) record {
	t.Helper()
	var got record
	if err := json.Unmarshal([]byte(raw), &got); err != nil {
		t.Fatalf("decoding record %q: %v", raw, err)
	}
	return got
}

func TestLogWritesBothSinks(t *testing.T) {
	// Arrange.
	l, stderr, file := sinks(t, false)

	// Act.
	l.With(Context{Operation: "cycle"}).Log("hello")

	// Assert.
	if file.String() != stderr.String() {
		t.Fatalf("sinks disagree: file=%q stderr=%q", file.String(), stderr.String())
	}
	if got := decode(t, file.String()).Message; got != "hello" {
		t.Fatalf("message = %q, want %q", got, "hello")
	}
}

func TestVerboseSuppressedWhenDisabled(t *testing.T) {
	// Arrange.
	l, stderr, file := sinks(t, false)

	// Act.
	l.With(Context{Operation: "cycle"}).LogVerbose("chatter")

	// Assert.
	if file.Len() != 0 || stderr.Len() != 0 {
		t.Fatalf("verbose record emitted while disabled: file=%q stderr=%q", file.String(), stderr.String())
	}
}

func TestVerboseEmittedWhenEnabled(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, true)

	// Act.
	l.With(Context{Operation: "cycle"}).LogVerbose("chatter")

	// Assert.
	got := decode(t, file.String())
	if got.Verbosity != "verbose" || got.Level != "debug" {
		t.Fatalf("verbose classification = level %q verbosity %q, want debug/verbose", got.Level, got.Verbosity)
	}
}

func TestWorkspaceAttributionIsPromotedOnTheRecord(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)

	// Act.
	l.With(Context{
		Operation: "watch", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef", ClaudeSessionID: "session-1",
	}).Log("watching")

	// Assert.
	got := decode(t, file.String())
	if got.WorkspaceDir != "/work/repo" || got.WorkspaceID != "deadbeef" || got.ClaudeSessionID != "session-1" {
		t.Fatalf("promoted attribution = %#v", got)
	}
	if _, exists := got.Context["workspace_id"]; exists {
		t.Fatalf("workspace_id was buried in context: %#v", got.Context)
	}
}

func TestRegisteredFileSuppliesWorkspaceAttributionToPathOnlyRecords(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)
	bound := l.With(Context{Component: "sidecar"})
	bound.RegisterFile(Context{
		Path: "/tmp/session.jsonl", WorkspaceDir: "/work/repo",
		WorkspaceID: "deadbeef", ClaudeSessionID: "session-1",
	})

	// Act.
	bound.With(Context{Operation: "poll", Path: "/tmp/session.jsonl"}).Log("polling")

	// Assert.
	got := decode(t, file.String())
	if got.WorkspaceDir != "/work/repo" || got.WorkspaceID != "deadbeef" || got.ClaudeSessionID != "session-1" {
		t.Fatalf("registered attribution = %#v", got)
	}
}

func TestFileScopedRecordIsForwarded(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123"}
	l, terminal, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "watch", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1", Path: "/work/repo/session.jsonl",
	}).Log("watching")
	l.Close()

	// Assert.
	records := forwarder.Records()
	if len(records) != 1 {
		t.Fatalf("forwarded records = %d, want 1", len(records))
	}
	got := records[0]
	if got.WorkspaceDir != "/work/repo" || got.WorkspaceID != "deadbeef" || got.ClaudeSessionID != "session-1" {
		t.Fatalf("forwarded workspace identity = %+v", got)
	}
	if got.Context["pid"] != 4242 || got.Context["claude_session_id"] != "session-1" {
		t.Fatalf("forwarded process/session context = %v", got.Context)
	}
	if terminal.Len() != 0 || global.Len() != 0 {
		t.Fatalf("file-scoped record also reached a local sink: terminal=%q global=%q", terminal.String(), global.String())
	}
}

func TestFileScopedForwardingDoesNotBlockLogging(t *testing.T) {
	// Arrange.
	forwarder := &blockingForwarder{started: make(chan struct{}), release: make(chan struct{})}
	l, _, _ := forwardingSinks(t, false, forwarder)
	logged := make(chan struct{})

	// Act.
	go func() {
		l.With(Context{
			Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
			ClaudeSessionID: "session-1",
		}).Log("polling")
		close(logged)
	}()

	// Assert.
	select {
	case <-logged:
	case <-time.After(time.Second):
		close(forwarder.release)
		t.Fatal("file-scoped logging waited for ClientLog")
	}
	select {
	case <-forwarder.started:
	case <-time.After(time.Second):
		close(forwarder.release)
		t.Fatal("queued file-scoped record was not forwarded")
	}
	close(forwarder.release)
	l.Close()
}

func TestGlobalRecordIsNotForwarded(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123"}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{Operation: "start"}).Log("sidecar starting")
	l.Close()

	// Assert.
	if records := forwarder.Records(); len(records) != 0 {
		t.Fatalf("global record was forwarded: %+v", records)
	}
	if got := decode(t, global.String()).Operation; got != "start" {
		t.Fatalf("global sink operation = %q, want start", got)
	}
}

func TestForwardFailureIsRecordedOnceAndLoggingContinues(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", err: errors.New("connection refused")}
	l, _, global := forwardingSinks(t, false, forwarder)
	file := Context{WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef", ClaudeSessionID: "session-1"}

	// Act.
	l.With(mergeContext(file, Context{Operation: "poll"})).Log("polling")
	l.With(mergeContext(file, Context{Operation: "commit"})).Log("committing")
	l.With(Context{Operation: "rescan"}).Log("rescan complete")
	l.Close()

	// Assert: both records climbed the whole ladder, and the outage is stated
	// ONCE — with the two undelivered records themselves kept in the global
	// sink rather than dropped.
	if records := forwarder.Records(); len(records) != 2*defaultForwardAttempts {
		t.Fatalf("forward attempts = %d, want both file records retried %d times", len(records), defaultForwardAttempts)
	}
	operations := map[string]int{}
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		operations[decode(t, line).Operation]++
	}
	want := map[string]int{"sidecar.logging.forward-failure": 1, "rescan": 1, "poll": 1, "commit": 1}
	for operation, count := range want {
		if operations[operation] != count {
			t.Fatalf("global operations = %v, want %v", operations, want)
		}
	}
}

func TestForwardFailureIsRecordedAgainAfterRecovery(t *testing.T) {
	// Arrange: one outage, a recovery, then a second outage.
	script := append(failures(defaultForwardAttempts, errors.New("connection refused")), nil)
	script = append(script, failures(defaultForwardAttempts, errors.New("connection reset"))...)
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", errors: script}
	l, _, global := forwardingSinks(t, false, forwarder)
	file := Context{Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef", ClaudeSessionID: "session-1"}

	// Act.
	l.With(file).Log("first outage")
	l.With(file).Log("recovered")
	l.With(file).Log("second outage")
	l.Close()

	// Assert.
	failureRecords := 0
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		if decode(t, line).Operation == "sidecar.logging.forward-failure" {
			failureRecords++
		}
	}
	if failureRecords != 2 {
		t.Fatalf("forwarding failure records = %d, want one per outage window: %q", failureRecords, global.String())
	}
}

// A DAEMON THAT IS BOOTING IS NOT A DAEMON THAT IS GONE. Its boot
// reconciliation answers no rpc, so the first attempt deadlines and a later one
// lands; one attempt lost the diagnostic to a window that fixes itself.
func TestABootingDaemonIsRetriedUntilItAnswers(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:53952", errors: []error{
		errors.New("WatchWorkspaceRoster at 127.0.0.1:53952: deadline_exceeded"),
		errors.New("WatchWorkspaceRoster at 127.0.0.1:53952: deadline_exceeded"),
		nil,
	}}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert: it was delivered, so nothing was recorded as a failure.
	if got := len(forwarder.Records()); got != 3 {
		t.Fatalf("forward attempts = %d, want the two deadlines plus the delivery", got)
	}
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("a retry that succeeded was still reported as a failure: %q", global.String())
	}
}

// A DAEMON SEEN SERVING AND THEN REPLACED IS NOT A STUCK DAEMON. The forwarder
// wraps a connection failure with ErrForwardTargetNotThere when the target
// this attempt dialed is provably gone (dead pid, or daemon.addr changed);
// forwardLoop must not manufacture a WARN for that even though the daemon was
// seen serving earlier in the ladder — that WARN belongs only to a daemon that
// is genuinely stuck, not one that was replaced mid-flight.
func TestATargetGoneAfterSeenServingIsTransientNotAWarn(t *testing.T) {
	// Arrange: the daemon answers Ready every time (seenServing latches true),
	// but every Forward attempt fails because the target it dialed is gone.
	forwarder := &recordingForwarder{
		address: "127.0.0.1:8123",
		err:     fmt.Errorf("ClientLog at 127.0.0.1:8123: dial tcp: connection refused: %w", ErrForwardTargetNotThere),
	}
	l, _, global := forwardingSinks(t, true, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert: no WARN was manufactured, but the transient is narrated at DEBUG
	// and the record itself is still kept in the global sink.
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("a target-not-there failure was reported as a WARN outage: %q", global.String())
	}
	operations := map[string]int{}
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		operations[decode(t, line).Operation]++
	}
	want := map[string]int{"sidecar.logging.forward-deferred": 1, "poll": 1}
	for operation, count := range want {
		if operations[operation] != count {
			t.Fatalf("global operations = %v, want %v", operations, want)
		}
	}
}

// A NEVER-SERVED ADDRESS IS A BOOTING ADDRESS, EVEN WITH A GLOBALLY-LATCHED
// seenServing. The forwarder wraps a connection failure with
// ErrForwardTargetBooting when the address this attempt dialed has never once
// been seen accepting, regardless of this ladder's own seenServing (which
// tracks only whether SOME daemon answered a probe or forward, never which
// address). forwardLoop must not manufacture a WARN for that per-address
// startup transient — the realtest 1 shape: daemon A was seen serving, and
// daemon B's address then refuses during its own boot window.
func TestATargetBootingIsTransientNotAWarn(t *testing.T) {
	// Arrange: Ready answers true from the first probe (seenServing latches
	// true), but every Forward attempt fails because the address this record
	// targets has never itself been seen accepting.
	forwarder := &recordingForwarder{
		address: "127.0.0.1:8123",
		err:     fmt.Errorf("ClientLog at 127.0.0.1:8123: dial tcp: connection refused: %w", ErrForwardTargetBooting),
	}
	l, _, global := forwardingSinks(t, true, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert: no WARN was manufactured, but the transient is narrated at DEBUG
	// and the record itself is still kept in the global sink.
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("a booting-target failure was reported as a WARN outage: %q", global.String())
	}
	operations := map[string]int{}
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		operations[decode(t, line).Operation]++
	}
	want := map[string]int{"sidecar.logging.forward-deferred": 1, "poll": 1}
	for operation, count := range want {
		if operations[operation] != count {
			t.Fatalf("global operations = %v, want %v", operations, want)
		}
	}
}

// AN UNRESOLVABLE WORKSPACE IS FORWARDED UNATTRIBUTED, NOT WARNED, NOT RETRIED.
// The record named a workspace that a healthy, fully-delivered roster does not
// contain -- a macOS temp-root -- so the daemon is serving fine but the dir
// cannot be attributed. It must be narrated at DEBUG, kept in the global sink,
// and attempted only ONCE: retrying cannot make an absent dir appear.
func TestAnUnresolvableWorkspaceIsForwardedUnattributedAtDebug(t *testing.T) {
	// Arrange: the daemon answers Ready (it is serving), but every Forward
	// returns the unresolvable-workspace sentinel -- the roster delivered and
	// this dir was absent from it.
	forwarder := &recordingForwarder{
		address: "127.0.0.1:8123",
		err:     fmt.Errorf("workspace %q is absent from the delivered roster: %w", "/tmp", ErrForwardWorkspaceUnresolvable),
	}
	l, _, global := forwardingSinks(t, true, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/tmp", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert: no WARN outage was manufactured, the transient is narrated at
	// DEBUG, and the record itself is kept unattributed in the global sink.
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("an unresolvable-workspace forward was reported as a WARN outage: %q", global.String())
	}
	operations := map[string]int{}
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		operations[decode(t, line).Operation]++
	}
	want := map[string]int{"sidecar.logging.forward-deferred": 1, "poll": 1}
	for operation, count := range want {
		if operations[operation] != count {
			t.Fatalf("global operations = %v, want %v", operations, want)
		}
	}
	// The ladder is abandoned at once: exactly one Forward attempt, never the
	// six-rung retry ladder that a transport transient would climb.
	if attempts := len(forwarder.Records()); attempts != 1 {
		t.Fatalf("forward attempts = %d, want a single attempt with no retry", attempts)
	}
}

// The count is what separates "the daemon was slow to boot" from "the daemon is
// not there", so the one failure record carries it.
func TestTheForwardFailureRecordCarriesItsAttemptCount(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", err: errors.New("connection refused")}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert.
	var failure record
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		if got := decode(t, line); got.Operation == "sidecar.logging.forward-failure" {
			failure = got
		}
	}
	if failure.Level != "warn" {
		t.Fatalf("forward-failure level = %q, want warn", failure.Level)
	}
	if got := failure.Context["attempt"]; got != float64(defaultForwardAttempts) {
		t.Fatalf("attempt = %v, want the whole ladder (%d)", got, defaultForwardAttempts)
	}
}

// AN UNDELIVERABLE DIAGNOSTIC IS NOT A DISCARDED ONE: the workspace sink is
// unreachable, so the record lands in the global durable sink instead.
func TestAnUndeliverableRecordIsKeptInTheGlobalSink(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", err: errors.New("connection refused")}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert.
	var kept record
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		if got := decode(t, line); got.Operation == "poll" {
			kept = got
		}
	}
	if kept.Message != "polling" {
		t.Fatalf("the undelivered file-scoped record is not in the global sink: %q", global.String())
	}
	if kept.Context["forward_undelivered"] != true {
		t.Fatalf("the kept record does not say it was never delivered: %v", kept.Context)
	}
	if kept.WorkspaceDir != "/work/repo" {
		t.Fatalf("the kept record lost its workspace attribution: %+v", kept)
	}
}

func TestForwardedRecordCarriesItsTimestampAndVerboseClass(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123"}
	l, _, _ := forwardingSinks(t, true, forwarder)

	// Act.
	l.With(Context{
		Operation: "tail-pickup", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).LogVerbose("picked up one frame")
	l.Close()

	// Assert.
	got := forwarder.Records()[0]
	wantTimestamp := sharedlogging.Timestamp(time.Date(2026, 8, 29, 12, 0, 0, 123456000, time.UTC).Local())
	if got.Timestamp != wantTimestamp {
		t.Fatalf("forwarded timestamp = %q, want the sidecar's own instant %q", got.Timestamp, wantTimestamp)
	}
	if got.Level != "debug" || !got.Verbose {
		t.Fatalf("forwarded classification = level %q verbose %t, want debug/true", got.Level, got.Verbose)
	}
}

func TestFilteredRecordStillRejectsIncompleteWorkspaceAttribution(t *testing.T) {
	// Arrange.
	var stderr, file bytes.Buffer
	l := NewAtLevel(&stderr, &file, sharedlogging.LevelError)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("filtered record accepted an incomplete workspace identity")
		}
	}()

	// Act.
	l.With(Context{Operation: "watch", WorkspaceDir: "/work/repo"}).LogVerbose("watching")
}

func TestCorrelationKeysRendered(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)
	ctx := Context{
		Operation: "write-batch", Component: "storeclient", Producer: "shim-claude-sidecar",
		AgentID: "agent-1", VendorSessionID: "vendor-1", BookAgentID: "book-1",
		WriteID: "w1", UpsertKey: "activity:a1", Position: "p1", WriteSeq: Seq(7),
		WatchTokenHash: "deadbeef", RPC: "/store.v1.ShimStore/WriteBatch",
		FileID: "16777232:99", Path: "/tmp/t.jsonl", Offset: Off(512),
		TaskID: "b1", ActivityID: "a1", TurnID: "t1", StoreSocket: "/tmp/s.sock",
	}

	// Act.
	l.With(ctx).Log("wrote")

	// Assert.
	got := decode(t, file.String()).Context
	want := map[string]any{
		"component": "storeclient", "producer": "shim-claude-sidecar", "agent_id": "agent-1",
		"vendor_session_id": "vendor-1", "book_agent_id": "book-1", "write_id": "w1",
		"upsert_key": "activity:a1", "position": "p1", "write_seq": float64(7),
		"watch_token_hash": "deadbeef", "rpc": "/store.v1.ShimStore/WriteBatch",
		"file_id": "16777232:99", "path": "/tmp/t.jsonl", "offset": float64(512),
		"task_id": "b1", "activity_id": "a1", "turn_id": "t1", "store_socket": "/tmp/s.sock",
	}
	for key, wantValue := range want {
		if got[key] != wantValue {
			t.Errorf("context[%q] = %v, want %v", key, got[key], wantValue)
		}
	}
	if len(got) != len(want) {
		t.Errorf("context has %d keys, want %d: %v", len(got), len(want), got)
	}
}

func TestRetiredAddressingKeysAbsent(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)

	// Act.
	l.With(Context{Operation: "cycle", AgentID: "agent-1"}).Log("no session ordinals here")

	// Assert.
	for _, retired := range []string{"claude_session_id", "seq", "from_seq", "replay_from_seq", "agent_repl_session_id"} {
		if strings.Contains(file.String(), retired) {
			t.Errorf("record still carries the retired key %q: %s", retired, file.String())
		}
	}
}

func TestUnsetOffsetOmitted(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)

	// Act.
	l.With(Context{Operation: "cycle", Path: "/tmp/t.jsonl"}).Log("no offset known")

	// Assert.
	if _, ok := decode(t, file.String()).Context["offset"]; ok {
		t.Fatalf("absent offset rendered as a sentinel: %s", file.String())
	}
}

func TestZeroOffsetRendered(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)

	// Act.
	l.With(Context{Operation: "cycle", Offset: Off(0)}).Log("start of file")

	// Assert.
	if got := decode(t, file.String()).Context["offset"]; got != float64(0) {
		t.Fatalf("offset = %v, want 0 present", got)
	}
}

func TestWithOverridesEarlierValues(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, false)
	base := l.With(Context{Operation: "cycle", Component: "root", AgentID: "agent-1"})

	// Act.
	base.With(Context{Operation: "poll", Component: "tail"}).Log("bound")

	// Assert.
	got := decode(t, file.String())
	if got.Operation != "poll" || got.Context["component"] != "tail" || got.Context["agent_id"] != "agent-1" {
		t.Fatalf("merged record = %+v", got)
	}
}

func TestMissingOperationPanics(t *testing.T) {
	// Arrange.
	l, _, _ := sinks(t, false)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a record with no operation was accepted")
		}
	}()

	// Act.
	l.With(Context{}).Log("nameless")
}

func TestInvalidLevelPanics(t *testing.T) {
	// Arrange.
	l, _, _ := sinks(t, false)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("an invalid level was accepted")
		}
	}()

	// Act.
	l.With(Context{Operation: "cycle", Level: "catastrophe"}).Log("bad level")
}

// failingWriter fails every write, standing in for a full or unlinked log file.
type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("disk gone") }

func TestSinkFailurePanicsAfterTerminalReport(t *testing.T) {
	// Arrange.
	stderr := &bytes.Buffer{}
	l := New(stderr, failingWriter{})
	l.now = func() time.Time { return time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC) }
	l.pid = func() int { return 4242 }
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a failed persistent sink did not stop the caller")
		}
		if !strings.Contains(stderr.String(), "sidecar.logging.sink-failure") {
			t.Fatalf("sink failure was not narrated to the terminal: %q", stderr.String())
		}
	}()

	// Act.
	l.With(Context{Operation: "cycle"}).Log("doomed")
}

func TestSinkEmergencySkipsPersistentSink(t *testing.T) {
	// Arrange.
	stderr := &bytes.Buffer{}
	l := New(stderr, failingWriter{})
	l.now = func() time.Time { return time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC) }
	l.pid = func() int { return 4242 }

	// Act.
	l.With(Context{Operation: "store-write", Level: "error", SinkEmergency: true}).Log("store unreachable")

	// Assert.
	if got := decode(t, stderr.String()).Operation; got != "store-write" {
		t.Fatalf("emergency record = %q, want the caller's operation", got)
	}
}

func TestARecoveryRecordCarriesItsAttemptAndArmedBackoff(t *testing.T) {
	// Arrange. An outage's progress must be filterable: "which attempt" and
	// "how long until the next one" are facts a reader joins on, not prose.
	var sink bytes.Buffer
	log := New(io.Discard, &sink).With(Context{Component: "sidecar"})

	// Act.
	log.With(Context{
		Operation: "recover-cursors", Level: "warn",
		Attempt: Attempt(3), BackoffMs: BackoffMs(1500 * time.Millisecond),
	}).Log("recovery attempt failed")

	// Assert.
	ctx := decodeOneContext(t, &sink)
	if got := ctx["attempt"]; got != float64(3) {
		t.Fatalf("attempt = %v, want 3", got)
	}
	if got := ctx["backoff_ms"]; got != float64(1500) {
		t.Fatalf("backoff_ms = %v, want 1500", got)
	}
}

func TestARecordThatArmsNoBackoffOmitsTheKeyEntirely(t *testing.T) {
	// Arrange. Absence is presence-shaped: an unset backoff must be missing
	// rather than reported as a zero delay that reads as "retrying now".
	var sink bytes.Buffer
	log := New(io.Discard, &sink).With(Context{Component: "sidecar"})

	// Act.
	log.With(Context{Operation: "recover-cursors"}).Log("cursors recovered")

	// Assert.
	ctx := decodeOneContext(t, &sink)
	if _, ok := ctx["backoff_ms"]; ok {
		t.Fatalf("backoff_ms is present on a record that armed none: %v", ctx)
	}
	if _, ok := ctx["attempt"]; ok {
		t.Fatalf("attempt is present on a record that made none: %v", ctx)
	}
}

// decodeOneContext reads the single record a test wrote and returns its context.
func decodeOneContext(t *testing.T, sink *bytes.Buffer) map[string]any {
	t.Helper()
	lines := strings.Split(strings.TrimSpace(sink.String()), "\n")
	if len(lines) != 1 {
		t.Fatalf("records = %d, want exactly 1: %q", len(lines), sink.String())
	}
	var rec struct {
		Context map[string]any `json:"context"`
	}
	if err := json.Unmarshal([]byte(lines[0]), &rec); err != nil {
		t.Fatalf("record is not JSON: %v", err)
	}
	return rec.Context
}

// TestDurableOnlyWithholdsAnOrdinaryRecordFromTheTerminal covers the bound on
// the launchd stderr file: the record stream is durable-sink-only, so the file
// nobody rolls stops receiving a second copy of a log that is already rotated.
func TestDurableOnlyWithholdsAnOrdinaryRecordFromTheTerminal(t *testing.T) {
	// Arrange.
	stderr, file := &bytes.Buffer{}, &bytes.Buffer{}
	l := NewDurableOnly(stderr, file)
	l.now = func() time.Time { return time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC) }
	l.pid = func() int { return 4242 }

	// Act.
	l.With(Context{Operation: "cycle"}).Log("an ordinary lifecycle record")

	// Assert: durably recorded, and not mirrored.
	if got := decode(t, file.String()).Operation; got != "cycle" {
		t.Fatalf("the durable sink holds operation %q, want the caller's", got)
	}
	if stderr.Len() != 0 {
		t.Fatalf("an ordinary record reached the terminal under NewDurableOnly: %q", stderr.String())
	}
}

// TestDurableOnlyStillNarratesAnEmergencyToTheTerminal covers the carve-out: a
// failure OF the durable sink can only be reported through the terminal, so
// withholding the ordinary stream must not close that channel.
func TestDurableOnlyStillNarratesAnEmergencyToTheTerminal(t *testing.T) {
	// Arrange.
	stderr := &bytes.Buffer{}
	l := NewDurableOnly(stderr, &bytes.Buffer{})
	l.now = func() time.Time { return time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC) }
	l.pid = func() int { return 4242 }

	// Act.
	l.With(Context{Operation: "store-write", Level: "error", SinkEmergency: true}).Log("store unreachable")

	// Assert.
	if got := decode(t, stderr.String()).Operation; got != "store-write" {
		t.Fatalf("emergency record = %q, want the caller's operation on the terminal", got)
	}
}

// A resource forwarded before its producer has published is a STARTUP
// TRANSIENT. A daemon that is merely not-yet-serving and then becomes reachable
// within the ladder is delivered to, so nothing is warned and the record is not
// forwarded prematurely.
func TestANotYetServingDaemonThatBecomesReachableIsNotWarned(t *testing.T) {
	// Arrange: the daemon is not serving for the first two readiness probes,
	// then answers and the forward lands.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", notReadyFor: 2}
	l, _, global := forwardingSinks(t, true, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert: it forwarded exactly once, only after the daemon was live, and
	// nothing was reported as a failure or a lost transient.
	if got := len(forwarder.Records()); got != 1 {
		t.Fatalf("forward attempts = %d, want one forward after the daemon began serving", got)
	}
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("a booting daemon that became reachable was reported as a failure: %q", global.String())
	}
	if strings.Contains(global.String(), "forward-deferred") {
		t.Fatalf("a delivered record was still recorded as a deferred transient: %q", global.String())
	}
}

// A daemon that never begins serving across the whole ladder is a STARTUP
// TRANSIENT, not an outage: no WARN, the transient is narrated at DEBUG, and the
// record is not forwarded prematurely.
func TestADaemonThatNeverBeginsServingIsNotWarned(t *testing.T) {
	// Arrange: readiness never reports the daemon live.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", notReadyFor: defaultForwardAttempts}
	l, _, global := forwardingSinks(t, true, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert: it never forwarded prematurely, and the transient is DEBUG, not a
	// WARN.
	if got := len(forwarder.Records()); got != 0 {
		t.Fatalf("forward attempts = %d, want none against a daemon that never served", got)
	}
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("a daemon that never began serving manufactured a failure WARN: %q", global.String())
	}
	var deferred record
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		if got := decode(t, line); got.Operation == "sidecar.logging.forward-deferred" {
			deferred = got
		}
	}
	if deferred.Level != "debug" {
		t.Fatalf("forward-deferred level = %q, want debug: %q", deferred.Level, global.String())
	}
}

// The startup-transient DEBUG narration is withheld from a production INFO log,
// so a boot leaves no forward record at all while the undelivered diagnostic is
// still persisted.
func TestTheStartupTransientNarrationIsWithheldBelowDebug(t *testing.T) {
	// Arrange: a production-level (INFO) forwarding logger and a daemon that
	// never begins serving.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", notReadyFor: defaultForwardAttempts}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert.
	if strings.Contains(global.String(), "forward-deferred") {
		t.Fatalf("the DEBUG transient narration leaked into an INFO log: %q", global.String())
	}
	if strings.Contains(global.String(), "forward-failure") {
		t.Fatalf("a startup transient manufactured a failure WARN: %q", global.String())
	}
}

// AN UNDELIVERABLE DIAGNOSTIC IS NOT A DISCARDED ONE, even when the daemon never
// began serving: the record still lands in the global durable sink.
func TestAStartupTransientStillPersistsItsRecord(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", notReadyFor: defaultForwardAttempts}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert.
	var kept record
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		if got := decode(t, line); got.Operation == "poll" {
			kept = got
		}
	}
	if kept.Message != "polling" {
		t.Fatalf("the undelivered record is not in the global sink: %q", global.String())
	}
	if kept.Context["forward_undelivered"] != true {
		t.Fatalf("the kept record does not say it was never delivered: %v", kept.Context)
	}
	if kept.WorkspaceDir != "/work/repo" {
		t.Fatalf("the kept record lost its workspace attribution: %+v", kept)
	}
}

// A daemon that WAS serving and then fails the forward is a genuine outage,
// stated once at WARN — the ready-then-unreachable case the transient path must
// not swallow.
func TestAServingDaemonThatBecomesUnreachableIsWarned(t *testing.T) {
	// Arrange: the daemon is live on every readiness probe, but the forward
	// itself fails throughout.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", err: errors.New("connection reset")}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{
		Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef",
		ClaudeSessionID: "session-1",
	}).Log("polling")
	l.Close()

	// Assert.
	var failure record
	for _, line := range strings.Split(strings.TrimSpace(global.String()), "\n") {
		if got := decode(t, line); got.Operation == "sidecar.logging.forward-failure" {
			failure = got
		}
	}
	if failure.Level != "warn" {
		t.Fatalf("a serving daemon that went unreachable was not warned: %q", global.String())
	}
	if got := len(forwarder.Records()); got != defaultForwardAttempts {
		t.Fatalf("forward attempts = %d, want the whole ladder against a serving daemon", got)
	}
}

// ---------------------------------------------------------------------------
// The startup catch-up window.
// ---------------------------------------------------------------------------

// catchupSubject builds a debug-threshold logger with an open catch-up window
// over the sidecar's six corpus-walk operations, so a subject can state one
// record and read what the window did to it.
func catchupSubject(t *testing.T) (*Logger, *bytes.Buffer) {
	t.Helper()
	l, _, file := sinks(t, true)
	l.BeginCatchup("launch", "record-spawn", "hold-spool", "tail-pickup", "boot-rewind", "lost-policy")
	return l, file
}

// Each of the six operations the boot walk restates wholesale is DEBUG while
// the window is open. One case per operation: the registration of each is its
// own edge, and a list that silently loses one is exactly the regression.
func TestCatchupWindowStatesEachCorpusWalkOperationAtDebug(t *testing.T) {
	for _, operation := range []string{"launch", "record-spawn", "hold-spool", "tail-pickup", "boot-rewind", "lost-policy"} {
		t.Run(operation, func(t *testing.T) {
			// Arrange.
			l, file := catchupSubject(t)

			// Act.
			l.With(Context{Operation: operation}).Log("one backlog item")

			// Assert.
			got := decode(t, file.String())
			if got.Level != "debug" {
				t.Fatalf("%s during catch-up = level %q, want debug", operation, got.Level)
			}
		})
	}
}

// NOTHING IS SILENCED: the demoted record is still written in full.
func TestCatchupWindowStillWritesTheDemotedRecord(t *testing.T) {
	// Arrange.
	l, file := catchupSubject(t)

	// Act.
	l.With(Context{Operation: "boot-rewind", Path: "/corpus/a.jsonl"}).Log("rewound to the turn start")

	// Assert.
	got := decode(t, file.String())
	if got.Message != "rewound to the turn start" || got.Context["path"] != "/corpus/a.jsonl" {
		t.Fatalf("the demoted record lost its detail: %+v", got)
	}
}

// An operation the window does not name is untouched: the leveling is a named
// list, never a blanket quieting of the boot.
func TestCatchupWindowLeavesAnUnregisteredOperationAtInfo(t *testing.T) {
	// Arrange.
	l, file := catchupSubject(t)

	// Act.
	l.With(Context{Operation: "start"}).Log("sidecar starting")

	// Assert.
	if got := decode(t, file.String()).Level; got != "info" {
		t.Fatalf("unregistered operation = level %q, want info", got)
	}
}

// A WARNING RAISED DURING CATCH-UP IS STILL A WARNING. Only INFO is the boot
// walk restating itself; a conclusion the walk reaches is news at any hour.
func TestCatchupWindowKeepsAWarningAtWarn(t *testing.T) {
	// Arrange.
	l, file := catchupSubject(t)

	// Act.
	l.With(Context{Operation: "launch", Level: "warn"}).Log("a spawning call named no task")

	// Assert.
	if got := decode(t, file.String()).Level; got != "warn" {
		t.Fatalf("warning during catch-up = level %q, want warn", got)
	}
}

// The window's end states the totals, one summary per operation, so the owner
// still sees "6,172 boot-rewind records" without reading 6,172 lines.
func TestEndCatchupStatesOneSummaryCarryingTheCount(t *testing.T) {
	tests := []struct {
		name      string
		operation string
		items     int
	}{
		{name: "a single item", operation: "tail-pickup", items: 1},
		{name: "a corpus walk", operation: "boot-rewind", items: 2464},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			l, file := catchupSubject(t)
			for i := 0; i < test.items; i++ {
				l.With(Context{Operation: test.operation}).Log("one backlog item")
			}
			file.Reset()

			// Act.
			l.EndCatchup()

			// Assert.
			got := decode(t, file.String())
			if got.Operation != "catchup-summary" || got.Level != "info" {
				t.Fatalf("summary = operation %q level %q, want catchup-summary at info", got.Operation, got.Level)
			}
			if got.Context["reason"] != test.operation {
				t.Fatalf("summary reason = %v, want %q", got.Context["reason"], test.operation)
			}
			if count, _ := got.Context["repeat_count"].(float64); int(count) != test.items {
				t.Fatalf("summary repeat_count = %v, want %d", got.Context["repeat_count"], test.items)
			}
		})
	}
}

// AN EMPTY BACKLOG STATES NOTHING: an operation that demoted nothing gets no
// summary, so a boot with no corpus is silent rather than six zero lines.
func TestEndCatchupStatesNothingForAnOperationThatDemotedNothing(t *testing.T) {
	// Arrange.
	l, file := catchupSubject(t)
	l.With(Context{Operation: "boot-rewind"}).Log("one backlog item")
	file.Reset()

	// Act.
	l.EndCatchup()

	// Assert: exactly one summary, for the one operation that tallied.
	if lines := strings.Count(strings.TrimSpace(file.String()), "\n") + 1; lines != 1 {
		t.Fatalf("EndCatchup wrote %d line(s), want one summary: %q", lines, file.String())
	}
}

// STEADY STATE IS STATED PER ITEM. Once the window closes, the same operation
// is INFO per record exactly as before — the leveling is the boot, not a policy.
func TestAfterCatchupEndsTheSameOperationIsInfoAgain(t *testing.T) {
	// Arrange.
	l, file := catchupSubject(t)
	l.With(Context{Operation: "tail-pickup"}).Log("backlog")
	l.EndCatchup()
	file.Reset()

	// Act.
	l.With(Context{Operation: "tail-pickup"}).Log("picked up 1 record(s)")

	// Assert.
	if got := decode(t, file.String()).Level; got != "info" {
		t.Fatalf("post-catch-up record = level %q, want info", got)
	}
}

// Closing a window that was never opened is a no-op: a shutdown that races the
// first drained pass must not panic or invent a summary.
func TestEndCatchupOnAClosedWindowStatesNothing(t *testing.T) {
	// Arrange.
	l, _, file := sinks(t, true)

	// Act.
	l.EndCatchup()

	// Assert.
	if file.Len() != 0 {
		t.Fatalf("EndCatchup on a closed window wrote %q", file.String())
	}
}

// ---------------------------------------------------------------------------
// The bounded shutdown drain.
// ---------------------------------------------------------------------------

// A drain that cannot finish is ABANDONED AND STATED. This is the teardown that
// held a launchd stop past three minutes: the daemon is gone, the queue is
// deep, and the closing forward loop dials once per record forever.
func TestCloseWithinAbandonsAStuckDrainAndStatesWhatItWaitedOn(t *testing.T) {
	// Arrange: a forwarder that blocks inside Forward, and two queued records —
	// the first wedges the loop, the second is what is still pending.
	forwarder := &blockingForwarder{started: make(chan struct{}), release: make(chan struct{})}
	l, _, global := forwardingSinks(t, false, forwarder)
	scoped := Context{Operation: "tail-pickup", WorkspaceDir: "/w", WorkspaceID: "w1"}
	l.With(scoped).Log("first")
	<-forwarder.started
	l.With(scoped).Log("second")
	global.Reset()

	// Act.
	pending, drained := l.CloseWithin(20 * time.Millisecond)
	defer close(forwarder.release)

	// Assert.
	if drained {
		t.Fatal("CloseWithin reported a drained queue while the forward loop was wedged")
	}
	if pending != 1 {
		t.Fatalf("pending = %d, want the one record still queued", pending)
	}
	got := decode(t, global.String())
	if got.Operation != "shutdown-drain" || got.Level != "info" {
		t.Fatalf("abandonment record = operation %q level %q, want shutdown-drain at info", got.Operation, got.Level)
	}
	if !strings.Contains(got.Message, "1 record(s) still queued") {
		t.Fatalf("abandonment record does not name what it waited on: %q", got.Message)
	}
}

// The healthy teardown is unaffected: the queue drains, the bound never fires,
// and nothing is stated about it.
func TestCloseWithinDrainsAHealthyQueueSilently(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123"}
	l, _, global := forwardingSinks(t, false, forwarder)
	l.With(Context{Operation: "tail-pickup", WorkspaceDir: "/w", WorkspaceID: "w1"}).Log("first")
	global.Reset()

	// Act.
	pending, drained := l.CloseWithin(DefaultShutdownDrain)

	// Assert.
	if !drained || pending != 0 {
		t.Fatalf("CloseWithin(healthy) = (%d, %t), want (0, true)", pending, drained)
	}
	if global.Len() != 0 {
		t.Fatalf("a healthy drain stated %q, want silence", global.String())
	}
}
