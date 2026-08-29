package harness

import (
	"bufio"
	"encoding/json"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	workspacev1 "agentrepl/proto/workspace/v1"
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
		case <-d.ctx.Done():
			d.t.Fatalf("waiting for %s in %s: %v", what, path, d.ctx.Err())
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
// intends to produce. Anything else at that level fails the test at cleanup,
// which is what drives the daemon's warning count to zero.
func (d *Daemon) ExpectWarnings(operations ...string) {
	d.t.Helper()
	d.mu.Lock()
	first := len(d.expected) == 0
	for _, op := range operations {
		d.expected[op] = true
	}
	d.mu.Unlock()
	if first {
		d.t.Cleanup(d.assertNoUnexpectedWarnings)
	}
}

// AllowAllWarnings is the escape hatch for the tests whose subject IS the
// daemon's own loud failure and whose operation names are not yet knowable.
const AllowAllWarnings = "*"

func (d *Daemon) assertNoUnexpectedWarnings() {
	d.mu.Lock()
	expected := make(map[string]bool, len(d.expected))
	for k, v := range d.expected {
		expected[k] = v
	}
	d.mu.Unlock()
	if expected[AllowAllWarnings] {
		return
	}

	var unexpected []LogRecord
	for _, r := range d.RunLog() {
		if warningLevels[strings.ToLower(r.Level)] && !expected[r.Operation] {
			unexpected = append(unexpected, r)
		}
	}
	for _, dir := range d.watchedWorkspaceDirs() {
		for _, r := range d.WorkspaceLog(dir, "daemon") {
			if warningLevels[strings.ToLower(r.Level)] && !expected[r.Operation] {
				unexpected = append(unexpected, r)
			}
		}
	}
	if len(unexpected) == 0 {
		return
	}
	var b strings.Builder
	for _, r := range unexpected {
		b.WriteString("\n  " + r.Level + " " + r.Operation + ": " + r.Message)
	}
	d.t.Errorf("the daemon produced %d unexpected warning records; declare them with ExpectWarnings if they are intended:%s", len(unexpected), b.String())
}

// WatchWorkspaceLogs adds a workspace's own log sink to the warning sweep.
func (d *Daemon) WatchWorkspaceLogs(dir string) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.workspaceDirs = append(d.workspaceDirs, dir)
}

func (d *Daemon) watchedWorkspaceDirs() []string {
	d.mu.Lock()
	defer d.mu.Unlock()
	out := make([]string, len(d.workspaceDirs))
	copy(out, d.workspaceDirs)
	return out
}

// ClientLogPath is the sink ClientLog persists a webview's records to.
func ClientLogPath(ws *workspacev1.WorkspaceRef) string {
	return WorkspaceLogPath(ws.GetDir(), "webapp")
}
