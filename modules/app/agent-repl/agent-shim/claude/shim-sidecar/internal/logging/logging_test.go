package logging

import (
	"bytes"
	"encoding/json"
	"errors"
	"io"
	"os"
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

func TestMain(m *testing.M) {
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
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
	records []ForwardRecord
}

func (f *recordingForwarder) Forward(record ForwardRecord) (string, error) {
	f.records = append(f.records, record)
	return f.address, f.err
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
	return l, terminal, global
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

	// Assert.
	if len(forwarder.records) != 1 {
		t.Fatalf("forwarded records = %d, want 1", len(forwarder.records))
	}
	got := forwarder.records[0]
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

func TestGlobalRecordIsNotForwarded(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123"}
	l, _, global := forwardingSinks(t, false, forwarder)

	// Act.
	l.With(Context{Operation: "start"}).Log("sidecar starting")

	// Assert.
	if len(forwarder.records) != 0 {
		t.Fatalf("global record was forwarded: %+v", forwarder.records)
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

	// Assert.
	if len(forwarder.records) != 2 {
		t.Fatalf("forward attempts = %d, want both file records attempted", len(forwarder.records))
	}
	lines := strings.Split(strings.TrimSpace(global.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("global records = %d, want one forwarding failure plus the later lifecycle record: %q", len(lines), global.String())
	}
	if got := decode(t, lines[0]).Operation; got != "sidecar.logging.forward-failure" {
		t.Fatalf("first global operation = %q, want the forwarding failure", got)
	}
	if got := decode(t, lines[1]).Operation; got != "rescan" {
		t.Fatalf("second global operation = %q, want proof logging continued", got)
	}
}

func TestForwardFailureIsRecordedAgainAfterRecovery(t *testing.T) {
	// Arrange.
	forwarder := &recordingForwarder{address: "127.0.0.1:8123", err: errors.New("connection refused")}
	l, _, global := forwardingSinks(t, false, forwarder)
	file := Context{Operation: "poll", WorkspaceDir: "/work/repo", WorkspaceID: "deadbeef", ClaudeSessionID: "session-1"}

	// Act.
	l.With(file).Log("first outage")
	forwarder.err = nil
	l.With(file).Log("recovered")
	forwarder.err = errors.New("connection reset")
	l.With(file).Log("second outage")

	// Assert.
	lines := strings.Split(strings.TrimSpace(global.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("forwarding failure records = %d, want one per outage window: %q", len(lines), global.String())
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

	// Assert.
	got := forwarder.records[0]
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
