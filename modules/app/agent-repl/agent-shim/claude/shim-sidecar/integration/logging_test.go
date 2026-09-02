package integration

import (
	"strings"
	"testing"
	"time"

	sharedlogging "agentrepl/logging"
)

// SUBJECT 11 — the sidecar's log is a contract, not a convenience.
//
// It is strict JSONL; the green path carries zero errors; and every write record
// carries the correlation vocabulary the integration loop reads back
// (file_id, path, offset, write_id, agent_id).

// ingestGreenPath drives one clean ingest of the captured transcript and answers
// the log it produced.
func ingestGreenPath(t *testing.T) []logRecord {
	t.Helper()
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)
	opts.ExtraEnv = []string{"AGENT_REPL_LOG_VERBOSE=1"}

	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())
	return readLog(t, opts.LogPath)
}

// TestTheLogIsStrictJsonl asserts every emitted line parses as a JSON object.
// readLog fails the test on the first line that does not.
func TestTheLogIsStrictJsonl(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	if len(records) == 0 {
		t.Fatalf("a whole ingest emitted no log records at all")
	}
}

// TestEveryRecordCarriesTheRequiredFields asserts the contract's required
// fields are present on every record.
func TestEveryRecordCarriesTheRequiredFields(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	for i, r := range records {
		if r.Timestamp == "" {
			t.Errorf("log record %d carries no timestamp", i)
		}
		// The RUNTIME is what separates these records from the shim's and the
		// store's when all three are read together, so it is checked for its
		// VALUE rather than for being non-empty.
		if r.Runtime != "sidecar" {
			t.Errorf("log record %d names runtime %q, want the sidecar's own runtime", i, r.Runtime)
		}
		if r.PID == 0 {
			t.Errorf("log record %d carries no pid", i)
		}
		if r.Level == "" {
			t.Errorf("log record %d carries no level", i)
		}
		// VERBOSITY is what says whether a record is lifecycle or per-record
		// chatter; a record without it cannot be filtered down to the spine.
		switch r.Verbosity {
		case "normal", "verbose":
		default:
			t.Errorf("log record %d carries verbosity %q, want normal or verbose", i, r.Verbosity)
		}
		if r.Operation == "" {
			t.Errorf("log record %d carries no operation", i)
		}
		if r.Message == "" {
			t.Errorf("log record %d carries no message", i)
		}
		// The CONTEXT object is always present, even when a site owns no
		// correlation fact: a reader joining on keys must be able to look one up
		// without first testing whether the object exists.
		if r.Context == nil {
			t.Errorf("log record %d carries no context object", i)
		}
	}
}

// TestEveryTimestampIsTheContractsRendering asserts the one rendering every
// agent-repl runtime writes: RFC 3339, local zone, fixed-width microseconds and
// an explicit numeric offset.
//
// Go's own RFC3339Nano drops trailing zeros, which sorts a record landing on a
// whole second out of order against its neighbors — and these records are read
// interleaved with the shim's and the store's, so a divergence here is a
// timeline nobody can merge.
func TestEveryTimestampIsTheContractsRendering(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	for i, r := range records {
		if _, err := time.Parse(sharedlogging.TimestampLayout, r.Timestamp); err != nil {
			t.Errorf("log record %d carries timestamp %q, which is not the contract's layout %q: %v",
				i, r.Timestamp, sharedlogging.TimestampLayout, err)
		}
	}
}

// TestTheGreenPathLogsNoErrors asserts a clean ingest of a real capture never
// reaches an error branch.
func TestTheGreenPathLogsNoErrors(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	errs := logsAtLevel(records, "error")
	for _, r := range errs {
		t.Errorf("green-path error record: op=%q message=%q context=%v", r.Operation, r.Message, r.Context)
	}
	if len(errs) != 0 {
		t.Fatalf("a clean ingest of a real capture produced %d error records", len(errs))
	}
}

// TestWriteRecordsCarryTheirCorrelationKeys asserts a record that reports a
// write names the file, the position and the write it made.
func TestWriteRecordsCarryTheirCorrelationKeys(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	writes := logsWithContextKey(records, "write_id")
	if len(writes) == 0 {
		t.Fatalf("no log record named a write_id; the write path must be correlatable")
	}
	for _, r := range writes {
		for _, key := range []string{"path", "offset", "file_id"} {
			if _, ok := r.Context[key]; !ok {
				t.Errorf("write record %q carries no %q; its context was %v", r.Message, key, r.Context)
			}
		}
	}
}

// TestPageLineRecordsNameTheirAgent asserts agent_id is a dedicated context key
// rather than prose inside the message.
func TestPageLineRecordsNameTheirAgent(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	if len(logsWithContextKey(records, "agent_id")) == 0 {
		t.Fatalf("no log record named an agent_id; the agent-keyed spine must be correlatable")
	}
}

// TestRetiredCorrelationKeysAreGone asserts the old (session_id, seq) addressing
// vocabulary died with the addressing it named.
func TestRetiredCorrelationKeysAreGone(t *testing.T) {
	// Arrange + Act.
	records := ingestGreenPath(t)

	// Assert.
	retired := []string{"claude_session_id", "seq", "from_seq", "replay_from_seq", "replay_through_seq"}
	for _, r := range records {
		for _, key := range retired {
			if _, ok := r.Context[key]; ok {
				t.Errorf("record %q carries retired correlation key %q", r.Message, key)
			}
		}
	}
	for _, r := range records {
		if strings.Contains(r.Message, "claude_session_id") {
			t.Errorf("record %q names a retired key in its message text", r.Message)
		}
	}
}

// TestHotDiagnosticsRideTheVerboseHelper asserts per-record chatter is gated:
// with AGENT_REPL_LOG_VERBOSE unset, the log is lifecycle-sized rather than
// one record per converted line.
func TestHotDiagnosticsRideTheVerboseHelper(t *testing.T) {
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	captured := loadCapturedSession(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act: no AGENT_REPL_LOG_VERBOSE in the environment.
	startSidecar(t, opts)
	g := newGrowingFile(t, tree.sessionPath(captured.Slug, captured.Session))
	for _, line := range captured.Lines {
		g.AppendLine(line)
	}
	awaitCursorInBatches(ctx, t, fake, g.Path(), g.Offset())

	// Assert.
	for _, r := range readLog(t, opts.LogPath) {
		if r.Verbosity == "verbose" {
			t.Errorf("a verbose record reached the durable log with AGENT_REPL_LOG_VERBOSE unset: %q", r.Message)
		}
	}
}
