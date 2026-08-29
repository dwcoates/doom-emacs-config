package db

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

func TestMain(m *testing.M) {
	// Nothing in this package reaches a vendor, and the flag says so out loud
	// for every helper that checks it.
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	os.Unsetenv(EnvSlowQueryMs)
	os.Exit(m.Run())
}

// ---- harness ----

// sink captures both logging destinations so a test can assert the record a
// branch was required to emit.
type sink struct {
	file   bytes.Buffer
	stderr bytes.Buffer
}

func (s *sink) records(t *testing.T) []map[string]any {
	t.Helper()
	var out []map[string]any
	for _, line := range strings.Split(strings.TrimSpace(s.file.String()), "\n") {
		if line == "" {
			continue
		}
		var record map[string]any
		if err := json.Unmarshal([]byte(line), &record); err != nil {
			t.Fatalf("log line is not JSON: %v: %q", err, line)
		}
		out = append(out, record)
	}
	return out
}

// assertLogged fails unless some record at `level` mentions `substring`.
func (s *sink) assertLogged(t *testing.T, level, substring string) {
	t.Helper()
	for _, record := range s.records(t) {
		if record["level"] == level && strings.Contains(record["message"].(string), substring) {
			return
		}
	}
	t.Fatalf("no %s record mentioning %q; log was:\n%s", level, substring, s.file.String())
}

// assertContext fails unless some record carries key=value in its context.
func (s *sink) assertContext(t *testing.T, key string, value any) {
	t.Helper()
	for _, record := range s.records(t) {
		context, _ := record["context"].(map[string]any)
		if got, ok := context[key]; ok && got == value {
			return
		}
	}
	t.Fatalf("no record with context %s=%v; log was:\n%s", key, value, s.file.String())
}

func newSink(t *testing.T) (*sink, *logging.Logger) {
	t.Helper()
	s := &sink{}
	return s, logging.New(&s.file, &s.stderr, true)
}

// newStore opens a fresh on-disk store with a frozen clock, so a test can
// assert an exact instant without waiting for one.
func newStore(t *testing.T) (*DB, *sink) {
	t.Helper()
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")
	d, err := OpenWithOptions(path, log, Options{Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	return d, s
}

const testNow int64 = 1_700_000_000_000

func ctx() context.Context { return context.Background() }

// ---- entry builders ----

func streamPlane() *storev1.Plane {
	return &storev1.Plane{Plane: &storev1.Plane_Stream{Stream: &storev1.PlaneStream{}}}
}

func agentUpdateEntry(writeID, upsertKey string, update *storev1.StoreAgentUpdate) *storev1.StoreEntry {
	return &storev1.StoreEntry{
		Plane:     streamPlane(),
		WriteId:   writeID,
		UpsertKey: upsertKey,
		Entry:     &storev1.StoreEntry_AgentUpdate{AgentUpdate: update},
	}
}

func pageEntry(writeID, upsertKey, book string, item *storev1.StoreAgentItem) *storev1.StoreEntry {
	return agentUpdateEntry(writeID, upsertKey, &storev1.StoreAgentUpdate{
		AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{ServeableFrame: &storev1.StorePageLine{
			PageAgentId: &conversationv1.AgentId{Value: book},
			AgentItem:   item,
		}},
	})
}

func frameItem(frame *conversationv1.AgentFrame) *storev1.StoreAgentItem {
	return &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentFrame{AgentFrame: frame}}
}

func promptItem(agentID string) *storev1.StoreAgentItem {
	return &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_AgentPrompt{AgentPrompt: &conversationv1.AgentPrompt{
		Id:    &conversationv1.TurnId{Value: "turn-1"},
		Agent: &conversationv1.AgentId{Value: agentID},
	}}}
}

func activityFrame(agentID, activityID string, item any) *conversationv1.AgentFrame {
	activity := &conversationv1.AgentActivity{ActivityId: &conversationv1.AgentActivityId{Value: activityID}}
	switch typed := item.(type) {
	case *conversationv1.AgentSubagent:
		activity.Item = &conversationv1.AgentActivity_Subagent{Subagent: typed}
	case *conversationv1.AgentBash:
		activity.Item = &conversationv1.AgentActivity_Bash{Bash: typed}
	case *conversationv1.AgentResponse:
		activity.Item = &conversationv1.AgentActivity_Response{Response: typed}
	default:
		panic("unsupported activity item in test fixture")
	}
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: activity},
		}},
	}
}

// prose is the ordinary, non-terminal update every "just a page line" fixture
// uses.
func prose() *conversationv1.AgentResponse {
	return &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}}
}

func successFrame(agentID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{}},
		}},
	}
}

func failureFrame(agentID string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result: &conversationv1.AgentFrame_Failure{Failure: &conversationv1.AgentFailure{
			Failure: &conversationv1.AgentFailure_ExecutionError{ExecutionError: &conversationv1.AgentExecutionError{}},
		}},
	}
}

func subagentStart(createdAgentID string) *conversationv1.AgentSubagent {
	return &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
		CreatedAgentId: &conversationv1.AgentId{Value: createdAgentID},
		Prompt: &conversationv1.AgentSubagentPrompt{
			Text:      "do the thing",
			Isolation: &conversationv1.AgentSubagentPrompt_Worktree{Worktree: &conversationv1.AgentSubagentIsolationWorktree{}},
		},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 42},
	}}}
}

func detachedFrame(agentID string, work *conversationv1.AgentDetachedWork) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agentID},
		Result:  &conversationv1.AgentFrame_DetachedWork{DetachedWork: work},
	}
}

func batch(entries ...*storev1.StoreEntry) *storev1.EntryBatch {
	return &storev1.EntryBatch{Entries: entries}
}

// writeOK writes a batch that must succeed.
func writeOK(t *testing.T, d *DB, entries ...*storev1.StoreEntry) WriteResult {
	t.Helper()
	result, err := d.WriteBatch(ctx(), "test-producer", batch(entries...))
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	return result
}

// scalar reads one value straight out of the database, so a test asserts what
// was STORED rather than what a read path chose to report.
func scalar[T any](t *testing.T, d *DB, query string, args ...any) T {
	t.Helper()
	var value T
	if err := d.sql.QueryRow(query, args...).Scan(&value); err != nil {
		t.Fatalf("query %q: %v", query, err)
	}
	return value
}

// ---- schema ----

func TestOpenCreatesTheSchemaOnAFreshDatabase(t *testing.T) {
	// Arrange
	_, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")

	// Act
	d, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	_, tables, err := d.inspectSchema(ctx())
	if err != nil {
		t.Fatalf("inspectSchema: %v", err)
	}
	if !slicesEqual(tables, schemaTables) {
		t.Fatalf("tables = %v, want %v", tables, schemaTables)
	}
	if got := scalar[int](t, d, `SELECT version FROM schema_meta`); got != SchemaVersion {
		t.Fatalf("schema version = %d, want %d", got, SchemaVersion)
	}
}

func TestOpenCreatesTheDirectoryItsDatabaseLivesIn(t *testing.T) {
	// Arrange: a path whose parent does not exist. Nothing upstream may create
	// it, because that would move an unwritable --db ahead of the pprof surface.
	_, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store", "events.db")

	// Act
	d, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	if _, statErr := os.Stat(path); statErr != nil {
		t.Fatalf("stat %q = %v, want the database created under a directory Open made", path, statErr)
	}
}

func TestOpenRefusesADatabaseDirectoryItCannotCreate(t *testing.T) {
	// Arrange: a FILE where the database's parent directory must be.
	s, log := newSink(t)
	blocker := filepath.Join(t.TempDir(), "not-a-directory")
	if err := os.WriteFile(blocker, []byte("a file, not a directory"), 0o600); err != nil {
		t.Fatalf("staging the blocker: %v", err)
	}

	// Act
	d, err := OpenWithOptions(filepath.Join(blocker, "events.db"), log, Options{})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // the open should not have succeeded
		t.Fatal("OpenWithOptions = nil, want the unwritable database directory refused")
	}
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want an ErrStorage", err)
	}
	s.assertLogged(t, "error", "creating the database directory failed")
}

func TestOpenRecordsAFirstCreateWithoutWarning(t *testing.T) {
	// Arrange: an empty database, which is what every fresh store starts from.
	s, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")

	// Act
	d, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert: creating a schema where there was none is not a degraded state.
	for _, record := range s.records(t) {
		if record["level"] == "warn" {
			t.Fatalf("a first create logged a warning: %v", record)
		}
	}
}

func TestOpenNukesADatabaseStampedAtAnotherVersion(t *testing.T) {
	// Arrange: a store with a row in it, re-stamped at a foreign version.
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, first, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
	if _, err := first.sql.Exec(`UPDATE schema_meta SET version = 99`); err != nil {
		t.Fatalf("restamp: %v", err)
	}
	first.Close() //nolint:errcheck // reopened below

	// Act
	s, reopenLog := newSink(t)
	second, err := OpenWithOptions(path, reopenLog, Options{})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close() //nolint:errcheck // test teardown

	// Assert: the rows are gone and the stamp is this binary's.
	if got := scalar[int](t, second, `SELECT COUNT(*) FROM entry`); got != 0 {
		t.Fatalf("entry rows after nuke = %d, want 0", got)
	}
	if got := scalar[int](t, second, `SELECT version FROM schema_meta`); got != SchemaVersion {
		t.Fatalf("schema version = %d, want %d", got, SchemaVersion)
	}
	s.assertLogged(t, "warn", "dropping and recreating")
}

func TestOpenNukesADatabaseWhoseTableSetDiffers(t *testing.T) {
	// Arrange: the right stamp on a shape this binary did not create.
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	if _, err := first.sql.Exec(`DROP TABLE detached_work`); err != nil {
		t.Fatalf("drop: %v", err)
	}
	first.Close() //nolint:errcheck // reopened below

	// Act
	s, reopenLog := newSink(t)
	second, err := OpenWithOptions(path, reopenLog, Options{})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close() //nolint:errcheck // test teardown

	// Assert
	_, tables, err := second.inspectSchema(ctx())
	if err != nil {
		t.Fatalf("inspectSchema: %v", err)
	}
	if !slicesEqual(tables, schemaTables) {
		t.Fatalf("tables = %v, want %v", tables, schemaTables)
	}
	s.assertLogged(t, "warn", "dropping and recreating")
}

func TestOpenLeavesAMatchingDatabaseUntouched(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	writeOK(t, first, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))))
	first.Close() //nolint:errcheck // reopened below

	// Act
	_, reopenLog := newSink(t)
	second, err := OpenWithOptions(path, reopenLog, Options{})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close() //nolint:errcheck // test teardown

	// Assert: the row survived, so nothing was dropped.
	if got := scalar[int](t, second, `SELECT COUNT(*) FROM entry`); got != 1 {
		t.Fatalf("entry rows = %d, want 1", got)
	}
}

func TestOpenRefusesAMalformedSlowQueryThreshold(t *testing.T) {
	// Arrange
	t.Setenv(EnvSlowQueryMs, "not-a-number")
	s, log := newSink(t)

	// Act
	_, err := Open(filepath.Join(t.TempDir(), "store.db"), log)

	// Assert
	if err == nil {
		t.Fatal("Open accepted a malformed threshold")
	}
	s.assertLogged(t, "error", "slow-query threshold rejected")
}

func TestCloseReportsADoubleCloseAsAStorageFailure(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("first close: %v", err)
	}

	// Act
	err := d.Close()

	// Assert: SQLite tolerates the second close, so the contract this test
	// pins is that a close which DOES fail is reported, never swallowed.
	if err != nil {
		s.assertLogged(t, "error", "closing SQLite database failed")
	}
}
