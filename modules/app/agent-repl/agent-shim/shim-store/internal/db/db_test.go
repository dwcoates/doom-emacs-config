package db

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/logging"
)

func TestMain(m *testing.M) {
	// Nothing in this package reaches a vendor, and the flag says so out loud
	// for every helper that checks it.
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	if err := os.Unsetenv(EnvSlowQueryMs); err != nil {
		panic(err)
	}
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

// recordsAtLevel returns every message logged at `level`, for an assertion
// about what was NOT said.
func recordsAtLevel(t *testing.T, s *sink, level string) []string {
	t.Helper()
	var out []string
	for _, record := range s.records(t) {
		if record["level"] == level {
			message, _ := record["message"].(string)
			out = append(out, message)
		}
	}
	return out
}

// assertTracedRefusal is the assertion for a REFUSED REQUEST: this layer traces
// it at verbose with its statement and table, and writes NO normal-level record
// for it, because the single normal-level record belongs to the server — the
// only layer that knows the rpc, the request id and the producer the refusal
// belongs to. Two normal-level records on one refusal is what "logged exactly
// once" exists to prevent.
func (s *sink) assertTracedRefusal(t *testing.T, substring string) {
	t.Helper()
	traced := false
	for _, record := range s.records(t) {
		message, _ := record["message"].(string)
		if !strings.Contains(message, substring) {
			continue
		}
		if record["verbosity"] == "verbose" {
			traced = true
			continue
		}
		t.Fatalf("a refused request wrote a normal-level record; the server owns that record: %v\nlog was:\n%s", record, s.file.String())
	}
	if !traced {
		t.Fatalf("no verbose trace mentioning %q; log was:\n%s", substring, s.file.String())
	}
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

// newStoreWithClock opens a fresh on-disk store whose clock is the caller's, so
// a test can advance it between writes and assert an ORDER rather than an
// instant.
func newStoreWithClock(t *testing.T, now func() int64) *DB {
	t.Helper()
	_, log := newSink(t)
	path := filepath.Join(t.TempDir(), "store.db")
	d, err := OpenWithOptions(path, log, Options{Now: now})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	t.Cleanup(func() { d.Close() }) //nolint:errcheck // best-effort test teardown
	return d
}

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

func peerItem(agentID string) *storev1.StoreAgentItem {
	return &storev1.StoreAgentItem{Item: &storev1.StoreAgentItem_PeerMessage{PeerMessage: &conversationv1.PeerMessage{
		Agent:  &conversationv1.AgentId{Value: agentID},
		Sender: "Explore",
		Body:   "hi",
		Id:     "u1",
	}}}
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

// proseSaying is a SETTLED response whose markdown is the row's content, so a
// test can tell one write of a unit from a later write of the same unit.
func proseSaying(markdown string) *conversationv1.AgentResponse {
	return &conversationv1.AgentResponse{
		Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{
				Prose:      &conversationv1.AgentResponseProse{Markdown: markdown},
				Authorship: &conversationv1.AgentResponseSuccess_FromModel{FromModel: &conversationv1.AgentResponseFromModel{}},
			},
		},
	}
}

// proseSettledAt is a SETTLED response carrying its settle instant, so a test
// can assert the instant survives the store's write → read round trip. atMs of
// 0 leaves settled_at UNSET, the genuine "the producer observed no instant"
// case the daemon legitimately falls back to Now() for.
func proseSettledAt(markdown string, atMs int64) *conversationv1.AgentResponse {
	success := &conversationv1.AgentResponseSuccess{
		Prose:      &conversationv1.AgentResponseProse{Markdown: markdown},
		Authorship: &conversationv1.AgentResponseSuccess_FromModel{FromModel: &conversationv1.AgentResponseFromModel{}},
	}
	if atMs != 0 {
		success.SettledAt = &conversationv1.AgentActivitySettledAt{AtMs: atMs}
	}
	return &conversationv1.AgentResponse{
		Result: &conversationv1.AgentResponse_Success{Success: success},
	}
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
	return subagentStartAt(createdAgentID, 42)
}

// subagentStartAt is subagentStart with the PRODUCER's start instant chosen by
// the caller — the shim's Date.now() on the stream plane, the vendor's
// transcript timestamp on the file plane, and 0 when that timestamp was missing
// or unparseable.
func subagentStartAt(createdAgentID string, atMs int64) *conversationv1.AgentSubagent {
	return &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
		CreatedAgentId: &conversationv1.AgentId{Value: createdAgentID},
		Prompt: &conversationv1.AgentSubagentPrompt{
			Text:      "do the thing",
			Isolation: &conversationv1.AgentSubagentPrompt_Worktree{Worktree: &conversationv1.AgentSubagentIsolationWorktree{}},
		},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: atMs},
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
	result, err := d.WriteBatch(ctx(), "test-producer", WriteInteractive, batch(entries...), nil)
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

// fileIdentity is the inode a path names, so a test can tell a file that was
// UNLINKED and recreated from one that was edited in place. Content and mtime
// cannot: a recreated database and a dropped-and-recreated one look identical
// by both.
func fileIdentity(t *testing.T, path string) uint64 {
	t.Helper()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat %q: %v", path, err)
	}
	stat, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		t.Fatalf("stat %q carries no inode", path)
	}
	return uint64(stat.Ino)
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
	// Arrange: a store with a row in it, re-stamped at a version ABOVE this
	// binary's — a database only a newer store could have written.
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
	s.assertLogged(t, "error", "removing the database file and its WAL siblings and recreating")
}

func TestOpenRecordsASupersededSchemaVersionAtInfo(t *testing.T) {
	// Arrange: a database stamped one version BELOW this binary's, which is
	// what every deploy that bumped the schema meets. Recreating it is the
	// store's documented convention (nuked, never migrated), so the record
	// carries the fact and not a fault.
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	if _, err := first.sql.Exec(fmt.Sprintf(`UPDATE schema_meta SET version = %d`, SchemaVersion-1)); err != nil {
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

	// Assert
	s.assertLogged(t, "info", "the on-disk schema is a superseded version")
	if warned := recordsAtLevel(t, s, "warn"); len(warned) != 0 {
		t.Fatalf("a superseded schema logged %d warning(s), want none: %v", len(warned), warned)
	}
}

func TestOpenNamesBothVersionsWhenItReplacesASupersededSchema(t *testing.T) {
	// Arrange: the operator's question at a nuke is "from what, to what". The
	// live record said neither until it did (found version=5, want version=6).
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	if _, err := first.sql.Exec(fmt.Sprintf(`UPDATE schema_meta SET version = %d`, SchemaVersion-1)); err != nil {
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

	// Assert
	s.assertLogged(t, "info", fmt.Sprintf("found version=%d", SchemaVersion-1))
	s.assertLogged(t, "info", fmt.Sprintf("want version=%d", SchemaVersion))
}

func TestOpenReportsADatabaseCarryingNoSchemaStampAsAnError(t *testing.T) {
	// Arrange: tables, but no `schema_meta` — version 0. That is somebody
	// else's file at the store's path, never a version of ours this binary
	// superseded, so it is discarded loudly rather than as a routine bump.
	path := filepath.Join(t.TempDir(), "store.db")
	staged, err := sql.Open("sqlite", "file:"+path)
	if err != nil {
		t.Fatalf("staging a foreign database: %v", err)
	}
	if _, err := staged.Exec(`CREATE TABLE somebody_elses (id INTEGER)`); err != nil {
		t.Fatalf("staging a foreign table: %v", err)
	}
	staged.Close() //nolint:errcheck // staged and done
	s, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert
	s.assertLogged(t, "error", "is not a version it superseded")
}

func TestOpenRemovesTheFileOfADatabaseStampedAtAnotherVersion(t *testing.T) {
	// Arrange: a foreign shape is DISCARDED BY UNLINK, never emptied in place.
	// DROP TABLE walks every page of what it discards, and on a multi-gigabyte
	// events.db that is minutes of boot with no socket — long enough that a
	// deploy waiting on the socket gives up and leaves the stack half-bounced.
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	if _, err := first.sql.Exec(`UPDATE schema_meta SET version = 99`); err != nil {
		t.Fatalf("restamp: %v", err)
	}
	first.Close() //nolint:errcheck // reopened below
	before := fileIdentity(t, path)

	// Act
	_, reopenLog := newSink(t)
	second, err := OpenWithOptions(path, reopenLog, Options{})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close() //nolint:errcheck // test teardown

	// Assert: a DIFFERENT file stands where the old one did.
	if after := fileIdentity(t, path); after == before {
		t.Fatalf("the superseded database file survived the nuke (identity %v unchanged), want it removed and recreated", after)
	}
}

func TestOpenRemovesTheWalSiblingsOfADatabaseStampedAtAnotherVersion(t *testing.T) {
	// Arrange: a -wal or -shm left beside a recreated database belongs to the
	// file that was just discarded, and SQLite meeting it on the next open is
	// how a "fresh" store comes up carrying fragments of the one it replaced.
	dir := t.TempDir()
	path := filepath.Join(dir, "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	if _, err := first.sql.Exec(`UPDATE schema_meta SET version = 99`); err != nil {
		t.Fatalf("restamp: %v", err)
	}
	first.Close() //nolint:errcheck // reopened below
	for _, sibling := range []string{path + "-wal", path + "-shm"} {
		if err := os.WriteFile(sibling, []byte("stale sibling bytes"), 0o600); err != nil {
			t.Fatalf("staging %q: %v", sibling, err)
		}
	}

	// Act
	_, reopenLog := newSink(t)
	second, err := OpenWithOptions(path, reopenLog, Options{})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close() //nolint:errcheck // test teardown

	// Assert: a live store writes its own -wal, so the assertion is on the
	// STAGED bytes rather than on the paths existing at all.
	for _, sibling := range []string{path + "-wal", path + "-shm"} {
		data, readErr := os.ReadFile(sibling)
		if readErr != nil {
			continue
		}
		if string(data) == "stale sibling bytes" {
			t.Errorf("the nuke left the stale sibling %q behind", sibling)
		}
	}
}

func TestOpenSurfacesAFailureToRemoveTheDatabaseItMustDiscard(t *testing.T) {
	// Arrange: a database this binary must replace, in a directory nothing may
	// be unlinked from. A store that cannot discard the file in its way has no
	// database at all, so the boot fails rather than continuing over it.
	dir := t.TempDir()
	path := filepath.Join(dir, "store.db")
	staged, err := sql.Open("sqlite", "file:"+path)
	if err != nil {
		t.Fatalf("staging a foreign database: %v", err)
	}
	if _, err := staged.Exec(`CREATE TABLE schema_meta (version INTEGER NOT NULL);
	  INSERT INTO schema_meta(version) VALUES (99);`); err != nil {
		t.Fatalf("staging a foreign schema: %v", err)
	}
	staged.Close() //nolint:errcheck // staged and done
	if err := os.Chmod(dir, 0o500); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o700) })
	s, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // the open should not have succeeded
		t.Fatal("OpenWithOptions = nil, want the un-removable database surfaced")
	}
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want an ErrStorage", err)
	}
	s.assertLogged(t, "error", "removing the superseded database failed")
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
	s.assertLogged(t, "error", "removing the database file and its WAL siblings and recreating")
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

	// Assert: the row survived, so nothing was discarded.
	if got := scalar[int](t, second, `SELECT COUNT(*) FROM entry`); got != 1 {
		t.Fatalf("entry rows = %d, want 1", got)
	}
}

func TestOpenLeavesTheFileOfAMatchingDatabaseInPlace(t *testing.T) {
	// Arrange: the nuke unlinks. A database this binary DID create must never
	// meet it, so the file a matching reopen serves is the same file.
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	first.Close() //nolint:errcheck // reopened below
	before := fileIdentity(t, path)

	// Act
	_, reopenLog := newSink(t)
	second, err := OpenWithOptions(path, reopenLog, Options{})
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer second.Close() //nolint:errcheck // test teardown

	// Assert
	if after := fileIdentity(t, path); after != before {
		t.Fatalf("file identity = %v, want the matching database's own file %v left in place", after, before)
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

func TestOpenNukesAFileThatIsNotADatabaseAtAll(t *testing.T) {
	// Arrange: a truncated copy, a half-written file, somebody's notes. A --db
	// path this binary cannot read is the SAME situation as a schema it did not
	// create, and refusing to boot would wedge the service on bytes nobody can
	// read.
	path := filepath.Join(t.TempDir(), "store.db")
	if err := os.WriteFile(path, []byte("this is not a SQLite database"), 0o600); err != nil {
		t.Fatalf("stage garbage: %v", err)
	}
	s, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{})

	// Assert
	if err != nil {
		t.Fatalf("OpenWithOptions over a garbage file = %v, want the file removed and recreated", err)
	}
	defer d.Close() //nolint:errcheck // test teardown
	if got := scalar[int](t, d, `SELECT version FROM schema_meta`); got != SchemaVersion {
		t.Fatalf("schema version = %d, want %d", got, SchemaVersion)
	}
	s.assertLogged(t, "error", "cannot be read by this binary")
}

func TestOpenRemovesTheWalSiblingsOfAnUnreadableDatabase(t *testing.T) {
	// Arrange: SQLite opening a fresh database beside a stale WAL is how a
	// "recreated" store comes up carrying fragments of the one it replaced.
	dir := t.TempDir()
	path := filepath.Join(dir, "store.db")
	for _, name := range []string{path, path + "-wal", path + "-shm"} {
		if err := os.WriteFile(name, []byte("garbage"), 0o600); err != nil {
			t.Fatalf("stage %q: %v", name, err)
		}
	}
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	defer d.Close() //nolint:errcheck // test teardown

	// Assert: whatever WAL files exist now are this database's own, not the
	// staged bytes.
	if data, readErr := os.ReadFile(path + "-wal"); readErr == nil && string(data) == "garbage" {
		t.Fatal("the stale -wal survived the recreate")
	}
	if data, readErr := os.ReadFile(path + "-shm"); readErr == nil && string(data) == "garbage" {
		t.Fatal("the stale -shm survived the recreate")
	}
}

func TestOpenStillFailsWhenTheRecreateItselfCannotSucceed(t *testing.T) {
	// Arrange: the nuke happens ONCE. A second failure after a clean recreate is
	// a real problem — an unwritable directory, a full disk — and is returned
	// rather than retried forever.
	dir := t.TempDir()
	path := filepath.Join(dir, "store.db")
	if err := os.WriteFile(path, []byte("not a database"), 0o600); err != nil {
		t.Fatalf("stage garbage: %v", err)
	}
	if err := os.Chmod(dir, 0o500); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(dir, 0o700) })
	_, log := newSink(t)

	// Act
	d, err := OpenWithOptions(path, log, Options{})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // test teardown
		t.Fatal("OpenWithOptions succeeded on a read-only directory holding a garbage file")
	}
}

func TestOpenStatesTheFailureToRecreateAfterTheSupersededFileIsGone(t *testing.T) {
	// Arrange: a superseded database that is removed successfully, and a
	// recreate that then fails. At that point there is no database at all, so
	// the failure is stated here rather than left to whatever the caller does
	// with the error.
	path := filepath.Join(t.TempDir(), "store.db")
	_, log := newSink(t)
	first, err := OpenWithOptions(path, log, Options{})
	if err != nil {
		t.Fatalf("first open: %v", err)
	}
	if _, err := first.sql.Exec(fmt.Sprintf(`UPDATE schema_meta SET version = %d`, SchemaVersion-1)); err != nil {
		t.Fatalf("restamp: %v", err)
	}
	first.Close() //nolint:errcheck // reopened below
	previous := reopenAfterNuke
	t.Cleanup(func() { reopenAfterNuke = previous })
	reopenAfterNuke = func(string, string, string, *logging.Logger, Options, func() int64) (*DB, error) {
		return nil, errors.New("no space left on device")
	}
	s, reopenLog := newSink(t)

	// Act
	d, err := OpenWithOptions(path, reopenLog, Options{})

	// Assert
	if err == nil {
		d.Close() //nolint:errcheck // the open should not have succeeded
		t.Fatal("OpenWithOptions succeeded though the recreate failed")
	}
	s.assertLogged(t, "error", "could not be recreated at version=")
}

// ---- the synthetic corpus a latency bound is measured against ----

// syntheticCorpusRows is how big the corpus a latency bound is proved against
// is: the owner's store carried 225k `entry` rows and 318k `write_ledger` rows
// when a 30-row batch was reported over budget, and a bound proved on a corpus
// SMALLER than the one that produced the report proves nothing about it. 600k
// of each is comfortably past what the box has ever held.
const syntheticCorpusRows = 600_000

// syntheticCorpusFiles is how many source files the corpus is spread across.
// It is the store's own shape: the owner's `cursor` table held 2844 rows, and
// the number matters because the ledger's retention window is a per-FILE
// question — the sweep's plan is driven by this table, so a corpus with one
// file would flatter it.
const syntheticCorpusFiles = 2800

// syntheticCorpusOffsetStep is how far apart two consecutive ledger rows of one
// file sit. A retention window expressed as a multiple of it is how a test says
// "only the oldest N writes per file are prunable".
const syntheticCorpusOffsetStep = 4096

// seedSyntheticCorpus fills `entry`, `write_ledger` and `cursor` with a corpus
// the size of a real one, DIRECTLY rather than through WriteBatch.
//
// THE POINT IS THE INDEX DEPTH THE MEASURED STATEMENT SEES, not how the rows
// got there — every other test in this package writes through the real path,
// and driving 600k rows through it would cost 20k transactions to arrive at
// exactly the same b-trees. The rows are inserted from a recursive CTE in one
// transaction so the fixture costs seconds rather than minutes.
//
// Ledger offsets ascend per file, so `spread` is how far the newest row of a
// file sits above its oldest: a retention window narrower than the spread makes
// the older rows prunable, which is what gives a sweep real work to do.
func seedSyntheticCorpus(t *testing.T, d *DB) (spread int64) {
	t.Helper()
	const offsetStep = syntheticCorpusOffsetStep
	spread = int64(syntheticCorpusRows/syntheticCorpusFiles) * offsetStep

	tx, err := d.sql.Begin()
	if err != nil {
		t.Fatalf("seeding the corpus: %v", err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

	const seqCTE = `WITH RECURSIVE seq(n) AS (SELECT 0 UNION ALL SELECT n+1 FROM seq WHERE n < ?) `
	statements := []struct {
		what string
		sql  string
		args []any
	}{
		{
			what: "entry rows",
			sql: seqCTE + `INSERT INTO entry
			  (position, upsert_key, write_id, write_seq, plane, kind, book_agent_id, run_id, top_level, frame,
			   first_inserted_at_ms, last_written_at_ms)
			  SELECT n+1, 'corpus-key-'||n, 'corpus-write-'||n, n+1, 2, 'page_line',
			         'corpus-agent-'||(n % ?), NULL, NULL, x'00', ?, ?
			    FROM seq`,
			args: []any{syntheticCorpusRows - 1, syntheticCorpusFiles, testNow, testNow},
		},
		{
			what: "write_ledger rows",
			sql: seqCTE + `INSERT INTO write_ledger
			  (write_id, upsert_key, write_seq, applied_at_ms, source_file_id, source_offset)
			  SELECT 'corpus-write-'||n, 'corpus-key-'||n, n+1, ?,
			         'corpus-file-'||(n % ?), (n / ?) * ?
			    FROM seq`,
			args: []any{syntheticCorpusRows - 1, testNow, syntheticCorpusFiles, syntheticCorpusFiles, offsetStep},
		},
		{
			what: "cursor rows",
			sql: seqCTE + `INSERT INTO cursor (file_id, path, offset, carry, updated_at_ms)
			  SELECT 'corpus-file-'||n, '/corpus/'||n||'.jsonl', ?, NULL, ?
			    FROM seq`,
			args: []any{syntheticCorpusFiles - 1, spread, testNow},
		},
	}
	for _, statement := range statements {
		if _, err := tx.Exec(statement.sql, statement.args...); err != nil {
			t.Fatalf("seeding the corpus (%s): %v", statement.what, err)
		}
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("seeding the corpus: %v", err)
	}
	return spread
}

// thirtyRowFileBatch is the batch shape the owner's slow-query warning named: a
// sidecar's file-plane batch of thirty page lines with the cursor advance they
// were read behind.
func thirtyRowFileBatch(tag string, fileID string, offset int64) *storev1.EntryBatch {
	entries := make([]*storev1.StoreEntry, 0, 30)
	for i := 0; i < 30; i++ {
		key := fmt.Sprintf("%s-%d", tag, i)
		entry := pageEntry(key, key, "agent-1", frameItem(activityFrame("agent-1", "act-"+key, prose())))
		entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
		entries = append(entries, entry)
	}
	return &storev1.EntryBatch{
		Entries:       entries,
		CursorAdvance: &storev1.CursorState{FileId: fileID, Path: "/corpus/live.jsonl", Offset: offset},
	}
}

// queryPlan renders SQLite's plan for one statement as one line per step, so a
// test can state which rows of which table a statement is allowed to touch.
//
// A PLAN IS A STRUCTURAL ASSERTION AND A DURATION IS NOT. The cost of a table
// SCAN is invisible on a warm, quiet box and ruinous on the owner's loaded one
// — the same sweep batch measured 111ms here and 1464ms there — so a wall-clock
// bound cannot tell a seek from a scan, while the plan says it outright and says
// it the same way on every machine.
func queryPlan(t *testing.T, d *DB, statement string, args ...any) string {
	t.Helper()
	rows, err := d.sql.Query("EXPLAIN QUERY PLAN "+statement, args...)
	if err != nil {
		t.Fatalf("EXPLAIN QUERY PLAN %s: %v", statement, err)
	}
	defer rows.Close() //nolint:errcheck // the deferred close of a read
	var plan []string
	for rows.Next() {
		var id, parent, notused int
		var detail string
		if err := rows.Scan(&id, &parent, &notused, &detail); err != nil {
			t.Fatalf("scanning a plan row: %v", err)
		}
		plan = append(plan, detail)
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("iterating plan rows: %v", err)
	}
	return strings.Join(plan, "\n")
}

// assertNoTableScan fails unless every step of the plan is an indexed seek.
// SQLite writes a full walk as "SCAN <table>" and an indexed lookup as
// "SEARCH <table> USING …", so the vocabulary is the assertion.
func assertNoTableScan(t *testing.T, what, plan string) {
	t.Helper()
	for _, step := range strings.Split(plan, "\n") {
		if strings.HasPrefix(strings.TrimSpace(step), "SCAN ") {
			t.Fatalf("%s walks a whole table or index instead of seeking:\n%s", what, plan)
		}
	}
}

// ---- connection pragmas ----

// pragmaOn reads one integer pragma on one connection.
func pragmaOn(t *testing.T, conn *sql.Conn, name string) int64 {
	t.Helper()
	var value int64
	if err := conn.QueryRowContext(ctx(), "PRAGMA "+name).Scan(&value); err != nil {
		t.Fatalf("PRAGMA %s: %v", name, err)
	}
	return value
}

func TestTheWriteConnectionRunsNoAutocheckpoint(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	conn, err := d.sql.Conn(ctx())
	if err != nil {
		t.Fatalf("Conn: %v", err)
	}
	defer conn.Close() //nolint:errcheck // best-effort test teardown

	// Act
	pages := pragmaOn(t, conn, "wal_autocheckpoint")

	// Assert
	if pages != 0 {
		t.Fatalf("wal_autocheckpoint = %d, want 0: checkpoints are the store's bulk job, never a commit's", pages)
	}
}

func TestTheWriteConnectionLimitsTheWALFileItLeavesBehind(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	conn, err := d.sql.Conn(ctx())
	if err != nil {
		t.Fatalf("Conn: %v", err)
	}
	defer conn.Close() //nolint:errcheck // best-effort test teardown

	// Act
	limit := pragmaOn(t, conn, "journal_size_limit")

	// Assert
	if limit != JournalSizeLimitBytes {
		t.Fatalf("journal_size_limit = %d, want %d", limit, JournalSizeLimitBytes)
	}
}

// TestEveryConnectionCarriesTheCacheAndMapSizes holds two read connections open
// at once, so the second is a genuinely new connection of the pool rather than
// the first one handed back.
func TestEveryConnectionCarriesTheCacheAndMapSizes(t *testing.T) {
	d, _ := newStore(t)
	firstRead, err := d.read.Conn(ctx())
	if err != nil {
		t.Fatalf("read Conn: %v", err)
	}
	defer firstRead.Close() //nolint:errcheck // best-effort test teardown
	secondRead, err := d.read.Conn(ctx())
	if err != nil {
		t.Fatalf("second read Conn: %v", err)
	}
	defer secondRead.Close() //nolint:errcheck // best-effort test teardown
	write, err := d.sql.Conn(ctx())
	if err != nil {
		t.Fatalf("write Conn: %v", err)
	}
	defer write.Close() //nolint:errcheck // best-effort test teardown

	tests := []struct {
		name   string
		conn   *sql.Conn
		pragma string
		want   int64
	}{
		{"the write connection's cache", write, "cache_size", -WriteCacheKiB},
		{"the write connection's map", write, "mmap_size", MmapSizeBytes},
		{"a read connection's cache", firstRead, "cache_size", -ReadCacheKiB},
		{"a read connection's map", firstRead, "mmap_size", MmapSizeBytes},
		{"a second read connection's cache", secondRead, "cache_size", -ReadCacheKiB},
		{"a second read connection's map", secondRead, "mmap_size", MmapSizeBytes},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Act
			got := pragmaOn(t, test.conn, test.pragma)

			// Assert
			if got != test.want {
				t.Fatalf("PRAGMA %s = %d, want %d", test.pragma, got, test.want)
			}
		})
	}
}

func TestCloseReleasesTheWALIndexDescriptor(t *testing.T) {
	// Arrange
	_, log := newSink(t)
	d, err := OpenWithOptions(filepath.Join(t.TempDir(), "store.db"), log, Options{Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	writeOne(t, d, "a")
	shm := d.wal.shm

	// Act
	err = d.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	if shm == nil || d.wal.shm != nil {
		t.Fatalf("the WAL-index descriptor was never opened or was not released: before=%v after=%v", shm, d.wal.shm)
	}
	if _, statErr := shm.Stat(); !errors.Is(statErr, os.ErrClosed) {
		t.Fatalf("the descriptor is still open after Close: %v", statErr)
	}
}

func TestCloseReportsAWALIndexDescriptorThatWillNotClose(t *testing.T) {
	// Arrange: a descriptor already closed, so closing it again fails.
	s, log := newSink(t)
	d, err := OpenWithOptions(filepath.Join(t.TempDir(), "store.db"), log, Options{Now: func() int64 { return testNow }})
	if err != nil {
		t.Fatalf("OpenWithOptions: %v", err)
	}
	writeOne(t, d, "a")
	d.wal.shm.Close() //nolint:errcheck // closed early on purpose

	// Act
	err = d.Close()

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("Close = %v, want a storage failure", err)
	}
	s.assertLogged(t, "error", "closing the WAL-index descriptor failed")
}
