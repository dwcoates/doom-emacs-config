package wsm

import (
	"context"
	"database/sql"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sourcescan"
)

func TestOpenRefusesUnusablePaths(t *testing.T) {
	tests := []struct {
		name string
		path string
		want string
	}{
		{name: "empty", path: "", want: "empty db path"},
		{name: "in memory", path: ":memory:", want: "reopen-durable"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			_, err := Open(context.Background(), tc.path)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Open(%q) = %v, want an error containing %q", tc.path, err, tc.want)
			}
		})
	}
}

func TestOpenSetsTheDeclaredPragmas(t *testing.T) {
	tests := []struct {
		name  string
		query string
		want  string
	}{
		{name: "journal mode", query: `PRAGMA journal_mode`, want: "wal"},
		{name: "busy timeout", query: `PRAGMA busy_timeout`, want: "5000"},
		{name: "foreign keys", query: `PRAGMA foreign_keys`, want: "1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := fileStore(t)

			// Act
			got := scalar[string](t, s, tc.query)

			// Assert
			if !strings.EqualFold(got, tc.want) {
				t.Fatalf("%s = %q, want %q", tc.query, got, tc.want)
			}
		})
	}
}

// TestOpenSynchronousPragma pins that SQLite's forced flushes are off ONLY on
// a handle the test-run seam (WithUnsyncedWrites) asked for: a live daemon
// opens without it and keeps SQLite's own default, FULL.
func TestOpenSynchronousPragma(t *testing.T) {
	tests := []struct {
		name string
		opts []Option
		want int
	}{
		{name: "the production open keeps SQLite's default FULL", opts: nil, want: 2},
		{name: "the test-run seam turns forced flushes off", opts: []Option{WithUnsyncedWrites()}, want: 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			handle, err := Open(context.Background(), filepath.Join(t.TempDir(), "wsm.db"), tc.opts...)
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			t.Cleanup(func() { handle.Close() })

			// Act
			got := scalar[int](t, handle.(*store), `PRAGMA synchronous`)

			// Assert
			if got != tc.want {
				t.Fatalf("PRAGMA synchronous = %d, want %d", got, tc.want)
			}
		})
	}
}

// TestPromoteKeepsTheUnsyncedSeam pins that the writing handle a promotion
// opens carries the seam the read-only open was given, so a promoted test
// handle does not start forcing flushes.
func TestPromoteKeepsTheUnsyncedSeam(t *testing.T) {
	// Arrange
	path := writableStore(t)
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	t.Cleanup(func() { ro.Close() })

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Assert
	if got := scalar[int](t, ro.(*store), `PRAGMA synchronous`); got != 0 {
		t.Fatalf("PRAGMA synchronous after Promote = %d, want 0", got)
	}
}

// TestAReadThatTookTheHandleBeforeAPromotionStillReads pins the read side of
// a promotion: a reader answered the read-only handle by db() just before
// Promote swapped it must still be able to query it afterward. Promote used
// to close that handle at the swap, and the successor's registry read for an
// adopting page met `sql: database is closed` (2026-10-06 handover,
// daemon.wsm.workspace ERROR, then a false unknown_workspace refusal).
func TestAReadThatTookTheHandleBeforeAPromotionStillReads(t *testing.T) {
	// Arrange
	path := writableStore(t)
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	t.Cleanup(func() { ro.Close() })
	taken := ro.(*store).db()

	// Act
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}
	var tasks int
	err = taken.QueryRowContext(context.Background(), `SELECT count(*) FROM tasks`).Scan(&tasks)

	// Assert
	if err != nil {
		t.Fatalf("a read on the handle taken before the promotion = %v, want it served", err)
	}
}

// TestAWriteRacingAPromotionSeesOneWholeHandle pins that a write running
// while Promote swaps the handle reads the handle and its read-only flag as
// one: either the read-only handle, refused, or the writing one, committed.
// write() used to read both fields with no synchronization while Promote
// assigned them under mu, a data race `go test -race` reports here.
func TestAWriteRacingAPromotionSeesOneWholeHandle(t *testing.T) {
	// Arrange
	path := writableStore(t)
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	t.Cleanup(func() { ro.Close() })
	// The writer keeps writing until one commits, so some of its writes run
	// after the swap with nothing ordering them after it but the handle
	// itself.
	wrote := make(chan error, 1)
	go func() {
		for {
			_, err := ro.CreateTask(context.Background(), "racing")
			if !errors.Is(err, ErrReadOnly) {
				wrote <- err
				return
			}
		}
	}()

	// Act
	promoteErr := ro.Promote(context.Background())
	writeErr := <-wrote

	// Assert
	if promoteErr != nil {
		t.Fatalf("Promote: %v", promoteErr)
	}
	if writeErr != nil {
		t.Fatalf("the first write past the promotion = %v, want it committed", writeErr)
	}
}

// TestCloseClosesTheHandleAPromotionRetired pins the other half: the
// read-only handle a promotion retires lives exactly as long as the store,
// so Close ends it.
func TestCloseClosesTheHandleAPromotionRetired(t *testing.T) {
	// Arrange
	path := writableStore(t)
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	retired := ro.(*store).db()
	if err := ro.Promote(context.Background()); err != nil {
		t.Fatalf("Promote: %v", err)
	}

	// Act
	if err := ro.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert
	if err := retired.PingContext(context.Background()); err == nil || !strings.Contains(err.Error(), "database is closed") {
		t.Fatalf("the retired handle after Close answers %v, want sql: database is closed", err)
	}
}

func TestOpenUsesASingleConnection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got := s.db().Stats().MaxOpenConnections

	// Assert
	if got != 1 {
		t.Fatalf("MaxOpenConnections = %d, want 1", got)
	}
}

func TestOpenCreatesTheSchemaOnAFreshFile(t *testing.T) {
	// Arrange
	s, log := fileStore(t)

	// Act
	version := scalar[int](t, s, `SELECT version FROM layout WHERE id = 1`)

	// Assert
	if version != LayoutVersion {
		t.Fatalf("layout version = %d, want %d", version, LayoutVersion)
	}
	if !loggedOperation(log, "daemon.wsm.open", "info") {
		t.Fatalf("a fresh database's creation was not logged: %v", log.Records())
	}
}

func TestOpenCreatesEveryDeclaredTable(t *testing.T) {
	tests := []string{
		"layout", "repositories", "tasks", "workspaces", "creation_jobs", "sessions", "leases",
		"held_prompts", "turns", "idempotency_keys", "merge_ledger", "merge_tab_intervals",
		"merge_queue", "merge_queue_repos", "faults", "drain_schedule",
	}
	for _, table := range tests {
		t.Run(table, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?`, table)

			// Assert
			if got != 1 {
				t.Fatalf("table %q exists %d times, want 1", table, got)
			}
		})
	}
}

// TestOpenRefusesAForeignLayoutVersion covers the layouts this build cannot
// interpret. An OLDER layout is NOT among them any more: it is migrated
// forward (migrate_test.go), because the workspace state is the user's data.
func TestOpenRefusesAForeignLayoutVersion(t *testing.T) {
	tests := []struct {
		name    string
		version int
	}{
		{name: "newer than this build", version: LayoutVersion + 1},
		{name: "older than any migration reaches", version: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "wsm.db")
			first, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			stampLayout(t, first.(*store), tc.version)
			first.Close()

			// Act
			log := dlog.NewTestLogger()
			_, err = Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))

			// Assert
			var refusal *LayoutError
			if !errors.As(err, &refusal) {
				t.Fatalf("Open = %v, want a *LayoutError", err)
			}
			if refusal.File != tc.version || refusal.Binary != LayoutVersion {
				t.Fatalf("refusal = %+v, want file %d and binary %d", refusal, tc.version, LayoutVersion)
			}
			if !loggedOperation(log, "daemon.wsm.open", "error") {
				t.Fatalf("the layout refusal was not logged at error: %v", log.Records())
			}
		})
	}
}

func TestOpenRefusesADatabaseWithNoLayoutRow(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	corrupt(t, first.(*store), `DELETE FROM layout`)
	first.Close()

	// Act
	_, err = Open(context.Background(), path, WithUnsyncedWrites())

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "layout" {
		t.Fatalf("Open = %v, want a *DecodeError naming the layout table", err)
	}
}

func TestOpenReadOnlyRefusesAMissingFile(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "absent.db")

	// Act
	_, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())

	// Assert
	if err == nil {
		t.Fatalf("OpenReadOnly on a missing file succeeded")
	}
	if _, statErr := os.Stat(path); statErr == nil {
		t.Fatalf("OpenReadOnly created %q; the read-only mode must leave no residue", path)
	}
}

func TestOpenReadOnlyReportsItsMode(t *testing.T) {
	// Arrange
	path := writableStore(t)

	// Act
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	defer ro.Close()

	// Assert
	if !ro.ReadOnly() {
		t.Fatalf("ReadOnly() = false on a handle opened read-only")
	}
}

func TestOpenReadOnlyRefusesEveryWrite(t *testing.T) {
	// Arrange
	path := writableStore(t)
	log := dlog.NewTestLogger()
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	defer ro.Close()

	// Act
	_, _, err = ro.RegisterWorkspace(context.Background(), t.TempDir(), RegisterFacts{RepoDir: t.TempDir()})

	// Assert
	if !errors.Is(err, ErrReadOnly) {
		t.Fatalf("RegisterWorkspace on a read-only handle = %v, want ErrReadOnly", err)
	}
	if !loggedOperation(log, "daemon.wsm.register_workspace", "error") {
		t.Fatalf("the read-only refusal was not logged at error: %v", log.Records())
	}
}

func TestOpenReadOnlyChangesNothing(t *testing.T) {
	// Arrange
	path := writableStore(t)
	before := digest(t, path)
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}

	// Act — every write path the interface exposes for a bare workspace-less op.
	_ = ro.ClearDrainSchedule(context.Background())
	_ = ro.PutDrainSchedule(context.Background(), DrainSchedule{Reason: "deploy", Deadline: instant, SetAt: instant})
	if _, err := ro.Tasks(context.Background()); err != nil {
		t.Fatalf("Tasks on a read-only handle: %v", err)
	}
	ro.Close()

	// Assert
	if after := digest(t, path); after != before {
		t.Fatalf("the read-only handle changed the database file")
	}
}

func TestOpenReadOnlyRefusesEngineLevelWrites(t *testing.T) {
	// Arrange — query_only(1) must refuse at the engine, not only in this
	// package's guard, so a raw statement is the thing to try.
	path := writableStore(t)
	ro, err := OpenReadOnly(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	defer ro.Close()

	// Act
	_, err = ro.(*store).db().ExecContext(context.Background(), `INSERT INTO tasks (id, title, done, created_at) VALUES ('x', 'y', 0, 0)`)

	// Assert
	if err == nil {
		t.Fatalf("a raw INSERT through a read-only handle succeeded")
	}
}

func TestOpenReadOnlyRefusesAForeignLayoutVersion(t *testing.T) {
	// Arrange
	path := writableStore(t)
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	stampLayout(t, first.(*store), LayoutVersion+1)
	first.Close()

	// Act
	_, err = OpenReadOnly(context.Background(), path, WithUnsyncedWrites())

	// Assert
	var refusal *LayoutError
	if !errors.As(err, &refusal) {
		t.Fatalf("OpenReadOnly = %v, want a *LayoutError", err)
	}
}

func TestWriteLogsTheOperationOnSuccess(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	testWorkspace(t, s)

	// Assert
	if !loggedOperation(log, "daemon.wsm.register_workspace", "debug") {
		t.Fatalf("a successful write was not logged at debug: %v", log.Records())
	}
}

func TestWithLoggerIgnoresANilLogger(t *testing.T) {
	// Arrange / Act — a nil option value must not install a nil sink that would
	// panic on the first record.
	handle, err := Open(context.Background(), filepath.Join(t.TempDir(), "wsm.db"), WithLogger(nil), WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert
	if _, err := handle.Tasks(context.Background()); err != nil {
		t.Fatalf("Tasks: %v", err)
	}
}

// stampLayout rewrites the file's layout version, which is how a test stands in
// for a database written by another build.
func stampLayout(t *testing.T, s *store, version int) {
	t.Helper()
	corrupt(t, s, `UPDATE layout SET version = ? WHERE id = 1`, version)
}

// writableStore creates and closes a fresh store, returning its path for a
// read-only reopen.
func writableStore(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "wsm.db")
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if _, err := handle.CreateTask(context.Background(), "seed"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	return path
}

// digest is the database file's bytes, for proving a read-only handle wrote
// nothing.
func digest(t *testing.T, path string) string {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %q: %v", path, err)
	}
	return string(raw)
}

// loggedOperation reports whether the logger captured a record for operation at
// level.
func loggedOperation(log *dlog.TestLogger, operation, level string) bool {
	for _, record := range log.Records() {
		if record.Operation == operation && record.Level == level {
			return true
		}
	}
	return false
}

// TestAnAbsentRecordIsReadAtDebug pins that a lookup finding nothing is an
// ANSWER, not a failure. ErrNotFound is what every per-workspace rpc's
// unknown-workspace refusal is built from, so an error line here would sit on
// an ordinary refusal path and drown the reads that really did break.
func TestAnAbsentRecordIsReadAtDebug(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.Workspace(context.Background(), "no-such-workspace")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("Workspace(unknown) = %v, want ErrNotFound", err)
	}
	if loggedOperation(log, "daemon.wsm.workspace", "error") {
		t.Fatalf("an absent record was recorded at error: %v", log.Records())
	}
	if !loggedOperation(log, "daemon.wsm.workspace", "debug") {
		t.Fatalf("an absent record was not recorded at debug: %v", log.Records())
	}
}

// TestAReadThatBreaksIsStillAnError pins that the ErrNotFound branch narrows
// nothing else: a read that genuinely failed keeps its error record.
func TestAReadThatBreaksIsStillAnError(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act
	if _, err := s.Workspace(context.Background(), "anything"); err == nil {
		t.Fatal("a read on a closed handle = success, want an error")
	}

	// Assert
	if !loggedOperation(log, "daemon.wsm.workspace", "error") {
		t.Fatalf("a broken read was not recorded at error: %v", log.Records())
	}
}

// TestReadRecordsACancelledCallerWithoutAnError pins the read helper's
// cancellation arm: a read whose CALLER went away is the client leaving, not a
// broken read, and it must not put an ERROR line on every orderly exit. A read
// that failed for any other reason still records ERROR.
func TestReadRecordsACancelledCallerWithoutAnError(t *testing.T) {
	tests := []struct {
		name      string
		ctx       func(t *testing.T) context.Context
		wantLevel string
		wantMsg   string
	}{
		{
			name: "cancelled caller",
			ctx: func(t *testing.T) context.Context {
				ctx, cancel := context.WithCancel(context.Background())
				cancel()
				return ctx
			},
			wantLevel: "info",
			wantMsg:   "the read ended when its caller's context was cancelled",
		},
		{
			name: "deadline exceeded",
			ctx: func(t *testing.T) context.Context {
				ctx, cancel := context.WithDeadline(context.Background(), time.Now().Add(-time.Second))
				t.Cleanup(cancel)
				return ctx
			},
			wantLevel: "info",
			wantMsg:   "the read ended when its caller's context was cancelled",
		},
		{
			name:      "a genuine read failure",
			ctx:       func(t *testing.T) context.Context { return context.Background() },
			wantLevel: "error",
			wantMsg:   "refused the read",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act. A cancelled context makes the driver refuse the query; the
			// background case is refused by the broken statement instead.
			ctx := tc.ctx(t)
			err := s.read(ctx, "daemon.wsm.test_read", dlog.Context{"workspace": string(ws.ID)},
				func(ctx context.Context) error {
					return s.db().QueryRowContext(ctx, "SELECT no_such_column FROM workspaces").Scan(new(int))
				})

			// Assert.
			if err == nil {
				t.Fatalf("the read reported success; want a failure")
			}
			var found bool
			for _, rec := range log.Records() {
				if rec.Operation != "daemon.wsm.test_read" {
					continue
				}
				if rec.Level != tc.wantLevel || rec.Message != tc.wantMsg {
					t.Fatalf("recorded %s %q, want %s %q",
						rec.Level, rec.Message, tc.wantLevel, tc.wantMsg)
				}
				found = true
			}
			if !found {
				t.Fatalf("no record for the read: %v", log.Records())
			}
		})
	}
}

// ---- endTx: the one way a transaction ends without a Commit ----

var errWriteRefused = errors.New("the write's own refusal")

// commitBehindTheTx ends the transaction with its own COMMIT, behind
// database/sql's back, so the rollback that follows reaches SQLite and fails
// there with "no transaction is active".
func commitBehindTheTx(ctx context.Context, tx *sql.Tx) error {
	if _, err := tx.ExecContext(ctx, `COMMIT`); err != nil {
		return err
	}
	return errWriteRefused
}

func rollbackFailures(log *dlog.TestLogger) []dlog.Record {
	var out []dlog.Record
	for _, record := range log.Records() {
		if strings.Contains(record.Message, "could not roll the transaction back") {
			out = append(out, record)
		}
	}
	return out
}

func TestAWriteWhoseRollbackFailsRecordsItAtError(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_ = s.write(context.Background(), "daemon.wsm.test", dlog.Context{"workspace": "ws-1"}, commitBehindTheTx)

	// Assert
	failures := rollbackFailures(log)
	if len(failures) != 1 || failures[0].Level != "error" || failures[0].Operation != "daemon.wsm.test" {
		t.Fatalf("rollback failure records = %+v, want one ERROR at the write's operation", failures)
	}
	if failures[0].Context["workspace"] != "ws-1" || !strings.Contains(failures[0].Context["error"].(string), "no transaction is active") {
		t.Fatalf("context = %v, want the write's fields and SQLite's cause", failures[0].Context)
	}
}

func TestAWriteWhoseRollbackFailsStillReturnsItsOwnCause(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.write(context.Background(), "daemon.wsm.test", dlog.Context{}, commitBehindTheTx)

	// Assert
	if !errors.Is(err, errWriteRefused) {
		t.Fatalf("write = %v, want the write's own refusal, not the rollback's", err)
	}
}

func TestARefusedWriteWhoseRollbackSucceedsRecordsNoRollbackFailure(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_ = s.write(context.Background(), "daemon.wsm.test", dlog.Context{}, func(context.Context, *sql.Tx) error { return errWriteRefused })

	// Assert
	if failures := rollbackFailures(log); len(failures) != 0 {
		t.Fatalf("rollback failure records = %+v, want none", failures)
	}
}

func TestEndTxAfterACommitRecordsNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	tx, err := s.db().BeginTx(context.Background(), nil)
	if err != nil {
		t.Fatalf("BeginTx: %v", err)
	}
	if err := tx.Commit(); err != nil {
		t.Fatalf("Commit: %v", err)
	}

	// Act
	s.endTx(tx, "daemon.wsm.test", dlog.Context{})

	// Assert
	if failures := rollbackFailures(log); len(failures) != 0 {
		t.Fatalf("rollback failure records = %+v, want none after a Commit", failures)
	}
}

// Every rollback in this package goes through endTx, so no site can quietly
// drop a failed one again.
func TestEveryRollbackGoesThroughEndTx(t *testing.T) {
	// Act
	rollbacks := sourcescan.Count(t, ".Rollback()")

	// Assert
	if rollbacks != 1 {
		t.Fatalf("production source calls Rollback %d times; only endTx may", rollbacks)
	}
}
