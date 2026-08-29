package wsm

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
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
			s, _ := testStore(t)

			// Act
			got := scalar[string](t, s, tc.query)

			// Assert
			if !strings.EqualFold(got, tc.want) {
				t.Fatalf("%s = %q, want %q", tc.query, got, tc.want)
			}
		})
	}
}

func TestOpenUsesASingleConnection(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got := s.db.Stats().MaxOpenConnections

	// Assert
	if got != 1 {
		t.Fatalf("MaxOpenConnections = %d, want 1", got)
	}
}

func TestOpenCreatesTheSchemaOnAFreshFile(t *testing.T) {
	// Arrange
	s, log := testStore(t)

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

func TestOpenRefusesAForeignLayoutVersion(t *testing.T) {
	tests := []struct {
		name    string
		version int
	}{
		{name: "newer than this build", version: LayoutVersion + 1},
		{name: "older than this build", version: LayoutVersion - 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "wsm.db")
			first, err := Open(context.Background(), path)
			if err != nil {
				t.Fatalf("Open: %v", err)
			}
			stampLayout(t, first.(*store), tc.version)
			first.Close()

			// Act
			log := dlog.NewTestLogger()
			_, err = Open(context.Background(), path, WithLogger(log))

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
	first, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	corrupt(t, first.(*store), `DELETE FROM layout`)
	first.Close()

	// Act
	_, err = Open(context.Background(), path)

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
	_, err := OpenReadOnly(context.Background(), path)

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
	ro, err := OpenReadOnly(context.Background(), path)
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
	ro, err := OpenReadOnly(context.Background(), path, WithLogger(log))
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
	ro, err := OpenReadOnly(context.Background(), path)
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
	ro, err := OpenReadOnly(context.Background(), path)
	if err != nil {
		t.Fatalf("OpenReadOnly: %v", err)
	}
	defer ro.Close()

	// Act
	_, err = ro.(*store).db.ExecContext(context.Background(), `INSERT INTO tasks (id, title, done, created_at) VALUES ('x', 'y', 0, 0)`)

	// Assert
	if err == nil {
		t.Fatalf("a raw INSERT through a read-only handle succeeded")
	}
}

func TestOpenReadOnlyRefusesAForeignLayoutVersion(t *testing.T) {
	// Arrange
	path := writableStore(t)
	first, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	stampLayout(t, first.(*store), LayoutVersion+1)
	first.Close()

	// Act
	_, err = OpenReadOnly(context.Background(), path)

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
	handle, err := Open(context.Background(), filepath.Join(t.TempDir(), "wsm.db"), WithLogger(nil))
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
	handle, err := Open(context.Background(), path)
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
