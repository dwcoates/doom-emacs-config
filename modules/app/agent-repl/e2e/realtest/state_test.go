//go:build realtest

package realtest

import (
	"context"
	"database/sql"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	_ "modernc.org/sqlite"
)

// The state reader's unit tests. They start no editor, touch no real path and
// read no owner state: every database here is built under t.TempDir() with the
// same driver and the same WAL settings the daemon uses, so the thing under
// test — reading a WAL database that has an uncheckpointed log and no `-shm`
// beside it — is reproduced rather than approximated.

// stateFixtureDSN opens a scratch database the way the daemon opens the real
// one: WAL, with automatic checkpointing off so a log written here stays
// written and is not silently folded into the main file.
func stateFixtureDSN(path string) string {
	return "file:" + path + "?_pragma=journal_mode(WAL)&_pragma=wal_autocheckpoint(0)&_pragma=busy_timeout(5000)"
}

// openStateFixture opens a scratch database and registers its close.
func openStateFixture(t *testing.T, path string) *sql.DB {
	t.Helper()
	db, err := sql.Open("sqlite", stateFixtureDSN(path))
	if err != nil {
		t.Fatalf("open the fixture database %s: %v", path, err)
	}
	t.Cleanup(func() { db.Close() })
	if err := db.Ping(); err != nil {
		t.Fatalf("reach the fixture database %s: %v", path, err)
	}
	return db
}

// seedStateFixture creates the `workspaces` table these tests read and puts one
// open and one closed workspace in it.
func seedStateFixture(t *testing.T, db *sql.DB) {
	t.Helper()
	stmts := []string{
		`CREATE TABLE workspaces (id TEXT PRIMARY KEY, dir TEXT, name TEXT, closed INTEGER);`,
		`INSERT INTO workspaces VALUES ('w1', '/repos/one', 'one', 0);`,
		`INSERT INTO workspaces VALUES ('w2', '/repos/two|piped', 'two', 1);`,
	}
	for _, stmt := range stmts {
		if _, err := db.Exec(stmt); err != nil {
			t.Fatalf("seed the fixture with %q: %v", stmt, err)
		}
	}
}

// crashedWALFixture reproduces THE STATE A STOPPED DAEMON LEAVES BEHIND: a
// database file, a `-wal` holding committed transactions that were never
// checkpointed into it, and NO `-shm`.
//
// It is built by writing through a live connection and copying the pair aside
// while that connection still holds the log open, which is exactly what a
// daemon that is killed rather than closed leaves on disk. Closing the
// connection first would not do: SQLite checkpoints and removes the log on the
// last close, and the resulting single file would not exercise anything.
func crashedWALFixture(t *testing.T) (dbPath string) {
	t.Helper()
	live := filepath.Join(t.TempDir(), "live.db")
	db := openStateFixture(t, live)
	seedStateFixture(t, db)

	dead := filepath.Join(t.TempDir(), "wsm.db")
	for _, suffix := range []string{"", "-wal"} {
		bytes, err := os.ReadFile(live + suffix)
		if err != nil {
			t.Fatalf("build the crashed fixture: read %s: %v", live+suffix, err)
		}
		if err := os.WriteFile(dead+suffix, bytes, 0o600); err != nil {
			t.Fatalf("build the crashed fixture: write %s: %v", dead+suffix, err)
		}
	}
	if _, err := os.Stat(dead + "-shm"); !os.IsNotExist(err) {
		t.Fatalf("the crashed fixture was supposed to have no -shm beside it: %v", err)
	}
	return dead
}

// sealedWALFixture is crashedWALFixture with the containing directory made
// unwritable, which is the condition the old read actually died on: SQLite
// cannot lay a `-shm` down beside the database, and a `-readonly` open of a WAL
// database with no `-shm` fails outright rather than reading without one.
//
// The directory is restored before the test tree is torn down; a t.TempDir()
// left at 0500 cannot be removed.
func sealedWALFixture(t *testing.T) (dbPath string) {
	t.Helper()
	dbPath = crashedWALFixture(t)
	dir := filepath.Dir(dbPath)
	t.Cleanup(func() { os.Chmod(dir, 0o700) })
	if err := os.Chmod(dir, 0o500); err != nil {
		t.Fatalf("seal the fixture directory %s: %v", dir, err)
	}
	return dbPath
}

// snapshotsUnder points snapshots at a directory the test owns, so what is left
// behind after a read can be asserted on.
func snapshotsUnder(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	previous := stateSnapshotRoot
	stateSnapshotRoot = root
	t.Cleanup(func() { stateSnapshotRoot = previous })
	return root
}

func entriesIn(t *testing.T, dir string) []string {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("list %s: %v", dir, err)
	}
	names := make([]string, 0, len(entries))
	for _, e := range entries {
		names = append(names, e.Name())
	}
	return names
}

// TestReadOnlySqliteWritesASharedMemoryFileBesideTheOwnersDatabase is the first
// half of why the old read had to go: `-readonly` did NOT keep the harness from
// writing into the owner's state directory. It lays a `-shm` down beside the
// database, because that is what reading a WAL database requires.
func TestReadOnlySqliteWritesASharedMemoryFileBesideTheOwnersDatabase(t *testing.T) {
	// Arrange.
	dbPath := crashedWALFixture(t)

	// Act.
	out, err := exec.Command("sqlite3", "-readonly", dbPath, "SELECT count(*) FROM workspaces;").CombinedOutput()
	if err != nil {
		t.Fatalf("the read-only read was expected to succeed here; sqlite3 said: %s", strings.TrimSpace(string(out)))
	}

	// Assert.
	if _, statErr := os.Stat(dbPath + "-shm"); statErr != nil {
		t.Fatalf("expected `-readonly` to have created a -shm beside the database: %v", statErr)
	}
}

// TestReadOnlySqliteFailsWhenItCannotCreateTheSharedMemoryFile is the second
// half, and the failure the realtests actually reported: take away the ability
// to write beside the database and the read-only read does not degrade, it
// dies with SQLITE_CANTOPEN.
func TestReadOnlySqliteFailsWhenItCannotCreateTheSharedMemoryFile(t *testing.T) {
	// Arrange.
	dbPath := sealedWALFixture(t)

	// Act.
	out, err := exec.Command("sqlite3", "-readonly", dbPath, "SELECT count(*) FROM workspaces;").CombinedOutput()

	// Assert.
	if err == nil {
		t.Fatalf("the read-only read was expected to fail; it printed %q", string(out))
	}
	if !strings.Contains(string(out), "unable to open database file") {
		t.Fatalf("expected the SQLITE_CANTOPEN wording the realtests died on; sqlite3 said: %s",
			strings.TrimSpace(string(out)))
	}
}

// TestQueryStateDBReadsAWalDatabaseItMayNotWriteBeside is the regression: the
// same database, the same sealed directory, and the read now succeeds because
// the harness reads a copy it owns instead of the owner's file.
func TestQueryStateDBReadsAWalDatabaseItMayNotWriteBeside(t *testing.T) {
	// Arrange.
	snapshotsUnder(t)
	dbPath := sealedWALFixture(t)

	// Act.
	rows, err := queryStateDB(context.Background(), dbPath, "SELECT count(*) FROM workspaces;")

	// Assert.
	if err != nil {
		t.Fatalf("read a WAL database the harness may not write beside: %v", err)
	}
	if len(rows) != 1 || rows[0][0] != "2" {
		t.Fatalf("expected one row counting the two seeded workspaces, got %q", rows)
	}
}

// TestQueryStateDBSeesUncheckpointedLogContent is requirement 2 of the fix: the
// `-wal` travels with the snapshot, so a transaction the daemon committed but
// never checkpointed is part of the answer rather than silently missing from
// it.
func TestQueryStateDBSeesUncheckpointedLogContent(t *testing.T) {
	// Arrange: every seeded row lives only in the log, because the fixture
	// never checkpoints.
	snapshotsUnder(t)
	dbPath := crashedWALFixture(t)
	mainFileOnly := filepath.Join(t.TempDir(), "wsm.db")
	bytes, err := os.ReadFile(dbPath)
	if err != nil {
		t.Fatalf("read the fixture main file: %v", err)
	}
	if err := os.WriteFile(mainFileOnly, bytes, 0o600); err != nil {
		t.Fatalf("write the log-less copy: %v", err)
	}
	withoutLog, err := queryStateDB(context.Background(), mainFileOnly, "SELECT count(*) FROM workspaces;")
	if err == nil && len(withoutLog) == 1 && withoutLog[0][0] != "0" {
		t.Fatalf("the fixture was supposed to hold its rows in the log only; the main file alone already reports %q", withoutLog)
	}

	// Act.
	rows, err := queryStateDB(context.Background(), dbPath, "SELECT id FROM workspaces ORDER BY id;")

	// Assert.
	if err != nil {
		t.Fatalf("read the fixture with its log: %v", err)
	}
	if len(rows) != 2 || rows[0][0] != "w1" || rows[1][0] != "w2" {
		t.Fatalf("expected the two logged workspaces, got %q", rows)
	}
}

// TestQueryStateDBReadsWhileAWriterHoldsTheDatabase covers realtests 2 and 4,
// where the daemon is up and writing: the snapshot is a file copy, so a held
// write transaction neither blocks it nor is blocked by it.
func TestQueryStateDBReadsWhileAWriterHoldsTheDatabase(t *testing.T) {
	// Arrange: a writer with an open IMMEDIATE transaction, synchronized
	// through channels so nothing here waits on a clock.
	snapshotsUnder(t)
	dbPath := filepath.Join(t.TempDir(), "wsm.db")
	db := openStateFixture(t, dbPath)
	seedStateFixture(t, db)

	held := make(chan struct{})
	release := make(chan struct{})
	var writer sync.WaitGroup
	writer.Add(1)
	var writeErr error
	go func() {
		defer writer.Done()
		tx, err := db.Begin()
		if err != nil {
			writeErr = err
			close(held)
			return
		}
		if _, err := tx.Exec(`INSERT INTO workspaces VALUES ('w3', '/repos/three', 'three', 0);`); err != nil {
			writeErr = err
			tx.Rollback()
			close(held)
			return
		}
		close(held)
		<-release
		writeErr = tx.Rollback()
	}()
	<-held
	defer func() {
		close(release)
		writer.Wait()
		if writeErr != nil {
			t.Errorf("the holding writer reported: %v", writeErr)
		}
	}()

	// Act.
	rows, err := queryStateDB(context.Background(), dbPath, "SELECT id FROM workspaces ORDER BY id;")

	// Assert: the two committed workspaces, and not the writer's uncommitted
	// third.
	if err != nil {
		t.Fatalf("read while a writer holds the database: %v", err)
	}
	if len(rows) != 2 || rows[0][0] != "w1" || rows[1][0] != "w2" {
		t.Fatalf("expected the two committed workspaces, got %q", rows)
	}
}

// TestQueryStateDBSurfacesASnapshotFailureAndNeverReadsAsEmpty is requirement 5:
// nothing here may come back as zero rows with no error, because a realtest
// would assert against zero workspaces and report nonsense about the owner's
// editor.
func TestQueryStateDBSurfacesASnapshotFailureAndNeverReadsAsEmpty(t *testing.T) {
	tests := []struct {
		name    string
		build   func(t *testing.T, dir string) string
		wanting string
	}{
		{
			name:    "the database is not there at all",
			build:   func(_ *testing.T, dir string) string { return filepath.Join(dir, "absent.db") },
			wanting: "absent.db",
		},
		{
			name: "the path is a directory",
			build: func(t *testing.T, dir string) string {
				path := filepath.Join(dir, "wsm.db")
				if err := os.Mkdir(path, 0o700); err != nil {
					t.Fatalf("make the directory fixture: %v", err)
				}
				return path
			},
			wanting: "wsm.db",
		},
		{
			name: "the file is not a database",
			build: func(t *testing.T, dir string) string {
				path := filepath.Join(dir, "wsm.db")
				if err := os.WriteFile(path, []byte("this is not a database"), 0o600); err != nil {
					t.Fatalf("write the garbage fixture: %v", err)
				}
				return path
			},
			wanting: "wsm.db",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			snapshotsUnder(t)
			dbPath := tc.build(t, t.TempDir())

			// Act.
			rows, err := queryStateDB(context.Background(), dbPath, "SELECT id FROM workspaces;")

			// Assert.
			if err == nil {
				t.Fatalf("expected a loud failure, got %d rows and no error", len(rows))
			}
			if rows != nil {
				t.Fatalf("a failed snapshot must not also return rows; got %q", rows)
			}
			if !strings.Contains(err.Error(), tc.wanting) {
				t.Fatalf("the failure must name the path; %q does not mention %q", err.Error(), tc.wanting)
			}
		})
	}
}

// TestQueryStateDBRemovesItsSnapshotAfterAFailedRead is requirement 4's hard
// half: cleanup on the failure path, not only the happy one.
func TestQueryStateDBRemovesItsSnapshotAfterAFailedRead(t *testing.T) {
	// Arrange.
	root := snapshotsUnder(t)
	dbPath := crashedWALFixture(t)

	// Act: a query against a table that is not there fails after the snapshot
	// has already been taken and verified.
	if _, err := queryStateDB(context.Background(), dbPath, "SELECT * FROM no_such_table;"); err == nil {
		t.Fatal("expected the query against a missing table to fail")
	}

	// Assert.
	if left := entriesIn(t, root); len(left) != 0 {
		t.Fatalf("a failed read left snapshots behind: %q", left)
	}
}

// TestQueryStateDBRemovesItsSnapshotAfterASuccessfulRead is the other half.
func TestQueryStateDBRemovesItsSnapshotAfterASuccessfulRead(t *testing.T) {
	// Arrange.
	root := snapshotsUnder(t)
	dbPath := crashedWALFixture(t)

	// Act.
	if _, err := queryStateDB(context.Background(), dbPath, "SELECT id FROM workspaces;"); err != nil {
		t.Fatalf("read the fixture: %v", err)
	}

	// Assert.
	if left := entriesIn(t, root); len(left) != 0 {
		t.Fatalf("a successful read left snapshots behind: %q", left)
	}
}

// TestQueryStateDBLeavesTheOwnersFilesAlone is the intent the old `-readonly`
// carried and did not keep: no `-shm`, no checkpoint, not one new byte beside
// the owner's database.
func TestQueryStateDBLeavesTheOwnersFilesAlone(t *testing.T) {
	// Arrange.
	snapshotsUnder(t)
	dbPath := crashedWALFixture(t)
	dir := filepath.Dir(dbPath)
	before := entriesIn(t, dir)
	mainBefore, err := os.ReadFile(dbPath)
	if err != nil {
		t.Fatalf("read the fixture main file: %v", err)
	}

	// Act.
	if _, err := queryStateDB(context.Background(), dbPath, "SELECT id FROM workspaces;"); err != nil {
		t.Fatalf("read the fixture: %v", err)
	}

	// Assert.
	if after := entriesIn(t, dir); strings.Join(after, ",") != strings.Join(before, ",") {
		t.Fatalf("the read changed what sits beside the owner's database: %q became %q", before, after)
	}
	mainAfter, err := os.ReadFile(dbPath)
	if err != nil {
		t.Fatalf("re-read the fixture main file: %v", err)
	}
	if string(mainAfter) != string(mainBefore) {
		t.Fatal("the read rewrote the owner's database file")
	}
}

// TestReadWorkspacesSplitsOpenFromClosedOverASnapshot is the caller's contract
// over the new read path, including a directory containing the pipe the
// separator exists to survive.
func TestReadWorkspacesSplitsOpenFromClosedOverASnapshot(t *testing.T) {
	// Arrange.
	snapshotsUnder(t)
	dbPath := crashedWALFixture(t)

	// Act.
	open, closed, err := ReadWorkspaces(context.Background(), dbPath)

	// Assert.
	if err != nil {
		t.Fatalf("read the workspaces: %v", err)
	}
	if len(open) != 1 || open[0].ID != "w1" || open[0].Dir != "/repos/one" {
		t.Fatalf("expected the one open workspace, got %+v", open)
	}
	if len(closed) != 1 || closed[0].ID != "w2" || closed[0].Dir != "/repos/two|piped" {
		t.Fatalf("expected the one closed workspace with its piped directory intact, got %+v", closed)
	}
}

// TestReadWorkspacesSurfacesAnUnreadableDatabase is the same no-empty rule at
// the caller's level: a state database that cannot be snapshotted must never
// reach a realtest as "the owner has no workspaces".
func TestReadWorkspacesSurfacesAnUnreadableDatabase(t *testing.T) {
	// Arrange.
	snapshotsUnder(t)
	dbPath := filepath.Join(t.TempDir(), "wsm.db")

	// Act.
	open, closed, err := ReadWorkspaces(context.Background(), dbPath)

	// Assert.
	if err == nil {
		t.Fatal("expected a missing state database to be reported")
	}
	if open != nil || closed != nil {
		t.Fatalf("a failed read must not look like an empty registry; got %+v / %+v", open, closed)
	}
	if !strings.Contains(err.Error(), dbPath) {
		t.Fatalf("the failure must name the database; got %q", err.Error())
	}
}
