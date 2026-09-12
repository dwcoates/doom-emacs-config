package wsm

import (
	"context"
	"database/sql"
	"errors"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

func TestOpenMigratesALayoutThreeDatabaseForward(t *testing.T) {
	// Arrange — a file written by the build BEFORE ported_prompts landed.
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT version FROM layout WHERE id = 1`); got != LayoutVersion {
		t.Fatalf("layout version after the open = %d, want %d", got, LayoutVersion)
	}
}

func TestTheMigrationCreatesTheTableTheNewLayoutAdded(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'ported_prompts'`)
	if got != 1 {
		t.Fatalf("ported_prompts exists %d times after the migration, want 1", got)
	}
}

func TestTheMigrationLeavesThePreExistingRowsInPlace(t *testing.T) {
	// Arrange — the user's workspace state, written under layout 3.
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	ws, err := handle.Workspace(context.Background(), fixtureWorkspaceID)
	if err != nil {
		t.Fatalf("Workspace(%q) after the migration: %v", fixtureWorkspaceID, err)
	}
	if ws.Name != fixtureWorkspaceName {
		t.Fatalf("the migrated workspace's name = %q, want %q", ws.Name, fixtureWorkspaceName)
	}
}

func TestTheOpenPathRecordsWhichMigrationsRan(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	log := dlog.NewTestLogger()

	// Act
	handle, err := Open(context.Background(), path, WithLogger(log))
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	record, ok := recordFor(log, "daemon.wsm.open", "info", "migrated the state database forward")
	if !ok {
		t.Fatalf("the migration was not recorded: %v", log.Records())
	}
	if got, _ := record.Context["migrations"].(string); !strings.Contains(got, "4:ported_prompts") {
		t.Fatalf("the migration record names %q, want it to name the 4:ported_prompts step", got)
	}
}

func TestTheMigrationCopiesTheFileAsideFirst(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	log := dlog.NewTestLogger()

	// Act
	handle, err := Open(context.Background(), path, WithLogger(log))
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert — the copy the record names is on disk.
	record, ok := recordFor(log, "daemon.wsm.open", "info", "migrated the state database forward")
	if !ok {
		t.Fatalf("the migration was not recorded: %v", log.Records())
	}
	backup, _ := record.Context["backup"].(string)
	if backup == "" {
		t.Fatalf("the migration record names no pre-migration copy: %v", record.Context)
	}
	if _, err := os.Stat(backup); err != nil {
		t.Fatalf("the pre-migration copy %q the record names: %v", backup, err)
	}
}

func TestOpenLeavesADatabaseAlreadyAtThisLayoutAlone(t *testing.T) {
	// Arrange — a file this build itself wrote, so nothing is to be migrated.
	path := writableStore(t)
	log := dlog.NewTestLogger()

	// Act
	handle, err := Open(context.Background(), path, WithLogger(log))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert — no step ran, so no copy was taken either.
	if _, ok := recordFor(log, "daemon.wsm.open", "info", "migrated the state database forward"); ok {
		t.Fatalf("a database already at layout %d was migrated: %v", LayoutVersion, log.Records())
	}
	if copies := backupsBeside(t, path); len(copies) != 0 {
		t.Fatalf("a database already at layout %d was copied aside: %v", LayoutVersion, copies)
	}
}

func TestOpenRefusesALayoutNewerThanThisBuild(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	stampRaw(t, path, LayoutVersion+1)
	log := dlog.NewTestLogger()

	// Act
	_, err := Open(context.Background(), path, WithLogger(log))

	// Assert — a downgrade is not a migration.
	var refusal *LayoutError
	if !errors.As(err, &refusal) {
		t.Fatalf("Open on a newer layout = %v, want a *LayoutError", err)
	}
	if !strings.Contains(refusal.Error(), "a downgrade is not a migration") {
		t.Fatalf("refusal = %q, want it to say a downgrade is not a migration", refusal.Error())
	}
}

func TestOpenRefusesALayoutNoMigrationReaches(t *testing.T) {
	// Arrange — older than the oldest step this build carries.
	path := layout3Fixture(t)
	stampRaw(t, path, 1)

	// Act
	_, err := Open(context.Background(), path)

	// Assert
	var refusal *LayoutError
	if !errors.As(err, &refusal) || refusal.File != 1 {
		t.Fatalf("Open on layout 1 = %v, want a *LayoutError naming file version 1", err)
	}
}

func TestOpenReadOnlyRefusesAnOlderLayoutRatherThanMigratingIt(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	_, err := OpenReadOnly(context.Background(), path)

	// Assert
	var refusal *LayoutError
	if !errors.As(err, &refusal) {
		t.Fatalf("OpenReadOnly on a layout-3 database = %v, want a *LayoutError", err)
	}
	if !strings.Contains(refusal.Error(), "read-only handle cannot migrate") {
		t.Fatalf("refusal = %q, want it to name the read-only handle", refusal.Error())
	}
}

func TestAFailedMigrationIsRolledBackAndRefused(t *testing.T) {
	// Arrange — a layout-3 file that ALREADY carries the table the 3 -> 4 step
	// creates, so the step's DDL fails partway through the transaction.
	path := layout3Fixture(t)
	execRaw(t, path, `CREATE TABLE ported_prompts (workspace_id TEXT PRIMARY KEY)`)

	// Act
	_, err := Open(context.Background(), path)

	// Assert
	var refusal *MigrationError
	if !errors.As(err, &refusal) {
		t.Fatalf("Open on an unmigratable layout-3 database = %v, want a *MigrationError", err)
	}
	if refusal.From != 3 || refusal.To != 4 {
		t.Fatalf("refusal = %+v, want a 3 -> 4 step", refusal)
	}
}

func TestAFailedMigrationLeavesTheFileAtItsOwnLayout(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	execRaw(t, path, `CREATE TABLE ported_prompts (workspace_id TEXT PRIMARY KEY)`)

	// Act
	if _, err := Open(context.Background(), path); err == nil {
		t.Fatal("Open on an unmigratable layout-3 database succeeded, want a refusal")
	}

	// Assert — the rollback left the stamp where it was.
	if got := rawScalar[int](t, path, `SELECT version FROM layout WHERE id = 1`); got != 3 {
		t.Fatalf("layout version after a refused migration = %d, want 3", got)
	}
}

func TestAFailedMigrationLeavesTheRowsIntact(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	execRaw(t, path, `CREATE TABLE ported_prompts (workspace_id TEXT PRIMARY KEY)`)

	// Act
	if _, err := Open(context.Background(), path); err == nil {
		t.Fatal("Open on an unmigratable layout-3 database succeeded, want a refusal")
	}

	// Assert
	if got := rawScalar[int](t, path, `SELECT count(*) FROM workspaces`); got != 1 {
		t.Fatalf("workspaces after a refused migration = %d rows, want the fixture's 1", got)
	}
}

func TestAFailedMigrationNamesThePreMigrationCopy(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	execRaw(t, path, `CREATE TABLE ported_prompts (workspace_id TEXT PRIMARY KEY)`)

	// Act
	_, err := Open(context.Background(), path)

	// Assert
	var refusal *MigrationError
	if !errors.As(err, &refusal) {
		t.Fatalf("Open = %v, want a *MigrationError", err)
	}
	if _, statErr := os.Stat(refusal.Backup); statErr != nil {
		t.Fatalf("the refusal names copy %q: %v", refusal.Backup, statErr)
	}
}

func TestPlanMigrationsAnswersTheChainItCanApply(t *testing.T) {
	tests := []struct {
		name string
		from int
		want bool
	}{
		{name: "one step behind", from: LayoutVersion - 1, want: true},
		{name: "already current", from: LayoutVersion, want: false},
		{name: "newer than this build", from: LayoutVersion + 1, want: false},
		{name: "older than any step", from: 0, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			plan, ok := planMigrations(tc.from)

			// Assert
			if ok != tc.want {
				t.Fatalf("planMigrations(%d) = %v, want %v", tc.from, ok, tc.want)
			}
			if ok && plan[len(plan)-1].To != LayoutVersion {
				t.Fatalf("planMigrations(%d) ends at layout %d, want %d", tc.from, plan[len(plan)-1].To, LayoutVersion)
			}
		})
	}
}

// TestTheMigrationListReachesThisBuildsLayout pins the defect that started
// this file: a build whose LayoutVersion has moved past the last migration
// refuses every database the previous build wrote.
func TestTheMigrationListReachesThisBuildsLayout(t *testing.T) {
	// Arrange / Act
	last := migrations[len(migrations)-1]

	// Assert
	if last.To != LayoutVersion {
		t.Fatalf("the last migration carries a file to layout %d, but this build writes %d; every older database would be refused", last.To, LayoutVersion)
	}
}

// fixtureWorkspaceID and fixtureWorkspaceName identify the workspace row the
// layout-3 fixture carries, so a migration's assertions can name the user's
// own data rather than a row count.
const (
	fixtureWorkspaceID   = WorkspaceID("ws-layout3")
	fixtureWorkspaceName = "carried-forward"
)

// layout3Fixture builds a real layout-3 database from the FROZEN old schema in
// testdata and seeds it with a repository and a workspace, so what a migration
// carries forward is the shape a previous build actually wrote — never one the
// current code produced.
func layout3Fixture(t *testing.T) string {
	t.Helper()
	ddl, err := os.ReadFile(filepath.Join("testdata", "layout3.sql"))
	if err != nil {
		t.Fatalf("read the layout-3 schema: %v", err)
	}
	path := filepath.Join(t.TempDir(), "wsm.db")
	withRawDB(t, path, func(db *sql.DB) {
		if _, err := db.Exec(string(ddl)); err != nil {
			t.Fatalf("create the layout-3 fixture: %v", err)
		}
		if _, err := db.Exec(`INSERT INTO repositories (id, dir, name, default_branch) VALUES ('repo-1', '/tmp/repo-1', 'repo', 'master')`); err != nil {
			t.Fatalf("seed the fixture's repository: %v", err)
		}
		if _, err := db.Exec(`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
			VALUES (?, 'repo-1', '/tmp/ws-1', ?, 'feature', 'master', 0, 0, 0, 1)`, string(fixtureWorkspaceID), fixtureWorkspaceName); err != nil {
			t.Fatalf("seed the fixture's workspace: %v", err)
		}
	})
	return path
}

// withRawDB opens the file with the bare driver, so a test can write what the
// package's own open path would refuse to produce.
func withRawDB(t *testing.T, path string, body func(*sql.DB)) {
	t.Helper()
	db, err := sql.Open("sqlite", path)
	if err != nil {
		t.Fatalf("open %q raw: %v", path, err)
	}
	defer db.Close()
	body(db)
}

// execRaw runs one statement against the file without this package's open
// path, which is how a test arranges a database a migration cannot apply to.
func execRaw(t *testing.T, path, query string, args ...any) {
	t.Helper()
	withRawDB(t, path, func(db *sql.DB) {
		if _, err := db.Exec(query, args...); err != nil {
			t.Fatalf("exec %q: %v", query, err)
		}
	})
}

// stampRaw rewrites the file's layout version without opening it through this
// package, so a test can stand in for a file another build wrote.
func stampRaw(t *testing.T, path string, version int) {
	t.Helper()
	execRaw(t, path, `UPDATE layout SET version = ? WHERE id = 1`, version)
}

// rawScalar reads one value straight out of the file, for the assertions whose
// subject is that the file was NOT opened successfully.
func rawScalar[T any](t *testing.T, path, query string, args ...any) T {
	t.Helper()
	var out T
	withRawDB(t, path, func(db *sql.DB) {
		if err := db.QueryRow(query, args...).Scan(&out); err != nil {
			t.Fatalf("rawScalar %q: %v", query, err)
		}
	})
	return out
}

// backupsBeside lists the pre-migration copies sitting next to a database.
func backupsBeside(t *testing.T, path string) []string {
	t.Helper()
	got, err := filepath.Glob(path + ".layout*.bak-*")
	if err != nil {
		t.Fatalf("glob the copies beside %q: %v", path, err)
	}
	return got
}

// recordFor answers the first captured record matching an operation, level and
// message, so a test can assert on the fields it carried.
func recordFor(log *dlog.TestLogger, operation, level, message string) (dlog.Record, bool) {
	for _, record := range log.Records() {
		if record.Operation == operation && record.Level == level && record.Message == message {
			return record, true
		}
	}
	return dlog.Record{}, false
}

// THE DURABLE RESIDUE. A build before the creation path minted the host
// identity filed session rows with an empty one, and the host view is withheld
// for such a row forever. The layout-5 step is the one-shot catch-up over that
// backlog.

func TestTheMigrationMintsAnIdentityForASessionRowThatCarriesNone(t *testing.T) {
	// Arrange — the residue: a session row filed with no host identity.
	path := layout3Fixture(t)
	seedIdentitylessSession(t, path)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	got := scalar[string](t, s, `SELECT host_session_id FROM sessions WHERE workspace_id = 'ws-layout3'`)
	if got == "" {
		t.Fatalf("the healed session still carries no host session id")
	}
}

func TestTheMintedIdentityHasTheShapeTheDaemonMints(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	seedIdentitylessSession(t, path)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert — sixteen lowercase hex characters, exactly NewHostSessionID's shape.
	s := handle.(*store)
	got := scalar[string](t, s, `SELECT host_session_id FROM sessions WHERE workspace_id = 'ws-layout3'`)
	if !regexp.MustCompile(`^[0-9a-f]{16}$`).MatchString(got) {
		t.Fatalf("the minted identity = %q, want sixteen lowercase hex characters", got)
	}
}

func TestTheMigrationLeavesAnIdentityItAlreadyCarriesAlone(t *testing.T) {
	// Arrange — a session row that already names its identity.
	path := layout3Fixture(t)
	execRaw(t, path, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at)
		VALUES ('ws-layout3', 'kept-identity', 'vendor-1', '/root/.claude', 'opus', 'default', 1, 1)`)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[string](t, s, `SELECT host_session_id FROM sessions WHERE workspace_id = 'ws-layout3'`); got != "kept-identity" {
		t.Fatalf("host_session_id after the migration = %q, want it left alone", got)
	}
}

// seedIdentitylessSession files the residue row directly, because PutSession
// refuses to produce one.
func seedIdentitylessSession(t *testing.T, path string) {
	t.Helper()
	execRaw(t, path, `INSERT INTO sessions (workspace_id, host_session_id, vendor_session_id, config_dir, model, permission_mode, started_at, last_engagement_at)
		VALUES ('ws-layout3', '', '', '/root/.claude', 'opus', 'default', 1, 1)`)
}
