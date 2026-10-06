package wsm

import (
	"context"
	"database/sql"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/dlog"
)

// orphanFixture writes a database whose single workspace names a repository
// that is not there.
//
// It is built through a RAW handle on purpose. The pragma that enforces
// foreign keys is per-connection and this package's own opens always carry it,
// so a violating row cannot be written through them at all — which is the
// invariant working. A raw handle is exactly the shape of the thing that
// produced one in the field (the `sqlite3` CLI defaults `foreign_keys` OFF),
// and it is the only way a test can arrange the state the boot check exists to
// report.
func orphanFixture(t *testing.T) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "wsm.db")
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	s := handle.(*store)
	testWorkspace(t, s)
	if err := handle.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	withRawDB(t, path, func(db *sql.DB) {
		if _, err := db.Exec(`DELETE FROM repositories`); err != nil {
			t.Fatalf("delete the fixture's repositories: %v", err)
		}
	})
	return path
}

func TestTheSchemaRefusesAWorkspaceNamingAnUnregisteredRepository(t *testing.T) {
	// Arrange — a store opened this package's own way, so foreign keys are on.
	s, _ := testStore(t)

	// Act — an INSERT naming a repository nothing registered.
	_, err := s.db().ExecContext(context.Background(),
		`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		 VALUES ('ws-1', 'repo-nobody-registered', '/tmp/ws-1', 'ws', 'feature', 'master', 0, 0, 0, 1)`)

	// Assert.
	if err == nil {
		t.Fatalf("the insert was accepted; want the foreign key to refuse it")
	}
	if !strings.Contains(strings.ToLower(err.Error()), "foreign key") {
		t.Fatalf("insert refusal = %v, want a foreign-key refusal", err)
	}
}

func TestRegisterWorkspaceWritesTheRepositoryRowBeforeTheWorkspace(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)

	// Act.
	ws := testWorkspace(t, s)

	// Assert — the workspace's repo_id names a repository that is there. The
	// insert could not have committed otherwise, which is the ordering.
	got := scalar[int](t, s, `SELECT count(*) FROM repositories WHERE id = ?`, string(ws.Repo))
	if got != 1 {
		t.Fatalf("repositories rows for the registered workspace's repo = %d, want 1", got)
	}
}

func TestOpenReportsAWorkspaceWhoseRepositoryIsUnregistered(t *testing.T) {
	// Arrange.
	path := orphanFixture(t)
	log := dlog.NewTestLogger()

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert.
	if record, ok := findRecord(log, "error", "workspaces name a repository the registry does not carry"); !ok {
		t.Fatalf("the open recorded no violation; records = %+v", log.Records())
	} else if record.Context["workspaces"] != 1 {
		t.Fatalf("reported workspaces = %v, want 1", record.Context["workspaces"])
	}
}

func TestTheReportedViolationNamesTheRemedy(t *testing.T) {
	// Arrange.
	path := orphanFixture(t)
	log := dlog.NewTestLogger()

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert.
	record, ok := findRecord(log, "error", "workspaces name a repository the registry does not carry")
	if !ok {
		t.Fatalf("the open recorded no violation; records = %+v", log.Records())
	}
	if record.Context["remediation"] != orphanRemediation {
		t.Fatalf("remediation = %v, want %q", record.Context["remediation"], orphanRemediation)
	}
}

func TestTheReportedViolationIsRecordedExactlyOnce(t *testing.T) {
	// Arrange — two violating workspaces, so a per-row record would show up.
	path := orphanFixture(t)
	withRawDB(t, path, func(db *sql.DB) {
		if _, err := db.Exec(`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
			VALUES ('ws-2', 'repo-gone', '/tmp/ws-2', 'second', 'feature', 'master', 0, 0, 0, 2)`); err != nil {
			t.Fatalf("seed the second violating workspace: %v", err)
		}
	})
	log := dlog.NewTestLogger()

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert.
	var seen int
	for _, record := range log.Records() {
		if record.Message == "workspaces name a repository the registry does not carry" {
			seen++
		}
	}
	if seen != 1 {
		t.Fatalf("violation records = %d, want exactly 1", seen)
	}
}

func TestOpenDeletesNothingItReports(t *testing.T) {
	// Arrange.
	path := orphanFixture(t)

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(dlog.NewTestLogger()))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert — the row the open complained about is still there.
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM workspaces`); got != 1 {
		t.Fatalf("workspaces after the reporting open = %d, want the row left alone", got)
	}
}

func TestOpenRecordsNoViolationWhenEveryRepositoryIsRegistered(t *testing.T) {
	// Arrange — an ordinary file with one properly registered workspace.
	path := filepath.Join(t.TempDir(), "wsm.db")
	first, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	testWorkspace(t, first.(*store))
	if err := first.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}
	log := dlog.NewTestLogger()

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert.
	if _, ok := findRecord(log, "error", "workspaces name a repository the registry does not carry"); ok {
		t.Fatalf("a clean file reported a violation; records = %+v", log.Records())
	}
}

// findRecord answers the first captured record at level whose message matches.
func findRecord(log *dlog.TestLogger, level, message string) (dlog.Record, bool) {
	for _, record := range log.Records() {
		if record.Level == level && record.Message == message {
			return record, true
		}
	}
	return dlog.Record{}, false
}

// THE CONSTRAINT SURVIVES THE MIGRATION. `workspaces.repo_id` has carried
// `REFERENCES repositories(id)` since the layout landed -- the frozen layout-3
// fixture declares it too -- so no step in the chain has to add it, and this
// pins that a file carried FORWARD is held to it exactly as a fresh one is. A
// migration that rebuilt the table and dropped the reference would fail here
// rather than in the field.
func TestAMigratedFileStillRefusesAWorkspaceNamingAnUnregisteredRepository(t *testing.T) {
	// Arrange — a layout-3 file carried forward to this build's layout.
	handle, err := Open(context.Background(), layout3Fixture(t), WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()
	s := handle.(*store)

	// Act.
	_, err = s.db().ExecContext(context.Background(),
		`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
		 VALUES ('ws-late', 'repo-nobody-registered', '/tmp/ws-late', 'ws', 'feature', 'master', 0, 0, 0, 2)`)

	// Assert.
	if err == nil {
		t.Fatalf("the migrated file accepted the insert; want the foreign key to refuse it")
	}
	if !strings.Contains(strings.ToLower(err.Error()), "foreign key") {
		t.Fatalf("insert refusal = %v, want a foreign-key refusal", err)
	}
}

func TestOpenReportsAViolationAlreadyInAMigratedFile(t *testing.T) {
	// Arrange — a layout-3 file whose workspace's repository was removed by a
	// handle that did not enforce foreign keys.
	path := layout3Fixture(t)
	execRaw(t, path, `DELETE FROM repositories`)
	log := dlog.NewTestLogger()

	// Act.
	handle, err := Open(context.Background(), path, WithUnsyncedWrites(), WithLogger(log))
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert.
	if _, ok := findRecord(log, "error", "workspaces name a repository the registry does not carry"); !ok {
		t.Fatalf("the migrating open recorded no violation; records = %+v", log.Records())
	}
}
