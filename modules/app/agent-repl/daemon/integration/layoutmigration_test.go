//go:build integration

package integration

import (
	"database/sql"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// TestBootServesAWorkspaceRegisteredUnderTheOlderLayout is the defect this
// suite had no cover for: a deployed daemon whose LayoutVersion had moved past
// the state database on disk exited at boot with a layout refusal, so nothing
// served the editor and the whole workspace state was unreachable. The boot
// must migrate that file forward and go on serving what it holds.
func TestBootServesAWorkspaceRegisteredUnderTheOlderLayout(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace, then the state file taken back to the
	// layout the build before `ported_prompts` wrote.
	f := newRegistered(t, harness.Opts{DurableState: true})
	id := f.ws.GetId()
	f.d.Stop()
	demoteToLayout3(t, f.d)

	// Act: boot a daemon on that older state root.
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})

	// Assert: the workspace registered under layout 3 is served.
	roster := d2.WatchRoster()
	awaitRoster(t, d2, roster, "the roster after a migrated boot", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, id) != nil
	})
}

// layout3Schema is the frozen layout-3 schema the wsm package's migration
// tests build from (internal/wsm/testdata/layout3.sql): the shape the build
// before `ported_prompts` wrote, captured once and never regenerated.
var layout3Schema = filepath.Join("..", "internal", "wsm", "testdata", "layout3.sql")

// demoteToLayout3 replaces a stopped daemon's state database with a GENUINE
// layout-3 file: one built from the frozen layout-3 schema, carrying the
// repositories and workspaces the daemon registered. The daemon must be
// stopped: it holds the sole writing handle while it runs.
//
// IT BUILDS THE OLD FILE RATHER THAN UNDOING THE NEW ONE. It once undid every
// migration step since layout 3 from a hand-kept map of inverses, and every
// branch that appended a step without its inverse failed this test at the
// merge gate (five merges between 2026-10-31 and 2026-11-01, steps 92 to 97)
// rather than anywhere its author ran tests. A file built from the frozen
// schema needs nothing from a new step, so a new step cannot break it.
func demoteToLayout3(t *testing.T, d *harness.Daemon) {
	t.Helper()
	ddl, err := os.ReadFile(layout3Schema)
	if err != nil {
		t.Fatalf("read the frozen layout-3 schema: %v", err)
	}
	type repoRow struct{ id, dir, name, defaultBranch string }
	type workspaceRow struct {
		id, repoID, dir, name, branch, parentBranch string
		createdAt                                   int64
	}
	var repos []repoRow
	var workspaces []workspaceRow
	d.WithDB(func(db *sql.DB) {
		rows, err := db.Query(`SELECT id, dir, name, default_branch FROM repositories`)
		if err != nil {
			t.Fatalf("read the registered repositories: %v", err)
		}
		defer rows.Close()
		for rows.Next() {
			var r repoRow
			if err := rows.Scan(&r.id, &r.dir, &r.name, &r.defaultBranch); err != nil {
				t.Fatalf("scan a registered repository: %v", err)
			}
			repos = append(repos, r)
		}
		if err := rows.Err(); err != nil {
			t.Fatalf("read the registered repositories: %v", err)
		}
		wsRows, err := db.Query(`SELECT id, repo_id, dir, name, branch, parent_branch, created_at FROM workspaces`)
		if err != nil {
			t.Fatalf("read the registered workspaces: %v", err)
		}
		defer wsRows.Close()
		for wsRows.Next() {
			var w workspaceRow
			if err := wsRows.Scan(&w.id, &w.repoID, &w.dir, &w.name, &w.branch, &w.parentBranch, &w.createdAt); err != nil {
				t.Fatalf("scan a registered workspace: %v", err)
			}
			workspaces = append(workspaces, w)
		}
		if err := wsRows.Err(); err != nil {
			t.Fatalf("read the registered workspaces: %v", err)
		}
	})
	if len(workspaces) == 0 {
		t.Fatalf("the daemon registered no workspace to carry into the layout-3 file")
	}
	path := d.DBPath()
	for _, suffix := range []string{"", "-wal", "-shm"} {
		if err := os.Remove(path + suffix); err != nil && !errors.Is(err, fs.ErrNotExist) {
			t.Fatalf("remove the state database %s: %v", path+suffix, err)
		}
	}
	d.WithDB(func(db *sql.DB) {
		if _, err := db.Exec(string(ddl)); err != nil {
			t.Fatalf("create the layout-3 state database: %v", err)
		}
		for _, r := range repos {
			if _, err := db.Exec(`INSERT INTO repositories (id, dir, name, default_branch) VALUES (?, ?, ?, ?)`,
				r.id, r.dir, r.name, r.defaultBranch); err != nil {
				t.Fatalf("carry repository %s into the layout-3 file: %v", r.id, err)
			}
		}
		for _, w := range workspaces {
			if _, err := db.Exec(`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, closed, attention, is_current, created_at)
				VALUES (?, ?, ?, ?, ?, ?, 0, 0, 0, ?)`,
				w.id, w.repoID, w.dir, w.name, w.branch, w.parentBranch, w.createdAt); err != nil {
				t.Fatalf("carry workspace %s into the layout-3 file: %v", w.id, err)
			}
		}
	})
}
