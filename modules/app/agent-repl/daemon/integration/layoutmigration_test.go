//go:build integration

package integration

import (
	"database/sql"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
	"claude-repld/internal/wsm"
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
	f := newRegistered(t, harness.Opts{})
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

// TestBootStampsTheMigratedLayoutOnTheStateDatabase pins that the boot's
// migration is DURABLE, not a shape held only in the running process: the file
// carries this build's layout afterwards, so the next boot has nothing to do.
func TestBootStampsTheMigratedLayoutOnTheStateDatabase(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.d.Stop()
	demoteToLayout3(t, f.d)

	// Act
	d2 := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir})
	// The daemon holds the sole writing handle while it runs, so the file is
	// read after its orderly exit.
	d2.Stop()

	// Assert
	var version int
	d2.WithDB(func(db *sql.DB) {
		if err := db.QueryRow(`SELECT version FROM layout WHERE id = 1`).Scan(&version); err != nil {
			t.Fatalf("read the migrated layout version: %v", err)
		}
	})
	// The build's own layout, never a literal: a step appended to the
	// migration list must not have to be restated here to keep this pinned.
	if version != wsm.LayoutVersion {
		t.Fatalf("layout version after a migrated boot = %d, want %d", version, wsm.LayoutVersion)
	}
}

// demoteToLayout3 takes a stopped daemon's state database back to layout 3 by
// undoing exactly what every migration since layout 3 does: the 3 -> 4 step's
// table and index go, the column the 5 -> 6 step drops comes back, and the
// column the 6 -> 7 step adds goes. The daemon must be stopped: it holds the
// sole writing handle while it runs.
func demoteToLayout3(t *testing.T, d *harness.Daemon) {
	t.Helper()
	d.WithDB(func(db *sql.DB) {
		for _, stmt := range []string{
			`DROP INDEX ported_prompts_by_workspace`,
			`DROP TABLE ported_prompts`,
			`ALTER TABLE creation_jobs ADD COLUMN one_shot_finish TEXT NOT NULL DEFAULT ''`,
			`ALTER TABLE sessions DROP COLUMN selected_config_dir`,
			`UPDATE layout SET version = 3 WHERE id = 1`,
		} {
			if _, err := db.Exec(stmt); err != nil {
				t.Fatalf("demote the state database to layout 3 (%s): %v", stmt, err)
			}
		}
		var present int
		if err := db.QueryRow(`SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'ported_prompts'`).Scan(&present); err != nil {
			t.Fatalf("probe the demoted state database: %v", err)
		}
		if present != 0 {
			t.Fatalf("the demoted state database still carries ported_prompts; the fixture is not a layout-3 file")
		}
	})
}
