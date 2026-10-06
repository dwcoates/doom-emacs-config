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

// layout3Undo is the inverse of EVERY migration step this build can apply,
// keyed by the layout version that step produces. It is what takes a state
// file written by THIS build back to a genuine layout-3 file.
//
// IT IS KEYED BY VERSION SO IT CANNOT DRIFT SILENTLY. It was a flat list of
// statements once, hand-maintained against a migration list that grows, and it
// drifted exactly as such a list does: layout 10 appended `feed_text_scale`,
// the list was never extended, and the "layout 3" file it produced still
// carried a layout-10 table. The migration then replayed step 10 onto it, hit
// `table feed_text_scale already exists`, and rolled the whole chain back --
// so both tests here failed on a daemon that had done nothing wrong. The
// production refusal is CORRECT and is deliberately left alone: a migration
// that finds an object it is about to create must refuse rather than write
// over it. What was wrong was the fixture's claim about the file it built.
//
// demoteToLayout3 checks this map covers every version from 4 to
// wsm.LayoutVersion, so appending a migration step without its inverse fails
// loudly here instead of producing a file that is not the layout it claims.
var layout3Undo = map[int][]string{
	4: {
		`DROP INDEX ported_prompts_by_workspace`,
		`DROP TABLE ported_prompts`,
	},
	// The layout-5 step is a BACKFILL: it introduces no shape, so taking the
	// file back past it removes nothing. The rows it healed stay healed, which
	// is harmless -- re-running the backfill over them is a no-op.
	5:  nil,
	6:  {`ALTER TABLE creation_jobs ADD COLUMN one_shot_finish TEXT NOT NULL DEFAULT ''`},
	7:  {`ALTER TABLE sessions DROP COLUMN selected_config_dir`},
	8:  {`ALTER TABLE workspaces DROP COLUMN spawned_shim_pid`},
	9:  {`ALTER TABLE workspaces DROP COLUMN last_activity_at`},
	10: {`DROP TABLE feed_text_scale`},
	11: {`ALTER TABLE idempotency_keys DROP COLUMN accepted_at`},
	12: {`ALTER TABLE held_prompts DROP COLUMN delivery`},
	13: {
		`ALTER TABLE workspaces DROP COLUMN result_read`,
		`ALTER TABLE workspaces DROP COLUMN result_end`,
	},
	14: {
		`ALTER TABLE held_prompts DROP COLUMN coalesced`,
		`ALTER TABLE held_prompts DROP COLUMN act_value`,
		`ALTER TABLE held_prompts DROP COLUMN act_kind`,
	},
	15: {
		`ALTER TABLE merge_queue DROP COLUMN source_branch`,
		`ALTER TABLE merge_queue DROP COLUMN source_workspace`,
		`ALTER TABLE merge_queue DROP COLUMN source_keep_open`,
		`ALTER TABLE merge_queue DROP COLUMN source_kind`,
	},
	16: {`DROP TABLE durable_feed_rows`},
	17: {`ALTER TABLE repositories DROP COLUMN folded`},
	18: {`DROP TABLE rolled_back_turns`},
	19: {`DROP TABLE news_digest_sources`, `DROP TABLE news_digest`},
	20: {`DROP TABLE editor_instance`, `ALTER TABLE news_digest DROP COLUMN latest_made_at`, `ALTER TABLE news_digest DROP COLUMN latest_overlay`},
	21: {`DROP TABLE agent_repl_session`},
	22: {`DROP TABLE news_digest_items`, `ALTER TABLE news_digest DROP COLUMN history_since`},
	23: {`DROP TABLE sidebar_view`, `ALTER TABLE tasks DROP COLUMN folded`},
	24: {`DROP TABLE account_usage`},
	25: {
		`ALTER TABLE account_usage DROP COLUMN seat_sampled_at_ms`,
		`ALTER TABLE account_usage DROP COLUMN seat_currency`,
		`ALTER TABLE account_usage DROP COLUMN seat_spent_minor`,
		`ALTER TABLE account_usage DROP COLUMN seat_allotment_minor`,
	},
}

// demoteToLayout3 takes a stopped daemon's state database back to layout 3 by
// undoing every migration step since layout 3, newest first, and stamping the
// version. The daemon must be stopped: it holds the sole writing handle while
// it runs.
func demoteToLayout3(t *testing.T, d *harness.Daemon) {
	t.Helper()
	for v := 4; v <= wsm.LayoutVersion; v++ {
		if _, covered := layout3Undo[v]; !covered {
			t.Fatalf("layout3Undo has no inverse for migration step %d; a step was appended to wsm.migrations without one, so this fixture cannot build a layout-3 file", v)
		}
	}
	d.WithDB(func(db *sql.DB) {
		// NEWEST FIRST: a step's inverse may depend on shape a later step's
		// inverse has not yet removed.
		for v := wsm.LayoutVersion; v >= 4; v-- {
			for _, stmt := range layout3Undo[v] {
				if _, err := db.Exec(stmt); err != nil {
					t.Fatalf("demote the state database past layout %d (%s): %v", v, stmt, err)
				}
			}
		}
		if _, err := db.Exec(`UPDATE layout SET version = 3 WHERE id = 1`); err != nil {
			t.Fatalf("stamp the demoted state database at layout 3: %v", err)
		}
		// The file must actually BE layout 3, not merely stamped as one: every
		// table a migration since layout 3 creates has to be gone, or the
		// migration this fixture exists to drive will refuse the step that
		// creates it.
		for _, table := range []string{"ported_prompts", "feed_text_scale"} {
			var present int
			if err := db.QueryRow(`SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?`, table).Scan(&present); err != nil {
				t.Fatalf("probe the demoted state database for %s: %v", table, err)
			}
			if present != 0 {
				t.Fatalf("the demoted state database still carries %s; the fixture is not a layout-3 file", table)
			}
		}
	})
}
