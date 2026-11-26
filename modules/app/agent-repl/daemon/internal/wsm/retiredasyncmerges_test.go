package wsm

import (
	"testing"
)

// columnCount answers how many of COLUMNS TABLE holds in the file at PATH.
func columnCount(t *testing.T, path, table string, columns ...string) int {
	t.Helper()
	got := 0
	for _, c := range columns {
		got += rawScalar[int](t, path, `SELECT count(*) FROM pragma_table_info(?) WHERE name = ?`, table, c)
	}
	return got
}

var (
	// landingEffectColumns are the one-shot's own landing effects, which
	// layout 221 adds.
	landingEffectColumns = []string{"landing_close_after", "landing_cleanup_dir", "landing_compact_requester", "landing_profile_gate"}
	// retiredMergeWorkspaceColumns are the merge workspace's marks and the
	// async merge's landing effects, which layout 222 drops.
	retiredMergeWorkspaceColumns = []string{"merge_of", "merge_request_id", "merge_fork_head", "merge_revealed",
		"merge_cleanup_worktree", "merge_compact_after", "merge_close_on_landing", "merge_cleanup_dir", "landed_head", "landed_at"}
)

func TestTheLandingEffectsStepIsAdditiveAndAddsTheEffectsBesideTheOldOnes(t *testing.T) {
	// Arrange
	step := migrationTo(t, 221)
	path := fixtureAt(t, 220)

	// Act
	execRaw(t, path, step.DDL)

	// Assert
	if step.Name != "landing_effects" || step.Kind != MigrationAdditive || step.Rewrite == nil {
		t.Fatalf("migration 221 = %s (%v), want landing_effects, additive, with its row edit", step.Name, step.Kind)
	}
	if got := columnCount(t, path, "workspaces", landingEffectColumns...); got != len(landingEffectColumns) {
		t.Fatalf("%d of the %d landing effect columns exist, want all", got, len(landingEffectColumns))
	}
	if got := columnCount(t, path, "workspaces", retiredMergeWorkspaceColumns...); got != len(retiredMergeWorkspaceColumns) {
		t.Fatalf("%d of the %d async merge columns survive the additive step, want all: the previous build still reads them", got, len(retiredMergeWorkspaceColumns))
	}
}

func TestTheAsyncMergeRetirementIsAContractStep(t *testing.T) {
	// Arrange / Act
	step := migrationTo(t, 222)

	// Assert
	if step.Name != "retired_async_merges" || step.Kind != MigrationContract || step.Rewrite != nil {
		t.Fatalf("migration 222 = %s (%v, rewrite %v), want retired_async_merges, a contract step with no rewrite", step.Name, step.Kind, step.Rewrite != nil)
	}
}

func TestTheAsyncMergeRetirementDropsEveryRetiredShape(t *testing.T) {
	tests := []struct {
		name    string
		table   string
		columns []string
	}{
		{name: "the merge workspace's marks and the old landing effects", table: "workspaces", columns: retiredMergeWorkspaceColumns},
		{name: "the queue's detached-request columns", table: "merge_queue", columns: []string{"source_detach_id", "source_pick", "source_compact_after", "source_pre_deploy_hooks", "source_post_deploy_hooks"}},
		{name: "the off-queue record's adopted mark and request columns", table: "off_queue_merges", columns: []string{"adopted", "source_detach_id", "source_pre_deploy_hooks"}},
		{name: "the turn mirror", table: "turns", columns: []string{"mirror"}},
		{name: "the deploying merge's credit and async mark", table: "deploying_merges", columns: []string{"credited", "async"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, 221)

			// Act
			execRaw(t, path, migrationTo(t, 222).DDL)

			// Assert
			if got := columnCount(t, path, tt.table, tt.columns...); got != 0 {
				t.Fatalf("%d of %s's retired columns %v survive the contract step, want none", got, tt.table, tt.columns)
			}
		})
	}
}

func TestTheAsyncMergeRetirementDropsTheAsyncTables(t *testing.T) {
	for _, table := range []string{"async_merge_requests", "owed_handbacks", "removed_merge_workspaces"} {
		t.Run(table, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, 221)

			// Act
			execRaw(t, path, migrationTo(t, 222).DDL)

			// Assert
			if got := rawScalar[int](t, path, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = ?`, table); got != 0 {
				t.Fatalf("%s survives the contract step", table)
			}
		})
	}
}

func TestTheAsyncMergeRetirementKeepsTheLandingEffects(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 221)

	// Act
	execRaw(t, path, migrationTo(t, 222).DDL)

	// Assert
	if got := columnCount(t, path, "workspaces", landingEffectColumns...); got != len(landingEffectColumns) {
		t.Fatalf("%d of the %d landing effect columns survive the contract step, want all", got, len(landingEffectColumns))
	}
}

func TestTheAsyncMergeRetirementDropsAMergeWorkspacesQueueRowAndKeepsTheWorkspace(t *testing.T) {
	// Arrange: the fixture's one workspace is a merge workspace standing in
	// its queue.
	path := fixtureAt(t, 221)
	execRaw(t, path, `UPDATE workspaces SET merge_of = id`)
	execRaw(t, path, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at) VALUES ('/tmp/repo-1', ?, 1, 0, 1)`, string(fixtureWorkspaceID))

	// Act
	execRaw(t, path, migrationTo(t, 222).DDL)

	// Assert
	if got := rawScalar[int](t, path, `SELECT count(*) FROM merge_queue`); got != 0 {
		t.Fatalf("%d queue rows survive, want the merge workspace's dropped", got)
	}
	if got := rawScalar[int](t, path, `SELECT count(*) FROM workspaces WHERE id = ? AND closed = 0`, string(fixtureWorkspaceID)); got != 1 {
		t.Fatal("the merge workspace itself was dropped or closed, want it left open as a visible child")
	}
}
