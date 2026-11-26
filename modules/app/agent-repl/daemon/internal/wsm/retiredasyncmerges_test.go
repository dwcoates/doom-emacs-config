package wsm

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
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

func TestTheRetiredAsyncMergeRowsStepIsAdditiveWithARowEdit(t *testing.T) {
	// Arrange / Act
	step := migrationTo(t, 223)

	// Assert
	if step.Name != "retired_async_merge_rows" || step.Kind != MigrationAdditive || step.Rewrite == nil {
		t.Fatalf("migration 223 = %s (%v, rewrite %v), want retired_async_merge_rows, additive, with its row edit", step.Name, step.Kind, step.Rewrite != nil)
	}
}

func TestTheRetiredAsyncMergeRowsStepCarriesTheRetiredPickOrigin(t *testing.T) {
	tests := []struct {
		name   string
		origin string
		want   string
	}{
		{name: "a pick turn becomes a conflict repair", origin: "PROMPT_ORIGIN_MERGE_PICK", want: "PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR"},
		{name: "a turn of another origin is left alone", origin: "PROMPT_ORIGIN_USER_SENT", want: "PROMPT_ORIGIN_USER_SENT"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, 222)
			execRaw(t, path, `INSERT INTO turns (id, workspace_id, text, origin, displaced, started_at) VALUES ('t-1', ?, 'hi', ?, 0, 1)`,
				string(fixtureWorkspaceID), tt.origin)

			// Act
			execRaw(t, path, migrationTo(t, 223).DDL)

			// Assert
			if got := rawScalar[string](t, path, `SELECT origin FROM turns WHERE id = 't-1'`); got != tt.want {
				t.Fatalf("the turn's origin = %s, want %s", got, tt.want)
			}
		})
	}
}

// queueEntry is a stored merge queue entry labelled TEXT.
func queueEntry(text string) *frontendv1.FeedMergeQueueEntry {
	return &frontendv1.FeedMergeQueueEntry{Label: &frontendv1.FeedMergeQueueLabel{Text: text}}
}

// bubbledQueueEntry is a stored merge queue entry labelled TEXT that still
// carries the retired bubble.
func bubbledQueueEntry(t *testing.T, text string) *frontendv1.FeedMergeQueueEntry {
	t.Helper()
	return withRetiredBytes(queueEntry(text), retiredMergeQueueEntryBubble, marshalled(t, &frontendv1.FeedMergeQueueLabel{Text: "bubble"}))
}

// queueTabRow is a stored queue tab row holding QUEUE.
func queueTabRow(queue *frontendv1.FeedMergeQueue) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
		Kind: &frontendv1.FeedMergeTab_Queue{Queue: &frontendv1.FeedMergeTabQueue{Queue: queue}},
	}}}
}

// carriesUnknown reports whether any message of ROW carries unknown fields.
func carriesUnknown(t *testing.T, row *frontendv1.FeedRow) bool {
	t.Helper()
	found := false
	if _, err := walkMessages(row.ProtoReflect(), func(m protoreflect.Message) (bool, error) {
		found = found || len(m.GetUnknown()) > 0
		return false, nil
	}); err != nil {
		t.Fatal(err)
	}
	return found
}

// openedAt223 opens the layout-222 file at PATH holding RAW as its one
// stored row, so layout 223 runs over it, and answers the row it left.
func openedAt223(t *testing.T, raw []byte) []byte {
	t.Helper()
	path := fixtureAt(t, 222)
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'row-1', 0, 'k1', ?)`, raw)
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-222 database: %v", err)
	}
	t.Cleanup(func() { handle.Close() })
	return scalar[[]byte](t, handle.(*store), `SELECT row FROM durable_feed_rows WHERE row_id = 'row-1'`)
}

func TestTheRetiredAsyncMergeRowsStepDropsTheBubbleOfStoredQueueEntries(t *testing.T) {
	tests := []struct {
		name   string
		stored func(t *testing.T) *frontendv1.FeedMergeQueue
	}{
		{name: "an entry ahead", stored: func(t *testing.T) *frontendv1.FeedMergeQueue {
			return &frontendv1.FeedMergeQueue{Ahead: []*frontendv1.FeedMergeQueueEntry{bubbledQueueEntry(t, "ahead")}, Current: queueEntry("current")}
		}},
		{name: "the current entry", stored: func(t *testing.T) *frontendv1.FeedMergeQueue {
			return &frontendv1.FeedMergeQueue{Ahead: []*frontendv1.FeedMergeQueueEntry{queueEntry("ahead")}, Current: bubbledQueueEntry(t, "current")}
		}},
		{name: "an entry behind", stored: func(t *testing.T) *frontendv1.FeedMergeQueue {
			return &frontendv1.FeedMergeQueue{Current: queueEntry("current"), Behind: []*frontendv1.FeedMergeQueueEntry{bubbledQueueEntry(t, "behind")}}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			stored := tt.stored(t)
			want := proto.Clone(stored).(*frontendv1.FeedMergeQueue)
			for _, e := range append(append(want.GetAhead(), want.GetCurrent()), want.GetBehind()...) {
				e.ProtoReflect().SetUnknown(nil)
			}

			// Act
			out := openedAt223(t, marshalled(t, queueTabRow(stored)))

			// Assert
			row := &frontendv1.FeedRow{}
			if err := proto.Unmarshal(out, row); err != nil {
				t.Fatal(err)
			}
			if carriesUnknown(t, row) {
				t.Fatal("the stored queue tab still carries a retired field")
			}
			if !proto.Equal(row, queueTabRow(want)) {
				t.Fatalf("the stored queue tab = %v, want %v with nothing but the bubble dropped", row, queueTabRow(want))
			}
		})
	}
}

func TestTheRetiredAsyncMergeRowsStepLeavesEveryOtherRowByteForByte(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.FeedRow
	}{
		{name: "a queue tab without a retired field", row: queueTabRow(&frontendv1.FeedMergeQueue{
			Ahead: []*frontendv1.FeedMergeQueueEntry{queueEntry("ahead")}, Current: queueEntry("current"),
			Behind: []*frontendv1.FeedMergeQueueEntry{queueEntry("behind")},
		})},
		{name: "a merge head", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Merge{Merge: &frontendv1.FeedMerge{Head: &frontendv1.FeedMergeHead{
				BranchLine: &frontendv1.FeedMergeBranchLine{Source: &frontendv1.FeedMergeBranchLineSource{Text: "feature"}},
			}}},
		}}}},
		{name: "a user prompt", row: &frontendv1.FeedRow{Row: &frontendv1.FeedRow_UserPrompt{UserPrompt: &frontendv1.FeedUserPrompt{}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			raw := marshalled(t, tt.row)

			// Act
			out := openedAt223(t, raw)

			// Assert
			if string(out) != string(raw) {
				t.Fatal("a row with nothing retired was rewritten")
			}
		})
	}
}
