package wsm

import (
	"context"
	"database/sql"

	"agentrepl/logging/logsite"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
)

// LAYOUT 221 RETIRES THE ASYNC MERGE (owner request, 2026-11-26;
// docs/protobuf-design/2026-11-26-children-replace-async-merges.md). A
// workspace lands work it already has as a one-shot child cut from that
// work's branch, so nothing is merged in a hidden merge workspace any more.
// The step:
//
//   - drops every async request, and every queue row and off-queue record a
//     merge workspace's request held: none can run;
//   - drops the merge workspace's marks (workspaces.merge_of, its request,
//     its fork head and its reveal), its removal record, the owed handbacks
//     and the turn mirrors, which nothing reads;
//   - carries the async merge's landing effects onto the one-shot's own
//     (LandingEffects): the close-after, the cleanup directory and the
//     compaction of the requester, renamed for what they now are, plus the
//     profile gate. A merge workspace's effects are cleared first, since its
//     merge never runs again;
//   - drops the queue's and the off-queue record's detached-request columns,
//     and the adopted mark, which only a merge workspace ever carried;
//   - drops a merge workspace's deploy still being checked, and the deploying
//     merge's credit and async mark: every merge is its own workspace's;
//   - drops every ledger interval of the retired pick step, which no bubble
//     or roster stage draws any more.
//
// A MERGE WORKSPACE STILL OPEN STAYS OPEN, as the visible child of the
// workspace it was forked for: children are never hidden, and its work is
// the user's to land or close.
//
// IT IS A CONTRACT STEP: it only removes shapes no build after it writes, and
// the build before it still reads the columns it drops.
const retiredAsyncMergesDDL = `
DELETE FROM merge_queue WHERE source_detach_id IS NOT NULL;
DELETE FROM merge_queue WHERE workspace_id IN (SELECT id FROM workspaces WHERE merge_of IS NOT NULL);
DELETE FROM off_queue_merges WHERE source_detach_id IS NOT NULL OR adopted = 1;
DELETE FROM off_queue_merges WHERE workspace_id IN (SELECT id FROM workspaces WHERE merge_of IS NOT NULL);
UPDATE workspaces SET merge_compact_after = 0, merge_close_on_landing = NULL, merge_cleanup_worktree = 0, merge_cleanup_dir = NULL
 WHERE merge_of IS NOT NULL;
DROP INDEX workspaces_by_merge_of;
ALTER TABLE workspaces DROP COLUMN merge_of;
ALTER TABLE workspaces DROP COLUMN merge_request_id;
ALTER TABLE workspaces DROP COLUMN merge_fork_head;
ALTER TABLE workspaces DROP COLUMN merge_revealed;
ALTER TABLE workspaces DROP COLUMN merge_cleanup_worktree;
ALTER TABLE workspaces RENAME COLUMN merge_compact_after TO landing_compact_requester;
ALTER TABLE workspaces RENAME COLUMN merge_close_on_landing TO landing_close_after;
ALTER TABLE workspaces RENAME COLUMN merge_cleanup_dir TO landing_cleanup_dir;
ALTER TABLE workspaces ADD COLUMN landing_profile_gate INTEGER NOT NULL DEFAULT 0;
ALTER TABLE turns DROP COLUMN mirror;
DROP TABLE async_merge_requests;
DROP TABLE owed_handbacks;
DROP TABLE removed_merge_workspaces;
ALTER TABLE merge_queue DROP COLUMN source_detach_id;
ALTER TABLE merge_queue DROP COLUMN source_detach_name;
ALTER TABLE merge_queue DROP COLUMN source_pick;
ALTER TABLE merge_queue DROP COLUMN pick_started_at;
ALTER TABLE merge_queue DROP COLUMN pick_turn;
ALTER TABLE merge_queue DROP COLUMN source_detach_maker;
ALTER TABLE merge_queue DROP COLUMN source_detach_target_maker;
ALTER TABLE merge_queue DROP COLUMN source_compact_after;
ALTER TABLE merge_queue DROP COLUMN source_close_on_landing;
ALTER TABLE merge_queue DROP COLUMN source_cleanup_worktree;
ALTER TABLE merge_queue DROP COLUMN source_cleanup_dir;
ALTER TABLE merge_queue DROP COLUMN source_pre_deploy_hooks;
ALTER TABLE merge_queue DROP COLUMN source_post_deploy_hooks;
ALTER TABLE off_queue_merges DROP COLUMN adopted;
ALTER TABLE off_queue_merges DROP COLUMN source_detach_id;
ALTER TABLE off_queue_merges DROP COLUMN source_detach_name;
ALTER TABLE off_queue_merges DROP COLUMN source_pick;
ALTER TABLE off_queue_merges DROP COLUMN source_detach_maker;
ALTER TABLE off_queue_merges DROP COLUMN source_detach_target_maker;
ALTER TABLE off_queue_merges DROP COLUMN source_compact_after;
ALTER TABLE off_queue_merges DROP COLUMN source_close_on_landing;
ALTER TABLE off_queue_merges DROP COLUMN source_cleanup_worktree;
ALTER TABLE off_queue_merges DROP COLUMN source_cleanup_dir;
ALTER TABLE off_queue_merges DROP COLUMN source_pre_deploy_hooks;
ALTER TABLE off_queue_merges DROP COLUMN source_post_deploy_hooks;
DELETE FROM deploying_merges WHERE async = 1;
ALTER TABLE deploying_merges DROP COLUMN credited;
ALTER TABLE deploying_merges DROP COLUMN async;
DELETE FROM merge_tab_intervals WHERE kind = 'pick';
`

// The retired fields of stored feed rows this step drops or carries.
const (
	// retiredMergeHeadMode is frontend.v1.FeedMergeHead.mode: every merge
	// now runs in place, so a stored head's mode says nothing.
	retiredMergeHeadMode protowire.Number = 7
	// retiredMergeTabResolving is frontend.v1.FeedMergeTab.resolving, an
	// async merge's instruction turn. Its content was mirrored from the
	// merge workspace's feed, which nothing mirrors any more, so the tab
	// has nothing to draw and is dropped.
	retiredMergeTabResolving protowire.Number = 12
)

// retireAsyncMergeRows is layout 221's row edit: a stored merge head loses its
// mode, and a stored resolving tab is dropped.
func retireAsyncMergeRows(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	if err := dropDurableRows(ctx, tx, log, logsite.Msg("dropped the stored resolving tabs of retired async merges"), isRetiredResolvingTabRow); err != nil {
		return err
	}
	return rewriteDurableRows(ctx, tx, log, logsite.Msg("dropped the retired mode of stored merge heads"),
		func(raw []byte) ([]byte, bool, error) {
			return editFeedRow(raw, func(row *frontendv1.FeedRow) (bool, error) {
				return walkMessages(row.ProtoReflect(), func(m protoreflect.Message) (bool, error) {
					if _, head := m.Interface().(*frontendv1.FeedMergeHead); !head {
						return false, nil
					}
					return dropUnknown(m, retiredMergeHeadMode)
				})
			})
		})
}

// isRetiredResolvingTabRow reports whether RAW is a merge tab row of the
// retired resolving kind: no kind of today's, and the retired kind's field.
func isRetiredResolvingTabRow(raw []byte) (bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return false, err
	}
	tab := row.GetMergeTab()
	if tab == nil || tab.GetKind() != nil {
		return false, nil
	}
	retired := false
	err := eachUnknown(tab.ProtoReflect(), func(num protowire.Number, _ protowire.Type, _ int, _ []byte) {
		if num == retiredMergeTabResolving {
			retired = true
		}
	})
	return retired, err
}
