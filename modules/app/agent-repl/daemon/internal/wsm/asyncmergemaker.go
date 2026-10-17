package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// asyncMergeMakerDDL is the layout-61 addition: an async request's MAKER and
// TARGET (agentrepl.v1.CreateWorkspaceAsyncMerge), carried on the requester's
// detached request until its hand-off forks the merge workspace.
//
//   - source_detach_maker: the workspace whose conversation the merge
//     workspace forks -- the one the merged work was done in.
//   - source_detach_target_maker: 1 when the branch lands into the maker's
//     branch, 0 when it lands on the primary branch.
//
// IT IS ADDITIVE, and it BACKFILLS: a detached request the previous build
// recorded forked its requester and landed by its requester's geometry, so
// its maker is its requester; the target column's default (the primary
// branch) is what that build landed a top-level requester's merge on.
const asyncMergeMakerDDL = `
ALTER TABLE merge_queue ADD COLUMN source_detach_maker TEXT;
ALTER TABLE merge_queue ADD COLUMN source_detach_target_maker INTEGER NOT NULL DEFAULT 0;
UPDATE merge_queue SET source_detach_maker = workspace_id WHERE source_detach_id IS NOT NULL;
`

// ClearMergePick records that a merge workspace's pick SUCCEEDED while its
// merge stands in line: the instruction and the begun pick leave its row, so
// the next admission admits the merge itself rather than starting the pick
// again. A row with no begun pick is a defect in the caller, refused.
func (s *store) ClearMergePick(ctx context.Context, repo RepoKey, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.clear_merge_pick"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id), "cleared_at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx,
			`UPDATE merge_queue SET source_pick = NULL, pick_started_at = NULL, pick_turn = NULL
			  WHERE repo_key = ? AND workspace_id = ? AND state = ? AND source_pick IS NOT NULL AND pick_started_at IS NOT NULL`,
			key, id, int(MergeQueued))
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: begun pick of queued workspace %s", id))
	})
}
