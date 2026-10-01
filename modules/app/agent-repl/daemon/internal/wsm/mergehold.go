package wsm

import (
	"context"
	"database/sql"
	"fmt"
)

// bindMergeHold binds a merge hold to the merge it waits on, INSIDE the
// transaction that records the hold. It refuses unless the workspace's merge
// lease still stands, and it marks the merge's queue entry to keep its
// requester open, so the held prompt has a session to run in once the merge
// lands.
//
// THE RACE WITH THE RELEASE IS STRUCTURALLY ABSENT. The merge releases its
// lease in a transaction of its own and only then reads whether to close the
// requester. The database serializes the two writes, so a merge hold either
// lands before the release, and the merge reads keep_open, or is refused after
// it (ErrMergeLeaseGone), and the prompt takes the path of a workspace with no
// merge. No hold is ever recorded for a merge that already decided to close.
//
// A source that closes nothing of the requester's (another workspace's branch,
// a branch that is no workspace) has nothing to keep open, so its entry is
// left as it stands.
func bindMergeHold(ctx context.Context, tx *sql.Tx, ws WorkspaceID) error {
	var leases int
	if err := tx.QueryRowContext(ctx, `SELECT COUNT(*) FROM leases WHERE workspace_id = ? AND holder = ?`,
		ws, int(HolderMerge)).Scan(&leases); err != nil {
		return err
	}
	if leases == 0 {
		return fmt.Errorf("wsm: a merge hold on %s: %w", ws, ErrMergeLeaseGone)
	}
	rows, err := tx.QueryContext(ctx, `SELECT repo_key, source_kind FROM merge_queue WHERE workspace_id = ?`, ws)
	if err != nil {
		return err
	}
	type entry struct {
		repo string
		kind MergeSourceKind
	}
	var entries []entry
	for rows.Next() {
		var e entry
		if err := rows.Scan(&e.repo, &e.kind); err != nil {
			_ = rows.Close()
			return err
		}
		entries = append(entries, e)
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if err := rows.Err(); err != nil {
		return err
	}
	if len(entries) != 1 {
		return fmt.Errorf("wsm: a merge lease stands on %s with %d queue entries, want exactly 1", ws, len(entries))
	}
	if !entries[0].kind.ClosesRequester() {
		return nil
	}
	_, err = tx.ExecContext(ctx, `UPDATE merge_queue SET source_keep_open = 1 WHERE repo_key = ? AND workspace_id = ?`,
		entries[0].repo, ws)
	return err
}
