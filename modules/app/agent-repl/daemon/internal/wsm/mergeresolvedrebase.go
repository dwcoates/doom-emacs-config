package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// A REBASE A MERGE'S OWN CONFLICT REPAIR WORKED ON IS THE QUEUE'S DURABLE
// FACT (2026-11-20: sidebar-roster-streams' merge livelocked). A merge that
// steps aside from the head to resolve its rebase's conflicts leaves that
// rebase standing in its worktree for its repair turn; once the turn ended,
// the merge returns to the line and continues the rebase when it heads the
// queue. Its progress record says so only while its merge lease stands, and
// a requeue drops both: the merge then started afresh, read the standing
// rebase as a cut run's leftover, aborted the resolution, and met the same
// conflict on the same tip again, forever.
//
// So the return writes the fact on the queue entry itself, in the same
// transaction (ReturnRemediatedMerge): the tip the rebase replays onto, the
// branch's head before it, and the repairing turn. The entry outlives every
// requeue, so whichever run takes the merge up next reads the fact rather
// than guessing from the worktree, and the fact goes with the entry.
// ClearResolvedRebase retires it once a run decided on the standing rebase.

// mergeQueueResolvedRebaseDDL is the layout-179 addition: the rebase a
// merge's own conflict repair worked on (ResolvedRebase). IT IS ADDITIVE: the
// previous build's INSERT and UPDATE name none of the columns, so a row it
// writes takes NULL -- an entry with no resolved rebase, which is all that
// build knew.
const mergeQueueResolvedRebaseDDL = `
ALTER TABLE merge_queue ADD COLUMN resolved_tip TEXT;
ALTER TABLE merge_queue ADD COLUMN resolved_branch_head TEXT;
ALTER TABLE merge_queue ADD COLUMN resolved_turn TEXT;
`

// ResolvedRebase is the rebase a merge's own conflict repair worked on, left
// standing in its worktree for the merge to continue: Tip is the target's tip
// it replays onto, BranchHead the branch's head before it, and Turn the
// repairing turn that worked on it.
type ResolvedRebase struct {
	Tip        string
	BranchHead string
	Turn       TurnID
}

// validate refuses a resolved rebase missing any of its facts.
func (r ResolvedRebase) validate() error {
	if r.Tip == "" || r.BranchHead == "" || r.Turn == "" {
		return fmt.Errorf("wsm: a resolved rebase names its tip, its branch head and its turn (tip %q, branch head %q, turn %q)", r.Tip, r.BranchHead, r.Turn)
	}
	return nil
}

// resolvedRebaseColumns are the three columns as one write binds them: NULL
// for no resolved rebase.
func resolvedRebaseColumns(r *ResolvedRebase) (tip, head, turn sql.NullString) {
	if r == nil {
		return
	}
	return sql.NullString{String: r.Tip, Valid: true}, sql.NullString{String: r.BranchHead, Valid: true}, sql.NullString{String: string(r.Turn), Valid: true}
}

// decodeResolvedRebase answers the resolved rebase ROW's three columns name,
// nil when none is set; a row setting only some is a DecodeError.
func decodeResolvedRebase(row string, tip, head, turn sql.NullString) (*ResolvedRebase, error) {
	if !tip.Valid && !head.Valid && !turn.Valid {
		return nil, nil
	}
	r := ResolvedRebase{Tip: tip.String, BranchHead: head.String, Turn: TurnID(turn.String)}
	if err := r.validate(); err != nil {
		return nil, &DecodeError{Table: "merge_queue", Row: row, Field: "resolved_tip", Err: err}
	}
	return &r, nil
}

// errResolvedRebaseAside refuses a resolved rebase written on a return from
// any repair but a step aside for the head's conflicts.
var errResolvedRebaseAside = errors.New("wsm: only a merge returning from resolving its own rebase's conflicts carries a resolved rebase")

// ClearResolvedRebase retires the resolved rebase ID's entry in REPO's queue
// carries, answering whether it carried one. A workspace with no entry is
// ErrNotFound.
func (s *store) ClearResolvedRebase(ctx context.Context, repo RepoKey, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.clear_resolved_rebase"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(id)}, err))
		return false, err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(id)}
	cleared := false
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		if _, _, err := entryPlace(ctx, tx, key, id); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE merge_queue SET resolved_tip = NULL, resolved_branch_head = NULL, resolved_turn = NULL
WHERE repo_key = ? AND workspace_id = ? AND resolved_tip IS NOT NULL`, key, id)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		cleared = n > 0
		return err
	})
	return cleared, err
}
