package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// standingsDDL is the layout-64 addition: awaiting_correspondence, the
// workspace a workspace awaits correspondence from
// (agentrepl.v1.SetWorkspaceCorrespondence), NULL while it awaits none.
// Every close clears it (SetClosed), and forgetting the awaited workspace
// does too (SET NULL): a mark naming nobody waits on nothing.
const standingsDDL = `
ALTER TABLE workspaces ADD COLUMN awaiting_correspondence TEXT REFERENCES workspaces(id) ON DELETE SET NULL;
`

// SetAwaiting marks the workspace awaiting correspondence from AWAITED, or
// clears the mark with nil.
func (s *store) SetAwaiting(ctx context.Context, id WorkspaceID, awaited *WorkspaceID) error {
	var value any
	fields := dlog.Context{"awaiting": awaited != nil}
	if awaited != nil {
		value = string(*awaited)
		fields["awaited"] = string(*awaited)
	}
	return s.setWorkspaceColumns(ctx, "daemon.wsm.set_awaiting", id, fields,
		workspaceColumn{name: "awaiting_correspondence", value: value})
}

// SetOneShot marks the workspace's creation a one-shot, or unmarks it
// (agentrepl.v1.SetWorkspaceOneShot): the fact every surface draws a one-shot
// from (Workspace.OneShot). A workspace with no creation job -- one that was
// registered rather than created -- has nothing to mark, ErrNotFound. A mark
// on a repository's main workspace is refused, ErrMainWorkspaceOneShot.
func (s *store) SetOneShot(ctx context.Context, id WorkspaceID, oneShot bool) error {
	const op = "daemon.wsm.set_one_shot"
	return s.write(ctx, op, dlog.Context{"workspace": string(id), "one_shot": oneShot}, func(ctx context.Context, tx *sql.Tx) error {
		if err := refuseMainWorkspaceOneShot(ctx, tx, id, oneShot); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE creation_jobs SET one_shot = ? WHERE workspace_id = ?`, oneShot, id)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		if n != 1 {
			return fmt.Errorf("wsm: no creation job records workspace %s: %w", id, ErrNotFound)
		}
		return nil
	})
}

// refuseMainWorkspaceOneShot answers ErrMainWorkspaceOneShot when ONESHOT
// would mark ID, a repository's main workspace, a one-shot. It is the one
// check every one-shot write runs inside its own transaction, so the rule
// holds however the mark is written. An unmark, and a workspace the registry
// does not hold, pass: the write itself answers those.
func refuseMainWorkspaceOneShot(ctx context.Context, tx *sql.Tx, id WorkspaceID, oneShot bool) error {
	if !oneShot {
		return nil
	}
	var dir, repoDir string
	err := tx.QueryRowContext(ctx,
		`SELECT w.dir, r.dir FROM workspaces w JOIN repositories r ON r.id = w.repo_id WHERE w.id = ?`, id).Scan(&dir, &repoDir)
	if errors.Is(err, sql.ErrNoRows) {
		return nil
	}
	if err != nil {
		return err
	}
	if IsMainWorktree(dir, repoDir) {
		return fmt.Errorf("wsm: workspace %s is the main workspace of %s: %w", id, repoDir, ErrMainWorkspaceOneShot)
	}
	return nil
}
