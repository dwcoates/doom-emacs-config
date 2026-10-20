package wsm

import (
	"context"
	"database/sql"
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
// registered rather than created -- has nothing to mark, ErrNotFound.
func (s *store) SetOneShot(ctx context.Context, id WorkspaceID, oneShot bool) error {
	const op = "daemon.wsm.set_one_shot"
	return s.write(ctx, op, dlog.Context{"workspace": string(id), "one_shot": oneShot}, func(ctx context.Context, tx *sql.Tx) error {
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
