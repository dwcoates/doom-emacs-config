package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// owedNukeDDL is the layout-139 addition: when a nuke of the workspace began
// (workspaces.nuke_owed_at), NULL while none is under way. A nuke stamps it
// before it destroys anything, and Forget, its last step, removes the row
// with it, so a row that carries it is a nuke that stopped part way: a
// removal that failed after `git worktree move` set the tree aside, with the
// branch and the record left behind (2026-11-11). The boot finishes every
// owed nuke, and nothing restores the worktree of a row that owes one.
//
// IT IS ADDITIVE: the build before it names no such column.
const owedNukeDDL = `
ALTER TABLE workspaces ADD COLUMN nuke_owed_at INTEGER;
`

// OweNuke records that a nuke of workspace ID began AT. It is stamped before
// the nuke destroys anything, so a nuke that stops part way leaves the mark
// the boot finishes it from.
func (s *store) OweNuke(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.owe_nuke"
	fields := dlog.Context{"workspace": string(id), "at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET nuke_owed_at = ? WHERE id = ?`, nanos(at), id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}
