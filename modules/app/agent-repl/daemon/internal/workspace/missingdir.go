package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// AN OPEN WORKSPACE WHOSE DIRECTORY IS GONE IS SETTLED IN ONE PLACE (owner
// ruling, 2026-10-06): recreated from its branch when there is one to restore
// from, and closed by the daemon only when there is nothing to restore -- the
// branch is gone too, or the workspace was merged (its tree removed on
// purpose). The boot's missing-directory step and the roster's publish both
// ask here. The publish used to close such a row outright, so a worktree
// deleted while the daemon ran was never recreated: on 2026-10-24 a cleanup of
// the worktrees closed dozens of workspaces whose branches all survived, and
// they left the roster with their merge queue entries.

// SettleMissingDirectory settles the OPEN workspace ws, whose directory a stat
// found gone with statErr, answering whether it was recreated. Every outcome
// is recorded under op, the step that asked: a recreation and the ruled
// automatic close at INFO, a close that could not be written at ERROR. A
// restore git could not perform is the restorer's own ERROR, and the row is
// then closed like an unrestorable one, so the caller finishes and an
// explicit open retries the restore and surfaces its failure. Only a failed
// close is returned as an error.
func (v *verbs) SettleMissingDirectory(ctx context.Context, op string, ws wsm.Workspace, statErr error) (bool, error) {
	log := v.deps.Log.Global()
	fields := dlog.Context{
		dlog.KeyWorkspaceID:  string(ws.ID),
		dlog.KeyWorkspaceDir: ws.Dir,
		"error":              statErr.Error(),
	}
	restored, err := v.RestoreMissingWorktree(ctx, op, ws)
	if restored {
		log.Info(op, "the workspace directory was gone; its worktree was recreated from its branch", fields)
		return true, nil
	}
	if err != nil {
		fields["restore_error"] = err.Error()
	}
	if err := v.deps.DB.MarkClosed(ctx, ws.ID, wsm.Closure{At: v.now(), By: wsm.CloseByDaemon}); err != nil {
		fields["cause"] = err.Error()
		log.Error(op, "a workspace whose directory is gone could not be closed", fields)
		return false, fmt.Errorf("close the missing-directory workspace %q: %w", ws.ID, err)
	}
	log.Info(op, "the workspace directory is gone; the workspace is closed", fields)
	return false, nil
}
