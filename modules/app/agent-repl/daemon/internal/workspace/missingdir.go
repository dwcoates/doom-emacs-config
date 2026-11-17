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
// found gone with statErr, answering whether it was recreated. Its row is read
// again first, and one closed meanwhile or owing a nuke is left as it is. Every outcome
// is recorded under op, the step that asked: a recreation and the ruled
// automatic close at INFO, a close that could not be written at ERROR. A
// restore git could not perform is the restorer's own ERROR, and the row is
// then closed like an unrestorable one, so the caller finishes and an
// explicit open retries the restore and surfaces its failure. Only a failed
// read or close is returned as an error.
func (v *verbs) SettleMissingDirectory(ctx context.Context, op string, ws wsm.Workspace, statErr error) (bool, error) {
	log := v.deps.Log.Global()
	fields := dlog.Context{
		dlog.KeyWorkspaceID:  string(ws.ID),
		dlog.KeyWorkspaceDir: ws.Dir,
		"error":              statErr.Error(),
	}
	// THE ROW IS READ AGAIN, AFTER THE STAT THAT FOUND THE DIRECTORY GONE.
	// The caller's row was read BEFORE that stat, and every teardown writes
	// its row first (a landing stamps merged_at and closes it, a nuke stamps
	// what it owes) and only then removes the tree. So a row read after the
	// stat carries the teardown that removed the directory, and a stale one
	// can never send a restore into a removal still holding the branch
	// (deploy 2026-11-17T00:29:50: `worktree add` met the branch at the
	// landing's `.removing` path, exit 128).
	fresh, err := v.deps.DB.Workspace(ctx, ws.ID)
	if err != nil {
		fields["cause"] = err.Error()
		log.Error(op, "could not read again the workspace whose directory is gone; it is left as it is", fields)
		return false, fmt.Errorf("read the missing-directory workspace %q again: %w", ws.ID, err)
	}
	ws = fresh
	if ws.Closed {
		log.Info(op, "the workspace directory is gone and the workspace was closed meanwhile; it is left closed", fields)
		return false, nil
	}
	// A WORKSPACE THAT OWES A NUKE IS LEFT FOR THE NUKE: its tree is gone
	// because the nuke set it aside, and restoring it from the branch the
	// nuke has yet to delete would bring back what is being destroyed.
	if ws.NukeOwedAt != nil {
		log.Info(op, "the workspace directory is gone because a nuke is under way; it is left for the nuke to finish", fields)
		return false, nil
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
