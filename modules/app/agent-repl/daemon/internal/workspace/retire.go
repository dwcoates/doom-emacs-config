package workspace

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/landedbranch"
	"agent-repld/internal/wsm"
)

// retireTree retires RECORD, a closed or quiet workspace whose work landed,
// into the state a landed merge leaves its workspace in: its session
// stopped, its worktree (clean, judged by its caller) removed through git's
// clean door, the workspace stamped merged at AT, and its branch deleted
// once landedbranch.Retire judges it inside its parent branch. A branch left
// behind refuses every later create of the same name
// (2026-11-03, `ramdisk-leak-investigation`). Every step that fails is
// recorded under OP and answered; the branch's own outcome is recorded by
// landedbranch.Retire.
func (v *verbs) retireTree(ctx context.Context, log dlog.Logger, op string, record wsm.Workspace, repoDir string, at time.Time, fields dlog.Context) error {
	ws := record.ID
	// THE SESSION GOES BEFORE THE TREE, as it does for a merge's own
	// requester: no shim is left running in a directory that is gone.
	if err := v.kill(ctx, log, ws, wsm.CloseByUser); err != nil {
		return fmt.Errorf("retire %q: %w", ws, err)
	}
	if err := v.deps.Git.RemoveCleanWorktree(ctx, repoDir, record.Dir); err != nil {
		log.Error(op, "could not remove the workspace's worktree; it is closed and its worktree is kept", dlog.WithCause(fields, err))
		return fmt.Errorf("retire %q: remove the worktree: %w", ws, err)
	}
	if err := v.deps.DB.SetMergedAt(ctx, ws, at, nil); err != nil {
		log.Error(op, "could not stamp the retired workspace merged; its worktree is gone and it stays closed", dlog.WithCause(fields, err))
		return fmt.Errorf("retire %q: stamp merged: %w", ws, err)
	}
	_, _ = landedbranch.Retire(ctx, v.deps.Git, log, op, repoDir, record.Branch,
		landedbranch.Landing{Into: record.ParentBranch}, fields)
	v.republishRegistry(ctx, log, op)
	return nil
}

// parentOfOpen answers an open workspace cut from RECORD, empty when none is:
// a nested workspace merges into its parent's worktree, so that worktree stays.
func (v *verbs) parentOfOpen(ctx context.Context, record wsm.Workspace) (ids.WorkspaceID, error) {
	all, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return "", fmt.Errorf("list the workspaces: %w", err)
	}
	for _, ws := range all {
		if ws.Closed || ws.ID == record.ID {
			continue
		}
		if (ws.Parent != nil && *ws.Parent == record.ID) || (ws.Repo == record.Repo && ws.ParentBranch == record.Branch) {
			return ws.ID, nil
		}
	}
	return "", nil
}
