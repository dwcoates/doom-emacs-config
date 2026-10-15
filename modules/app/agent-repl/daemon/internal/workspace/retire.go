package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// opRetire is RetireLanded's operation.
const opRetire = "daemon.workspace.retire_landed"

// Why RetireLanded left a requester as it is. The empty value is a retirement.
const (
	// RetireKeptNotLanded: no landing stands, or the workspace has merged.
	RetireKeptNotLanded = "not_landed"
	// RetireKeptParent: an open workspace was cut from it, and merges into
	// its worktree.
	RetireKeptParent = "parent_of_open_workspace"
	// RetireKeptNotQuiet: a turn, live work, a held prompt or a merge stands.
	RetireKeptNotQuiet = "not_quiet"
	// RetireKeptMissing: the worktree's directory is not there.
	RetireKeptMissing = "directory_missing"
	// RetireKeptMovedOn: the worktree's HEAD is no longer the commit that
	// landed. The landing is cleared: what the branch holds now never landed.
	RetireKeptMovedOn = "moved_on"
	// RetireKeptDirty: the worktree has modified or untracked content.
	RetireKeptDirty = "dirty"
)

// RetireLanded retires a REQUESTER WHOSE DETACHED MERGE LANDED (its
// wsm.Landing) into the state an in-place merge leaves its requester in: its
// session stopped, its worktree removed, and the workspace closed and stamped
// merged at the instant its merge landed. Its branch is kept, as an in-place
// merge keeps it.
//
// It answers why it kept the workspace instead, empty when it retired it. It
// keeps every workspace that might still hold something that did not land: one
// whose HEAD moved past the landing (whose landing it clears), one with
// modified or untracked content, one that is not quiet, and one an open
// workspace was cut from. The worktree goes through git's clean door, so a
// tree dirtied after it was judged is refused by git and kept.
//
// The caller decides WHEN: it is the merge workspace sweep's, once the
// requester has been idle long enough.
func (v *verbs) RetireLanded(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	record, log, err := v.owned(ctx, opRetire, ws)
	if err != nil {
		return "", err
	}
	fields := dlog.Context{"workspace": string(ws), "dir": record.Dir, "branch": record.Branch}
	if record.Landed == nil || record.MergedAt != nil {
		return v.keepRetire(log, RetireKeptNotLanded, fields), nil
	}
	landing := *record.Landed
	fields["landed_head"], fields["landed_at"] = landing.Head, landing.At

	parent, err := v.parentOfOpen(ctx, record)
	if err != nil {
		log.Error(opRetire, "could not read whether an open workspace was cut from the requester; it is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: %w", ws, err)
	}
	if parent != "" {
		fields["child"] = string(parent)
		return v.keepRetire(log, RetireKeptParent, fields), nil
	}
	blocked, err := v.closeBlocker(ctx, ws)
	if err != nil {
		log.Error(opRetire, "could not judge whether the requester is quiet; it is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: %w", ws, err)
	}
	if blocked != nil {
		fields["blocked"] = blocked.Reason
		return v.keepRetire(log, RetireKeptNotQuiet, fields), nil
	}

	if _, err := os.Stat(record.Dir); errors.Is(err, fs.ErrNotExist) {
		return v.keepRetire(log, RetireKeptMissing, fields), nil
	} else if err != nil {
		log.Error(opRetire, "could not read the requester's worktree; it is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: stat %s: %w", ws, record.Dir, err)
	}
	head, err := v.deps.Git.ResolveRef(ctx, record.Dir, "HEAD")
	if err != nil {
		log.Error(opRetire, "could not read the requester's HEAD; it is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: resolve HEAD: %w", ws, err)
	}
	fields["head"] = head
	if head != landing.Head {
		if err := v.deps.DB.SetLanded(ctx, ws, nil); err != nil {
			log.Error(opRetire, "could not clear the landing of a requester that moved past it; it is kept", dlog.WithCause(fields, err))
			return "", fmt.Errorf("retire %q: clear the landing: %w", ws, err)
		}
		log.Info(opRetire, "the requester moved past the commit that landed; its landing is cleared and it is kept", fields)
		return RetireKeptMovedOn, nil
	}
	clean, err := v.deps.Git.IsClean(ctx, record.Dir)
	if err != nil {
		log.Error(opRetire, "could not read whether the requester's worktree is clean; it is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: cleanliness: %w", ws, err)
	}
	if !clean {
		return v.keepRetire(log, RetireKeptDirty, fields), nil
	}
	repoDir, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opRetire, "could not resolve the repository the requester's worktree is removed from; it is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: %w", ws, err)
	}
	fields["repo"] = repoDir

	// THE SESSION GOES BEFORE THE TREE, as it does for a merge's own
	// requester: no shim is left running in a directory that is gone.
	if err := v.kill(ctx, log, ws); err != nil {
		return "", fmt.Errorf("retire %q: %w", ws, err)
	}
	if err := v.deps.Git.RemoveCleanWorktree(ctx, repoDir, record.Dir); err != nil {
		log.Error(opRetire, "could not remove the requester's worktree; it is closed and its worktree is kept", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: remove the worktree: %w", ws, err)
	}
	if err := v.deps.DB.SetMergedAt(ctx, ws, landing.At, nil); err != nil {
		log.Error(opRetire, "could not stamp the retired requester merged; its worktree is gone and it stays closed", dlog.WithCause(fields, err))
		return "", fmt.Errorf("retire %q: stamp merged: %w", ws, err)
	}
	log.Info(opRetire, "retired a requester whose detached merge landed: its session stopped, its worktree removed, its branch kept", fields)
	v.republishRegistry(ctx, log, opRetire)
	return "", nil
}

// keepRetire records a requester RetireLanded left as it is, and answers why.
func (v *verbs) keepRetire(log dlog.Logger, reason string, fields dlog.Context) string {
	fields["reason"] = reason
	log.Debug(opRetire, "kept a requester whose detached merge landed", fields)
	return reason
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
