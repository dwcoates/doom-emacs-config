package merge

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// This file resolves a request's SOURCE into what the merge works on: the
// branch, the worktree checked out on it (where the rebase, the gate and the
// repairs happen), the target it lands in, and what closes once it lands.
//
// THE REBASE RUNS IN A WORKTREE CHECKED OUT ON THE BRANCH BEING MERGED (lead
// ruling, 2026-09-30): the requester's own worktree for its own branch; the
// other workspace's worktree for its branch; the branch's existing worktree
// when it has one; otherwise a worktree the daemon makes for it under its state
// directory and removes after the merge. A MERGE OF A BRANCH THAT IS NOT THE
// REQUESTER'S OWN NEVER CLOSES THE REQUESTER; merging another workspace's
// branch closes THAT workspace once it lands.

// mergeWorktreesDir is where the worktrees the daemon makes for a branch with
// none live, under the state root.
const mergeWorktreesDir = "merge-worktrees"

// subject is what one merge works on.
type subject struct {
	// branch is the branch merged.
	branch string
	// dir is the worktree checked out on branch. It is empty for a branch
	// already merged upstream, which is never rebased.
	dir string
	// targetDir is the checkout the merge lands in: the branch's parent for a
	// workspace, the repository's main worktree otherwise.
	targetDir string
	// closes is the workspace that closes once the branch lands, empty when
	// none does.
	closes ids.WorkspaceID
	// other is another workspace whose worktree the merge works in, empty when
	// it works in none but the requester's.
	other ids.WorkspaceID
	// made reports that the daemon made dir for this merge, and removes it.
	made bool
}

// targetLabel is the target as the bubble's head names it: the checkout the
// merge lands in.
func (s subject) targetLabel() string {
	return s.targetDir
}

// worktreeFor names the worktree the daemon makes for a branch with none: one
// directory per lease, so no two merges ever share one.
func worktreeFor(stateDir string, lease ids.LeaseID) string {
	return filepath.Join(stateDir, mergeWorktreesDir, string(lease))
}

// resolveSubject resolves the run's source into its subject, making the
// branch's worktree when it has none.
func (o *orchestrator) resolveSubject(ctx context.Context, r *run) (subject, error) {
	switch r.source.Kind {
	case wsm.MergeSourceOwnBranch:
		job, err := o.layoutFor(ctx, r.ws)
		if err != nil {
			return subject{}, err
		}
		branch, err := o.requestedBranch(ctx, r.ws, r.source, job.Layout.SourceDir)
		if err != nil {
			return subject{}, err
		}
		s := subject{branch: branch, dir: job.Layout.SourceDir, targetDir: job.Layout.TargetDir}
		if !r.source.KeepOpen {
			s.closes = r.ws
		}
		return s, nil
	case wsm.MergeSourceWorkspace:
		job, err := o.layoutFor(ctx, r.source.Workspace)
		if err != nil {
			return subject{}, err
		}
		branch, err := o.requestedBranch(ctx, r.ws, r.source, job.Layout.SourceDir)
		if err != nil {
			return subject{}, err
		}
		return subject{
			branch: branch, dir: job.Layout.SourceDir, targetDir: job.Layout.TargetDir,
			closes: r.source.Workspace, other: r.source.Workspace,
		}, nil
	case wsm.MergeSourceBranch:
		return o.branchSubject(ctx, r)
	case wsm.MergeSourceMergedUpstream:
		job, err := o.layoutFor(ctx, r.ws)
		if err != nil {
			return subject{}, err
		}
		main, err := r.git.MainWorktree(ctx, job.Layout.SourceDir)
		if err != nil {
			return subject{}, fmt.Errorf("merge: resolving the repository's main worktree: %w", err)
		}
		branch, err := o.requestedBranch(ctx, r.ws, r.source, job.Layout.SourceDir)
		if err != nil {
			return subject{}, err
		}
		return subject{branch: branch, targetDir: main, closes: r.ws}, nil
	default:
		return subject{}, fmt.Errorf("merge: the undeclared source %s", r.source.Kind)
	}
}

// requestedBranch answers the workspace branch a request merges: the one
// recorded when it was requested (checkSource). A request recorded by an
// earlier build carries none, and its branch is read from the worktree now,
// by the same rule the request would have been held to.
func (o *orchestrator) requestedBranch(ctx context.Context, ws ids.WorkspaceID, source wsm.MergeSource, dir string) (string, error) {
	if source.Branch != "" {
		return source.Branch, nil
	}
	o.log(ctx, ws).Info("daemon.merge.subject", "the request was recorded with no branch; reading the branch checked out in its worktree", dlog.Context{
		"workspace": string(ws), "source": source.Kind.String(), "worktree": dir})
	return o.checkedOutBranch(ctx, ws, dir)
}

// branchSubject resolves a branch that is no workspace: rebased in its own
// worktree when git lists one, else in a worktree made for the merge, and
// landed in the repository's main worktree.
func (o *orchestrator) branchSubject(ctx context.Context, r *run) (subject, error) {
	const op = "daemon.merge.subject"
	record, err := o.deps.DB.Workspace(ctx, r.ws)
	if err != nil {
		return subject{}, err
	}
	main, err := r.git.MainWorktree(ctx, record.Dir)
	if err != nil {
		return subject{}, fmt.Errorf("merge: resolving the repository's main worktree: %w", err)
	}
	s := subject{branch: r.source.Branch, targetDir: main}
	worktrees, err := r.git.ListWorktrees(ctx, main)
	if err != nil {
		return subject{}, fmt.Errorf("merge: listing the repository's worktrees: %w", err)
	}
	for _, wt := range worktrees {
		if wt.Branch == r.source.Branch {
			s.dir = wt.Dir
			o.log(ctx, r.ws).Info(op, "the branch has a worktree of its own; the merge works in it", dlog.Context{
				"workspace": string(r.ws), "branch": s.branch, "worktree": s.dir})
			return s, nil
		}
	}
	s.dir = worktreeFor(o.deps.StateDir, r.lease.ID)
	if err := os.MkdirAll(filepath.Dir(s.dir), 0o755); err != nil {
		return subject{}, fmt.Errorf("merge: making the directory for the branch's worktree: %w", err)
	}
	if err := r.git.AddWorktree(ctx, main, s.dir, s.branch); err != nil {
		return subject{}, fmt.Errorf("merge: checking %s out at %s: %w", s.branch, s.dir, err)
	}
	s.made = true
	o.log(ctx, r.ws).Info(op, "the branch has no worktree; the merge made one for it", dlog.Context{
		"workspace": string(r.ws), "branch": s.branch, "worktree": s.dir})
	return s, nil
}

// sourceLabel is what a merge in line lands, as its bubble's head names it
// before the merge is admitted: the same line the running merge draws.
func (o *orchestrator) sourceLabel(ctx context.Context, ws ids.WorkspaceID, source wsm.MergeSource) (string, error) {
	switch source.Kind {
	case wsm.MergeSourceOwnBranch:
		job, err := o.layoutFor(ctx, ws)
		if err != nil {
			return "", err
		}
		branch, err := o.requestedBranch(ctx, ws, source, job.Layout.SourceDir)
		if err != nil {
			return "", err
		}
		return branchLabel(branch, job.Layout.TargetDir), nil
	case wsm.MergeSourceWorkspace:
		job, err := o.layoutFor(ctx, source.Workspace)
		if err != nil {
			return "", err
		}
		branch, err := o.requestedBranch(ctx, ws, source, job.Layout.SourceDir)
		if err != nil {
			return "", err
		}
		return branchLabel(branch, job.Layout.TargetDir), nil
	case wsm.MergeSourceMergedUpstream:
		job, err := o.layoutFor(ctx, ws)
		if err != nil {
			return "", err
		}
		main, err := o.deps.Git.MainWorktree(ctx, job.Layout.SourceDir)
		if err != nil {
			return "", err
		}
		branch, err := o.requestedBranch(ctx, ws, source, job.Layout.SourceDir)
		if err != nil {
			return "", err
		}
		return branchLabel(branch, main), nil
	case wsm.MergeSourceBranch:
		record, err := o.deps.DB.Workspace(ctx, ws)
		if err != nil {
			return "", err
		}
		main, err := o.deps.Git.MainWorktree(ctx, record.Dir)
		if err != nil {
			return "", err
		}
		return branchLabel(source.Branch, main), nil
	default:
		return "", fmt.Errorf("merge: the undeclared source %s", source.Kind)
	}
}
