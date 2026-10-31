package worktreereap

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/landedbranch"
)

// opLandedBranch is the landed-branch pass's operation.
const opLandedBranch = "daemon.worktreereap.landed_branch"

// sweepLandedBranches HEALS the branches landed workspaces left behind before
// a landing deleted its own branch (landedbranch.Retire): every branch a
// MERGED workspace of the repository names is retired through the same door
// a landing uses, so it is deleted only once its tip is verified inside the
// branch it landed into and no worktree has it checked out. A stale landed
// branch refused every later create of its workspace's name (2026-10-30,
// `ramdisk-leak-investigation`).
//
// A BRANCH ANY UNMERGED WORKSPACE NAMES IS NEVER TOUCHED: a name reused after
// its landing belongs to the workspace that reused it, and a closed workspace
// that never landed is restored from its branch when it is reopened.
//
// A dry run judges nothing here: a deletion is the one thing the pass does,
// and Retire judges and deletes in one step.
func (s *repoSweep) sweepLandedBranches(ctx context.Context) {
	if s.opts.DryRun {
		return
	}
	held := map[string]bool{}
	for _, ws := range s.workspaces {
		if ws.Repo == s.repo.ID && ws.MergedAt == nil && ws.Branch != "" {
			held[ws.Branch] = true
		}
	}
	seen := map[string]bool{}
	for _, ws := range s.workspaces {
		if ctx.Err() != nil {
			return
		}
		if ws.Repo != s.repo.ID || ws.MergedAt == nil || ws.Branch == "" || held[ws.Branch] || seen[ws.Branch] {
			continue
		}
		seen[ws.Branch] = true
		into := ws.ParentBranch
		if into == "" {
			into = s.defaultBranch
		}
		outcome, err := landedbranch.Retire(ctx, s.r.deps.Git, s.r.deps.Log, opLandedBranch, s.repo.Dir, ws.Branch, into,
			dlog.Context{"workspace": string(ws.ID), "repo_id": string(s.repo.ID)})
		if err != nil {
			s.report.Failures++
			continue
		}
		if outcome == landedbranch.Deleted {
			s.report.BranchesDeleted++
		}
	}
}
