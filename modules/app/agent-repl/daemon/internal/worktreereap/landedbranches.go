package worktreereap

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/landedbranch"
	"agent-repld/internal/wsm"
)

// opLandedBranch is the landed-branch pass's operation.
const opLandedBranch = "daemon.worktreereap.landed_branch"

// sweepLandedBranches HEALS the branches landed workspaces left behind: every
// branch a MERGED workspace of the repository names is retired through the
// same door a landing uses, so it is deleted only once it is judged landed
// and no worktree has it checked out. A stale landed branch refused every
// later create of its workspace's name (2026-10-30 and 2026-11-03,
// `ramdisk-leak-investigation`).
//
// A BRANCH IS JUDGED BY ITS MOST RECENT LANDING. Several merged workspaces may
// have named one branch in turn; the branch as it stands is the last one's, so
// that workspace's recorded landed head (wsm.Workspace.Landed, set by a
// detached merge, whose rebased landing no ancestry can show) is the one
// passed. A landing that recorded no head is judged by ancestry alone.
//
// A BRANCH ANY UNMERGED WORKSPACE NAMES IS NEVER TOUCHED: a name reused after
// its landing belongs to the workspace that reused it, and a closed workspace
// that never landed is restored from its branch when it is reopened.
//
// A LANDING TARGET THAT IS GONE IS JUDGED BY THE DEFAULT BRANCH: a workspace
// that landed into its parent's branch outlives that branch once the parent
// lands and retires it, and every landing chain ends in the default branch,
// so a tip inside it has landed whatever ref it targeted. Asking git about the
// deleted ref failed every sweep and kept the branch for good (2026-11-14:
// `merge-queue/successor-role-flags-landing`, landed into the since-retired
// `fix/successor-role-flags`). The target is probed as any ref, not as a
// local branch, because a landing target may be a remote-tracking ref such
// as `origin/master`, which is present though no local branch is named so.
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
	latest := map[string]wsm.Workspace{}
	var order []string
	for _, ws := range s.workspaces {
		if ws.Repo != s.repo.ID || ws.MergedAt == nil || ws.Branch == "" || held[ws.Branch] {
			continue
		}
		prior, seen := latest[ws.Branch]
		if !seen {
			order = append(order, ws.Branch)
		}
		if !seen || ws.MergedAt.After(*prior.MergedAt) {
			latest[ws.Branch] = ws
		}
	}
	for _, branch := range order {
		if ctx.Err() != nil {
			return
		}
		ws := latest[branch]
		landing := landedbranch.Landing{Into: ws.ParentBranch}
		if landing.Into == "" {
			landing.Into = s.defaultBranch
		} else if landing.Into != s.defaultBranch {
			present, err := s.r.deps.Git.RefExists(ctx, s.repo.Dir, landing.Into)
			if err != nil {
				s.r.deps.Log.Error(opLandedBranch, "could not probe the branch a landed branch landed into; the branch is kept",
					dlog.WithCause(dlog.Context{"repo": s.repo.Dir, "branch": branch, "into": landing.Into,
						"workspace": string(ws.ID), "repo_id": string(s.repo.ID)}, err))
				s.report.Failures++
				continue
			}
			if !present {
				s.r.deps.Log.Info(opLandedBranch, "the branch a landed branch landed into is gone; judging it by the default branch",
					dlog.Context{"repo": s.repo.Dir, "branch": branch, "gone_into": landing.Into, "into": s.defaultBranch,
						"workspace": string(ws.ID), "repo_id": string(s.repo.ID)})
				landing.Into = s.defaultBranch
			}
		}
		if ws.Landed != nil {
			landing.Head = ws.Landed.Head
		}
		outcome, err := landedbranch.Retire(ctx, s.r.deps.Git, s.r.deps.Log, opLandedBranch, s.repo.Dir, branch, landing,
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
