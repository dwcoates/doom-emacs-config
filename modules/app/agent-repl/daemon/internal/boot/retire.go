package boot

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// retireGoneRepositories retires every registered repository whose directory
// is GONE and whose workspaces are all closed.
//
// Such a row names nothing: no worktree can be restored from a repository that
// is not there, yet the row drew in the sidebar and every worktree reap logged
// "the repository's main worktree is gone; there is nothing to sweep" for it,
// boot after boot, forever. It is retired through wsm.RetireRepository, which
// removes the repository row the way Forget's last-workspace tail does and
// takes its closed workspaces' records with it.
//
// IT RUNS AT BOOT, beside closeMissingDirs, because the boot is where the
// registry is reconciled against the disk before anything serves it: no
// session runs, no editor has drawn the roster, and a workspace this same boot
// closed for its missing directory is judged closed here. A directory removed
// while the daemon runs is retired at the next boot; until then the reaper
// already passes over it.
//
// A STAT THAT DOES NOT SAY "NOT EXIST" IS NEVER READ AS GONE, as in
// closeMissingDirs. A gone repository that still has an OPEN workspace, or
// whose closed workspaces still hold live state, is KEPT and stated at WARN:
// a live workspace under a repository that is not there is an inconsistency
// worth seeing, and retiring it would delete what a user can still act on.
func (s *sequence) retireGoneRepositories(ctx context.Context, log dlog.Logger, report *Report) error {
	const op = "daemon.boot.retire_gone_repository"
	repositories, err := s.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(op, "the repository registry could not be read", dlog.Context{"error": err.Error()})
		return fmt.Errorf("boot: read the repository registry: %w", err)
	}
	workspaces, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(op, "the workspace registry could not be read", dlog.Context{"error": err.Error()})
		return fmt.Errorf("boot: read the workspace registry: %w", err)
	}
	open := map[ids.RepoID][]string{}
	for _, ws := range workspaces {
		if !ws.Closed {
			open[ws.Repo] = append(open[ws.Repo], string(ws.ID))
		}
	}
	for _, repo := range repositories {
		_, statErr := os.Stat(repo.Dir)
		if statErr == nil {
			continue
		}
		fields := dlog.Context{"repo": string(repo.ID), "repo_dir": repo.Dir, "error": statErr.Error()}
		if !errors.Is(statErr, fs.ErrNotExist) {
			log.Warn(op, "the repository directory could not be stat-ed; never read as gone", fields)
			continue
		}
		if live := open[repo.ID]; len(live) > 0 {
			fields["open_workspaces"] = live
			log.Warn(op, "the repository directory is gone but a workspace under it is still open; the repository is kept", fields)
			continue
		}
		retired, err := s.deps.DB.RetireRepository(ctx, repo.ID)
		if errors.Is(err, wsm.ErrRepositoryInUse) {
			fields["refusal"] = err.Error()
			log.Warn(op, "the repository directory is gone but its workspaces still hold live state; the repository is kept", fields)
			continue
		}
		if err != nil {
			fields["retire_error"] = err.Error()
			log.Error(op, "a repository whose directory is gone could not be retired", fields)
			return fmt.Errorf("boot: retire the gone repository %s: %w", repo.ID, err)
		}
		retiredIDs := make([]string, 0, len(retired.Workspaces))
		for _, ws := range retired.Workspaces {
			retiredIDs = append(retiredIDs, string(ws))
		}
		fields["workspaces_retired"] = retiredIDs
		log.Info(op, "the repository directory is gone and its workspaces are all closed; the repository is retired", fields)
		report.RetiredRepositories = append(report.RetiredRepositories, repo.ID)
	}
	return nil
}
