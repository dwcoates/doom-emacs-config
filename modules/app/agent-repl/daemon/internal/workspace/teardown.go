package workspace

import (
	"context"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// Kill is the big red button: FORCED session death. It never blocks on what is
// running, and it destroys no data — the worktree, the branch and every durable
// record survive, and the terminal it writes is REHYDRATABLE, so the
// conversation can be resumed later.
//
// The order matters: the session is asked to die forcefully FIRST, so the shim
// writes its own terminals, and only then is the process stopped. Everything
// still without a terminal is closed in ONE transaction.
func (v *verbs) Kill(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, "KillWorkspace", ws)
	if err != nil {
		return err
	}
	// A WAITING MERGE ENDS WITH THE WORKSPACE. Close refuses outright while a
	// merge is queued, but Kill never blocks on what is running, so this is the
	// one door a queued merge's workspace can leave through — and a merge left
	// on the queue behind it would wait forever on a workspace that is gone.
	v.deps.Merge.OnWorkspaceClosed(ctx, ws)
	return v.kill(ctx, log, ws)
}

// kill is Kill's body, shared with Nuke, which kills before it destroys.
func (v *verbs) kill(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) error {
	if shim, live := v.deps.Shim(ws); live {
		// THE LATCH IS ARMED BEFORE THE ASK, and therefore before the
		// unconditional process stop the ask's failure leads to: the stop is a
		// teardown this daemon ordered, and every side that later sees the
		// departure has to read it as one. See Shim.StandDown.
		ordered := shim.StandDown()
		if err := shim.KillSession(ctx, true); err != nil {
			// A shim that will not answer is not a reason to leave the
			// workspace alive: the process stop below is unconditional, and the
			// refusal is evidence. It is INFO when this daemon ordered the
			// stand-down -- the escalation is the mechanism working -- and WARN
			// only for a kill outside one, which is a DETACHED client, whose
			// process belongs to the successor daemon.
			evidence := dlog.Context{"cause": err.Error()}
			if ordered {
				log.Info(opKill, "the forced KillSession did not answer", evidence)
			} else {
				log.Warn(opKill, "the forced KillSession did not answer", evidence)
			}
		} else {
			log.Debug(opKill, "the session was killed forcefully", nil)
		}
	} else {
		log.Debug(opKill, "no live session to kill", nil)
	}

	if err := v.deps.Sessions.Stop(ctx, ws, true); err != nil {
		log.Error(opKill, "could not stop the shim process", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("kill %q: stop the shim: %w", ws, err)
	}

	at := v.now()
	report, err := v.deps.Queue.CloseOrphans(ctx, ws, at)
	if err != nil {
		log.Error(opKill, "could not close the orphaned turns", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("kill %q: close orphans: %w", ws, err)
	}
	if err := v.deps.DB.SetSessionTerminal(ctx, ws, wsm.SessionTerminal{
		Kind:   "killed",
		Detail: "KillWorkspace",
		At:     at,
	}); err != nil {
		// A registered workspace may have no session: a freshly-opened one
		// whose bring-up never ran has no session row, so there is no terminal
		// to record. Killing it must not require one -- that specific absence
		// is benign, and the rest of the teardown below still runs so the
		// workspace is actually gone. Every OTHER terminal-recording failure --
		// a real error for a session that DOES exist -- still fails the kill.
		if errors.Is(err, wsm.ErrNotFound) {
			log.Debug(opKill, "no session to record a terminal for", dlog.Context{"cause": err.Error()})
		} else {
			log.Error(opKill, "could not record the session terminal", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("kill %q: record the terminal: %w", ws, err)
		}
	}

	// A killed workspace's roster row carries closed = true: Emacs derives its
	// tab set from that flag, and a killed workspace has no editor state left.
	if err := v.deps.DB.SetClosed(ctx, ws, true); err != nil {
		log.Error(opKill, "could not record the close", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("kill %q: record closed: %w", ws, err)
	}

	log.Info(opKill, "killed the workspace's session", dlog.Context{"orphans_closed": len(report.Turns)})
	v.republishRegistry(ctx, log, opKill)
	return nil
}

// Nuke destroys data: it kills the session when one is live, deletes the
// worktree AND the branch, and forgets the record. A nuked workspace LEAVES the
// roster entirely — it is the only verb in this package with no undo.
func (v *verbs) Nuke(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, "NukeWorkspace", ws)
	if err != nil {
		return err
	}

	// The waiting merge goes first, for the same reason Kill drops it: the
	// nuke destroys the very worktree the merge would have run against.
	v.deps.Merge.OnWorkspaceClosed(ctx, ws)

	if v.deps.Sessions.Live(ws) {
		if err := v.kill(ctx, log, ws); err != nil {
			return fmt.Errorf("nuke %q: %w", ws, err)
		}
	} else {
		log.Debug(opNuke, "no live session to kill before the nuke", nil)
	}

	repo, err := v.repoDirOf(ctx, record)
	if err != nil {
		log.Error(opNuke, "could not resolve the repository to nuke from", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("nuke %q: %w", ws, err)
	}
	if err := v.deps.Git.Nuke(ctx, repo, record.Dir, record.Branch); err != nil {
		// A GIT FAILURE HERE IS AN ANSWER, not an internal error: the contract
		// spells NukeWorkspaceError.git_failed for exactly this, and it carries
		// git's own account so the caller learns WHAT would not be destroyed
		// rather than only that something did not work. The record has not been
		// forgotten, so nothing is half-nuked.
		log.Debug(opNuke, "git refused to destroy the worktree and branch", dlog.Context{
			"repo": repo, "dir": record.Dir, "branch": record.Branch, "cause": err.Error(),
		})
		return refuse(log, "NukeWorkspace", ArmGitFailed,
			fmt.Sprintf("destroying %q: %v", record.Dir, err), false)
	}

	report, err := v.deps.DB.Forget(ctx, ws)
	if err != nil {
		log.Error(opNuke, "could not forget the workspace record", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("nuke %q: forget: %w", ws, err)
	}

	log.Info(opNuke, "nuked the workspace", dlog.Context{
		"dir": record.Dir, "branch": record.Branch,
		"repository_forgotten": report.RepositoryDir,
	})
	v.republishRegistry(ctx, log, opNuke)
	return nil
}

// repoDirOf answers the repository directory a workspace belongs to, from the
// registry rather than from the worktree's own path.
func (v *verbs) repoDirOf(ctx context.Context, record wsm.Workspace) (string, error) {
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		return "", fmt.Errorf("list the repositories: %w", err)
	}
	for _, repo := range repositories {
		if repo.ID == record.Repo {
			return repo.Dir, nil
		}
	}
	return "", fmt.Errorf("repository %q is not registered", record.Repo)
}
