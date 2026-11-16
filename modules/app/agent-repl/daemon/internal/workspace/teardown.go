package workspace

import (
	"context"
	"errors"
	"fmt"
	"os"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimlife"
	"agent-repld/internal/wsm"
)

// Teardown is the SLOW HALF of a kill or a nuke: the session's death, and for
// a nuke the worktree and the branch. It runs after the fast half
// (BeginKill, BeginNuke) has already marked the workspace closed, so a caller
// can answer its client between the two and run this detached.
type Teardown func(ctx context.Context) error

// Kill is the big red button: FORCED session death. It never blocks on what is
// running, and it destroys no data — the worktree, the branch and every durable
// record survive, and the terminal it writes is REHYDRATABLE, so the
// conversation can be resumed later.
//
// The order matters: the session is asked to die forcefully FIRST, so the shim
// writes its own terminals, and only then is the process stopped. Everything
// still without a terminal is closed in ONE transaction.
func (v *verbs) Kill(ctx context.Context, ws ids.WorkspaceID) error {
	teardown, err := v.BeginKill(ctx, ws)
	if err != nil {
		return err
	}
	return teardown(ctx)
}

// BeginKill is Kill's fast half. It answers every refusal a kill can have (the
// workspace is not this daemon's to kill, or it has open children), marks the
// workspace CLOSED, so the roster says so everywhere before the session has
// begun to die, and drops a queued merge; the Teardown it answers kills the
// session.
func (v *verbs) BeginKill(ctx context.Context, ws ids.WorkspaceID) (Teardown, error) {
	_, log, err := v.owned(ctx, "KillWorkspace", ws)
	if err != nil {
		return nil, err
	}
	// THE CLOSE IS RECORDED FIRST, because it is the one step a kill can be
	// refused at: a workspace with open children is not killed
	// (wsm/family.go), and a refused kill must leave its queued merge where it
	// stands.
	if err := v.markClosedFirst(ctx, log, opKill, ws, wsm.HowKilled); err != nil {
		return nil, fmt.Errorf("kill %q: %w", ws, err)
	}
	// A WAITING MERGE ENDS WITH THE WORKSPACE. Close refuses outright while a
	// merge is queued, but Kill never blocks on what is running, so this is the
	// one door a queued merge's workspace can leave through — and a merge left
	// on the queue behind it would wait forever on a workspace that is gone.
	v.deps.Merge.OnWorkspaceClosed(ctx, ws)
	return func(ctx context.Context) error { return v.kill(ctx, log, ws, wsm.CloseByUser) }, nil
}

// markClosedFirst marks a workspace closed and republishes the roster BEFORE
// its teardown runs: the tab goes the moment the verb is accepted, and a
// roster push arriving while the session dies must already say closed, or it
// would stand the tab again. A shim dying under the teardown is not revived
// either, since a closed workspace's session is never brought back.
//
// HOW is how the workspace ends (killed or nuked), which the close edge tells
// its followers (wsm.Closure.How).
func (v *verbs) markClosedFirst(ctx context.Context, log dlog.Logger, op string, ws ids.WorkspaceID, how wsm.CloseHow) error {
	if err := v.deps.DB.MarkClosed(ctx, ws, wsm.Closure{At: v.now(), By: wsm.CloseByUser, How: how}); err != nil {
		if refusedByFamily(log, op, err) {
			return err
		}
		log.Error(op, "could not record the close ahead of the teardown", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("record closed: %w", err)
	}
	log.Info(op, "marked the workspace closed; its teardown follows", nil)
	v.republishRegistry(ctx, log, op)
	return nil
}

// kill is Kill's body, shared with Nuke, which kills before it destroys, and
// with the retirements. BY is who the close it records is by.
func (v *verbs) kill(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, by wsm.CloseCause) error {
	report, err := v.endSession(ctx, log, ws)
	if err != nil {
		return err
	}
	// The session is gone, and with it, often, the request that asked for
	// the kill: the close is recorded whatever became of that request
	// (endSession).
	ctx = context.WithoutCancel(ctx)

	// A killed workspace's roster row carries closed = true: Emacs derives its
	// tab set from that flag, and a killed workspace has no editor state left.
	if err := v.deps.DB.MarkClosed(ctx, ws, wsm.Closure{At: v.now(), By: by}); err != nil {
		log.Error(opKill, "could not record the close", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("kill %q: record closed: %w", ws, err)
	}

	log.Info(opKill, "killed the workspace's session", dlog.Context{"orphans_closed": len(report.Turns)})
	v.republishRegistry(ctx, log, opKill)
	return nil
}

// endSession ends WS's session at once, its turn cut: the shim's session is
// killed, the process stopped, the cut turns closed and the session's
// terminal recorded. It closes nothing: Kill closes the workspace after it,
// and a deploy check workspace standing by keeps its workspace open.
func (v *verbs) endSession(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (wsm.OrphanReport, error) {
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

	if err := v.deps.Sessions.Stop(shimlife.WithStandDown(ctx, shimlife.StandDownKill), ws, true); err != nil {
		log.Error(opKill, "could not stop the shim process", dlog.Context{"cause": err.Error()})
		return wsm.OrphanReport{}, fmt.Errorf("kill %q: stop the shim: %w", ws, err)
	}

	// THE BOOKKEEPING OUTLIVES ITS CALLER: a kill is often asked by the very
	// session it just ended (a deploy check's stand-by or stop, a decision
	// that ends its chain), and ending that session cancels the request's
	// context. On the request's context the orphaned turns were never
	// closed (2026-11-16, four times: "wsm: begin transaction: context
	// canceled"), so the steps after the stop run on one the session's end
	// cannot cancel. The store bounds each of them itself.
	ctx = context.WithoutCancel(ctx)
	at := v.now()
	report, err := v.deps.Queue.CloseOrphans(ctx, ws, at, wsm.LeaveCut)
	if err != nil {
		log.Error(opKill, "could not close the orphaned turns", dlog.Context{"cause": err.Error()})
		return wsm.OrphanReport{}, fmt.Errorf("kill %q: close orphans: %w", ws, err)
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
			return wsm.OrphanReport{}, fmt.Errorf("kill %q: record the terminal: %w", ws, err)
		}
	}
	return report, nil
}

// Nuke destroys data: it kills the session when one is live, deletes the
// worktree AND the branch, and forgets the record. A nuked workspace LEAVES the
// roster entirely — it is the only verb in this package with no undo.
func (v *verbs) Nuke(ctx context.Context, ws ids.WorkspaceID) error {
	teardown, err := v.BeginNuke(ctx, ws)
	if err != nil {
		return err
	}
	return teardown(ctx)
}

// BeginNuke is Nuke's fast half, as BeginKill is Kill's: every ownership
// refusal, the queued merge dropped, the workspace marked closed. The Teardown
// it answers kills the session and destroys the worktree and the branch; a git
// failure there is the typed `git_failed` refusal, and leaves the workspace
// closed but recorded.
func (v *verbs) BeginNuke(ctx context.Context, ws ids.WorkspaceID) (Teardown, error) {
	record, log, err := v.owned(ctx, "NukeWorkspace", ws)
	if err != nil {
		return nil, err
	}

	// The close is recorded first, the one step a nuke can be refused at, as
	// for a kill; then the waiting merge goes, for the same reason Kill drops
	// it: the nuke destroys the very worktree the merge would have run
	// against.
	if err := v.markClosedFirst(ctx, log, opNuke, ws, wsm.HowNuked); err != nil {
		return nil, fmt.Errorf("nuke %q: %w", ws, err)
	}
	v.deps.Merge.OnWorkspaceClosed(ctx, ws)
	return func(ctx context.Context) error { return v.nuke(ctx, log, record) }, nil
}

// nuke is Nuke's slow half.
func (v *verbs) nuke(ctx context.Context, log dlog.Logger, record wsm.Workspace) error {
	ws := record.ID
	if v.deps.Sessions.Held(ws) {
		if err := v.kill(ctx, log, ws, wsm.CloseByUser); err != nil {
			return fmt.Errorf("nuke %q: %w", ws, err)
		}
	} else {
		log.Debug(opNuke, "no live session to kill before the nuke", nil)
	}

	// THE NUKE IS OWED BEFORE ANYTHING IS DESTROYED, so one that stops part
	// way -- a forced removal that failed after it set the tree aside, with
	// the branch and this record left -- is finished by the next boot
	// (FinishOwedNukes) rather than waiting for someone to nuke it again,
	// and its missing worktree is never restored meanwhile.
	if err := v.deps.DB.OweNuke(ctx, ws, v.now()); err != nil {
		log.Error(opNuke, "could not record the nuke as owed; nothing was destroyed", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("nuke %q: record it owed: %w", ws, err)
	}

	if err := v.destroyStorage(ctx, log, record); err != nil {
		return err
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

// FinishOwedNukes finishes every nuke that stopped part way (wsm
// Workspace.NukeOwedAt): the boot runs it before it reads the registry, so a
// nuke whose forced removal failed after setting the tree aside is finished
// on its own. A nuke that fails again is recorded at ERROR and stays owed for
// the next boot; only a registry that cannot be read is returned.
func (v *verbs) FinishOwedNukes(ctx context.Context) error {
	log := v.deps.Log.Global()
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(opNuke, "could not list the workspaces to finish the owed nukes", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("finish the owed nukes: list the workspaces: %w", err)
	}
	for _, record := range workspaces {
		if record.NukeOwedAt == nil {
			continue
		}
		// The worktree is gone or set aside, so its records go to the
		// central sink naming it.
		wsLog := v.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{
			dlog.KeyWorkspaceID: string(record.ID), "nuke_owed_at": record.NukeOwedAt.UTC(),
		})
		if err := v.nuke(ctx, wsLog, record); err != nil {
			wsLog.Error(opNuke, "an owed nuke could not be finished; it stays owed for the next boot", dlog.Context{
				"dir": record.Dir, "branch": record.Branch, "cause": err.Error(),
			})
			continue
		}
		wsLog.Info(opNuke, "finished a nuke that had stopped part way", dlog.Context{"dir": record.Dir, "branch": record.Branch})
	}
	return nil
}

// destroyStorage destroys what a workspace keeps, as its kind keeps it: a git
// workspace's worktree and branch, a zettel chat thread's graph and directory.
// A failure is the kind's typed refusal, and the record is kept, so nothing is
// half-nuked and a nuke tried again picks up where this one stopped.
func (v *verbs) destroyStorage(ctx context.Context, log dlog.Logger, record wsm.Workspace) error {
	switch record.Kind {
	case wsm.WorkspaceKindRepository:
		repo, err := v.repoDirOf(ctx, record)
		if err != nil {
			log.Error(opNuke, "could not resolve the repository to nuke from", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("nuke %q: %w", record.ID, err)
		}
		if err := v.deps.Git.Nuke(ctx, repo, record.Dir, record.Branch); err != nil {
			// A GIT FAILURE HERE IS AN ANSWER, not an internal error: the
			// contract spells NukeWorkspaceError.git_failed for exactly this,
			// and it carries git's own account so the caller learns WHAT would
			// not be destroyed rather than only that something did not work.
			log.Debug(opNuke, "git refused to destroy the worktree and branch", dlog.Context{
				"repo": repo, "dir": record.Dir, "branch": record.Branch, "cause": err.Error(),
			})
			return refuse(log, "NukeWorkspace", ArmGitFailed,
				fmt.Sprintf("destroying %q: %v", record.Dir, err), false)
		}
		return nil
	case wsm.WorkspaceKindThread:
		// THE GRAPH GOES FIRST: it is the thread's only copy of its
		// conversation's shape, and a directory without its graph is the
		// state a retried nuke can finish from, never the reverse.
		if err := v.deps.Threads.Delete(ctx, record.ID); err != nil {
			return refuse(log, "NukeWorkspace", ArmThreadDeleteFailed,
				fmt.Sprintf("deleting the thread's graph: %v", err), false)
		}
		if err := os.RemoveAll(record.Dir); err != nil {
			log.Error(opNuke, "could not delete the thread's directory", dlog.Context{"dir": record.Dir, "cause": err.Error()})
			return refuse(log, "NukeWorkspace", ArmThreadDeleteFailed,
				fmt.Sprintf("deleting %q: %v", record.Dir, err), false)
		}
		return nil
	default:
		panic(fmt.Sprintf("workspace: nuke of %q, whose kind %q this build does not know", record.ID, record.Kind))
	}
}

// repoDirOf answers the repository directory a workspace belongs to, from the
// registry rather than from the worktree's own path.
func (v *verbs) repoDirOf(ctx context.Context, record wsm.Workspace) (string, error) {
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		return "", fmt.Errorf("list the repositories: %w", err)
	}
	if repo, found := wsm.RepositoryWithID(repositories, record.Repo); found {
		return repo.Dir, nil
	}
	return "", fmt.Errorf("repository %q is not registered", record.Repo)
}
