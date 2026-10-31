package merge

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is a merge MOVING WITH ITS WORKSPACE across a blue-green handover.
//
// A MERGE BELONGS TO THE DAEMON THAT SERVES ITS REQUESTER. It drives that
// workspace's own session, and a handover moves the session -- the shim is
// detached here and adopted by the successor -- so the merge moves with it,
// at the same moment, and never runs in two daemons or in none.
//
// THE MOVE IS A SUSPENSION AND A RESUME, the restart's own two halves. The
// transfer suspends the workspace's merge at a stopping point before the shim
// is detached (SuspendForTransfer: no git started after, the git in flight
// waited out, the merge's turn wait cut -- the turn itself lives on in the
// shim the successor adopts), which leaves its lease, queue entry and progress
// record standing; the successor's adoption of the workspace then takes every
// queue entry of it back exactly as a boot does (AdoptWorkspace), resuming the
// merge at its recorded step under the same lease, in the same bubble. A
// transfer that fails is taken back here by the same AdoptWorkspace.
//
// WHY NOT HOLD THE HANDOVER UNTIL THE MERGE ENDS: a handover never waits on
// work (owner ruling, 2026-09-27), and a merge's tests and repairs are
// unbounded work. A step boundary is what both ends already agree on: the
// progress record is written at every one, so the successor needs nothing the
// record does not carry.

// errHandedOver is the cause a transfer cuts a suspended run's waits with.
var errHandedOver = errors.New("merge: the workspace is handed over to the successor; the merge is suspended for it to resume")

// SuspendForTransfer brings the workspace's merge to a stopping point for its
// transfer, and stops this daemon admitting, running or queueing any merge of
// the workspace until it is taken back (AdoptWorkspace).
//
// It is bounded: the git in flight within MergeGitStopBound, the run's letting
// go within TerminalDrainBound. A run that does not let go is an error -- the
// transfer must not hand a session over to a successor while this daemon still
// drives it -- and the caller takes the workspace back.
func (o *orchestrator) SuspendForTransfer(ctx context.Context, ws ids.WorkspaceID) error {
	return o.suspendFor(ctx, ws, suspendTransfer)
}

// SuspendForRestart is SuspendForTransfer's stopping point taken for a
// RestartWorkspace (owner ruling, 2026-10-08): the restart bounces the shim the
// merge drives, so the merge lets go first and ResumeAfterRestart takes it
// back from the same durable rows a handover's successor reads. A merge only
// WAITING for the workspace to fall free lets go of its wait; it never blocks
// the restart, which is also the escape hatch for live work that went stale.
func (o *orchestrator) SuspendForRestart(ctx context.Context, ws ids.WorkspaceID) error {
	return o.suspendFor(ctx, ws, suspendRestart)
}

// ResumeAfterRestart takes the workspace's merges back after a restart's
// bounce, exactly as a successor adopts them (AdoptWorkspace).
func (o *orchestrator) ResumeAfterRestart(ctx context.Context, ws ids.WorkspaceID) error {
	o.deps.Log.Global().Info("daemon.merge.restart", "the restart's bounce finished; the workspace's merges are taken back from their durable rows",
		dlog.Context{"workspace": string(ws)})
	return o.AdoptWorkspace(ctx, ws)
}

// suspension names why a merge is brought to a stopping point, and every
// sentence a record says about a run it took.
type suspension struct {
	op    string
	cause error
	// moved is said once a suspended run let go.
	moved string
	// by names the suspension in a record, as what stopped the merge.
	by string
	// resumes says what becomes of the suspended merge from here.
	resumes string
	// gateRerun says what becomes of a test gate the suspension cut.
	gateRerun string
	// stepBoundary asks a run that began its steps to finish the step in
	// flight and let go at the next boundary, rather than at once.
	stepBoundary bool
	// handedOn reports that the suspension hands the merge to whoever owns it
	// next -- another daemon, or the next boot -- rather than to this
	// daemon's own take-back (AdoptWorkspace).
	handedOn bool
}

// takenBack reports whether this daemon takes a merge the suspension took
// back itself, so an abandonment arriving after it ends the merge here.
func (s suspension) takenBack() bool { return !s.handedOn }

var (
	suspendTransfer = suspension{op: "daemon.merge.transfer", cause: errHandedOver, handedOn: true,
		moved:     "a merge is suspended at a stopping point for its workspace's transfer; the successor resumes it at the step it recorded",
		by:        "its workspace's transfer",
		resumes:   "the successor resumes it at the step it recorded",
		gateRerun: "it runs again from its start when the successor resumes the merge"}
	suspendPause = suspension{op: "daemon.merge.pause", cause: errPaused,
		moved:     "a merge is suspended for its queue's pause; it resumes at the step it recorded once the queue is resumed",
		by:        "its queue's pause",
		resumes:   "it stays suspended at the step it recorded until the queue is resumed",
		gateRerun: "it runs again from its start once the queue is resumed"}
	suspendRestart = suspension{op: "daemon.merge.restart", cause: errRestarting, stepBoundary: true,
		moved:     "a merge is suspended at a stopping point for its workspace's restart; it resumes at the step it recorded once the restart finished",
		by:        "its workspace's restart",
		resumes:   "it resumes at the step it recorded once the restart finished",
		gateRerun: "it runs again from its start once the restart finished"}
	// suspendExit is the daemon's exit (Drain). Its records are the drain's
	// own, so it is never handed to suspendFor and says nothing on moving.
	suspendExit = suspension{op: "daemon.merge.drain", cause: errDaemonExiting, handedOn: true,
		by:        "the daemon's exit",
		resumes:   "the next boot resumes it at the step it recorded",
		gateRerun: "it runs again from its start when the next boot resumes the merge"}
)

// suspendLocked marks R suspending for WHY. It is the ONE way a run starts
// suspending, so a suspended run always names the suspension that took it.
// The first suspension stands: a run already suspending was cut by it, and a
// later one (the daemon's exit under a paused merge) takes nothing further.
// Called with the orchestrator's mutex held.
func (r *run) suspendLocked(why suspension) {
	if r.suspending {
		return
	}
	r.suspending = true
	r.suspendedBy = why
}

// suspendedFor answers the suspension that took R, and false while R is not
// suspending.
func (r *run) suspendedFor() (suspension, bool) {
	r.o.mu.Lock()
	defer r.o.mu.Unlock()
	return r.suspendedBy, r.suspending
}

// errPaused is the cause a pause cuts a running merge with.
var errPaused = errors.New("merge: the queue was paused; the merge is suspended until the queue is resumed")

// suspendForPause brings every merge running in the paused queues to a
// stopping point, at once, and takes each straight back from its durable
// rows: it then waits, recorded, as a resume the paused pump does not start,
// and the next resume of the queue (or the next boot, under no pause) goes
// on with it at the step it recorded. Every merge is attempted; the first
// failure is answered.
func (o *orchestrator) suspendForPause(ctx context.Context, repos []wsm.RepoKey) error {
	paused := map[wsm.RepoKey]bool{}
	for _, repo := range repos {
		paused[repo] = true
	}
	o.mu.Lock()
	var running []ids.WorkspaceID
	for ws, r := range o.runsByWorkspace {
		if paused[r.repo] {
			running = append(running, ws)
		}
	}
	o.mu.Unlock()
	sort.Slice(running, func(i, j int) bool { return running[i] < running[j] })
	var first error
	for _, ws := range running {
		if err := o.suspendFor(ctx, ws, suspendPause); err != nil {
			o.deps.Log.Global().Error("daemon.merge.pause", "could not suspend a running merge for its queue's pause", dlog.Context{
				"workspace": string(ws), "error": err.Error()})
			if first == nil {
				first = err
			}
			continue
		}
		if err := o.AdoptWorkspace(ctx, ws); err != nil && first == nil {
			first = err
		}
	}
	return first
}

// errRestarting is the cause a restart cuts a suspended run's waits with.
var errRestarting = errors.New("merge: the workspace is restarting; the merge is suspended until the restart finished")

// suspendFor is the one stopping point a transfer and a restart share.
func (o *orchestrator) suspendFor(ctx context.Context, ws ids.WorkspaceID, why suspension) error {
	op := why.op
	o.mu.Lock()
	o.takenAway[ws] = why
	wait := o.requested[ws]
	delete(o.requested, ws)
	// A MERGE REMEDIATING OUT OF LINE MOVES WITH THE WORKSPACE: the wait on
	// its fixing turn is withdrawn here, never left to fail when the session
	// leaves, which would put the merge back in line with its fix still
	// running (2026-10-31: a deploy's handover returned one mid-fix). Its
	// take-back -- the successor's, or this daemon's own -- arms it again
	// (recoverRemediating).
	o.forgetRemediationLocked(ws)
	r, running := o.runsByWorkspace[ws]
	mark, terminal := o.terminals[ws]
	// AN ABANDONED RUN IS ENDING (dequeue): no suspension takes it, and it
	// is waited for as a terminal is.
	abandoned := running && r.abandonedBy != ""
	// A RESTART WAITS FOR THE STEP IN FLIGHT (owner ruling, 2026-10-10). A
	// run that began its steps is asked to stop at its next step boundary and
	// goes on until it gets there; a transfer, and a run still only waiting
	// for its workspace, is suspended at once. The ask and the run's own
	// beginSteps are decided under this one lock, so a run that begins its
	// steps right now either sees the ask or is seen executing here.
	atBoundary := running && !terminal && !abandoned && why.stepBoundary
	if atBoundary {
		r.stopAtBoundary = true
		// A RUN IDLING BETWEEN STEPS in its chain stands at a boundary now:
		// it is suspended at once rather than waited for.
		atBoundary = r.executing && !r.idling
	}
	if running && !terminal && !abandoned && !atBoundary {
		r.suspendLocked(why)
	}
	o.mu.Unlock()
	log := o.deps.Log.Global()
	fields := dlog.Context{"workspace": string(ws)}
	if wait != nil {
		// THE REQUEST STAYS RECORDED: the successor re-arms it at adoption.
		wait.cancel()
		log.Info(op, "a merge request waiting for its turn to end moves with the workspace; the successor re-arms it", fields)
	}
	if !running {
		o.forgetMoved(ws)
		log.Debug(op, "the workspace has no merge running here; nothing to suspend", fields)
		return nil
	}
	fields["lease"] = string(r.lease.ID)
	bound := o.letGoBound(r)
	if abandoned {
		return o.awaitOwnEnd(log, op, ws, withField(fields, "cause", string(r.abandonedFor())), r.finished, ownEnd{
			refusal: "merge: the abandoned merge of %s did not end within %s",
			late:    "an abandoned merge did not end before the suspension's bound; the suspension is refused",
			ended:   "the merge was abandoned before the suspension took it; nothing is left to suspend",
		})
	}
	if terminal {
		// A RUN IN ITS TERMINAL FINISHES HERE: its teardown is short and
		// local, and what it releases is the merge's end, not a step to move.
		return o.awaitOwnEnd(log, op, ws, fields, mark.done, ownEnd{
			refusal: "merge: the merge of %s did not finish its terminal within %s",
			late:    "a merge in its terminal did not finish before the transfer's bound; the transfer is refused",
			ended:   "a merge finished its terminal before its workspace moved",
		})
	}
	if atBoundary {
		// NO TIMER: the boundary comes when the step in flight ends, however
		// long its tests or its repair turn run, and the restart's caller
		// waits for it (owner ruling, 2026-10-10). A run that concludes
		// instead of reaching another boundary has nothing left to resume.
		fields["step"] = r.progressStep()
		log.Info(op, "the restart waits for the merge to finish the step in flight; it lets go at the next step boundary", fields)
		if o.onBoundaryAsk != nil {
			o.onBoundaryAsk()
		}
		select {
		case <-r.atBoundary:
		case <-r.finished:
			o.forgetMoved(ws)
			log.Info(op, "the merge concluded before its next step boundary; the restart proceeds with nothing to resume", fields)
			return nil
		case <-ctx.Done():
			// THE CALLER LEFT: the ask is withdrawn, so the merge never stops
			// for a restart no one is waiting to run -- unless it already
			// stopped, which the take-back after a refused restart resumes.
			if !r.withdrawBoundaryStop() {
				log.Info(op, "the restart's caller left as the merge reached its step boundary; the take-back resumes it", fields)
			} else {
				log.Info(op, "the restart's caller left before the merge's step boundary; the ask is withdrawn and the merge goes on", fields)
			}
			return ctx.Err()
		}
		fields["step"] = r.progressStep()
		log.Info(op, why.moved, fields)
	} else {
		o.awaitStoppingPoints(op, []*run{r})
		fields["step"] = r.progressStep()
		log.Info(op, why.moved, fields)
		r.cancel(why.cause)
	}
	select {
	case <-r.finished:
	case <-time.After(bound):
		err := fmt.Errorf("merge: the merge of %s did not let go within %s", ws, bound)
		log.Error(op, "a suspended merge did not let go before the transfer's bound; the transfer is refused", withField(fields, "error", err.Error()))
		return err
	case <-ctx.Done():
		return ctx.Err()
	}
	o.forgetMoved(ws)
	return nil
}

// letGoBound is how long a suspension waits for the cut run R to let go.
//
// A WAITING MEMBER'S REBASE LETS GO THROUGH GIT. The cut stands it down by
// aborting the rebase standing in its worktree (standDownBackground), which is
// git work, and the terminal drain's bound is sized for SQLite writes, not
// for git: the git bound is given on top of it (2026-10-26: a waiting
// member's abort took 2.3s against 2s, and the handover's transfer of its
// workspace was refused).
func (o *orchestrator) letGoBound(r *run) time.Duration {
	if r.background {
		return o.gitStopBound() + o.terminalDrainBound()
	}
	return o.terminalDrainBound()
}

// ownEnd is what awaitOwnEnd says of one way a run ends by itself: the
// refusal's error (formatted with the workspace and the bound), the record of
// a run that outlived the bound, and the record of one that ended.
type ownEnd struct{ refusal, late, ended string }

// awaitOwnEnd waits, within the terminal drain's bound, for a run a
// suspension leaves to end by itself -- one in its terminal, or abandoned --
// and forgets what this daemon remembers of its merges once it has. A run
// still going at the bound refuses the suspension, recorded at ERROR.
func (o *orchestrator) awaitOwnEnd(log dlog.Logger, op string, ws ids.WorkspaceID, fields dlog.Context, done <-chan struct{}, says ownEnd) error {
	bound := o.terminalDrainBound()
	select {
	case <-done:
	case <-time.After(bound):
		err := fmt.Errorf(says.refusal, ws, bound)
		log.Error(op, says.late, withField(fields, "error", err.Error()))
		return err
	}
	o.forgetMoved(ws)
	log.Info(op, says.ended, fields)
	return nil
}

// forgetMoved drops what this daemon remembers of a workspace's merges in
// memory: the successor owns them from here, through their durable rows.
func (o *orchestrator) forgetMoved(ws ids.WorkspaceID) {
	o.mu.Lock()
	defer o.mu.Unlock()
	delete(o.ledgerOf, ws)
	delete(o.repoOf, ws)
	delete(o.resumes, ws)
	delete(o.displaces, ws)
}

// AdoptWorkspace takes every merge of one workspace into this daemon from its
// durable rows: a recorded request is re-armed, a queued merge put back in
// line, an admitted one resumed at its recorded step (recoverEntry, the boot's
// own per-entry recovery). It is the successor's half of a handover, called
// once the workspace is its own, and the incumbent's take-back of a transfer
// that did not land.
func (o *orchestrator) AdoptWorkspace(ctx context.Context, ws ids.WorkspaceID) error {
	const op = "daemon.merge.adopt"
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the merge queues to take the workspace's merges", dlog.Context{
			"workspace": string(ws), "error": err.Error()})
		return fmt.Errorf("merge: adopt the merges of %q: %w", ws, err)
	}
	var repos []wsm.RepoKey
	for repo, entries := range queues {
		for _, entry := range entries {
			if entry.Workspace != ws {
				continue
			}
			if err := o.recoverEntry(ctx, repo, entry); err != nil {
				return err
			}
			repos = append(repos, repo)
		}
	}
	sort.Slice(repos, func(i, j int) bool { return repos[i] < repos[j] })
	// THE MARK CLEARS WITH THE ABANDONMENT IT HELD, in one lock and only once
	// the merges are back: a dequeue either saw the mark and left its
	// abandonment here, or finds the merge as it stands now -- resuming, or
	// waiting in line -- and ends it there itself.
	o.mu.Lock()
	delete(o.takenAway, ws)
	cause, abandoned := o.abandonsAtTakeBack[ws]
	delete(o.abandonsAtTakeBack, ws)
	o.mu.Unlock()
	if abandoned {
		o.deps.Log.Global().Info(op, "an abandonment that reached the merge while a suspension held it ends the merge at its take-back", dlog.Context{
			"workspace": string(ws), "cause": string(cause)})
		if err := o.dequeue(ctx, ws, cause); err != nil {
			return err
		}
	}
	for _, repo := range repos {
		if err := o.republishQueue(ctx, repo); err != nil {
			return err
		}
		o.kick(repo)
	}
	o.deps.Log.Global().Info(op, "took the workspace's merges", dlog.Context{
		"workspace": string(ws), "entries": len(repos)})
	return nil
}

// holds reports whether this daemon already holds a workspace's merges.
func (o *orchestrator) holds(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	_, held := o.repoOf[ws]
	return held
}

// movedAway reports whether a workspace's merges were handed to another
// daemon, which this one then admits nothing of.
func (o *orchestrator) movedAway(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.isTakenAwayLocked(ws)
}

// isTakenAwayLocked reports whether a suspension took ws's merges and their
// take-back has not cleared the mark yet. Callers hold o.mu.
func (o *orchestrator) isTakenAwayLocked(ws ids.WorkspaceID) bool {
	_, away := o.takenAway[ws]
	return away
}

// ReviewQueues offers every repository with a merge queue to this daemon's
// pump, which judges each from its durable rows: its pause flag, the merges a
// restart or a transfer suspended, and its line. The takeover asks it once
// the outgoing daemon is gone (rollout.MergeMover).
func (o *orchestrator) ReviewQueues(ctx context.Context) error {
	const op = "daemon.merge.review"
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the merge queues to review them", dlog.Context{"error": err.Error()})
		return fmt.Errorf("merge: review the queues: %w", err)
	}
	repos := make([]string, 0, len(queues))
	for repo := range queues {
		repos = append(repos, string(repo))
	}
	sort.Strings(repos)
	// A MERGE NO HANDOVER GAVE THIS DAEMON IS TAKEN HERE. A workspace with no
	// session and no serving row of the outgoing daemon's (a hidden merge
	// workspace) is never transferred, so its merge ran on there until that
	// daemon's exit suspended it "for the next boot" -- and a successor runs
	// no boot. Its admitted entry then held the queue's head with nothing to
	// resume it (2026-10-30, land-relationship-backfill: the queue stood
	// still until a bounce). Every entry this daemon does not hold yet is
	// recovered as a boot recovers it.
	for _, repo := range repos {
		takenUp := false
		for _, entry := range queues[wsm.RepoKey(repo)] {
			if o.holds(entry.Workspace) {
				continue
			}
			o.deps.Log.Global().Info(op, "takes up a merge no handover gave this daemon", dlog.Context{
				"workspace": string(entry.Workspace), "repo": repo, "state": entry.State.String()})
			if err := o.recoverEntry(ctx, wsm.RepoKey(repo), entry); err != nil {
				return err
			}
			takenUp = true
		}
		if takenUp {
			if err := o.republishQueue(ctx, wsm.RepoKey(repo)); err != nil {
				return err
			}
		}
		o.kick(wsm.RepoKey(repo))
	}
	o.deps.Log.Global().Info(op, "offered every merge queue to this daemon's pump", dlog.Context{"repos": repos})
	return nil
}
