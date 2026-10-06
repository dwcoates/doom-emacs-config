package merge

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
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
	const op = "daemon.merge.transfer"
	o.mu.Lock()
	o.transferred[ws] = true
	wait := o.requested[ws]
	delete(o.requested, ws)
	r, running := o.runsByWorkspace[ws]
	mark, terminal := o.terminals[ws]
	if running && !terminal {
		r.suspending = true
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
	bound := o.terminalDrainBound()
	if terminal {
		// A RUN IN ITS TERMINAL FINISHES HERE: its teardown is short and
		// local, and what it releases is the merge's end, not a step to move.
		select {
		case <-mark.done:
		case <-time.After(bound):
			err := fmt.Errorf("merge: the merge of %s did not finish its terminal within %s", ws, bound)
			log.Error(op, "a merge in its terminal did not finish before the transfer's bound; the transfer is refused", withField(fields, "error", err.Error()))
			return err
		}
		o.forgetMoved(ws)
		log.Info(op, "a merge finished its terminal before its workspace moved", fields)
		return nil
	}
	o.awaitStoppingPoints(op, []*run{r})
	fields["step"] = r.progressStep()
	log.Info(op, "a merge is suspended at a stopping point for its workspace's transfer; the successor resumes it at the step it recorded", fields)
	r.cancel(errHandedOver)
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
	o.mu.Lock()
	delete(o.transferred, ws)
	o.mu.Unlock()
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

// movedAway reports whether a workspace's merges were handed to another
// daemon, which this one then admits nothing of.
func (o *orchestrator) movedAway(ws ids.WorkspaceID) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.transferred[ws]
}
