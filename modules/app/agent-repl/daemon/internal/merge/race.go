package merge

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is the RACE TO LAND (owner ruling, 2026-10-12): the two merges at
// the front of a queue each rebase onto the TARGET's tip, not one onto the
// other, and each runs the whole sequence toward landing. Whichever passes
// its gate on the target's tip first lands.
//
// THE HEAD WINS by landing as it always has; the merge behind it then finds
// the target moved, its gate on the old tip is cut, and it starts over on the
// new tip as the new head.
//
// THE MERGE BEHIND WINS by passing its gate on the target's tip while the head
// has not begun committing: it takes the head's place at the front (the
// store's YieldMergeHead), the head is stopped and goes back to waiting right
// behind it, the winner is promoted and lands on the tree it already tested,
// and the head then starts over from a rebase onto the new tip, with a new
// merge behind it.
//
// THE DECISION IS ATOMIC UNDER THE ORCHESTRATOR'S LOCK. A head that has begun
// committing (beginLanding) is never overtaken, and a head being overtaken
// never begins committing: it waits for the stop, or, if the overtaking could
// not be recorded, goes on.

// raceLost is the cause a head that lost the race to land is stopped with.
type raceLost struct{ winner ids.WorkspaceID }

func (e *raceLost) Error() string {
	return fmt.Sprintf("merge: the merge behind this one (%s) passed its gate on the target's tip first and lands ahead of it", e.winner)
}

// raceLostOf answers the winner a run's context was stopped for, and false
// when it was not.
func raceLostOf(ctx context.Context) (ids.WorkspaceID, bool) {
	var lost *raceLost
	if errors.As(context.Cause(ctx), &lost) {
		return lost.winner, true
	}
	return "", false
}

// overtaking is a race the merge behind a head is winning: the head it is
// overtaking, and done, which closes once the overtaking is over either way.
type overtaking struct {
	head *run
	done chan struct{}
}

// noteTargetTip records the target's tip as a run read it, and tells the
// chain when it moved: the merge behind the head rebases onto the target's
// tip, so a moved tip is a new base for it.
func (o *orchestrator) noteTargetTip(repo wsm.RepoKey, tip string) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.targetTips[repo] == tip {
		return
	}
	o.targetTips[repo] = tip
	o.signalChainLocked(repo)
}

// targetTip is the target's tip as last read, empty before any run read it.
func (o *orchestrator) targetTip(repo wsm.RepoKey) string {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.targetTipLocked(repo)
}

// targetTipLocked is the target's tip as last read, empty before any run read
// it. Callers hold o.mu.
func (o *orchestrator) targetTipLocked(repo wsm.RepoKey) string {
	return o.targetTips[repo]
}

// beginLanding marks r as committing, so nothing overtakes it from here, and
// answers false (with the overtaking to wait on) when r is being overtaken.
func (o *orchestrator) beginLanding(r *run) (bool, <-chan struct{}) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if ot, ok := o.overtakings[r.repo]; ok && ot.head == r {
		return false, ot.done
	}
	r.landing = true
	return true, nil
}

// claimRace is the merge behind the head, its gate passed on base, asking to
// land ahead of the head. It answers once the head is stopped (the pump then
// promotes r), or at once when the race is not r's to win: the target moved
// (its tip is noted, and r rebases onto it), the head is already committing,
// or there is no head to overtake.
func (o *orchestrator) claimRace(ctx context.Context, r *run, base string) {
	const op = "daemon.merge.race"
	log := o.log(ctx, r.ws)
	tip, err := r.git.ResolveRef(ctx, r.subject.targetDir, "HEAD")
	if err != nil {
		if r.stopping(err) {
			return
		}
		log.Error(op, "could not read the target's tip to claim the race to land", dlog.Context{"workspace": string(r.ws), "error": err.Error()})
		return
	}
	if tip != base {
		log.Info(op, "the target moved under the merge's passed gate; it rebases onto the new tip", dlog.Context{
			"workspace": string(r.ws), "tested_on": base, "tip": tip})
		o.noteTargetTip(r.repo, tip)
		return
	}
	o.mu.Lock()
	head := o.running[r.repo]
	_, contended := o.overtakings[r.repo]
	switch {
	case head == nil, head == r, contended, o.draining:
		o.mu.Unlock()
		return
	case head.landing:
		o.mu.Unlock()
		log.Debug(op, "the head is already committing; the merge behind it waits its turn", dlog.Context{
			"workspace": string(r.ws), "head": string(head.ws)})
		return
	}
	ot := &overtaking{head: head, done: make(chan struct{})}
	o.overtakings[r.repo] = ot
	o.mu.Unlock()
	defer func() {
		o.mu.Lock()
		delete(o.overtakings, r.repo)
		o.mu.Unlock()
		close(ot.done)
	}()
	if err := o.deps.DB.YieldMergeHead(context.WithoutCancel(ctx), r.repo, head.ws, r.ws); err != nil {
		log.Error(op, "could not record the race to land the merge won; the head goes on and lands first", dlog.Context{
			"workspace": string(r.ws), "head": string(head.ws), "error": err.Error()})
		return
	}
	log.Info(op, "the merge behind the head passed its gate on the target's tip first; it lands ahead, and the head starts over behind it", dlog.Context{
		"workspace": string(r.ws), "head": string(head.ws), "tip": tip})
	o.log(ctx, head.ws).Info(op, "the merge behind this one passed its gate on the target's tip first; this merge stops and starts over behind it", dlog.Context{
		"workspace": string(head.ws), "winner": string(r.ws), "tip": tip})
	head.cancel(&raceLost{winner: r.ws})
	select {
	case <-head.finished:
	case <-ctx.Done():
	}
	o.kick(r.repo)
}

// yieldTerminal ends a head that lost the race to land. Nothing of it is over:
// its entry already waits right behind the winner (YieldMergeHead), and its
// bubble goes on, so it gives back only what a run holds -- the output
// address, the occupancy, the lease, the progress record, the displaced turn,
// the queue's tree and the slot -- and publishes no terminal.
func (r *run) yieldTerminal(ctx context.Context, winner ids.WorkspaceID) {
	const op = "daemon.merge.race"
	if r.exiting() {
		r.suspend(ctx, &raceLost{winner: winner})
		return
	}
	log := r.o.log(ctx, r.ws)
	r.o.deps.Feed.SetOutputAddress(r.ws, nil)
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	r.closeOpenRounds(ctx, "abandoned")
	if err := r.o.deps.DB.ReleaseLease(ctx, r.lease.ID); err != nil {
		log.Error(op, "could not release the lease of the merge that lost the race to land", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	if _, err := r.o.deps.DB.DropMergeProgress(ctx, r.lease.ID); err != nil {
		log.Error(op, "could not drop the progress record of the merge that lost the race to land", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	r.o.mu.Lock()
	if r.o.running[r.repo] == r {
		delete(r.o.running, r.repo)
	}
	delete(r.o.runsByWorkspace, r.ws)
	r.o.mu.Unlock()
	r.resubmitDisplaced(ctx)
	r.dropTree(ctx)
	r.o.releaseSlot(ctx, r)
	log.Info(op, "the merge that lost the race to land waits behind the winner and starts over once it lands", dlog.Context{
		"workspace": string(r.ws), "winner": string(winner)})
	if err := r.o.republishQueue(ctx, r.repo); err != nil {
		log.Error(op, "could not republish the queue", dlog.Context{"repo": string(r.repo), "error": err.Error()})
	}
	r.o.kick(r.repo)
}
