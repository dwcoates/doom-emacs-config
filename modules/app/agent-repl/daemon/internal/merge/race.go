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
// store's YieldMergeHead), and the winner is promoted and lands on the tree it
// already tested.
//
// THE LOSER IS SUSPENDED, NEVER TORN DOWN (owner ruling, 2026-10-13). It is
// brought to a stopping point exactly as a pause brings a merge (suspendFor),
// keeping its lease and its progress record, and taken straight back from its
// durable rows (AdoptWorkspace), still admitted right behind the winner. An
// agent turn it was waiting on -- a conflict's resolution, a fixing attempt --
// lives on in the session. It resumes only once it heads the queue again
// (resumeIn, extendOnce): it finishes the step it was on, reattaching to that
// turn, and only then rebases onto the target's new tip and starts over
// (resumeAttempt).
//
// THE DECISION IS ATOMIC UNDER THE ORCHESTRATOR'S LOCK. A head that has begun
// committing (beginLanding), or has entered its terminal, is never overtaken,
// and a head being overtaken never begins committing: it waits for the stop,
// or, if the overtaking could not be recorded, goes on.
//
// THE HEAD'S LEAVING AND THE YIELD ARE DECIDED BY THE STORE. A head that is
// not committing can still fail or be abandoned after the race was claimed,
// and its queue entry then goes in a transaction of its own. Whichever of
// that removal and YieldMergeHead commits first decides: a yield that finds
// the head gone answers wsm.ErrMergeHeadLeft, and there is no race -- the
// merge behind it simply heads the queue.

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
		if r.stopping(err) || r.cut(err) {
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
	if _, ending := o.terminals[head.ws]; ending || head.abandonedBy != "" {
		o.mu.Unlock()
		log.Debug(op, "the head is already ending; the merge behind it heads the queue once it has left", dlog.Context{
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
	// THE TEST SEAM, NIL IN PRODUCTION: the window between the race's claim
	// and its record, where the head can still leave the queue.
	if o.onRaceClaimed != nil {
		o.onRaceClaimed(head.ws)
	}
	err = o.deps.DB.YieldMergeHead(context.WithoutCancel(ctx), r.repo, head.ws, r.ws)
	if errors.Is(err, wsm.ErrMergeHeadLeft) {
		log.Info(op, "the head left the queue before the race to land was recorded; there is no race, and the merge behind it heads the queue", dlog.Context{
			"workspace": string(r.ws), "head": string(head.ws), "tip": tip})
		o.kick(r.repo)
		return
	}
	if err != nil {
		log.Error(op, "could not record the race to land the merge won; the head goes on and lands first", dlog.Context{
			"workspace": string(r.ws), "head": string(head.ws), "error": err.Error()})
		return
	}
	log.Info(op, "the merge behind the head passed its gate on the target's tip first; it lands ahead, and the head is suspended behind it", dlog.Context{
		"workspace": string(r.ws), "head": string(head.ws), "tip": tip})
	o.log(ctx, head.ws).Info(op, "the merge behind this one passed its gate on the target's tip first; this merge is suspended at its step and resumes there once the winner is done", dlog.Context{
		"workspace": string(head.ws), "winner": string(r.ws), "tip": tip})
	settle := context.WithoutCancel(ctx)
	lost := suspension{op: op, cause: &raceLost{winner: r.ws},
		moved:     "the merge that lost the race to land is suspended at the step it recorded; it resumes there once it heads the queue again",
		by:        "the race to land it lost",
		resumes:   "it resumes at the step it recorded once it heads the queue again",
		gateRerun: "it runs again from its start once the merge heads the queue again"}
	if err := o.suspendFor(settle, head.ws, lost); err != nil {
		log.Error(op, "could not suspend the head that lost the race to land", dlog.Context{
			"workspace": string(r.ws), "head": string(head.ws), "error": err.Error()})
	} else if err := o.AdoptWorkspace(settle, head.ws); err != nil {
		log.Error(op, "could not take back the head that lost the race to land", dlog.Context{
			"workspace": string(r.ws), "head": string(head.ws), "error": err.Error()})
	}
	o.kick(r.repo)
}
