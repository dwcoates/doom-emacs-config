package merge

import (
	"context"
	"errors"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// This file is a repository's TEST TURN (owner ruling, 2026-10-14): only one
// merge of a repository runs its test gate at a time.
//
// THE TURN IS THE ORCHESTRATOR'S, held under its one lock, and it is never
// taken by a timer, a retry or a poll. A merge ready to run its gate --
// freshly rebased, or done with a fixing attempt -- joins the BACK of its
// repository's LINE; the turn passes to the FRONT of the line, under the same
// lock, the moment its holder's run ends. So while one merge fixes failures,
// the other's tests run, and the fixed one runs again once that run ends: the
// two alternate, and neither can be passed over for good.
//
// THE TURN IS GIVEN BACK WHENEVER A GATE RUN ENDS: passed, failed, cut
// because the target moved, or stopped (the daemon's exit included) -- gate
// gives it back on every way out. A run that leaves its chain gives back any
// turn or place in line it still holds, in the one place a run leaves its
// chain (releaseSlot).
//
// THE TURN IS TAKEN BEFORE THE GOVERNOR'S PERMIT, so a merge waiting for its
// repository's turn never holds a share of the machine's gates (cpugov) that
// another repository's gate could use. The host's suite slot
// (bin/suite-slot.sh) is machine-wide and separate from it.
//
// THE HEAD IS NEVER HELD BY THE GOVERNOR, through the turn either. A merge
// behind the head that holds the turn but whose gate the governor has not
// admitted yet runs nothing, so it gives the turn to its head the moment the
// head stands in line (admitGate), and joins the line again behind it. Only
// a merge whose tests are actually running ever keeps its head waiting.
//
// A RUN WAITING IN LINE STANDS AT A STEP BOUNDARY: nothing of it is in
// flight, so a restart's stop at the next boundary takes it at once, and it
// resumes at its tests step, which runs its gate again from the start.

// turnLine is one repository's test turn: the run holding it, and the runs
// waiting for it in the order they joined.
type turnLine struct {
	holder  *run
	waiting []*run
	// changed closes, and is replaced, whenever the turn passes, so a waiter
	// that read the line under the lock misses no grant.
	changed chan struct{}
}

// turnPlace is where a run stands for its repository's turn, read whole under
// one lock.
type turnPlace struct {
	// holds reports the run holds the turn.
	holds bool
	// holder is the run holding the turn when this one does not.
	holder *run
	// place is the run's 1-based place in the line; 0 when it holds the turn.
	place int
	// changed closes when the turn next passes.
	changed <-chan struct{}
}

// joinTestTurn puts r at the back of its repository's line, or gives it the
// turn at once when nobody holds it, and answers where it stands. A run that
// joins the line stands idling there (a step boundary). A run that already
// holds the turn or stands in the line is a defect: gate gives the turn back
// on every way out, so no run asks twice.
func (o *orchestrator) joinTestTurn(r *run) turnPlace {
	o.mu.Lock()
	defer o.mu.Unlock()
	line := o.testTurns[r.repo]
	if line == nil {
		line = &turnLine{changed: make(chan struct{})}
		o.testTurns[r.repo] = line
	}
	if line.holder == r || line.indexOf(r) >= 0 {
		panic("merge: a merge asked for its repository's test turn while it already held it or stood in line for it")
	}
	if line.holder == nil {
		if len(line.waiting) > 0 {
			panic("merge: a repository's test-turn line stands with nobody holding the turn")
		}
		line.holder = line.signalLocked(r)
		return turnPlace{holds: true, changed: line.changed}
	}
	line.waiting = append(line.waiting, r)
	line.signalLocked(line.holder)
	r.idling = true
	return turnPlace{holder: line.holder, place: len(line.waiting), changed: line.changed}
}

// signalLocked tells every waiter on the line that it changed, and answers
// holder for the caller to install. Called with o.mu held.
func (l *turnLine) signalLocked(holder *run) *run {
	close(l.changed)
	l.changed = make(chan struct{})
	return holder
}

// headWaitingLocked reports whether the repository's head stands in the line
// for its turn. Called with o.mu held.
func (l *turnLine) headWaitingLocked() bool {
	for _, w := range l.waiting {
		if !w.speculative {
			return true
		}
	}
	return false
}

// stopWaiting marks r as no longer idling in its repository's line.
func (o *orchestrator) stopWaiting(r *run) {
	o.mu.Lock()
	defer o.mu.Unlock()
	r.idling = false
}

// testTurnPlace reads where r stands for its repository's turn.
func (o *orchestrator) testTurnPlace(r *run) turnPlace {
	o.mu.Lock()
	defer o.mu.Unlock()
	line := o.testTurns[r.repo]
	if line == nil {
		panic("merge: a merge waits for a test turn its repository has no line for")
	}
	if line.holder == r {
		return turnPlace{holds: true, changed: line.changed}
	}
	i := line.indexOf(r)
	if i < 0 {
		panic("merge: a merge waits for its repository's test turn while standing in no line")
	}
	return turnPlace{holder: line.holder, place: i + 1, changed: line.changed}
}

// indexOf answers r's index in the line, -1 when it is not there.
func (l *turnLine) indexOf(r *run) int {
	for i, w := range l.waiting {
		if w == r {
			return i
		}
	}
	return -1
}

// awaitTestTurn takes r's repository's test turn for the gate run in ROUND,
// waiting in line behind the run holding it. The run's subtab draws the wait
// and names the holder, redrawn whenever the turn passes to another merge. A
// stopped run (ctx done) leaves the line, or gives back the turn granted in
// the same instant, and answers ctx's cause.
func (r *run) awaitTestTurn(ctx context.Context, round tabRound) error {
	const op = "daemon.merge.test_turn"
	o := r.o
	if o.onTestTurnAsk != nil {
		o.onTestTurnAsk(ctx, r.ws)
		if ctx.Err() != nil {
			return context.Cause(ctx)
		}
	}
	joined := o.deps.Now()
	place := o.joinTestTurn(r)
	if place.holds {
		o.log(ctx, r.ws).Info(op, "the merge took its repository's test turn at once; nobody else's tests were running", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "round": round.n, "waited_seconds": 0.0})
		return nil
	}
	defer o.stopWaiting(r)
	if o.onTestTurnLine != nil {
		o.onTestTurnLine(r.ws)
	}
	o.log(ctx, r.ws).Info(op, "the merge joined the line for its repository's test turn; another merge's tests are running", dlog.Context{
		"workspace": string(r.ws), "repo": string(r.repo), "round": round.n, "holder": string(place.holder.ws), "place": place.place})
	// A RESTART THAT ASKED THIS RUN TO STOP AT ITS NEXT BOUNDARY while its
	// step ran finds it here: the line is a boundary.
	if r.stopAtStepBoundary() {
		o.releaseTestTurn(context.WithoutCancel(ctx), r, "stopped at a step boundary for a restart")
		o.log(ctx, r.ws).Info("daemon.merge.restart", "the merge reached the step boundary its workspace's restart waits for, in line for its test turn; it lets go here and resumes at its tests", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "step": TabTests})
		return errMergeStopping
	}
	var drawn ids.WorkspaceID
	for {
		if place.holds {
			o.log(ctx, r.ws).Info(op, "the merge got its repository's test turn; its tests run now", dlog.Context{
				"workspace": string(r.ws), "repo": string(r.repo), "round": round.n,
				"waited_seconds": o.deps.Now().Sub(joined).Seconds()})
			return nil
		}
		if place.holder.ws != drawn {
			drawn = place.holder.ws
			r.publishRun(round, round.waitingForTurn(o.workspaceName(ctx, drawn)), nil, nil)
		}
		select {
		case <-place.changed:
		case <-ctx.Done():
			o.releaseTestTurn(context.WithoutCancel(ctx), r, "stopped while waiting for its turn")
			return context.Cause(ctx)
		}
		place = o.testTurnPlace(r)
	}
}

// releaseTestTurn gives back r's repository's test turn, passing it to the
// front of the line, or takes r out of the line when it was waiting; ENDED
// says how its run ended. A run that neither holds the turn nor waits for it
// is left as it is, so every way out may call it.
func (o *orchestrator) releaseTestTurn(ctx context.Context, r *run, ended string) {
	const op = "daemon.merge.test_turn"
	o.mu.Lock()
	line := o.testTurns[r.repo]
	if line == nil {
		o.mu.Unlock()
		return
	}
	if line.holder != r {
		i := line.indexOf(r)
		if i < 0 {
			o.mu.Unlock()
			return
		}
		line.waiting = append(line.waiting[:i:i], line.waiting[i+1:]...)
		line.signalLocked(line.holder)
		o.mu.Unlock()
		o.log(ctx, r.ws).Info(op, "the merge left the line for its repository's test turn", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "ended": ended})
		return
	}
	var next *run
	if len(line.waiting) > 0 {
		next = line.waiting[0]
		line.waiting = line.waiting[1:]
	} else {
		delete(o.testTurns, r.repo)
	}
	line.holder = line.signalLocked(next)
	o.mu.Unlock()
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "ended": ended}
	if next != nil {
		fields["next"] = string(next.ws)
		o.log(ctx, r.ws).Info(op, "the merge gave back its repository's test turn; it passes to the merge first in line", fields)
		return
	}
	o.log(ctx, r.ws).Info(op, "the merge gave back its repository's test turn; nobody is in line for it", fields)
}

// gateEnded names how a gate run ended, for the record of its turn given
// back.
func gateEnded(ctx context.Context, verdict gateVerdict, err error) string {
	switch {
	case err != nil && errors.Is(context.Cause(ctx), errBaseMoved):
		return "cut: the target moved"
	case err != nil && ctx.Err() != nil:
		return "stopped"
	case err != nil:
		return "could not run"
	case verdict.broken != "":
		return "the gate failed to run"
	case verdict.result.Passed:
		return "passed"
	}
	return "failed"
}

// errTurnToHead is the cause a governed gate's wait for admission is cut with
// when its repository's head stands in line for the turn it holds: the gate
// runs nothing yet, so the head goes first.
var errTurnToHead = errors.New("merge: the head stands in line for the test turn this merge holds while the governor has not admitted its gate")

// watchHeadInLine cuts ctx with errTurnToHead the moment r, holding its
// repository's turn, finds its head in line for it. It answers its stop,
// which waits for the watch to end.
func (o *orchestrator) watchHeadInLine(ctx context.Context, r *run, cut context.CancelCauseFunc) func() {
	return o.watch(ctx, cut, func() (bool, <-chan struct{}) {
		o.mu.Lock()
		line := o.testTurns[r.repo]
		if line == nil || line.holder != r {
			o.mu.Unlock()
			panic("merge: a gate waits for the governor without holding its repository's test turn")
		}
		head, changed := line.headWaitingLocked(), line.changed
		o.mu.Unlock()
		if head {
			cut(errTurnToHead)
			return true, nil
		}
		return false, changed
	})
}
