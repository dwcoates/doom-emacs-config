package merge

import (
	"context"
	"errors"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/sessionwatcher"
)

// mediationLife is the lifetime of the deploy mediation's work in this
// process: the pass that finds the mediations owed (Mediate) and every round
// it starts. Each enters it, and the daemon's exit stops it (stopMediation)
// before the state client closes: no pass or round starts after the stop,
// the context every one runs under is cancelled, and the drain waits for
// each to return (2026-11-14: a pass running through an exit read a closed
// database, failing TestALandedMergesLedgerRecordsEachStepsInterval).
//
// A ROUND CUT HERE LOSES NOTHING. Its turn lives on in the mediator's
// session, and the mediation records that its mediator was told
// (wsm.Mediation.Prompted), so the next boot's pass reattaches the turn.
type mediationLife struct {
	mu sync.Mutex
	// stopped is set by the daemon's exit; nothing enters after it.
	stopped bool
	// ctx is what every pass and round runs under, cancelled by the stop;
	// built at the first entry.
	ctx    context.Context
	cancel context.CancelFunc
	// running counts the passes and rounds entered and not yet left.
	running int
	// idle is closed when running falls to zero after a stop; nil otherwise.
	idle chan struct{}
	// handedOver is set when a handover cut a round (mediationFailed): the
	// successor runs it, and should the handover end with this daemon
	// serving again, this daemon runs it again (resumeHandedOver).
	handedOver bool
}

// enterMediation enters a pass or a round, answering the context it runs
// under and the leave it calls once it returns; false once the daemon's exit
// stopped the mediation, entering nothing.
func (o *orchestrator) enterMediation() (context.Context, func(), bool) {
	l := &o.mediationLife
	l.mu.Lock()
	defer l.mu.Unlock()
	if l.stopped {
		return nil, nil, false
	}
	if l.ctx == nil {
		l.ctx, l.cancel = context.WithCancel(context.Background())
	}
	l.running++
	var once sync.Once
	return l.ctx, func() { once.Do(o.leaveMediation) }, true
}

// leaveMediation leaves one pass or round, closing idle when it was the last.
func (o *orchestrator) leaveMediation() {
	l := &o.mediationLife
	l.mu.Lock()
	defer l.mu.Unlock()
	l.running--
	if l.running < 0 {
		panic("merge: a mediation left more often than it entered")
	}
	if l.running == 0 && l.idle != nil {
		close(l.idle)
		l.idle = nil
	}
}

// stopMediation stops the mediation for the daemon's exit: nothing enters
// after it, and every pass and round running is cut. It answers what closes
// once the last of them returns, nil when none runs.
func (o *orchestrator) stopMediation() <-chan struct{} {
	l := &o.mediationLife
	l.mu.Lock()
	defer l.mu.Unlock()
	l.stopped = true
	if l.cancel != nil {
		l.cancel()
	}
	if l.running == 0 {
		return nil
	}
	if l.idle == nil {
		l.idle = make(chan struct{})
	}
	return l.idle
}

// mediationNotRun is the record of a pass or round the daemon's exit kept
// from running.
const mediationNotRun = "the mediations were not run: this daemon is stopping; the daemon that serves the queues next runs them"

// mediationFailed records a pass's or a round's failure ERR and answers
// whether it was a cut: at INFO when the daemon's exit (or the caller) cut
// CTX under it, since the next boot's pass runs it again, or when a handover
// took the mediator's session away (cutByHandover), since the daemon that
// serves the queues next runs it again; and at ERROR as MESSAGE otherwise.
func (o *orchestrator) mediationFailed(ctx context.Context, fields dlog.Context, message string, err error) bool {
	if cutUnder(ctx, err) {
		o.deps.Log.Global().Info(opMediation, "the mediation was cut by the daemon's exit; the next boot's pass runs it again", withField(withField(fields, "cut", message), "cause", err.Error()))
		return true
	}
	if o.cutByHandover(err) {
		o.deps.Log.Global().Info(opMediation, mediationHandedOver, withField(withField(fields, "cut", message), "cause", err.Error()))
		return true
	}
	o.deps.Log.Global().Error(opMediation, message, withField(fields, "error", err.Error()))
	return false
}

// mediationHandedOver is the record of a round a handover cut.
const mediationHandedOver = "the mediation was cut by a handover; the daemon that serves the queues next runs it again"

// cutByHandover answers whether ERR is a handover's cut, marking the round
// owed a rerun here should this daemon serve again: the mediator's session
// watcher closed while this daemon does not serve the queues. A WORKSPACE
// TRANSFERS ONLY INSIDE A HANDOVER, and a standing handover never serves the
// queues (rollout's servesQueueLocked), so the session a transfer takes away
// is always read as this cut (2026-11-14: the mediator's transfer under a
// round awaiting its turn was recorded at ERROR). A watcher closed while this
// daemon serves is no handover's, and stays an error.
func (o *orchestrator) cutByHandover(err error) bool {
	if !errors.Is(err, sessionwatcher.ErrWatcherClosed) || o.deps.Standing.ServesQueue() {
		return false
	}
	l := &o.mediationLife
	l.mu.Lock()
	defer l.mu.Unlock()
	l.handedOver = true
	return true
}

// resumeHandedOver runs the mediation pass again when a handover cut a round
// and this daemon serves the queues again: the handover ended without a
// successor taking them, so nobody else runs the round.
func (o *orchestrator) resumeHandedOver(ctx context.Context) {
	l := &o.mediationLife
	l.mu.Lock()
	owed := l.handedOver
	l.handedOver = false
	l.mu.Unlock()
	if !owed {
		return
	}
	o.deps.Log.Global().Info(opMediation, "this daemon serves the queues again after a handover cut a mediation round; the round runs again", nil)
	if !o.async {
		o.Mediate(ctx)
		return
	}
	inflight.Go(func() { o.Mediate(ctx) })
}
