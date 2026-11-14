package merge

import (
	"context"
	"sync"

	"agent-repld/internal/dlog"
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
// CTX under it, since the next boot's pass runs it again, and at ERROR as
// MESSAGE otherwise.
func (o *orchestrator) mediationFailed(ctx context.Context, fields dlog.Context, message string, err error) bool {
	if cutUnder(ctx, err) {
		o.deps.Log.Global().Info(opMediation, "the mediation was cut by the daemon's exit; the next boot's pass runs it again", withField(withField(fields, "cut", message), "cause", err.Error()))
		return true
	}
	o.deps.Log.Global().Error(opMediation, message, withField(fields, "error", err.Error()))
	return false
}
