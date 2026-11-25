package main

import (
	"context"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/merge"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// opMediationHook is the operation the deploy edges' hand-off to the merge
// queue's mediation is filed under.
const opMediationHook = "daemon.cmd.mediation_hook"

// mediationHook carries a deploy's two edges (deployprogress.Edges) to the
// merge queue's deploy mediation: a deploy that began owes its mediations,
// and one that ended (or a fresh boot) runs their rounds. The merge
// orchestrator is built after the deploy's progress sink, so it is bound
// later; an edge before the binding is a defect, recorded at ERROR.
type mediationHook struct {
	ctx context.Context
	now func() time.Time
	log dlog.Logger

	mu sync.RWMutex
	o  merge.Orchestrator
	// owed is the start of the deploy that began here and has not ended,
	// zero when none has: the check an unchanged deploy releases.
	owed time.Time
	// serving is set once this daemon's serving settled (servingSettled):
	// its state handle writes from then on, so a death it reaps is owed at
	// once. crashOwed is a death reaped before then, owed at the settle.
	serving   bool
	crashOwed bool
}

func (h *mediationHook) bind(o merge.Orchestrator) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.o = o
}

func (h *mediationHook) bound(edge string) merge.Orchestrator {
	o := h.orchestrator()
	if o == nil {
		h.log.Error(opMediationHook, "a deploy edge came before the merge queue was built; no mediation follows it", dlog.Context{"edge": edge})
	}
	return o
}

// orchestrator is the merge orchestrator bound, nil before the binding. It
// is the one read of the binding.
func (h *mediationHook) orchestrator() merge.Orchestrator {
	h.mu.RLock()
	defer h.mu.RUnlock()
	return h.o
}

// began owes the mediations of the deploy that began now.
func (h *mediationHook) began() {
	at := h.now()
	h.mu.Lock()
	h.owed = at
	h.mu.Unlock()
	if o := h.bound("began"); o != nil {
		// A mediation that cannot be owed is recorded by the orchestrator.
		_ = o.OweMediations(h.ctx, at)
	}
}

// crashed owes the check of the predecessor's disorderly death, as a deploy
// that began now and ended crashed: the deploy mediator is prompted with the
// window that holds the death (readPredecessorDeath recorded it at ERROR),
// and the queue lands nothing past it until the mediator decides (2026-11-21:
// the daemon crash-looped for twenty minutes and nothing told the mediator).
// A check that cannot be owed, or whose ending cannot be recorded, is
// recorded by the orchestrator.
func (h *mediationHook) crashed() {
	o := h.bound("crashed")
	if o == nil {
		return
	}
	at := h.now()
	if err := o.OweMediations(h.ctx, at); err != nil {
		return
	}
	_ = o.NoteDeployEnding(h.ctx, at, wsm.DaemonCrashed)
}

// deathReaped owes the check of a daemon's disorderly death this daemon
// reaped (readPredecessorDeath recorded it): at once, with its rounds, once
// this daemon's serving settled; otherwise at the settle (servingSettled),
// since before it a joining successor's state handle is read-only and the
// check could not be written. A boot and every reap after it go through here.
func (h *mediationHook) deathReaped() {
	h.mu.Lock()
	if !h.serving {
		h.crashOwed = true
		h.mu.Unlock()
		return
	}
	h.mu.Unlock()
	h.crashed()
	h.rounds()
}

// servingSettled is told once this daemon owns everything it serves, at a
// fresh boot's settle or a successor's takeover, and owes the check of every
// death reaped before it (deathReaped), answering whether it owed one. Then
// it deploys again every landing whose deploy never owed its check
// (merge.Orchestrator.DeployUncheckedLandings): the deploy a predecessor was
// asked for died with it. Its rounds are the caller's.
func (h *mediationHook) servingSettled() bool {
	h.mu.Lock()
	h.serving = true
	owed := h.crashOwed
	h.crashOwed = false
	h.mu.Unlock()
	if owed {
		h.crashed()
	}
	if o := h.bound("serving_settled"); o != nil {
		// A read that fails is recorded by the orchestrator.
		_ = o.DeployUncheckedLandings(h.ctx)
	}
	return owed
}

// rounds runs the rounds of every mediation owed, off the caller's
// goroutine: a fresh boot takes up a round a crash cut short.
func (h *mediationHook) rounds() {
	if o := h.bound("rounds"); o != nil {
		inflight.Go(func() { o.Mediate(h.ctx) })
	}
}

// turnEnded settles, off the caller's goroutine, the check a fixer is
// dispatched for when WS, whose turn just ended, is that fixer
// (merge.Orchestrator.FixerTurnEnded). A turn that ends before the merge
// queue is built is no loss and no error: the queue's first pass reads every
// turn's recorded close.
func (h *mediationHook) turnEnded(ws ids.WorkspaceID) {
	o := h.orchestrator()
	if o == nil {
		return
	}
	inflight.Go(func() { o.FixerTurnEnded(h.ctx, ws) })
}

// turnSpent meters, off the caller's goroutine, what WS's turn that just
// ended spent when WS is a deploy mediator
// (merge.Orchestrator.MediatorTurnSpent). A turn that ends before the merge
// queue is built goes unmetered, which is recorded.
func (h *mediationHook) turnSpent(ws ids.WorkspaceID, spent footer.TurnSpent) {
	o := h.orchestrator()
	if o == nil {
		h.log.Info(opMediationHook, "a turn ended before the merge queue was built; its spend is not metered", dlog.Context{"workspace": string(ws), "turn": string(spent.Turn)})
		return
	}
	inflight.Go(func() { o.MediatorTurnSpent(h.ctx, ws, spent) })
}

// ended runs the rounds of every mediation owed, off the caller's goroutine.
// A deploy that began here and changed nothing (UNCHANGED) first releases
// the checks it owed (merge.Orchestrator.ReleaseUnchangedDeploy): there is
// nothing new for an intake to read. A deploy that ended here without
// beginning here is one this daemon took over from its predecessor: it
// never changed nothing, since it moved the daemon, and this successor is
// the one that sees it end, so it records that ending, its start read from
// the check owed (merge.Orchestrator.NoteTakenOverDeployEnding).
func (h *mediationHook) ended(ending deployprogress.Ending) {
	unchanged := ending == deployprogress.EndedUnchanged
	h.mu.Lock()
	owed := h.owed
	h.owed = time.Time{}
	h.mu.Unlock()
	o := h.bound("ended")
	if o == nil {
		return
	}
	takenOver := owed.IsZero() && !unchanged
	if unchanged && owed.IsZero() {
		h.log.Error(opMediationHook, "a deploy that changed nothing ended without having begun here; its check, if any, is left owed", nil)
		unchanged = false
	}
	inflight.Go(func() {
		// THE ENDING IS RECORDED BEFORE THE ROUND, so the intake is told
		// whether the deploy finished or stopped short (owner ruling,
		// 2026-11-17). A deploy this daemon took over began in its
		// predecessor; its ending is recorded here, by the check it owes.
		// An ending that cannot be recorded is recorded by the orchestrator.
		switch {
		case !owed.IsZero():
			_ = o.NoteDeployEnding(h.ctx, owed, wsmEnding(ending))
		case takenOver:
			_ = o.NoteTakenOverDeployEnding(h.ctx, wsmEnding(ending))
		}
		if unchanged {
			// A check that cannot be released is recorded by the orchestrator.
			_ = o.ReleaseUnchangedDeploy(h.ctx, owed)
		}
		o.Mediate(h.ctx)
	})
}

// wsmEnding is ENDING as a deploy's check records it.
func wsmEnding(ending deployprogress.Ending) wsm.DeployEnding {
	switch ending {
	case deployprogress.EndedFinished:
		return wsm.DeployFinished
	case deployprogress.EndedUnchanged:
		return wsm.DeployUnchanged
	case deployprogress.EndedStoppedShort:
		return wsm.DeployStoppedShort
	}
	panic(fmt.Sprintf("agent-repld: a deploy ending the edges never tell: %d", ending))
}
