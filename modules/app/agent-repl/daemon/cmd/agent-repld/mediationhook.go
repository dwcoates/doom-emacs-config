package main

import (
	"context"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/merge"
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
}

func (h *mediationHook) bind(o merge.Orchestrator) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.o = o
}

func (h *mediationHook) bound(edge string) merge.Orchestrator {
	h.mu.RLock()
	defer h.mu.RUnlock()
	if h.o == nil {
		h.log.Error(opMediationHook, "a deploy edge came before the merge queue was built; no mediation follows it", dlog.Context{"edge": edge})
	}
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

// rounds runs the rounds of every mediation owed, off the caller's
// goroutine: a fresh boot takes up a round a crash cut short.
func (h *mediationHook) rounds() {
	if o := h.bound("rounds"); o != nil {
		inflight.Go(func() { o.Mediate(h.ctx) })
	}
}

// ended runs the rounds of every mediation owed, off the caller's goroutine.
// A deploy that began here and changed nothing (UNCHANGED) first releases
// the checks it owed (merge.Orchestrator.ReleaseUnchangedDeploy): there is
// nothing new for an intake to read. A deploy that ended here without
// beginning here (one a successor took over) never changed nothing: it
// moved the daemon.
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
	if unchanged && owed.IsZero() {
		h.log.Error(opMediationHook, "a deploy that changed nothing ended without having begun here; its check, if any, is left owed", nil)
		unchanged = false
	}
	inflight.Go(func() {
		// THE ENDING IS RECORDED BEFORE THE ROUND, so the intake is told
		// whether the deploy finished or stopped short (owner ruling,
		// 2026-11-17). A deploy that ended here without beginning here has
		// no start to name its check by; its ending is the successor's.
		if !owed.IsZero() {
			// An ending that cannot be recorded is recorded by the orchestrator.
			_ = o.NoteDeployEnding(h.ctx, owed, wsmEnding(ending))
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
