package main

import (
	"context"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
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
	if o := h.bound("began"); o != nil {
		// A mediation that cannot be owed is recorded by the orchestrator.
		_ = o.OweMediations(h.ctx, h.now())
	}
}

// ended runs the rounds of every mediation owed, off the caller's goroutine.
func (h *mediationHook) ended() {
	if o := h.bound("ended"); o != nil {
		go o.Mediate(h.ctx)
	}
}
