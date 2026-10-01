package merge

import (
	"context"

	"claude-repld/internal/dlog"
)

// This file is a repository's SLOT: the one right to rebase, run the gate and
// move the target that a repository's merges take turns at.
//
// THE PUMP IS THE ONLY GRANTOR. A queue front is admitted into the slot by the
// pump, and nothing else takes it, so two candidates never race for it; the
// repository's kernel lock travels with it, taken when the pump grants the slot
// and released when its holder gives it back. NOTHING PARKS, so a run gives
// the slot back exactly once: when it ENDS (landed, failed, abandoned,
// stopped).

// releaseSlot gives back the repository slot and the kernel lock r holds, if
// it holds them, and kicks the pump so the next merge is admitted. It is the
// ONE place a slot is released.
func (o *orchestrator) releaseSlot(ctx context.Context, r *run) {
	o.mu.Lock()
	lock := r.lock
	r.lock = nil
	if o.running[r.repo] == r {
		delete(o.running, r.repo)
	}
	o.mu.Unlock()
	if lock == nil {
		return
	}
	if err := lock.Release(); err != nil {
		o.log(ctx, r.ws).Error("daemon.merge.slot", "could not release the repository's queue lock", dlog.Context{
			"repo": string(r.repo), "workspace": string(r.ws), "error": err.Error()})
	}
	o.kick(r.repo)
}
