package merge

import (
	"context"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// This file is a repository's SLOT: the one right to make a queue tree, run
// the gate and move the target that a repository's merges take turns at.
//
// THE PUMP IS THE ONLY GRANTOR. A queue front is admitted into the slot by the
// pump, and so is a parked run that wants it back; nothing else takes it. Two
// candidates therefore never race for it, and the repository's kernel lock
// travels with it: taken when the pump grants the slot, released when its
// holder gives it back.
//
// A RUN GIVES THE SLOT BACK TWO WAYS. It ENDS (landed, failed, abandoned,
// stopped), or it PARKS. A parked merge does not block its repository's queue
// (owner ruling, 2026-09-28): it yields the slot the moment it parks, the
// merges behind it proceed, and once its guidance turn has ended it waits for
// the slot again and makes its merge afresh on the target's new tip.

// tenancy is one grant of a repository's slot to one run. The pump that
// granted it waits on `back` for the run to give the slot up again.
type tenancy struct {
	back chan tenancyEnd
}

// tenancyEnd is how a tenancy ended: the run parked, or it ended with err.
type tenancyEnd struct {
	parked bool
	err    error
}

// newTenancy mints one grant. `back` is buffered so the run that gives the
// slot up never blocks on a pump.
func newTenancy() *tenancy {
	return &tenancy{back: make(chan tenancyEnd, 1)}
}

// releaseSlot gives back the repository slot and the kernel lock r holds, if
// it holds them, and kicks the pump so the next merge is admitted. It is the
// ONE place a slot is released: a run that ends and a run that parks both come
// through here. A run that holds nothing -- a parked run abandoned while it
// waits -- releases nothing.
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

// yieldSlot is the park's half of the release: the slot goes back AND the pump
// that granted it is told the run parked, so it goes on to the next merge
// instead of waiting for this one to end.
func (o *orchestrator) yieldSlot(ctx context.Context, r *run) {
	o.releaseSlot(ctx, r)
	if t := r.takeTenancy(); t != nil {
		t.back <- tenancyEnd{parked: true}
	}
}

// takeTenancy hands back the run's current grant and forgets it, nil when the
// run holds none.
func (r *run) takeTenancy() *tenancy {
	r.o.mu.Lock()
	defer r.o.mu.Unlock()
	t := r.tenancy
	r.tenancy = nil
	return t
}

// reacquire waits for the run's repository slot again, after a park. It asks
// the pump rather than taking the slot itself, so the slot has one grantor.
// A context that ends first -- an abandon, the daemon's exit -- withdraws the
// request; a grant that raced the withdrawal is still owned by this run, and
// its teardown gives it back.
func (r *run) reacquire(ctx context.Context) error {
	const op = "daemon.merge.slot"
	o := r.o
	granted := make(chan struct{}, 1)
	o.mu.Lock()
	r.granted = granted
	o.waiters[r.repo] = append(o.waiters[r.repo], r)
	o.mu.Unlock()
	o.log(ctx, r.ws).Info(op, "a resumed merge waits for its repository's slot, to make its merge afresh on the target's tip", dlog.Context{
		"workspace": string(r.ws), "repo": string(r.repo), "lease": string(r.lease.ID)})
	o.kick(r.repo)
	if o.onWait != nil {
		o.onWait(r.ws)
	}
	select {
	case <-granted:
		o.log(ctx, r.ws).Debug(op, "a resumed merge holds its repository's slot again", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo)})
		return nil
	case <-ctx.Done():
		o.mu.Lock()
		o.waiters[r.repo] = without(o.waiters[r.repo], r)
		o.mu.Unlock()
		return context.Cause(ctx)
	}
}

// without drops one run from a waiter list, keeping the order of the rest.
func without(list []*run, r *run) []*run {
	kept := list[:0]
	for _, w := range list {
		if w != r {
			kept = append(kept, w)
		}
	}
	return kept
}

// grantWaiter hands a free slot to the first parked run waiting for it. It
// answers the run and its grant, or ok=false when the slot is taken, nobody
// waits, the daemon is draining, or another daemon holds the repository.
func (o *orchestrator) grantWaiter(repo wsm.RepoKey) (*run, *tenancy, bool, error) {
	if !o.enterAdmission() {
		return nil, nil, false, nil
	}
	defer o.leaveAdmission()
	o.mu.Lock()
	idle := o.running[repo] == nil && len(o.waiters[repo]) > 0
	o.mu.Unlock()
	if !idle {
		return nil, nil, false, nil
	}
	lock, taken, err := acquireRepoLock(o.lockDir, string(repo))
	if err != nil {
		return nil, nil, false, err
	}
	if !taken {
		o.deps.Log.Global().Warn("daemon.merge.slot", "another daemon holds this repository's merge queue; a resumed merge waits", dlog.Context{"repo": string(repo)})
		return nil, nil, false, nil
	}
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.running[repo] != nil || len(o.waiters[repo]) == 0 {
		// The waiter withdrew while the lock was taken.
		if err := lock.Release(); err != nil {
			o.deps.Log.Global().Error("daemon.merge.slot", "could not release an ungranted queue lock", dlog.Context{"repo": string(repo), "error": err.Error()})
		}
		return nil, nil, false, nil
	}
	r := o.waiters[repo][0]
	o.waiters[repo] = o.waiters[repo][1:]
	t := newTenancy()
	o.running[repo] = r
	r.lock = lock
	r.tenancy = t
	r.granted <- struct{}{}
	return r, t, true, nil
}
