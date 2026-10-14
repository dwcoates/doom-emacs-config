package merge

import (
	"context"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// This file is a repository's SLOT and its KERNEL LOCK.
//
// The slot is the one right to move the target: the HEAD holds it. THE PUMP IS
// THE ONLY GRANTOR. A queue front is admitted into the slot, or a speculative
// run promoted into it, by the pump, and nothing else takes it, so two
// candidates never race for it.
//
// THE KERNEL LOCK IS THE REPOSITORY'S, held while any run of its chain stands
// in this process: taken when the repository's first run is admitted, and
// released only once its chain is empty again, so the head and every merge
// rebasing and testing behind it run under the one lock another daemon cannot
// take. NOTHING PARKS, so a run leaves its chain exactly once: when it ENDS
// (landed, failed, abandoned, stopped).

// holdRepoLock answers the repository's kernel lock: the one this process
// already holds for the repository's chain, or a fresh one. The bool is false
// when another daemon holds it.
func (o *orchestrator) holdRepoLock(repo wsm.RepoKey) (bool, error) {
	o.mu.Lock()
	_, held := o.repoLocks[repo]
	o.mu.Unlock()
	if held {
		return true, nil
	}
	lock, taken, err := acquireRepoLock(o.lockDir, string(repo))
	if err != nil || !taken {
		return false, err
	}
	o.mu.Lock()
	o.repoLocks[repo] = lock
	o.mu.Unlock()
	return true, nil
}

// releaseRepoLockIfIdle gives back the repository's kernel lock when no run of
// its chain stands and nothing holds its slot.
func (o *orchestrator) releaseRepoLockIfIdle(ctx context.Context, repo wsm.RepoKey) {
	o.mu.Lock()
	lock := o.repoLocks[repo]
	if lock == nil || len(o.chains[repo]) > 0 || o.running[repo] != nil {
		o.mu.Unlock()
		return
	}
	delete(o.repoLocks, repo)
	o.mu.Unlock()
	if err := lock.Release(); err != nil {
		o.deps.Log.Global().Error("daemon.merge.slot", "could not release the repository's queue lock", dlog.Context{
			"repo": string(repo), "error": err.Error()})
	}
}

// releaseSlot takes r out of its repository's chain and slot, gives back the
// repository's kernel lock when nothing of the repository runs any more, and
// kicks the pump so the next merge is admitted or promoted. It is the ONE
// place a run leaves its chain, and it acts once.
func (o *orchestrator) releaseSlot(ctx context.Context, r *run) {
	// A RUN LEAVING ITS CHAIN GIVES BACK ITS TEST TURN, or its place in line
	// for it (testturn.go), here with everything else it holds.
	o.releaseTestTurn(ctx, r, "left its chain")
	o.mu.Lock()
	if !r.joined {
		o.mu.Unlock()
		return
	}
	r.joined = false
	chain := o.chains[r.repo]
	for i, member := range chain {
		if member == r {
			chain = append(chain[:i:i], chain[i+1:]...)
			break
		}
	}
	if len(chain) == 0 {
		delete(o.chains, r.repo)
	} else {
		o.chains[r.repo] = chain
	}
	if o.running[r.repo] == r {
		delete(o.running, r.repo)
	}
	// EVERY MERGE BEHIND IT IS TOLD: its base is now the tip of the run ahead
	// of it, or the target's, and it restacks onto that.
	o.signalChainLocked(r.repo)
	o.mu.Unlock()
	o.releaseRepoLockIfIdle(ctx, r.repo)
	o.kick(r.repo)
}
