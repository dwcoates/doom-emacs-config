package merge

import (
	"context"

	"agent-repld/internal/dlog"
)

// This file is the MAIN SYNC'S HOLD on a repository's target: the daemon's
// fast-forward of the main checkout to its upstream (internal/mainsync) and a
// merge landing in the same checkout must never run at once, since both move
// the one branch in the one worktree.
//
// THE QUEUE IS THE ONE WRITER OF THE TARGET, so the sync asks it. The hold is
// marked under mu FIRST, and only then is the durable queue read: a merge
// whose admission began before the mark has its entry in the queue already
// (an entry is enqueued before it is admitted), so the read finds it and the
// hold is refused; a merge admitted after the mark sees it and waits
// (admitFront). Neither order lets the two run together.

// HoldTargetForSync implements Orchestrator.
func (o *orchestrator) HoldTargetForSync(ctx context.Context, dir string) (func(), bool, error) {
	const op = "daemon.merge.sync_hold"
	repo, err := o.repoKeyFor(ctx, dir)
	if err != nil {
		return nil, false, err
	}
	o.mu.Lock()
	if o.running[repo] != nil || len(o.chains[repo]) > 0 || o.syncing[repo] {
		o.mu.Unlock()
		return nil, false, nil
	}
	o.syncing[repo] = true
	o.mu.Unlock()
	unmark := func() {
		o.mu.Lock()
		delete(o.syncing, repo)
		o.mu.Unlock()
		o.kick(repo)
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		unmark()
		return nil, false, err
	}
	if len(inLine(entries)) > 0 {
		unmark()
		return nil, false, nil
	}
	o.deps.Log.Global().Debug(op, "a main sync holds the repository's target; no merge is admitted until it releases", dlog.Context{"repo": string(repo)})
	return unmark, true, nil
}
