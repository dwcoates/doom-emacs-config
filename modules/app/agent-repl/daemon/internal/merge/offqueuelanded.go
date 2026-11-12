package merge

import (
	"context"
	"sync"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// This file decides which merges off their queues are SHOWN (owner request,
// 2026-11-12: "if any (not necessarily all) of the workspace's commits made
// it into master, we should not display it"). A merge whose branch is gone,
// or any of whose commits master holds, by ancestry or as a patch-equivalent
// commit (`git cherry`'s "-"), is left out of the roster's off-queue rows and
// out of `merge-queue status`. Its record is KEPT: the owner asked to hide
// it, not to discard it, and a requeue still finds it by name.
//
// THE JUDGEMENT IS GIT'S, AND NO RENDER WAITS ON IT. It is made off every
// lock (judgeOffQueue), cached per (branch head, master head), and read from
// the cache by the roster's publish, which never shells out. It is made again
// at boot, after every landing (master moved), whenever a record has no
// judgement yet, and on every status read; a judgement whose two heads are
// unchanged is reused without asking git anything further.

// offQueueLanded is the cache of judgements, by the merge's workspace.
type offQueueLanded struct {
	mu    sync.Mutex
	facts map[ids.WorkspaceID]landedFact
	// running and again coalesce kicks: one judgement runs at a time, and a
	// kick during it runs one more after it.
	running, again bool
}

// landedFact is one merge's judgement and the two heads it was made at.
type landedFact struct {
	branchHead, masterHead string
	// hidden reports that the branch is gone, or master holds some of its
	// work.
	hidden bool
}

// offQueueHidden reports whether the merge of WS is left out; a merge not
// judged yet is shown.
func (o *orchestrator) offQueueHidden(ws ids.WorkspaceID) bool {
	o.offQueueLanded.mu.Lock()
	defer o.offQueueLanded.mu.Unlock()
	return o.offQueueLanded.facts[ws].hidden
}

// offQueueJudged reports whether every one of RECORDS has a judgement.
func (o *orchestrator) offQueueJudged(records []wsm.OffQueueMerge) bool {
	o.offQueueLanded.mu.Lock()
	defer o.offQueueLanded.mu.Unlock()
	for _, record := range records {
		if _, ok := o.offQueueLanded.facts[record.Workspace]; !ok {
			return false
		}
	}
	return true
}

// kickOffQueueLanded runs refreshOffQueueLanded on its own goroutine, once at
// a time, coalescing the kicks that arrive while it runs into one more run.
// A daemon that runs nothing in the background (a test's) runs nothing.
func (o *orchestrator) kickOffQueueLanded() {
	if !o.async {
		return
	}
	o.offQueueLanded.mu.Lock()
	if o.offQueueLanded.running {
		o.offQueueLanded.again = true
		o.offQueueLanded.mu.Unlock()
		return
	}
	o.offQueueLanded.running = true
	o.offQueueLanded.mu.Unlock()
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		for {
			o.refreshOffQueueLanded(context.Background())
			o.offQueueLanded.mu.Lock()
			if !o.offQueueLanded.again {
				o.offQueueLanded.running = false
				o.offQueueLanded.mu.Unlock()
				return
			}
			o.offQueueLanded.again = false
			o.offQueueLanded.mu.Unlock()
		}
	})
}

// refreshOffQueueLanded judges every merge off its queue and republishes the
// roster's off-queue rows when a merge's visibility changed.
func (o *orchestrator) refreshOffQueueLanded(ctx context.Context) {
	if o.judgeOffQueue(ctx) {
		o.publishOffQueue(ctx)
	}
}

// judgeOffQueue judges every merge off its queue, OFF EVERY LOCK but the
// cache's own, held only to read and write a judgement, and answers whether
// any merge's visibility changed. A merge whose judgement cannot be made is
// recorded at ERROR and keeps the judgement it had (shown when it had none).
func (o *orchestrator) judgeOffQueue(ctx context.Context) bool {
	const op = "daemon.merge.off_queue_landed"
	records, err := o.deps.DB.OffQueueMerges(ctx)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the merges off their queues to judge whether their work landed", dlog.WithCause(nil, err))
		return false
	}
	changed := false
	standing := map[ids.WorkspaceID]bool{}
	for _, record := range records {
		standing[record.Workspace] = true
		fact, err := o.judgeOne(ctx, record)
		if err != nil {
			o.log(ctx, record.Workspace).Error(op, "could not judge whether a merge off its queue has work on master; its row is drawn as it was",
				dlog.WithCause(dlog.Context{"workspace": string(record.Workspace)}, err))
			continue
		}
		o.offQueueLanded.mu.Lock()
		before, had := o.offQueueLanded.facts[record.Workspace]
		if o.offQueueLanded.facts == nil {
			o.offQueueLanded.facts = map[ids.WorkspaceID]landedFact{}
		}
		o.offQueueLanded.facts[record.Workspace] = fact
		o.offQueueLanded.mu.Unlock()
		// A MERGE NOT JUDGED BEFORE WAS SHOWN.
		if (had && before.hidden) != fact.hidden {
			changed = true
		}
	}
	// A JUDGEMENT OUTLIVES NO RECORD: a requeued, discarded or closed merge's
	// is dropped, so a later record of the same workspace is judged afresh.
	o.offQueueLanded.mu.Lock()
	for ws := range o.offQueueLanded.facts {
		if !standing[ws] {
			delete(o.offQueueLanded.facts, ws)
		}
	}
	o.offQueueLanded.mu.Unlock()
	return changed
}

// judgeOne judges RECORD: hidden when its branch is gone, its head is master's
// ancestor, or master holds a patch-equivalent of one of its commits. Two
// heads equal to the cached judgement's reuse it without asking further.
func (o *orchestrator) judgeOne(ctx context.Context, record wsm.OffQueueMerge) (landedFact, error) {
	ws, err := o.deps.DB.Workspace(ctx, record.Workspace)
	if err != nil {
		return landedFact{}, err
	}
	main, err := o.deps.Git.MainWorktree(ctx, ws.Dir)
	if err != nil {
		return landedFact{}, err
	}
	master, err := o.deps.Git.DefaultBranch(ctx, main)
	if err != nil {
		return landedFact{}, err
	}
	masterHead, err := o.deps.Git.ResolveRef(ctx, main, "refs/heads/"+master)
	if err != nil {
		return landedFact{}, err
	}
	branch := requestBranch(ws, record.Source)
	exists, err := o.deps.Git.BranchExists(ctx, main, branch)
	if err != nil {
		return landedFact{}, err
	}
	if !exists {
		return landedFact{masterHead: masterHead, hidden: true}, nil
	}
	branchHead, err := o.deps.Git.ResolveRef(ctx, main, "refs/heads/"+branch)
	if err != nil {
		return landedFact{}, err
	}
	o.offQueueLanded.mu.Lock()
	cached, had := o.offQueueLanded.facts[record.Workspace]
	o.offQueueLanded.mu.Unlock()
	if had && cached.branchHead == branchHead && cached.masterHead == masterHead {
		return cached, nil
	}
	fact := landedFact{branchHead: branchHead, masterHead: masterHead}
	ancestor, err := o.deps.Git.IsAncestor(ctx, main, branchHead, masterHead)
	if err != nil {
		return landedFact{}, err
	}
	if ancestor {
		fact.hidden = true
		return fact, nil
	}
	if fact.hidden, err = o.deps.Git.AnyPatchEquivalent(ctx, main, masterHead, branchHead); err != nil {
		return landedFact{}, err
	}
	return fact, nil
}
