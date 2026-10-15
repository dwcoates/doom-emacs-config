package merge

import (
	"context"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// This file tells the roster each repository's merge queue as the sidebar
// draws it (sidebar.MergeQueue): whether the queue is paused, the workspaces
// with a row on it, and the requesters whose own branches merge ASYNC on it.
// An async requester's row carries the `async_merging` marker beside its own
// status while its footer stays as it is (owner request, 2026-10-13), and a
// row whose merge is on a paused queue carries the `merge_paused` marker
// beside its merge indicator (owner ruling, 2026-10-14).
//
// THE MERGE QUEUE IS THE ONE TRUTH. An async merge is in flight exactly while
// a row of its repository's queue carries it: the requester's own row from the
// moment the request is recorded (its source is detached), then the merge
// workspace's row once the request is handed off to it (the workspace's
// MergeOf names the requester), until that row leaves the queue -- landed,
// failed, dropped or taken off. The queue is read whole and replaces the
// roster's whole, so no merge or pause can linger in it after the queue
// stopped saying so.
//
// EVERY CHANGE OF A QUEUE'S ROWS OR OF ITS PAUSE REPUBLISHES IT, because the
// orchestrator's DB is wrapped (rosterQueueDB) so that each of the five writes
// that add or remove a row or change the pause republishes its repository's
// queue once it has committed. No call site can forget to: there is no
// unwrapped path to those writes. The boot's and an adoption's republishQueue
// publish it too, for the queues this daemon did not write.

// rosterQueueDB is the orchestrator's DB, with every write that adds a row to
// or removes one from a merge queue, or pauses or resumes one, followed by
// that repository's roster queue.
type rosterQueueDB struct {
	wsm.DB
	o *orchestrator
}

// RequestMerge records a request and republishes its repository's queue.
func (d rosterQueueDB) RequestMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, source wsm.MergeSource, at time.Time) error {
	return d.republished(ctx, repo, d.DB.RequestMerge(ctx, repo, id, source, at))
}

// RequestOneShotMerge records a one-shot request and republishes its
// repository's queue.
func (d rosterQueueDB) RequestOneShotMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, source wsm.MergeSource, at time.Time) error {
	return d.republished(ctx, repo, d.DB.RequestOneShotMerge(ctx, repo, id, source, at))
}

// HandOffDetachedMerge moves a detached request onto its merge workspace and
// republishes its repository's queue.
func (d rosterQueueDB) HandOffDetachedMerge(ctx context.Context, repo wsm.RepoKey, requester wsm.WorkspaceID, request wsm.DetachedMergeID, mergeWS wsm.WorkspaceID, branch, pick string, at time.Time) error {
	return d.republished(ctx, repo, d.DB.HandOffDetachedMerge(ctx, repo, requester, request, mergeWS, branch, pick, at))
}

// RemoveMergeQueueEntry drops a row and republishes its repository's queue.
func (d rosterQueueDB) RemoveMergeQueueEntry(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, cause string) error {
	return d.republished(ctx, repo, d.DB.RemoveMergeQueueEntry(ctx, repo, id, cause))
}

// SetMergeQueuePaused pauses or resumes a queue and republishes it, so the
// roster's merge indicators follow the pause at once.
func (d rosterQueueDB) SetMergeQueuePaused(ctx context.Context, repo wsm.RepoKey, paused bool) error {
	return d.republished(ctx, repo, d.DB.SetMergeQueuePaused(ctx, repo, paused))
}

// republished answers ERR, the error of a write to REPO's queue, and
// republishes REPO's queue once the write has committed. A write that failed
// changed nothing, so it publishes nothing.
func (d rosterQueueDB) republished(ctx context.Context, repo wsm.RepoKey, err error) error {
	if err != nil {
		return err
	}
	d.o.publishRosterQueue(ctx, repo)
	return nil
}

// rosterQueuePublisher serializes the read and the publication of every
// roster queue, so a queue read before a later write can never be published
// after the queue read once that write committed.
type rosterQueuePublisher struct {
	mu sync.Mutex
}

// publishRosterQueue reads REPO's queue and its pause and tells the roster
// both, whole. A queue that cannot be read is recorded (at ERROR, or at INFO
// when its context ended under the read) and nothing is published: the roster
// keeps the last queue it was told until the next change of the queue
// republishes it.
func (o *orchestrator) publishRosterQueue(ctx context.Context, repo wsm.RepoKey) {
	const op = "daemon.merge.roster_queue"
	o.rosterQueue.mu.Lock()
	defer o.rosterQueue.mu.Unlock()
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, "could not read the merge queue, so the roster is not told the queue",
			"the merge queue's read ended when its context was cancelled; the roster is not told the queue",
			dlog.Context{"repo": string(repo)}, err)
		return
	}
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, "could not read whether the merge queue is paused, so the roster is not told the queue",
			"the pause's read ended when its context was cancelled; the roster is not told the queue",
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	requesters, err := o.asyncRequesters(ctx, entries)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, "could not resolve the requester of a merge workspace on the queue, so the roster is not told the queue",
			"the requesters' read ended when its context was cancelled; the roster is not told the queue",
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	queue := sidebar.MergeQueue{Paused: paused, AsyncRequesters: requesters}
	for _, entry := range entries {
		queue.Workspaces = append(queue.Workspaces, entry.Workspace)
	}
	o.deps.Log.Global().Debug(op, "told the roster the merge queue",
		dlog.Context{"repo": string(repo), "entries": len(entries), "paused": paused, "requesters": ids.Strings(requesters)})
	o.deps.Sidebar.SetMergeQueue(repo, queue)
}

// asyncRequesters answers, in queue order, the workspace whose own branch each
// async merge on ENTRIES merges: a detached request's own workspace, and a
// merge workspace's requester.
func (o *orchestrator) asyncRequesters(ctx context.Context, entries []wsm.MergeQueueEntry) ([]ids.WorkspaceID, error) {
	var out []ids.WorkspaceID
	for _, entry := range entries {
		if entry.Source.Detach != "" {
			out = append(out, entry.Workspace)
			continue
		}
		record, err := o.deps.DB.Workspace(ctx, entry.Workspace)
		if err != nil {
			return nil, err
		}
		if record.MergeOf != nil {
			out = append(out, record.MergeOf.Requester)
		}
	}
	return out, nil
}
