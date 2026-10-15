package merge

import (
	"context"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file tells the roster which workspaces' own branches merge ASYNC: the
// requesters whose roster rows carry the `async_merging` marker beside their
// own status, while their footer stays as it is (owner request, 2026-10-13).
//
// THE MERGE QUEUE IS THE ONE TRUTH. An async merge is in flight exactly while
// a row of its repository's queue carries it: the requester's own row from the
// moment the request is recorded (its source is detached), then the merge
// workspace's row once the request is handed off to it (the workspace's
// MergeOf names the requester), until that row leaves the queue -- landed,
// failed, dropped or taken off. The set is read off the queue whole and
// replaces the roster's whole, so no merge can linger in it after its row is
// gone.
//
// EVERY CHANGE OF A QUEUE'S ROWS REPUBLISHES IT, because the orchestrator's DB
// is wrapped (asyncMergesDB) so that each of the four writes that add or remove
// a row republishes its repository's set once it has committed. No call site
// can forget to: there is no unwrapped path to those writes. The boot's and an
// adoption's republishQueue publish it too, for the queues this daemon did not
// write.

// asyncMergesDB is the orchestrator's DB, with every write that adds a row to
// or removes one from a merge queue followed by that repository's async set.
type asyncMergesDB struct {
	wsm.DB
	o *orchestrator
}

// RequestMerge records a request and republishes its repository's async set.
func (d asyncMergesDB) RequestMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, source wsm.MergeSource, at time.Time) error {
	return d.republished(ctx, repo, d.DB.RequestMerge(ctx, repo, id, source, at))
}

// RequestOneShotMerge records a one-shot request and republishes its
// repository's async set.
func (d asyncMergesDB) RequestOneShotMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, source wsm.MergeSource, at time.Time) error {
	return d.republished(ctx, repo, d.DB.RequestOneShotMerge(ctx, repo, id, source, at))
}

// HandOffDetachedMerge moves a detached request onto its merge workspace and
// republishes its repository's async set.
func (d asyncMergesDB) HandOffDetachedMerge(ctx context.Context, repo wsm.RepoKey, requester wsm.WorkspaceID, request wsm.DetachedMergeID, mergeWS wsm.WorkspaceID, branch, pick string, at time.Time) error {
	return d.republished(ctx, repo, d.DB.HandOffDetachedMerge(ctx, repo, requester, request, mergeWS, branch, pick, at))
}

// RemoveMergeQueueEntry drops a row and republishes its repository's async
// set.
func (d asyncMergesDB) RemoveMergeQueueEntry(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, cause string) error {
	return d.republished(ctx, repo, d.DB.RemoveMergeQueueEntry(ctx, repo, id, cause))
}

// republished answers ERR, the error of a write to REPO's queue, and
// republishes REPO's async set once the write has committed. A write that
// failed changed no row, so it publishes nothing.
func (d asyncMergesDB) republished(ctx context.Context, repo wsm.RepoKey, err error) error {
	if err != nil {
		return err
	}
	d.o.publishAsyncMerges(ctx, repo)
	return nil
}

// asyncMergesPublisher serializes the read and the publication of every async
// set, so a set read before a later write can never be published after the
// set read once that write committed.
type asyncMergesPublisher struct {
	mu sync.Mutex
}

// publishAsyncMerges reads REPO's queue and tells the roster, whole, which
// workspaces' own branches merge async in it. A queue that cannot be read is
// recorded (at ERROR, or at INFO when its context ended under the read) and
// nothing is published: the roster keeps the last set
// it was told until the next change of the queue republishes it.
func (o *orchestrator) publishAsyncMerges(ctx context.Context, repo wsm.RepoKey) {
	const op = "daemon.merge.async_merges"
	o.asyncMerges.mu.Lock()
	defer o.asyncMerges.mu.Unlock()
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, "could not read the merge queue, so the roster is not told which workspaces' branches merge async",
			"the merge queue's read ended when its context was cancelled; the roster is not told which workspaces' branches merge async",
			dlog.Context{"repo": string(repo)}, err)
		return
	}
	requesters, err := o.asyncRequesters(ctx, entries)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, "could not resolve the requester of a merge workspace on the queue, so the roster is not told which workspaces' branches merge async",
			"the requesters' read ended when its context was cancelled; the roster is not told which workspaces' branches merge async",
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	names := make([]string, 0, len(requesters))
	for _, ws := range requesters {
		names = append(names, string(ws))
	}
	o.deps.Log.Global().Debug(op, "told the roster which workspaces' branches merge async",
		dlog.Context{"repo": string(repo), "entries": len(entries), "requesters": names})
	o.deps.Sidebar.SetAsyncMerges(repo, requesters)
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
