package merge

import (
	"context"
	"fmt"
	"sync"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// This file tells the roster each repository's merge queue as the sidebar
// draws it (sidebar.MergeQueue): whether the queue is paused, the workspaces
// with a row on it, the merges in line front first (which a repository
// section showing its merge queue lists), and the requesters whose own
// branches merge ASYNC on it.
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
// EVERY CHANGE OF A QUEUE'S ROWS, OF ITS LINE OR OF ITS PAUSE REPUBLISHES IT,
// because the orchestrator's DB is wrapped (rosterQueueDB) so that each write
// that adds or removes a row, puts a requested merge in line, reorders the
// line or changes the pause republishes its repository's queue once it has
// committed. No call site can forget to: there is no
// unwrapped path to those writes. The boot's and an adoption's republishQueue
// publish it too, for the queues this daemon did not write.

// rosterQueueDB is the orchestrator's DB, with every write that adds a row to
// or removes one from a merge queue, changes its line, or pauses or resumes
// one, followed by that repository's roster queue. AdmitMerge is not
// wrapped: starting the head merge changes neither the rows nor the line,
// and the row's own merge facts carry it to the roster.
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

// QueueMerge puts a requested merge in line and republishes its repository's
// queue.
func (d rosterQueueDB) QueueMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, ledger wsm.LeaseID) (int, error) {
	place, err := d.DB.QueueMerge(ctx, repo, id, ledger)
	return place, d.republished(ctx, repo, err)
}

// DropAdmittedMerge drops an admitted merge and republishes its repository's
// queue.
func (d rosterQueueDB) DropAdmittedMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, lease wsm.LeaseID, cause string) error {
	return d.republished(ctx, repo, d.DB.DropAdmittedMerge(ctx, repo, id, lease, cause))
}

// MoveMerge moves a waiting merge and republishes its repository's queue when
// it moved.
func (d rosterQueueDB) MoveMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, move wsm.MergeMove) (bool, error) {
	moved, err := d.DB.MoveMerge(ctx, repo, id, move)
	return moved, d.republishedIf(ctx, repo, moved, err)
}

// MoveMergeTo moves a waiting merge to a place beside another and
// republishes its repository's queue when it moved.
func (d rosterQueueDB) MoveMergeTo(ctx context.Context, repo wsm.RepoKey, id, anchor wsm.WorkspaceID, side wsm.MergeSide) (bool, error) {
	moved, err := d.DB.MoveMergeTo(ctx, repo, id, anchor, side)
	return moved, d.republishedIf(ctx, repo, moved, err)
}

// PrioritizeMerge moves a waiting merge to the front of the waiting and
// republishes its repository's queue when it moved.
func (d rosterQueueDB) PrioritizeMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID) (bool, error) {
	moved, err := d.DB.PrioritizeMerge(ctx, repo, id)
	return moved, d.republishedIf(ctx, repo, moved, err)
}

// YieldMergeHead gives the head's place to the race's winner and republishes
// its repository's queue.
func (d rosterQueueDB) YieldMergeHead(ctx context.Context, repo wsm.RepoKey, loser, winner wsm.WorkspaceID) error {
	return d.republished(ctx, repo, d.DB.YieldMergeHead(ctx, repo, loser, winner))
}

// SetMergeQueuePaused pauses or resumes a queue and republishes it, so the
// roster's merge indicators follow the pause at once.
func (d rosterQueueDB) SetMergeQueuePaused(ctx context.Context, repo wsm.RepoKey, pause *wsm.MergeQueuePause) error {
	return d.republished(ctx, repo, d.DB.SetMergeQueuePaused(ctx, repo, pause))
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

// republishedIf is republished for a write that may change nothing: it
// republishes only when CHANGED says the write moved something.
func (d rosterQueueDB) republishedIf(ctx context.Context, repo wsm.RepoKey, changed bool, err error) error {
	if err != nil || !changed {
		return err
	}
	return d.republished(ctx, repo, nil)
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
	pause, err := o.deps.DB.MergeQueuePause(ctx, repo)
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
	queue := sidebar.MergeQueue{Pause: pause, AsyncRequesters: requesters}
	for _, entry := range entries {
		queue.Workspaces = append(queue.Workspaces, entry.Workspace)
	}
	line, err := o.rosterLine(ctx, inLine(entries))
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, "could not resolve the merges in line as the roster draws them, so the roster is not told the queue",
			"the line's read ended when its context was cancelled; the roster is not told the queue",
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	queue.Line = line
	fields := dlog.Context{"repo": string(repo), "entries": len(entries), "line": ids.Strings(queue.LineWorkspaces()), "paused": pause != nil, "requesters": ids.Strings(requesters)}
	o.deps.Log.Global().Debug(op, "told the roster the merge queue", fields)
	o.deps.Sidebar.SetMergeQueue(repo, queue)
}

// asyncRequesters answers, in queue order, the workspace whose own branch each
// async merge on ENTRIES merges: a detached request's own workspace, and a
// merge workspace's requester.
func (o *orchestrator) asyncRequesters(ctx context.Context, entries []wsm.MergeQueueEntry) ([]ids.WorkspaceID, error) {
	var out []ids.WorkspaceID
	for _, entry := range entries {
		credited, async, err := o.creditOf(ctx, entry.Workspace, entry.Source.Detach != "")
		if err != nil {
			return nil, err
		}
		if async {
			out = append(out, credited)
		}
	}
	return out, nil
}

// creditOf answers the workspace the roster credits the merge running in WS
// to, and whether that merge is ASYNC: a detached request (DETACHED) is
// credited to its own workspace, a merge workspace to its requester, and
// every other merge to WS itself. It is the one place the roster's credit is
// decided, so the async-merging marker and the merge queue view never
// disagree on who a merge is drawn as.
func (o *orchestrator) creditOf(ctx context.Context, ws ids.WorkspaceID, detached bool) (ids.WorkspaceID, bool, error) {
	if detached {
		return ws, true, nil
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return "", false, err
	}
	if record.MergeOf != nil {
		return record.MergeOf.Requester, true, nil
	}
	return ws, false, nil
}

// rosterLine answers the merges in line as the roster draws them, front
// first: each one's credit and the bubble a click on it opens, and the front's
// stages read from its ledger.
func (o *orchestrator) rosterLine(ctx context.Context, line []wsm.MergeQueueEntry) ([]sidebar.QueueEntry, error) {
	out := make([]sidebar.QueueEntry, 0, len(line))
	for i, entry := range line {
		credited, async, err := o.creditOf(ctx, entry.Workspace, entry.Source.Detach != "")
		if err != nil {
			return nil, err
		}
		drawn := sidebar.QueueEntry{
			Workspace:     entry.Workspace,
			Credited:      credited,
			Async:         async,
			QueuedAt:      entry.EnqueuedAt,
			PlacedAtFront: entry.Source.Front,
			Admitted:      entry.State != wsm.MergeQueued,
		}
		if lease, ok := o.leaseOf(entry.Workspace); ok {
			drawn.Reveal = o.bubbleHeadID(entry.Workspace, lease)
			if i == 0 {
				stages, err := o.ledgerStages(ctx, entry.Workspace, lease)
				if err != nil {
					return nil, err
				}
				drawn.Stages = stages
			}
		}
		out = append(out, drawn)
	}
	return out, nil
}

// bubbleHeadID is the row id of the head of the merge bubble the merge
// running in WS draws under LEASE, on the root feed of the workspace the
// bubble is drawn in (bubbleOf): what a click on the merge in the roster's
// queue reveals.
func (o *orchestrator) bubbleHeadID(ws ids.WorkspaceID, lease ids.LeaseID) *frontendv1.FeedId {
	return feedid.Encode(headRef(o.bubbleOf(ws).ws, lease))
}

// ledgerStages answers the stages of the merge running in WS under LEASE, read
// from its ledger (stagesOf). A ledger not opened yet has no stages.
func (o *orchestrator) ledgerStages(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID) ([]*frontendv1.RosterMergeQueueStage, error) {
	ledgers, err := o.deps.DB.MergeLedger(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("merge: read the ledger of %s: %w", ws, err)
	}
	for _, ledger := range ledgers {
		if ledger.Lease == lease {
			return stagesOf(ledger.Intervals)
		}
	}
	return nil, nil
}
