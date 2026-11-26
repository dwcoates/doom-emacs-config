package merge

import (
	"context"
	"fmt"
	"slices"
	"sync"
	"time"

	"agentrepl/logging/logsite"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
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
// its repository's queue carries it: as an async request from the moment the
// request is recorded (wsm.AsyncMergeRequest), then as the merge workspace's
// row once the request is handed off to it (the workspace's MergeOf names the
// requester), until that row leaves the queue -- landed, failed, dropped or
// taken off. The queue is read whole and replaces the
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
func (d rosterQueueDB) MoveMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, move wsm.MergeMove) (wsm.MergeMoveOutcome, error) {
	outcome, err := d.DB.MoveMerge(ctx, repo, id, move)
	return outcome, d.republishedIf(ctx, repo, outcome.Moved(), err)
}

// MoveMergeTo moves a waiting merge to a place beside another and
// republishes its repository's queue when it moved.
func (d rosterQueueDB) MoveMergeTo(ctx context.Context, repo wsm.RepoKey, id, anchor wsm.WorkspaceID, side wsm.MergeSide) (bool, error) {
	moved, err := d.DB.MoveMergeTo(ctx, repo, id, anchor, side)
	return moved, d.republishedIf(ctx, repo, moved, err)
}

// PlaceMerge moves a waiting merge to a place in its queue and republishes
// its repository's queue when it moved.
func (d rosterQueueDB) PlaceMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID, place wsm.MergePlace) (bool, error) {
	moved, err := d.DB.PlaceMerge(ctx, repo, id, place)
	return moved, d.republishedIf(ctx, repo, moved, err)
}

// PrioritizeMerge moves a waiting merge to the front of the waiting and
// republishes its repository's queue when it moved.
func (d rosterQueueDB) PrioritizeMerge(ctx context.Context, repo wsm.RepoKey, id wsm.WorkspaceID) (bool, error) {
	moved, err := d.DB.PrioritizeMerge(ctx, repo, id)
	return moved, d.republishedIf(ctx, repo, moved, err)
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
	// orders is the order each repository's queue was last published in
	// (publishedOrder), so a change of it is recorded once, at INFO.
	orders map[wsm.RepoKey]publishedOrder
}

// publishedOrder is a queue's order as the roster is told it: the merges in
// line, head first, and the merges out of line, in the order they stepped
// aside. The roster numbers the waiting merges by their place in Line.
type publishedOrder struct {
	line, remediating []string
}

// noteOrder records, at INFO, REPO's queue order as QUEUE publishes it when
// it differs from the order last published, so which order the sidebar drew
// is read from the logs (2026-11-20: the owner saw a waiting merge numbered
// first that the queue did not run next, and no record said what the
// sidebar was told). The caller holds the publisher's lock.
func (o *orchestrator) noteOrder(repo wsm.RepoKey, queue sidebar.MergeQueue) {
	now := publishedOrder{line: ids.Strings(queue.LineWorkspaces())}
	for _, entry := range queue.Remediating {
		now.remediating = append(now.remediating, string(entry.Workspace))
	}
	before, published := o.rosterQueue.orders[repo]
	if published && slices.Equal(before.line, now.line) && slices.Equal(before.remediating, now.remediating) {
		return
	}
	if o.rosterQueue.orders == nil {
		o.rosterQueue.orders = map[wsm.RepoKey]publishedOrder{}
	}
	o.rosterQueue.orders[repo] = now
	o.deps.Log.Global().Info("daemon.merge.roster_queue", "the roster was told a new merge queue order: the line head first, the waiting merges numbered by their place after it",
		dlog.Context{"repo": string(repo), "line": now.line, "remediating": now.remediating})
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
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read the merge queue, so the roster is not told the queue"),
			logsite.Msg("the merge queue's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo)}, err)
		return
	}
	pause, err := o.deps.DB.MergeQueuePause(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read whether the merge queue is paused, so the roster is not told the queue"),
			logsite.Msg("the pause's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	// THE DEPLOY CHECK'S HOLD IS DRAWN FROM THE ONE READ THE HOLD ITSELF
	// TAKES (deployHoldOf), so the paused drawing and the hold never disagree.
	hold, err := o.deployHoldOf(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read whether the deploy check holds the merge queue, so the roster is not told the queue"),
			logsite.Msg("the deploy check hold's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	queue := sidebar.MergeQueue{Pause: pause, DeployCheck: rosterDeployCheck(hold)}
	for _, entry := range entries {
		queue.Workspaces = append(queue.Workspaces, entry.Workspace)
	}
	// THE QUEUE'S HOLD IS AN INPUT OF THE VIEW (viewOf): read once, it draws
	// the repository's status and every stage of the head alike.
	queueHeld, err := o.queueHeld(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read whether the merge queue is held, so the roster is not told the queue"),
			logsite.Msg("the hold's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	heldRounds, err := o.deps.DB.HeldMergeRounds(ctx)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read which merges are held, so the roster is not told the queue"),
			logsite.Msg("the holds' read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	held := make(map[wsm.LeaseID]int64, len(heldRounds))
	for lease, at := range heldRounds {
		held[lease] = at.UnixMilli()
	}
	o.republishBubbles(ctx, entries, queueHeld, held)
	line, err := o.rosterLine(ctx, inLine(entries), true, queueHeld)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not resolve the merges in line as the roster draws them, so the roster is not told the queue"),
			logsite.Msg("the line's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	queue.Line = line
	remediating, err := o.rosterLine(ctx, outOfLine(entries), false, queueHeld)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not resolve the merges remediating out of line as the roster draws them, so the roster is not told the queue"),
			logsite.Msg("the remediating merges' read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	queue.Remediating = remediating
	mediation, err := o.rosterMediation(ctx, repo)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read the deploy mediation the queue waits on, so the roster is not told the queue"),
			logsite.Msg("the mediation's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	queue.Mediation = mediation
	deploying, err := o.deps.DB.DeployingMergeOf(ctx, repo)
	if err == nil && deploying != nil {
		queue.Deploying, err = o.rosterDeploying(ctx, *deploying)
	}
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read the landed merge still deploying, so the roster is not told the queue"),
			logsite.Msg("the deploying merge's read ended when its context was cancelled; the roster is not told the queue"),
			dlog.Context{"repo": string(repo), "entries": len(entries)}, err)
		return
	}
	fields := dlog.Context{"repo": string(repo), "entries": len(entries), "line": ids.Strings(queue.LineWorkspaces()), "paused": pause != nil, "held_by": hold.heldBy()}
	o.deps.Log.Global().Debug(op, "told the roster the merge queue", fields)
	o.noteOrder(repo, queue)
	o.deps.Sidebar.SetMergeQueue(repo, queue)
	// EVERY QUEUE WRITE MAY MOVE A MERGE OFF ITS QUEUE OR BACK (an exit, a
	// requeue), so the merges off their queues are republished with it.
	o.publishOffQueueLocked(ctx)
}

// rosterDeployCheck is HOLD as the roster draws it: the queue's pause by its
// deploy check while the hold stands, nil while it holds nothing.
func rosterDeployCheck(hold deployHold) *sidebar.DeployCheckHold {
	if !hold.holdsQueue() {
		return nil
	}
	return &sidebar.DeployCheckHold{Since: hold.since()}
}

// publishOffQueue tells the roster every merge standing off its queue, whole.
func (o *orchestrator) publishOffQueue(ctx context.Context) {
	o.rosterQueue.mu.Lock()
	defer o.rosterQueue.mu.Unlock()
	o.publishOffQueueLocked(ctx)
}

// publishOffQueueLocked is publishOffQueue under the roster publisher's lock,
// so it is serialized with every queue's publication. A read that fails is
// recorded and nothing is published: the roster keeps the last set it was
// told until the next change republishes it.
func (o *orchestrator) publishOffQueueLocked(ctx context.Context) {
	const op = "daemon.merge.roster_off_queue"
	records, err := o.deps.DB.OffQueueMerges(ctx)
	if err != nil {
		recordFailed(o.deps.Log.Global(), op, logsite.Msg("could not read the merges off their queues, so the roster is not told them"),
			logsite.Msg("the read of the merges off their queues ended when its context was cancelled; the roster is not told them"), dlog.Context{}, err)
		return
	}
	// A MERGE NOT JUDGED YET IS DRAWN, and judged off this lock
	// (offqueuelanded.go): the publish never asks git.
	if !o.offQueueJudged(records) {
		o.kickOffQueueLanded()
	}
	entries := make([]sidebar.OffQueueEntry, 0, len(records))
	for _, record := range records {
		if o.offQueueHidden(record.Workspace) {
			continue
		}
		entries = append(entries, sidebar.OffQueueEntry{Workspace: record.Workspace, Cause: OffQueueWords(record.Cause)})
	}
	o.deps.Sidebar.SetOffQueue(entries)
}

// rosterLine answers the merges of LINE as the roster draws them, front
// first: each one's credit and the bubble a click on it opens. HEADED says
// LINE is the queue's line, whose front is the head: an admitted head's stages
// are read from its ledger. Every merge carries its clock, durable on its
// queue entry (wsm.MergeQueueEntry.Clock). The merges remediating out of
// line are drawn unheaded.
func (o *orchestrator) rosterLine(ctx context.Context, line []wsm.MergeQueueEntry, headed, queueHeld bool) ([]sidebar.QueueEntry, error) {
	out := make([]sidebar.QueueEntry, 0, len(line))
	for i, entry := range line {
		drawn := sidebar.QueueEntry{
			Workspace: entry.Workspace,
			Admitted:  entry.State != wsm.MergeQueued,
			Clock:     entry.Clock,
			Repair:    rosterRepair(entry.Slot),
		}
		if lease, ok := o.leaseOf(entry.Workspace); ok {
			drawn.Reveal = o.bubbleHeadID(entry.Workspace, lease)
			// A HEAD NOT ADMITTED RUNS NOTHING: its ledger holds an earlier
			// attempt's rounds, which drawn under it read as steps it is taking
			// (2026-11-15: a waiting head drew "rebasing" settled hours before).
			if headed && i == 0 && drawn.Admitted {
				stages, err := o.ledgerStages(ctx, entry.Workspace, lease, o.heldByQueue(queueHeld, entry))
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

// rosterRepair is the repair slot the roster draws a merge at, from its
// queue entry's stamped slot: RepairNone for every slot in line.
func rosterRepair(slot wsm.MergeSlot) sidebar.QueueRepair {
	switch slot {
	case wsm.MergeSlotFixingTests:
		return sidebar.RepairFixingTests
	case wsm.MergeSlotResolvingConflicts:
		return sidebar.RepairResolvingConflicts
	case wsm.MergeSlotAwaitingFixingTests:
		return sidebar.RepairAwaitingFixingTests
	}
	return sidebar.RepairNone
}

// bubbleHeadID is the row id of the head of the merge bubble the merge
// running in WS draws under LEASE, on WS's own root feed: what a click on the
// merge in the roster's queue reveals.
func (o *orchestrator) bubbleHeadID(ws ids.WorkspaceID, lease ids.LeaseID) *frontendv1.FeedId {
	return headFeedID(ws, lease)
}

// ledgerStages answers the stages of the merge running in WS under LEASE, read
// from its ledger and drawn held when QUEUEHELD (stagesOf). A ledger not
// opened yet has no stages.
func (o *orchestrator) ledgerStages(ctx context.Context, ws ids.WorkspaceID, lease ids.LeaseID, queueHeld bool) ([]*frontendv1.RosterMergeQueueStage, error) {
	ledgers, err := o.deps.DB.MergeLedger(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("merge: read the ledger of %s: %w", ws, err)
	}
	for _, ledger := range ledgers {
		if ledger.Lease == lease {
			return stagesOf(ledger.Intervals, queueHeld, false)
		}
	}
	return nil, nil
}
