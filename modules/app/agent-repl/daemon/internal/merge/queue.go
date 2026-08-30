package merge

import (
	"context"
	"errors"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// This file is the PER-REPO QUEUE: the pre-state refusals, the durable
// enqueue, pause and eviction, and the admission pump that starts the front.
//
// The queue is durable in WSM and keyed by the TARGET repository's common dir,
// so a bounce re-enqueues exactly what was waiting in the order it was waiting
// in. The kernel flock is what keeps two daemons from running one repository's
// queue; the durable rows keep the order, and the lock keeps the exclusivity.

// Enqueue queues a workspace's merge.
//
// IT REFUSES PRE-STATE. An unmergeable workspace leaves no enqueuing→failed
// trail: nothing is recorded before the refusal, so there is no state to stamp
// and nothing to clean up. The composer gate is the primary defense against a
// prompt arriving after a merge starts; these refusals are the race fallback.
func (o *orchestrator) Enqueue(ctx context.Context, ws ids.WorkspaceID) error {
	const op = "daemon.merge.enqueue"
	log := o.log(ctx, ws)
	job, err := o.layoutFor(ctx, ws)
	if err != nil {
		log.Warn(op, "refused a merge for a workspace with no recorded geometry", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	session, found, err := o.deps.DB.Session(ctx, ws)
	if err != nil {
		return err
	}
	if found && session.Terminal != nil && session.Terminal.Kind == "deleted" {
		err := refuse(ArmSessionDeleted, ws, "the session was deleted, so its configured prompts can never run")
		log.Warn(op, "refused a merge for a deleted session", dlog.Context{"workspace": string(ws), "error": err.Error()})
		return err
	}
	repo, err := o.repoKeyFor(ctx, job)
	if err != nil {
		return err
	}
	if _, running := o.runFor(ws); running {
		return refuse(ArmAlreadyMerging, ws, "this workspace's merge is already in flight")
	}
	o.publish(ws, MergeFacts{State: StateEnqueuing})
	position, err := o.deps.DB.EnqueueMerge(ctx, repo, ws, o.deps.Now())
	if err != nil {
		o.forget(ws)
		var queued *wsm.MergeQueuedError
		if errors.As(err, &queued) {
			arm := ArmAlreadyQueued
			if queued.State == wsm.MergeAdmitted {
				arm = ArmAlreadyMerging
			}
			refusal := refuse(arm, ws, "the merge already holds place %d in its repository's queue", queued.Position)
			log.Warn(op, "refused a duplicate enqueue", dlog.Context{"workspace": string(ws), "repo": string(repo), "position": queued.Position})
			return refusal
		}
		return err
	}
	o.mu.Lock()
	o.repoOf[ws] = repo
	o.mu.Unlock()
	log.Debug(op, "queued a merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "position": position})
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// Pause stops the queue from admitting new merges; an in-flight merge runs on.
//
// It pauses EVERY repository, because the operator control it answers has no
// repository in it: the queue the user means is the daemon's.
func (o *orchestrator) Pause(ctx context.Context) error { return o.setPaused(ctx, true) }

// Unpause resumes admitting merges.
func (o *orchestrator) Unpause(ctx context.Context) error { return o.setPaused(ctx, false) }

// setPaused is the one pause-write path, so both verbs refuse a no-op the same
// way: pausing a paused queue is a refusal rather than a quiet success, because
// the operator asked for a change that did not happen.
func (o *orchestrator) setPaused(ctx context.Context, paused bool) error {
	op := "daemon.merge.pause"
	if !paused {
		op = "daemon.merge.unpause"
	}
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return err
	}
	repos := make([]wsm.RepoKey, 0, len(queues))
	for repo := range queues {
		repos = append(repos, repo)
	}
	if len(repos) == 0 {
		arm, reason := ArmNotPaused, "no repository has a merge queue to resume"
		if paused {
			arm, reason = ArmAlreadyPaused, "no repository has a merge queue to pause"
		}
		return &RefusalError{Arm: arm, Reason: reason}
	}
	changed := 0
	for _, repo := range repos {
		was, err := o.deps.DB.MergeQueuePaused(ctx, repo)
		if err != nil {
			return err
		}
		if was == paused {
			continue
		}
		if err := o.deps.DB.SetMergeQueuePaused(ctx, repo, paused); err != nil {
			return err
		}
		changed++
	}
	if changed == 0 {
		arm, reason := ArmNotPaused, "the merge queue is not paused"
		if paused {
			arm, reason = ArmAlreadyPaused, "the merge queue is already paused"
		}
		o.deps.Log.Global().Warn(op, "refused a pause change that would not change anything", dlog.Context{"arm": arm})
		return &RefusalError{Arm: arm, Reason: reason}
	}
	o.deps.Log.Global().Debug(op, "changed the merge queue's pause state", dlog.Context{"paused": paused, "repos": changed})
	if !paused {
		for _, repo := range repos {
			o.kick(repo)
		}
	}
	return nil
}

// Evict removes a queued workspace from the queue. It is one of the THREE
// DISTINCT ENDS a merge can have before it runs — evict is the operator's,
// dequeue is the user's answer to the interrupt offer, and abandon is the
// merge's own give-up — and each records its own cause.
func (o *orchestrator) Evict(ctx context.Context, ws ids.WorkspaceID) error {
	return o.dropQueued(ctx, ws, "evicted", "the operator evicted this merge from the queue")
}

// dropQueued takes one workspace off its queue with the cause it was dropped
// for, and publishes the abandoned terminal.
func (o *orchestrator) dropQueued(ctx context.Context, ws ids.WorkspaceID, cause, summary string) error {
	const op = "daemon.merge.drop_queued"
	log := o.log(ctx, ws)
	repo, err := o.queueOf(ctx, ws)
	if err != nil {
		return err
	}
	if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, cause); err != nil {
		log.Error(op, "could not drop a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": cause, "error": err.Error()})
		return err
	}
	o.mu.Lock()
	delete(o.repoOf, ws)
	o.mu.Unlock()
	log.Warn(op, "dropped a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": cause})
	o.publishAbandoned(ctx, ws, summary)
	o.clearOffer(ws)
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// queueOf resolves the queue a workspace's merge is waiting on, refusing when
// nothing of that workspace is queued.
func (o *orchestrator) queueOf(ctx context.Context, ws ids.WorkspaceID) (wsm.RepoKey, error) {
	o.mu.Lock()
	repo, known := o.repoOf[ws]
	o.mu.Unlock()
	if known {
		return repo, nil
	}
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return "", err
	}
	for key, entries := range queues {
		for _, entry := range entries {
			if entry.Workspace == ws {
				return key, nil
			}
		}
	}
	return "", refuse(ArmNoSuchQueuedMerge, ws, "no merge of this workspace is on any queue")
}

// republishQueue redraws the queue tab of every bubble on one repository's
// queue. The snapshot is replaced whole on every queue change and on any change
// to the front's active tab, so a waiting user always sees the current order.
func (o *orchestrator) republishQueue(ctx context.Context, repo wsm.RepoKey) error {
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return err
	}
	names := map[ids.WorkspaceID]string{}
	dirs := map[ids.WorkspaceID]string{}
	for _, entry := range entries {
		record, err := o.deps.DB.Workspace(ctx, entry.Workspace)
		if err != nil {
			return err
		}
		names[entry.Workspace] = record.Name
		dirs[entry.Workspace] = record.Dir
	}
	o.mu.Lock()
	front := o.running[repo]
	o.mu.Unlock()
	var frontTab = tabLabel(TabQueue, 1)
	if front != nil {
		frontTab = tabLabel(front.activeTab(), front.roundOf(front.activeTab()))
	}
	for _, entry := range entries {
		lease, ok := o.leaseOf(entry.Workspace)
		if !ok {
			continue
		}
		snapshot := queueSnapshot(entries, entry.Workspace, names, dirs, frontTab)
		o.deps.Feed.UpsertSynthesized(entry.Workspace, mergeFeed(lease), tabRow(entry.Workspace, lease, TabQueue, 1,
			queueTab(snapshot, entry.Position == 1, o.nowMS())))
		facts, _ := o.Facts(entry.Workspace)
		facts.QueuePosition = entry.Position
		facts.QueueDepth = len(entries)
		if entry.State == wsm.MergeQueued {
			facts.State = StateQueued
		}
		o.publish(entry.Workspace, facts)
	}
	return nil
}

// leaseOf reports the merge lease a workspace's bubble is keyed by. A queued
// merge has no lease yet, so its bubble does not exist and nothing is drawn for
// it beyond its facts.
func (o *orchestrator) leaseOf(ws ids.WorkspaceID) (ids.LeaseID, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	r, ok := o.runsByWorkspace[ws]
	if !ok {
		return "", false
	}
	return r.lease.ID, true
}

// kick starts the admission pump for one repository, unless one is already
// running for it.
func (o *orchestrator) kick(repo wsm.RepoKey) {
	if !o.async {
		return
	}
	o.mu.Lock()
	if o.pumping[repo] {
		o.mu.Unlock()
		return
	}
	o.pumping[repo] = true
	o.mu.Unlock()
	go func() {
		defer func() {
			o.mu.Lock()
			o.pumping[repo] = false
			o.mu.Unlock()
		}()
		for {
			ran, err := o.pumpOnce(context.Background(), repo)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.pump", "the admission pump stopped on an error",
					dlog.Context{"repo": string(repo), "error": err.Error()})
				return
			}
			if !ran {
				return
			}
		}
	}()
}

// pumpOnce admits and runs at most one merge for a repository. It reports
// whether it ran one, so the pump loop ends on an empty or paused queue rather
// than spinning.
func (o *orchestrator) pumpOnce(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	const op = "daemon.merge.admit"
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		return false, err
	}
	if paused {
		return false, nil
	}
	o.mu.Lock()
	busy := o.running[repo] != nil
	o.mu.Unlock()
	if busy {
		return false, nil
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return false, err
	}
	if len(entries) == 0 {
		return false, nil
	}
	front := entries[0]
	lock, taken, err := acquireRepoLock(o.lockDir, string(repo))
	if err != nil {
		return false, err
	}
	if !taken {
		o.deps.Log.Global().Warn(op, "another daemon holds this repository's merge queue", dlog.Context{"repo": string(repo)})
		return false, nil
	}
	if err := o.deps.DB.AdmitMerge(ctx, repo, front.Workspace); err != nil {
		lock.Release()
		return false, err
	}
	if err := o.start(ctx, repo, front.Workspace, lock); err != nil {
		return true, err
	}
	return true, nil
}

// queueTab builds the queue tab: LIVE while this workspace waits, SETTLED the
// moment it reaches the front — the tab's whole job is the wait, so reaching
// the front is what completes it.
func queueTab(snapshot *frontendv1.FeedMergeQueue, atFront bool, atMS int64) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabQueue{Queue: snapshot}
	if atFront {
		inner.State = &frontendv1.FeedMergeTabQueue_Settled{Settled: settledOK(atMS)}
	} else {
		inner.State = &frontendv1.FeedMergeTabQueue_Live{Live: live()}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Queue{Queue: inner}}
}

// nowMS is the clock in the epoch milliseconds every drawn stamp uses.
func (o *orchestrator) nowMS() int64 { return o.deps.Now().UnixMilli() }
