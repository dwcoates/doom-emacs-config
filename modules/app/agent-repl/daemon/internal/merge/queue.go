package merge

import (
	"context"
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
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
	// THE BUBBLE EXISTS FROM HERE: the ledger identity it is addressed by is
	// minted at enqueue, so republishQueue below can draw the queue tab on it.
	o.mintLedger(ws)
	log.Debug(op, "queued a merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "position": position})
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// Pause stops the queue from admitting new merges; an in-flight merge runs on.
//
// A nil scope pauses EVERY repository, which is the daemon-wide switch an
// UNSET request ref means. A scope names one repository, whose queue is the
// only one touched.
func (o *orchestrator) Pause(ctx context.Context, scope *RepositoryScope) error {
	return o.setPaused(ctx, scope, true)
}

// Unpause resumes admitting merges, scoped exactly as Pause is.
func (o *orchestrator) Unpause(ctx context.Context, scope *RepositoryScope) error {
	return o.setPaused(ctx, scope, false)
}

// setPaused is the one pause-write path, so both verbs refuse a no-op the same
// way: pausing a paused queue is a refusal rather than a quiet success, because
// the operator asked for a change that did not happen.
func (o *orchestrator) setPaused(ctx context.Context, scope *RepositoryScope, paused bool) error {
	op := "daemon.merge.pause"
	if !paused {
		op = "daemon.merge.unpause"
	}
	repos, err := o.pauseScope(ctx, op, scope, paused)
	if err != nil {
		return err
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
	o.deps.Log.Global().Debug(op, "changed the merge queue's pause state", dlog.Context{"paused": paused, "repos": changed, "scoped": scope != nil})
	if !paused {
		for _, repo := range repos {
			o.kick(repo)
		}
	}
	return nil
}

// pauseScope resolves WHICH queues one pause or resume addresses: every
// repository that has a queue for a nil scope, and exactly the named
// repository's queue for a set one. A scope naming a repository the registry
// does not hold is REFUSED rather than silently pausing a key nobody owns.
func (o *orchestrator) pauseScope(ctx context.Context, op string, scope *RepositoryScope, paused bool) ([]wsm.RepoKey, error) {
	if scope != nil {
		if scope.ID == "" && scope.Dir == "" {
			o.deps.Log.Global().Warn(op, "refused a pause change scoped by an empty repository ref", dlog.Context{"arm": ArmUnknownRepository})
			return nil, &RefusalError{Arm: ArmUnknownRepository, Reason: "the request's repository ref names neither an id nor a dir"}
		}
		repos, err := o.deps.DB.ListRepositories(ctx)
		if err != nil {
			return nil, err
		}
		for _, repo := range repos {
			if scope.ID != "" && repo.ID != scope.ID {
				continue
			}
			if scope.Dir != "" && repo.Dir != scope.Dir {
				continue
			}
			// THE QUEUE IS KEYED BY THE COMMON DIR (repoKeyFor), never by the
			// registry's worktree dir: keying a scoped pause by repo.Dir wrote
			// and read a key no queue is ever stored under, so a scoped pause
			// never saw what an unscoped one had done.
			common, err := o.deps.Git.CommonDir(ctx, repo.Dir)
			if err != nil {
				return nil, fmt.Errorf("merge: resolving the repository of %s: %w", repo.Dir, err)
			}
			return []wsm.RepoKey{wsm.RepoKey(common)}, nil
		}
		o.deps.Log.Global().Warn(op, "refused a pause change for a repository the registry does not hold", dlog.Context{"arm": ArmUnknownRepository, "repository": string(scope.ID), "dir": scope.Dir})
		return nil, &RefusalError{Arm: ArmUnknownRepository, Reason: "no registered repository matches the request's repository ref"}
	}
	queues, err := o.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return nil, err
	}
	keys := make([]wsm.RepoKey, 0, len(queues))
	for repo := range queues {
		keys = append(keys, repo)
	}
	if len(keys) == 0 {
		arm, reason := ArmNotPaused, "no repository has a merge queue to resume"
		if paused {
			arm, reason = ArmAlreadyPaused, "no repository has a merge queue to pause"
		}
		o.deps.Log.Global().Warn(op, "refused a pause change with no queue to change", dlog.Context{"arm": arm})
		return nil, &RefusalError{Arm: arm, Reason: reason}
	}
	return keys, nil
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
	// THE LEDGER IDENTITY IS KEPT LONG ENOUGH TO END THE BUBBLE. It addresses
	// the queued merge's own bubble, and a bubble that simply stopped
	// mid-queue-tab would leave a reader with no terminal at all — so the
	// terminal is drawn against it before the identity is dropped.
	ledger := o.ledgerOf[ws]
	delete(o.ledgerOf, ws)
	o.mu.Unlock()
	log.Warn(op, "dropped a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": cause})
	o.publishAbandoned(ctx, ws, ledger, summary)
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
		// EVERY QUEUED MERGE HAS A BUBBLE. Its ledger identity is minted at
		// enqueue, so a merge that has not been admitted still has a head to
		// hang its queue tab on -- which is the tab a waiting user reads their
		// place from, and the first of the tab sequence.
		if lease, ok := o.leaseOf(entry.Workspace); ok {
			o.deps.Feed.UpsertSynthesized(entry.Workspace, feedid.Feed{Root: true},
				headRow(entry.Workspace, lease, branchLabel(names[entry.Workspace], dirs[entry.Workspace]), o.nowMS(), nil))
			snapshot := queueSnapshot(entries, entry.Workspace, names, dirs, frontTab)
			o.deps.Feed.UpsertSynthesized(entry.Workspace, mergeFeed(lease), tabRow(entry.Workspace, lease, TabQueue, 1,
				queueTab(snapshot, entry.Position == 1, o.nowMS())))
		}
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

// leaseOf reports the LEDGER identity a workspace's merge bubble is keyed by.
// It is minted at ENQUEUE, so a merge that is only queued already has a bubble
// -- the one its queue tab is drawn on -- and the occupancy lease taken at
// admission carries the same identity, which is what makes the queued bubble
// and the running one one bubble.
func (o *orchestrator) leaseOf(ws ids.WorkspaceID) (ids.LeaseID, bool) {
	o.mu.Lock()
	defer o.mu.Unlock()
	id, ok := o.ledgerOf[ws]
	return id, ok
}

// mintLedger mints a workspace's merge ledger identity, or answers the one it
// already has: a re-enqueue of a merge already in the queue keeps its bubble.
func (o *orchestrator) mintLedger(ws ids.WorkspaceID) ids.LeaseID {
	o.mu.Lock()
	defer o.mu.Unlock()
	if id, ok := o.ledgerOf[ws]; ok {
		return id
	}
	id := ids.LeaseID(wsm.NewLeaseID())
	o.ledgerOf[ws] = id
	return id
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
