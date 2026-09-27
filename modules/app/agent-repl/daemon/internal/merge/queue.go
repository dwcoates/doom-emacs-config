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

// AbandonCause names WHY a queued merge left its queue before it ever ran.
// `FeedMergeError` carries ONE `abandoned` arm for every one of them, so the
// cause reaches a reader ONLY as `FeedMergeAbandoned.summary` — which is why
// every cause is declared here, beside the one sentence it resolves to, rather
// than spelled at each call site. The value is also the durable drop reason
// `RemoveMergeQueueEntry` records, so the queue row and the bubble agree.
type AbandonCause string

const (
	// CauseUserDrop is the user taking a queued merge off the queue through
	// UpdateMergeQueue's evict.
	CauseUserDrop AbandonCause = "evicted"
	// CauseUserDequeue is the user releasing the slot in answer to the
	// interrupt's dequeue offer.
	CauseUserDequeue AbandonCause = "dequeued"
	// CauseWorkspaceClosed is the workspace itself being torn down — killed or
	// nuked — while its merge was still waiting.
	CauseWorkspaceClosed AbandonCause = "workspace_closed"
	// CauseDaemonShutdown is the daemon going away under a waiting merge: the
	// queue is durable, so this is only reached at the RESTORE, for a merge the
	// restart could not put back on its queue.
	CauseDaemonShutdown AbandonCause = "daemon_shutdown"
)

// abandonSummaries is the resolved sentence each cause draws as the bubble's
// collapsed line, exactly as `FeedMergeFailed.summary` is drawn.
var abandonSummaries = map[AbandonCause]string{
	CauseUserDrop:        "the operator evicted this merge from the queue",
	CauseUserDequeue:     "the user released this merge's queue slot",
	CauseWorkspaceClosed: "the workspace was closed while this merge was waiting in the queue",
	CauseDaemonShutdown:  "the daemon shut down while this merge was waiting in the queue, and the restart could not put it back",
}

// summary resolves one cause's sentence. The bool is false for a cause with no
// declared sentence, which is a programming error rather than an outcome: the
// caller reports it and still ends the bubble, because a reader losing the
// terminal entirely is worse than reading an unpolished one.
func (c AbandonCause) summary() (string, bool) {
	sentence, declared := abandonSummaries[c]
	return sentence, declared
}

// Evict removes a queued workspace from the queue. It is one of the FOUR
// DISTINCT ENDS a merge can have before it runs — evict is the user's own
// drop, dequeue is the user's answer to the interrupt offer, and the workspace
// close and the daemon shutdown are the merge's give-up under something else
// ending — and each records its own cause.
func (o *orchestrator) Evict(ctx context.Context, ws ids.WorkspaceID) error {
	return o.dropQueued(ctx, ws, CauseUserDrop)
}

// OnWorkspaceClosed abandons a workspace's WAITING merge when the workspace
// itself is torn down. A queued merge whose workspace is killed or nuked can
// never run, and left on the queue it would block the repository's queue
// forever on a workspace that is not there; abandoning it records WHY in the
// bubble instead. Nothing is dropped when the merge has already been admitted:
// a run in flight ends on its own terminal, not on this one.
func (o *orchestrator) OnWorkspaceClosed(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.merge.workspace_closed"
	if _, running := o.runFor(ws); running {
		return
	}
	if _, err := o.queueOf(ctx, ws); err != nil {
		// A REFUSAL IS THE ORDINARY ANSWER — this workspace simply had no
		// queued merge. Anything else is a store failure and is surfaced.
		if _, refused := Refused(err); !refused {
			o.log(ctx, ws).Error(op, "could not tell whether a torn-down workspace had a queued merge",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
		}
		return
	}
	if err := o.dropQueued(ctx, ws, CauseWorkspaceClosed); err != nil {
		o.log(ctx, ws).Error(op, "could not abandon the queued merge of a torn-down workspace",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
	}
}

// dropQueued takes one workspace off its queue with the cause it was dropped
// for, and publishes the abandoned terminal.
func (o *orchestrator) dropQueued(ctx context.Context, ws ids.WorkspaceID, cause AbandonCause) error {
	const op = "daemon.merge.drop_queued"
	log := o.log(ctx, ws)
	repo, err := o.queueOf(ctx, ws)
	if err != nil {
		return err
	}
	if err := o.deps.DB.RemoveMergeQueueEntry(ctx, repo, ws, string(cause)); err != nil {
		log.Error(op, "could not drop a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause), "error": err.Error()})
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
	log.Warn(op, "dropped a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause)})
	o.publishAbandoned(ctx, ws, ledger, cause)
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
	// A DRAINING DAEMON ADMITS NOTHING. The shutdown drain is bounded, and a
	// merge admitted inside it would be starting its first phase against a
	// state client that is about to close.
	if o.draining || o.pumping[repo] {
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
		// admitted counts what this burst ran, and the burst's own terminal
		// record carries it. THE RECORD IS THE END OF THE WHOLE MERGE, not of
		// its terminal row: the terminal is published partway through `finish`,
		// and the landing's durable stamps, the lease release, the queue
		// entry's removal and every synchronous republish they trigger all
		// follow it INSIDE pumpOnce, with this pump the last thing holding
		// them. Nothing else names that moment, so an observer that must not
		// disturb a merge in flight has nowhere else to wait; a burst that
		// admitted nothing is distinguishable by the count.
		admitted := 0
		for {
			ran, err := o.pumpOnce(context.Background(), repo)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.pump", "the admission pump stopped on an error",
					dlog.Context{"repo": string(repo), "admitted": admitted, "error": err.Error()})
				return
			}
			if !ran {
				o.deps.Log.Global().Debug("daemon.merge.pump", "the admission pump went idle",
					dlog.Context{"repo": string(repo), "admitted": admitted})
				return
			}
			admitted++
		}
	}()
}

// pumpOnce admits and runs at most one merge for a repository. It reports
// whether it ran one, so the pump loop ends on an empty or paused queue rather
// than spinning.
func (o *orchestrator) pumpOnce(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	front, lock, admitted, err := o.admitFront(ctx, repo)
	if err != nil || !admitted {
		return false, err
	}
	if err := o.start(ctx, repo, front, lock); err != nil {
		// ONE MERGE'S FAILURE IS NOT THE QUEUE'S. A run that reached its own
		// terminal has already recorded the failure at ERROR, published the
		// bubble's terminal and left the queue — so the pump goes on to the
		// next entry rather than stranding every merge behind this one.
		//
		// The test is whether the entry is STILL THERE: a failure early enough
		// to leave it admitted has no terminal and no teardown, and continuing
		// would re-admit the same entry forever. That one stops the pump, as
		// the caller's ERROR record says. A draining daemon's store may be
		// closing under the check, so the check is not made and the pump stops
		// on the failure, which the caller's ERROR record states.
		if !o.enterAdmission() {
			return true, err
		}
		still, checkErr := o.stillQueued(ctx, repo, front)
		o.admissions.Done()
		if checkErr != nil || still {
			return true, err
		}
		o.deps.Log.Global().Debug("daemon.merge.pump", "a merge ended on its own terminal; the queue continues",
			dlog.Context{"repo": string(repo), "workspace": string(front), "error": err.Error()})
		return true, nil
	}
	return true, nil
}

// enterAdmission registers one admission step against the shutdown drain,
// and reports false -- registering nothing -- once the daemon is draining.
// The caller ends the step with o.admissions.Done().
//
// IT IS WHAT MAKES "draining" AN EXCLUSION RATHER THAN A HINT. The pump used
// to READ the flag and then go on to read the store, so a drain that began in
// between closed the state client under the pump's next read (measured:
// `daemon.wsm.merge_queue: refused the read ... sql: database is closed` in
// TestStoppingTheDaemonInsideAMergesTerminalStampsTheLandingWithNoFailedWrites).
// The step is registered under the same lock the drain sets the flag under,
// and the drain waits for every registered step before it returns.
func (o *orchestrator) enterAdmission() bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.draining {
		return false
	}
	o.admissions.Add(1)
	return true
}

// admitFront admits the repository's queue front, as ONE admission step the
// shutdown drain waits for: every store read and the admission write happen
// inside it. It answers the admitted workspace and the queue lock the run
// holds, or admitted=false when there is nothing to admit (a draining daemon,
// a paused or empty queue, a run already in flight, another daemon's lock).
func (o *orchestrator) admitFront(ctx context.Context, repo wsm.RepoKey) (ids.WorkspaceID, *repoLock, bool, error) {
	const op = "daemon.merge.admit"
	// The pump loop re-enters this every iteration: a drain that began while a
	// burst was mid-flight stops the burst here rather than after it has taken
	// the next entry's lease.
	if !o.enterAdmission() {
		return "", nil, false, nil
	}
	defer o.admissions.Done()
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		return "", nil, false, err
	}
	if paused {
		return "", nil, false, nil
	}
	o.mu.Lock()
	busy := o.running[repo] != nil
	o.mu.Unlock()
	if busy {
		return "", nil, false, nil
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return "", nil, false, err
	}
	if len(entries) == 0 {
		return "", nil, false, nil
	}
	front := entries[0]
	lock, taken, err := acquireRepoLock(o.lockDir, string(repo))
	if err != nil {
		return "", nil, false, err
	}
	if !taken {
		o.deps.Log.Global().Warn(op, "another daemon holds this repository's merge queue", dlog.Context{"repo": string(repo)})
		return "", nil, false, nil
	}
	if err := o.deps.DB.AdmitMerge(ctx, repo, front.Workspace); err != nil {
		lock.Release()
		return "", nil, false, err
	}
	return front.Workspace, lock, true, nil
}

// stillQueued reports whether a workspace's entry is still on its repository's
// queue, which is how the pump tells a merge that ended from one that never
// started.
func (o *orchestrator) stillQueued(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID) (bool, error) {
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return false, err
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return true, nil
		}
	}
	return false, nil
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
