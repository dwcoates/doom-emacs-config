package merge

import (
	"context"
	"fmt"
	"maps"
	"slices"
	"sort"
	"strings"

	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is the PER-REPO QUEUE: pause and eviction, the queue's publication,
// and the admission pump that starts the front. A request's refusals and its
// wait for the requesting turn's end are request.go's.
//
// The queue is durable in WSM and keyed by the TARGET repository's common dir,
// so a bounce re-enqueues exactly what was waiting in the order it was waiting
// in. The kernel flock is what keeps two daemons from running one repository's
// queue; the durable rows keep the order, and the lock keeps the exclusivity.

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
	if paused {
		return o.suspendForPause(ctx, repos)
	}
	for _, repo := range repos {
		o.kick(repo)
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

// runningSummaries are the resolved sentences of a merge abandoned AFTER it
// was admitted. They are separate from abandonSummaries because those say
// "while this merge was waiting in the queue", which is false of a merge that
// ran (2026-09-28: "a merge left the queue without running" was logged for one
// that had run three repair rounds).
var runningSummaries = map[AbandonCause]string{
	CauseUserDrop:        "the operator evicted this merge while it was running",
	CauseUserDequeue:     "the user took this merge out of the queue while it was running",
	CauseWorkspaceClosed: "the workspace was closed while this merge was running",
}

// releases reports whether C is a RELEASE the user or the operator asked for
// (an evict, a dequeue), as against a merge given up under something else
// ending (its workspace closing, the daemon shutting down). A released merge's
// bubble is closed (closeReleasedBubble).
func (c AbandonCause) releases() bool {
	return c == CauseUserDrop || c == CauseUserDequeue
}

// summaryRunning resolves the sentence of a merge abandoned while it ran.
func (c AbandonCause) summaryRunning() (string, bool) {
	sentence, declared := runningSummaries[c]
	return sentence, declared
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
	return o.dequeue(ctx, ws, CauseUserDrop)
}

// Move moves a workspace's waiting merge one place up or down its queue, and
// redraws every bubble on that queue when the order changed.
func (o *orchestrator) Move(ctx context.Context, ws ids.WorkspaceID, move wsm.MergeMove) (bool, error) {
	return o.reorder(ctx, ws, reordering{
		op:      "daemon.merge.move",
		fields:  dlog.Context{"move": move.String()},
		failed:  "could not move a merge in its queue",
		unmoved: "a merge asked to move has no waiting merge to pass on that side, and stays where it is",
		moved:   "moved a merge one place in its queue",
		do: func(repo wsm.RepoKey) (bool, error) {
			return o.deps.DB.MoveMerge(ctx, repo, ws, move)
		},
	})
}

// reordering is one way a waiting merge's place in its queue is changed: the
// store's reorder, and the records each of its outcomes leaves.
type reordering struct {
	op string
	// fields are the reordering's own context, beside the workspace and the
	// repository every record carries.
	fields  dlog.Context
	failed  string
	unmoved string
	moved   string
	do      func(repo wsm.RepoKey) (bool, error)
}

// reorder runs one reordering of WS's waiting merge, answering whether it
// moved, and redraws every bubble on that queue when the order changed. A
// store failure is recorded at ERROR here, once; a workspace with nothing
// queued is the queue's refusal, answered as it is.
func (o *orchestrator) reorder(ctx context.Context, ws ids.WorkspaceID, r reordering) (bool, error) {
	log := o.log(ctx, ws)
	repo, err := o.queueOf(ctx, ws)
	if err != nil {
		return false, err
	}
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	maps.Copy(fields, r.fields)
	moved, err := r.do(repo)
	if err != nil {
		fields["error"] = err.Error()
		log.Error(r.op, r.failed, fields)
		return false, err
	}
	if !moved {
		log.Info(r.op, r.unmoved, fields)
		return false, nil
	}
	log.Info(r.op, r.moved, fields)
	return true, o.republishQueue(ctx, repo)
}

// dequeue takes one workspace's merge out, whatever it is doing. A merge still
// WAITING (or only requested) leaves its queue; a merge that is RUNNING is
// ABANDONED through the one release path. Before this a dequeue only ever dropped the queue
// row, so a running merge's lease, queue entry and repository lock outlived
// it and blocked its repository's queue until the daemon restarted
// (2026-09-28, lease c8a3a664006f46c1).
func (o *orchestrator) dequeue(ctx context.Context, ws ids.WorkspaceID, cause AbandonCause) error {
	if r, running := o.runFor(ws); running {
		return o.abandonRunning(ctx, r, cause)
	}
	// A MERGE A RESTART TOOK BACK BUT HAS NOT RESUMED is admitted, holds its
	// merge lease and has no run: it ends as an admitted merge, never as a
	// waiting one, or its lease would outlive its entry (dropPendingResume).
	o.mu.Lock()
	doc, pending := o.resumes[ws]
	delete(o.resumes, ws)
	o.mu.Unlock()
	if pending {
		return o.dropPendingResume(ctx, ws, doc, cause)
	}
	return o.dropQueued(ctx, ws, cause)
}

// dropPendingResume ends a merge the boot recovery or a handover took back
// and that has not resumed yet, the caller having taken it out of o.resumes:
// its queue entry, its merge lease and its progress record go in one store
// transaction (wsm.DropAdmittedMerge, the owner of "a merge lease stands
// with exactly one queue entry"), its scratch trees are swept, and its bubble
// ends on the abandoned terminal a running merge reads. A drop that fails
// changed nothing: the merge is put back to resume as it was.
func (o *orchestrator) dropPendingResume(ctx context.Context, ws ids.WorkspaceID, doc progressDoc, cause AbandonCause) error {
	const op = "daemon.merge.drop_pending_resume"
	log := o.log(ctx, ws)
	repo := wsm.RepoKey(doc.Repo)
	o.mu.Lock()
	lease, known := o.ledgerOf[ws]
	o.mu.Unlock()
	if !known {
		panic(fmt.Sprintf("merge: the merge of %s was taken back for its resume with no lease recorded", ws))
	}
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo), "lease": string(lease), "cause": string(cause), "step": doc.Step}
	putBack := func() {
		o.mu.Lock()
		o.resumes[ws] = doc
		o.mu.Unlock()
	}
	entry, err := o.entryOf(ctx, repo, ws)
	if err != nil {
		putBack()
		log.Error(op, "could not read the queue entry of a merge taken back for its resume; it is left to resume", withField(fields, "error", err.Error()))
		return err
	}
	if err := o.deps.DB.DropAdmittedMerge(ctx, repo, ws, lease, string(cause)); err != nil {
		putBack()
		log.Error(op, "could not drop a merge taken back for its resume; it is left to resume", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Queue.OnLeaseChanged(ws)
	o.sweepTrees(ctx, ws, doc.Subject.TargetDir, lease, true)
	o.mu.Lock()
	delete(o.repoOf, ws)
	delete(o.ledgerOf, ws)
	o.mu.Unlock()
	summary, declared := cause.summaryRunning()
	if !declared {
		summary = fmt.Sprintf("this merge was abandoned while it was running (%s)", cause)
		o.deps.Log.Global().Error("daemon.merge.abandoned", "a merge was abandoned under a cause with no declared summary",
			dlog.Context{"workspace": string(ws), "cause": string(cause)})
	}
	log.Info(op, "dropped a merge a restart took back before it resumed: its entry, its lease and its progress record together", fields)
	o.publishAbandonedWith(ctx, ws, lease, entry, cause, summary)
	o.clearOffer(ws)
	if err := o.republishQueue(ctx, repo); err != nil {
		return err
	}
	o.kick(repo)
	return nil
}

// OnWorkspaceClosed abandons a workspace's WAITING merge when the workspace
// itself is torn down. A queued merge whose workspace is killed or nuked can
// never run, and left on the queue it would block the repository's queue
// forever on a workspace that is not there; abandoning it records WHY in the
// bubble instead. Nothing is dropped when the merge has already been admitted:
// a run in flight ends on its own terminal, not on this one.
func (o *orchestrator) OnWorkspaceClosed(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.merge.workspace_closed"
	if r, running := o.runFor(ws); running {
		// A RUNNING MERGE ENDS WITH ITS WORKSPACE TOO. Left alone, a merge
		// running on a workspace that is gone would hold its lease for ever.
		if err := o.abandonRunning(ctx, r, CauseWorkspaceClosed); err != nil {
			o.log(ctx, ws).Error(op, "could not abandon the running merge of a torn-down workspace",
				dlog.Context{"workspace": string(ws), "error": err.Error()})
		}
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
	if err := o.dequeue(ctx, ws, CauseWorkspaceClosed); err != nil {
		o.log(ctx, ws).Error(op, "could not abandon the queued merge of a torn-down workspace",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
	}
}

// dropQueued takes one workspace off its queue with the cause it was dropped
// for, and publishes the abandoned terminal. A merge only REQUESTED -- its
// requesting turn still running -- has its wait withdrawn and draws nothing:
// it was never reported, so there is no bubble to end.
func (o *orchestrator) dropQueued(ctx context.Context, ws ids.WorkspaceID, cause AbandonCause) error {
	const op = "daemon.merge.drop_queued"
	log := o.log(ctx, ws)
	repo, err := o.queueOf(ctx, ws)
	if err != nil {
		return err
	}
	o.withdrawRequest(ws)
	entry, err := o.entryOf(ctx, repo, ws)
	if err != nil {
		log.Error(op, "could not read a queued merge's queue entry", dlog.Context{"workspace": string(ws), "repo": string(repo), "error": err.Error()})
		return err
	}
	// AN ADMITTED MERGE WITH NO RUN AND NO RESUME HERE is not a waiting one:
	// another daemon drives it, a handover is moving it, or its run is
	// starting. Dropping only its entry would leave its lease over none, so
	// it is refused, loudly.
	if entry.State == wsm.MergeAdmitted {
		err := fmt.Errorf("merge: the merge of %s is admitted with no run in this daemon; it is taken off where it runs", ws)
		log.Error(op, "refused to drop an admitted merge as a waiting one", dlog.Context{
			"workspace": string(ws), "repo": string(repo), "cause": string(cause), "error": err.Error()})
		return err
	}
	if ledger, ok := o.leaseOf(ws); ok {
		if err := o.settleLeavingQueueTab(ctx, repo, ws, ledger, string(cause)); err != nil {
			log.Error(op, "could not settle a dropped merge's queue tab", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause), "error": err.Error()})
		}
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
	// A RELEASE THE USER OR OPERATOR ASKED FOR IS NO WARNING: it is the queue
	// doing what it was told. A drop under something else ending stays WARN.
	if cause.releases() {
		log.Info(op, "released a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause)})
	} else {
		log.Warn(op, "dropped a queued merge", dlog.Context{"workspace": string(ws), "repo": string(repo), "cause": string(cause)})
	}
	o.publishAbandoned(ctx, ws, ledger, entry, cause)
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
// queue, and every WAITING merge's facts. The snapshot is replaced whole on
// every queue change and on every step change of the merge being worked on, so
// a waiting user always sees the current order and what the merge ahead is
// doing.
//
// A REQUESTED MERGE IS IN NOBODY'S LINE: its requesting turn has not ended, so
// it is on no snapshot and has no facts.
func (o *orchestrator) republishQueue(ctx context.Context, repo wsm.RepoKey) error {
	o.publishAsyncMerges(ctx, repo)
	stored, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return err
	}
	entries := inLine(stored)
	view, err := o.viewQueue(ctx, repo, entries)
	if err != nil {
		return err
	}
	o.mu.Lock()
	front := o.running[repo]
	o.mu.Unlock()
	var ahead *frontendv1.FooterStatusActivityMergeStep
	var aheadAt time.Time
	if front != nil {
		ahead, aheadAt = front.enqueuedLine(view.names[front.ws])
	}
	waiting := 0
	for _, entry := range entries {
		if entry.State == wsm.MergeQueued {
			waiting++
		}
	}
	place := 0
	var drawn, unledgered []string
	for _, entry := range entries {
		// EVERY MERGE IN LINE HAS A BUBBLE, from the moment it is queued, PINNED
		// to the feed's bottom until it resolves (owner ruling, 2026-10-10;
		// publishHead), so every row the workspace writes while the merge
		// waits lands above it. Its ledger identity is minted as it takes its
		// place, so the head has its queue tab to hang on from the start.
		if lease, ok := o.leaseOf(entry.Workspace); ok {
			clock, clocked, err := o.headClockMS(entry)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.queue", "a queued merge carries no queued time; the queue is not drawn",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace), "error": err.Error()})
				return err
			}
			if clocked {
				o.publishHead(entry.Workspace, lease, o.bubbleLabel(ctx, entry), clock, nil)
			}
			state, shown, err := o.queueTabState(entry)
			if err != nil {
				o.deps.Log.Global().Error("daemon.merge.queue", "could not resolve when a queued merge's queue tab began; the queue is not drawn",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace), "error": err.Error()})
				return err
			}
			if shown {
				o.publishRoundTab(entry.Workspace, lease, TabQueue, 1, queueTab(view.snapshot(entries, entry.Workspace), state))
				drawn = append(drawn, string(entry.Workspace))
			} else {
				o.log(ctx, entry.Workspace).Debug("daemon.merge.queue", "an admitted merge's run is not registered yet; its admission draws its queue tab",
					dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace)})
			}
		} else {
			// A MERGE IN LINE WITH NO BUBBLE IDENTITY IN THIS DAEMON has a
			// queue tab nothing here can redraw: the stale tab a reader
			// would otherwise find with no record of why.
			unledgered = append(unledgered, string(entry.Workspace))
		}
		if entry.State != wsm.MergeQueued {
			continue
		}
		// ENQUEUED k/n COUNTS ONLY THE WAITING: the merge being worked on is
		// not counted, so 1 means next and there is never a 0.
		place++
		o.publish(entry.Workspace, MergeFacts{
			State: StateQueued, Step: footer.StepEnqueued, QueuePlace: place, QueueWaiting: waiting,
			Line: ahead, LineAt: aheadAt,
		})
	}
	if len(unledgered) > 0 {
		o.deps.Log.Global().Info("daemon.merge.queue", "merges in line have no bubble identity in this daemon; their queue tabs are not redrawn",
			dlog.Context{"repo": string(repo), "workspaces": strings.Join(unledgered, ",")})
	}
	o.deps.Log.Global().Debug("daemon.merge.queue", "redrew the queue tab of every merge in line", dlog.Context{
		"repo": string(repo), "in_line": len(entries), "drawn": strings.Join(drawn, ",")})
	return nil
}

// queueView is what every drawing of one repository's queue reads: each
// in-line workspace's name and directory, and each entry's standing.
type queueView struct {
	names     map[ids.WorkspaceID]string
	dirs      map[ids.WorkspaceID]string
	standings []*frontendv1.FeedMergeQueueEntry
}

// viewQueue resolves the queue view of one repository's in-line entries.
func (o *orchestrator) viewQueue(ctx context.Context, repo wsm.RepoKey, entries []wsm.MergeQueueEntry) (queueView, error) {
	view := queueView{names: map[ids.WorkspaceID]string{}, dirs: map[ids.WorkspaceID]string{}}
	for _, entry := range entries {
		record, err := o.deps.DB.Workspace(ctx, entry.Workspace)
		if err != nil {
			return queueView{}, err
		}
		view.names[entry.Workspace] = record.Name
		view.dirs[entry.Workspace] = record.Dir
	}
	standings, err := o.queueStandings(entries)
	if err != nil {
		o.deps.Log.Global().Error("daemon.merge.queue", "could not resolve when a queued merge entered its stage; the queue is not drawn",
			dlog.Context{"repo": string(repo), "entries": len(entries), "error": err.Error()})
		return queueView{}, err
	}
	view.standings = standings
	return view, nil
}

// snapshot is the queue as self sees it.
func (v queueView) snapshot(entries []wsm.MergeQueueEntry, self ids.WorkspaceID) *frontendv1.FeedMergeQueue {
	return queueSnapshot(entries, self, v.names, v.dirs, v.standings)
}

// settleLeavingQueueTab draws a merge's own queue tab ONE LAST TIME as it
// leaves the queue -- landed, failed, abandoned or dropped -- before its entry
// is removed: SETTLED, with the queue as it stands at that moment and itself
// as the current entry. Its tab is no longer redrawn once it has left, because
// the queue it would show no longer holds it; every merge still in line is
// redrawn by the republish that follows the removal (owner ruling,
// 2026-10-12; AGENTS.md, "The merge queue's test gate and remediation
// contract").
//
// A merge only REQUESTED has no bubble and draws nothing.
func (o *orchestrator) settleLeavingQueueTab(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, ledger ids.LeaseID, why string) error {
	const op = "daemon.merge.queue"
	stored, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return err
	}
	entries := inLine(stored)
	idx := slices.IndexFunc(entries, func(e wsm.MergeQueueEntry) bool { return e.Workspace == ws })
	if idx < 0 || ledger == "" {
		o.log(ctx, ws).Debug(op, "a merge left a queue it never stood in line in; it has no queue tab to settle",
			dlog.Context{"workspace": string(ws), "repo": string(repo), "why": why})
		return nil
	}
	view, err := o.viewQueue(ctx, repo, entries)
	if err != nil {
		return err
	}
	state, shown, err := o.queueTabState(entries[idx])
	if err != nil {
		return err
	}
	if !shown || !state.settled {
		// IT LEFT WHILE STILL WAITING: its wait ends now.
		state = tabState{startedMS: state.startedMS, settled: true, endedMS: o.nowMS()}
		if !shown {
			queued, err := enqueuedMS(entries[idx])
			if err != nil {
				return err
			}
			state.startedMS = queued
		}
	}
	o.publishRoundTab(ws, ledger, TabQueue, 1, queueTab(view.snapshot(entries, ws), state))
	o.log(ctx, ws).Info(op, "settled the leaving merge's queue tab on the queue as it stood when it left", dlog.Context{
		"workspace": string(ws), "repo": string(repo), "ledger": string(ledger), "why": why, "in_line": len(entries)})
	return nil
}

// inLine answers a repository's stored queue less its REQUESTED merges, with
// the positions renumbered over what is left.
func inLine(stored []wsm.MergeQueueEntry) []wsm.MergeQueueEntry {
	entries := make([]wsm.MergeQueueEntry, 0, len(stored))
	for _, entry := range stored {
		if entry.State == wsm.MergeRequested {
			continue
		}
		entry.Position = len(entries) + 1
		entries = append(entries, entry)
	}
	return entries
}

// bubbleLabel is a queued merge's head line, read off what it merges. A label
// that cannot be read is recorded and the workspace's own id stands in: a
// label must not stop the queue from being drawn.
func (o *orchestrator) bubbleLabel(ctx context.Context, entry wsm.MergeQueueEntry) string {
	if r, running := o.runFor(entry.Workspace); running {
		return r.label()
	}
	label, err := o.sourceLabel(ctx, entry.Workspace, entry.Source)
	if err != nil {
		o.log(ctx, entry.Workspace).Error("daemon.merge.queue", "could not read what a queued merge lands for its bubble's label",
			dlog.Context{"workspace": string(entry.Workspace), "error": err.Error()})
		return string(entry.Workspace)
	}
	return label
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
// already has: a merge put back in line keeps its bubble.
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
		if o.onKick != nil {
			o.onKick(repo)
		}
		return
	}
	o.mu.Lock()
	// A DRAINING DAEMON ADMITS NOTHING. The shutdown drain is bounded, and a
	// merge admitted inside it would be starting its first phase against a
	// state client that is about to close.
	if o.draining {
		o.mu.Unlock()
		return
	}
	// A KICK TO A RUNNING PUMP IS REMEMBERED, NOT DROPPED. The pump reads the
	// mark under this lock before it goes idle, so a merge put in line in the
	// instant between the pump's last look and its exit is still admitted.
	if o.pumping[repo] {
		o.kicked[repo] = true
		o.mu.Unlock()
		// THE CHAIN GROWS BEHIND A RUNNING HEAD: the pump is busy with the
		// head, so the merge put in line (or taken back) behind it is offered
		// to the chain here.
		o.extendChainAsync(repo)
		return
	}
	o.pumping[repo] = true
	o.kicked[repo] = false
	o.mu.Unlock()
	o.extendChainAsync(repo)
	o.live.Add(1)
	go func() {
		defer o.live.Done()
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
				o.mu.Lock()
				o.pumping[repo] = false
				o.mu.Unlock()
				o.deps.Log.Global().Error("daemon.merge.pump", "the admission pump stopped on an error",
					dlog.Context{"repo": string(repo), "admitted": admitted, "error": err.Error()})
				return
			}
			if !ran {
				o.mu.Lock()
				again := o.kicked[repo]
				o.kicked[repo] = false
				if !again {
					o.pumping[repo] = false
				}
				o.mu.Unlock()
				if again {
					continue
				}
				o.deps.Log.Global().Debug("daemon.merge.pump", "the admission pump went idle",
					dlog.Context{"repo": string(repo), "admitted": admitted})
				return
			}
			admitted++
		}
	}()
}

// pumpOnce admits and runs at most one merge for a repository, and returns once
// it has ended. It reports whether it ran one, so the pump loop ends on an
// empty or paused queue rather than spinning.
//
// NOTHING PARKS, so a run holds its repository's slot from its admission to
// its end: a merge that gives up fails, leaves the queue at once, and gives the
// slot back through the one release path.
func (o *orchestrator) pumpOnce(ctx context.Context, repo wsm.RepoKey) (bool, error) {
	front, promoted, admitted, err := o.admitFront(ctx, repo)
	if err != nil || !admitted {
		return false, err
	}
	if promoted != nil {
		// A MERGE PROMOTED FROM BEHIND THE HEAD RUNS ON ITS OWN GOROUTINE,
		// already started; the pump holds the slot for it until it ends.
		<-promoted.finished
		return true, nil
	}
	if err := o.runAdmitted(ctx, repo, front); err != nil {
		// ONE MERGE'S FAILURE IS NOT THE QUEUE'S. A run that reached its own
		// terminal has already recorded the failure at ERROR, published the
		// bubble's terminal and left the queue -- so the pump goes on to the
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
		o.leaveAdmission()
		if checkErr != nil || still {
			return true, err
		}
		o.deps.Log.Global().Debug("daemon.merge.pump", "a merge ended on its own terminal; the queue continues",
			dlog.Context{"repo": string(repo), "workspace": string(front), "error": err.Error()})
	}
	return true, nil
}

// isDraining reports whether the daemon's orderly exit has begun.
func (o *orchestrator) isDraining() bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	return o.draining
}

// enterAdmission registers one admission step against the shutdown drain,
// and reports false -- registering nothing -- once the daemon is draining.
// The caller ends the step with o.leaveAdmission().
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
	o.admissions++
	return true
}

// leaveAdmission ends one step enterAdmission registered, and releases a drain
// waiting on the last of them. A leave with nothing registered is an
// accounting defect and panics: the count would otherwise go negative and the
// drain would stop waiting for steps that are still running.
func (o *orchestrator) leaveAdmission() {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.admissions <= 0 {
		panic("merge: leaveAdmission with no admission step registered")
	}
	o.admissions--
	if o.admissions == 0 && o.admissionsIdle != nil {
		close(o.admissionsIdle)
		o.admissionsIdle = nil
	}
}

// admitFront admits the repository's queue front, as ONE admission step the
// shutdown drain waits for: every store read and the admission write happen
// inside it. It answers the admitted workspace, or the speculative run it
// promoted into the slot, or admitted=false when there is nothing to admit (a
// draining daemon, a paused or empty queue, a run already in flight, another
// daemon's lock). The repository's kernel lock is held from here
// (holdRepoLock) for as long as any run of its chain stands.
func (o *orchestrator) admitFront(ctx context.Context, repo wsm.RepoKey) (ids.WorkspaceID, *run, bool, error) {
	const op = "daemon.merge.admit"
	// The pump loop re-enters this every iteration: a drain that began while a
	// burst was mid-flight stops the burst here rather than after it has taken
	// the next entry's lease.
	if !o.enterAdmission() {
		return "", nil, false, nil
	}
	defer o.leaveAdmission()
	o.mu.Lock()
	busy := o.running[repo] != nil
	o.mu.Unlock()
	if busy {
		return "", nil, false, nil
	}
	// A PAUSED QUEUE ADVANCES NOTHING (owner ruling, 2026-10-12): merges
	// still enqueue, but nothing near its head runs -- no admission, no
	// promotion of the merge behind a head that ended, and no resume of a
	// merge a restart or the pause itself suspended; those wait, recorded,
	// until the queue is resumed.
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		return "", nil, false, err
	}
	if paused {
		return "", nil, false, nil
	}
	// A MERGE A RESTART INTERRUPTED GOES FIRST: it was already running, and
	// its entry is admitted already. Of several, the first in line is the
	// head; the others resume behind it in its chain (extendChain).
	if ws, resuming, err := o.resumeIn(ctx, repo); err != nil {
		return "", nil, false, err
	} else if resuming {
		taken, err := o.holdRepoLock(repo)
		if err != nil {
			return "", nil, false, err
		}
		if !taken {
			o.deps.Log.Global().Warn(op, "another daemon holds this repository's merge queue", dlog.Context{"repo": string(repo)})
			return "", nil, false, nil
		}
		return ws, nil, true, nil
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return "", nil, false, err
	}
	// THE MERGE RIGHT BEHIND A HEAD THAT ENDED IS PROMOTED: it was admitted
	// when it began rebasing behind the head.
	if r, promoted := o.promote(repo, entries); promoted {
		return r.ws, r, true, nil
	}
	// ONE MERGE PER REPOSITORY AT A TIME, ACROSS DAEMONS. An entry admitted
	// with no run here is a merge another daemon is driving, or one a
	// handover is moving between them: nothing is admitted behind it until it
	// ends or resumes here.
	for _, entry := range entries {
		if entry.State == wsm.MergeAdmitted {
			o.deps.Log.Global().Debug(op, "a merge admitted elsewhere holds this repository's slot; nothing is admitted behind it",
				dlog.Context{"repo": string(repo), "workspace": string(entry.Workspace)})
			return "", nil, false, nil
		}
	}
	front, found := nextInLine(entries)
	if !found {
		return "", nil, false, nil
	}
	// A WORKSPACE HANDED TO ANOTHER DAEMON HAS ITS MERGES THERE.
	if o.movedAway(front.Workspace) {
		o.deps.Log.Global().Debug(op, "the queue front's workspace moved to another daemon; it admits the merge there",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace)})
		return "", nil, false, nil
	}
	// A FRONT WHOSE BACKGROUND REBASE IS STILL RUNNING is admitted once it
	// settles back into line (rebasewaiting.go kicks the pump then).
	o.mu.Lock()
	held := o.runsByWorkspace[front.Workspace]
	rebasing := held != nil && held.background
	o.mu.Unlock()
	if rebasing {
		o.deps.Log.Global().Debug(op, "the queue front is rebasing in the background; it is admitted once that settles",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace)})
		return "", nil, false, nil
	}
	// A FRONT ALREADY STARTING BEHIND THE HEAD is its chain's: it is promoted
	// once its admission is written, never started a second time here.
	if o.inChain(repo, front.Workspace) {
		o.deps.Log.Global().Debug(op, "the queue front is starting in its chain; it is promoted once admitted",
			dlog.Context{"repo": string(repo), "workspace": string(front.Workspace)})
		return "", nil, false, nil
	}
	taken, err := o.holdRepoLock(repo)
	if err != nil {
		return "", nil, false, err
	}
	if !taken {
		o.deps.Log.Global().Warn(op, "another daemon holds this repository's merge queue", dlog.Context{"repo": string(repo)})
		return "", nil, false, nil
	}
	if err := o.deps.DB.AdmitMerge(ctx, repo, front.Workspace); err != nil {
		o.releaseRepoLockIfIdle(ctx, repo)
		return "", nil, false, err
	}
	return front.Workspace, nil, true, nil
}

// resumeIn answers the merge of one repository the boot recovery handed the
// pump to resume as its head.
//
// THE HEAD AFTER A RESTART IS THE FIRST ADMITTED ENTRY IN QUEUE ORDER, never
// whichever recovery path ran first. The boot recovery takes back the merges
// whose leases it finds at once, while a merge that was mid-handover comes
// back only when its workspace's adoption takes it (AdoptWorkspace), possibly
// after a merge behind it was already handed over. So when the first admitted
// entry is not yet a resume here -- and has no run here either -- nothing of
// the repository resumes until it is: its adoption's kick resumes it as the
// head, and the merges behind it resume in its chain (extendChain). A merge
// resumed ahead of it would have stranded it, its bubble showing a step with
// no run behind it.
func (o *orchestrator) resumeIn(ctx context.Context, repo wsm.RepoKey) (ids.WorkspaceID, bool, error) {
	const op = "daemon.merge.resume"
	o.mu.Lock()
	any := false
	for _, doc := range o.resumes {
		if doc.Repo == string(repo) {
			any = true
			break
		}
	}
	o.mu.Unlock()
	if !any {
		return "", false, nil
	}
	entries, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		return "", false, err
	}
	o.mu.Lock()
	for _, entry := range entries {
		if entry.State != wsm.MergeAdmitted {
			continue
		}
		if doc, ok := o.resumes[entry.Workspace]; ok && doc.Repo == string(repo) {
			o.mu.Unlock()
			return entry.Workspace, true, nil
		}
		running := o.runsByWorkspace[entry.Workspace] != nil || o.inChainLocked(repo, entry.Workspace)
		var waiting []string
		for ws, doc := range o.resumes {
			if doc.Repo == string(repo) {
				waiting = append(waiting, string(ws))
			}
		}
		o.mu.Unlock()
		if !running {
			sort.Strings(waiting)
			o.deps.Log.Global().Info(op, "the queue's first admitted merge is not taken back yet; nothing of the repository resumes until its workspace's adoption resumes it as the head", dlog.Context{
				"repo": string(repo), "head": string(entry.Workspace), "waiting": strings.Join(waiting, ",")})
		}
		return "", false, nil
	}
	defer o.mu.Unlock()
	// A RESUME WITH NO ADMITTED QUEUE ENTRY is answered as before: its run
	// reads the entry it lands and ends on the missing one, loudly.
	for ws, doc := range o.resumes {
		if doc.Repo == string(repo) {
			return ws, true, nil
		}
	}
	return "", false, nil
}

// nextInLine answers the first entry of a queue that is waiting in line: a
// requested merge is in nobody's line, and an admitted one is already running.
func nextInLine(entries []wsm.MergeQueueEntry) (wsm.MergeQueueEntry, bool) {
	for _, entry := range entries {
		if entry.State == wsm.MergeQueued {
			return entry, true
		}
	}
	return wsm.MergeQueueEntry{}, false
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
func queueTab(snapshot *frontendv1.FeedMergeQueue, st tabState) *frontendv1.FeedMergeTab {
	inner := &frontendv1.FeedMergeTabQueue{Queue: snapshot}
	if live, settled := st.badge(); live != nil {
		inner.State = &frontendv1.FeedMergeTabQueue_Live{Live: live}
	} else {
		inner.State = &frontendv1.FeedMergeTabQueue_Settled{Settled: settled}
	}
	return &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Queue{Queue: inner}}
}

// queueSpan is one in-line merge's wait: when it was queued and, once its run
// was admitted, when that run began. Every drawing of an in-line merge's
// times (its queue tab, its head's clock) reads it, so they agree on when the
// wait ended.
type queueSpan struct {
	queuedMS   int64
	admitted   bool
	admittedMS int64
}

// queueSpanOf resolves one in-line merge's wait. A zero queued time is an
// invariant violation (enqueuedMS).
//
// An ADMITTED entry whose run is not registered yet is mid-admission (the
// pump admitted it and the run has not taken its slot): there is no start to
// end the wait at, and nothing is drawn from it (false); the admission's own
// drawing follows the moment the run exists.
func (o *orchestrator) queueSpanOf(entry wsm.MergeQueueEntry) (queueSpan, bool, error) {
	queuedMS, err := enqueuedMS(entry)
	if err != nil {
		return queueSpan{}, false, err
	}
	if entry.State != wsm.MergeAdmitted {
		return queueSpan{queuedMS: queuedMS}, true, nil
	}
	r, running := o.runFor(entry.Workspace)
	if !running {
		return queueSpan{}, false, nil
	}
	return queueSpan{queuedMS: queuedMS, admitted: true, admittedMS: r.startedMS}, true, nil
}

// queueTabState resolves one queued merge's queue tab badge. THE WAIT BEGAN
// WHEN THE MERGE WAS QUEUED, so the tab starts at the entry's queued time, and
// it ends the moment the merge reached the front: its run's start, which
// admission stamps. The end is that one instant on every republish, so a
// settled queue tab's drawn run time stays fixed while the run moves on.
func (o *orchestrator) queueTabState(entry wsm.MergeQueueEntry) (tabState, bool, error) {
	span, drawn, err := o.queueSpanOf(entry)
	if err != nil || !drawn {
		return tabState{}, drawn, err
	}
	return tabState{startedMS: span.queuedMS, settled: span.admitted, endedMS: span.admittedMS}, true, nil
}

// headClockMS resolves the instant one in-line merge's head clock counts from
// (frontend.v1.FeedMergeRuntime): WHEN IT WAS QUEUED while it waits, so a
// queue change never restarts it, and WHEN ITS RUN WAS ADMITTED once it has
// left the queue, so the head shows only the time it has been worked on. A
// mid-admission entry's head is not redrawn (false); the run draws it the
// moment it starts.
func (o *orchestrator) headClockMS(entry wsm.MergeQueueEntry) (int64, bool, error) {
	span, clocked, err := o.queueSpanOf(entry)
	if err != nil || !clocked {
		return 0, clocked, err
	}
	if span.admitted {
		return span.admittedMS, true, nil
	}
	return span.queuedMS, true, nil
}

// queueStandings resolves each in-line entry's standing for the queue
// snapshot, in queue order: the FRONT's active tab and when it began, and for
// every entry behind it, that it waits and since when it was queued.
//
// The front's stage is its run's ACTIVE ROUND -- its label and the instant
// its ledger interval began, read whole -- so the snapshot's duration starts
// over at zero whenever the front's active tab changes. A front with no run
// (a paused queue, an admission not yet registered, a run already torn down)
// or whose run is still in its queue round is in its QUEUE stage, which began
// when it was queued: the same start its own queue tab carries.
//
// A missing queued time is an invariant violation (the column is NOT NULL and
// every enqueue stamps it), answered as an error; it is never sent as zero.
func (o *orchestrator) queueStandings(entries []wsm.MergeQueueEntry) ([]*frontendv1.FeedMergeQueueEntry, error) {
	standings := make([]*frontendv1.FeedMergeQueueEntry, len(entries))
	for i, entry := range entries {
		queuedMS, err := enqueuedMS(entry)
		if err != nil {
			return nil, err
		}
		if i > 0 {
			standings[i] = &frontendv1.FeedMergeQueueEntry{Status: &frontendv1.FeedMergeQueueEntry_Waiting{
				Waiting: &frontendv1.FeedMergeQueueWaiting{StageEnteredAtMs: queuedMS}}}
			continue
		}
		merging := &frontendv1.FeedMergeQueueMerging{ActiveTab: tabLabel(TabQueue, 1), StageEnteredAtMs: queuedMS}
		if r, running := o.runFor(entry.Workspace); running {
			if active := r.activeRound(); active.kind != "" && active.kind != TabQueue {
				merging = &frontendv1.FeedMergeQueueMerging{
					ActiveTab:        frontTabLabel(active),
					StageEnteredAtMs: active.started.UnixMilli(),
				}
			}
		}
		standings[i] = &frontendv1.FeedMergeQueueEntry{Status: &frontendv1.FeedMergeQueueEntry_Merging{Merging: merging}}
	}
	return standings, nil
}

// enqueuedMS answers when an entry was queued, in epoch milliseconds. A zero
// time is an invariant violation, never a stamp.
func enqueuedMS(entry wsm.MergeQueueEntry) (int64, error) {
	if entry.EnqueuedAt.IsZero() {
		return 0, fmt.Errorf("merge: the queue entry of %s in %s carries no queued time", entry.Workspace, entry.Repo)
	}
	return entry.EnqueuedAt.UnixMilli(), nil
}

// nowMS is the clock in the epoch milliseconds every drawn stamp uses.
func (o *orchestrator) nowMS() int64 { return o.deps.Now().UnixMilli() }
