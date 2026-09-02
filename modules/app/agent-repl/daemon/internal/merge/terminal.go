package merge

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

// This file holds a merge's THREE DISTINCT ENDS and the teardown they share.
//
// The ends have distinct causes and must not be collapsed: ABANDONED is a merge
// taken off the queue before it ever ran (evicted by the operator, or released
// by the user's answer to the interrupt offer); FAILED is a run that started and
// gave up; MERGED is a run that landed. Only the last two have a teardown.
//
// THE ORDER OF TEARDOWN IS LOAD-BEARING. The terminal is published FIRST, then
// the lease is released, then the worktree is removed, then the displaced turn
// is resubmitted, and only then does the self-reload fire. Removing the worktree
// before the terminal would delete the tree a reader is still looking at, and
// firing the self-reload before the release would bounce the daemon while it
// still held a lease it would then have to recover.

// finish lands a completed run on its terminal and tears it down.
func (r *run) finish(ctx context.Context, out outcome) error {
	const op = "daemon.merge.finish"
	log := r.o.log(ctx, r.ws)
	endedMS := r.o.nowMS()

	if out.failed != "" {
		r.head(&frontendv1.FeedMergeError{
			EndedAtMs: endedMS,
			Reason:    &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: out.failed}},
		})
		r.facts(StateFailed, "")
		facts, _ := r.o.Facts(r.ws)
		facts.Detail = out.failed
		r.o.publish(r.ws, facts)
		log.Error(op, "a merge failed", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "summary": out.failed, "commit": out.landed})
	} else {
		r.head(&frontendv1.FeedMergeSuccess{EndedAtMs: endedMS, Commit: out.landed})
		r.facts(StateMerged, "")
		if err := r.o.deps.DB.SetMergedAt(ctx, r.ws, r.o.deps.Now()); err != nil {
			log.Error(op, "could not stamp the merge's landing", dlog.Context{"workspace": string(r.ws), "error": err.Error()})
		}
		if err := r.o.deps.DB.SetClosed(ctx, r.ws, true); err != nil {
			log.Error(op, "could not close the merged workspace", dlog.Context{"workspace": string(r.ws), "error": err.Error()})
		}
		// THE ROSTER'S DURABLE HALF MOVED: merged_at and closed are what put
		// the row under `recently_merged`, and nothing else republishes them.
		if r.o.deps.PublishRegistry != nil {
			if err := r.o.deps.PublishRegistry(ctx); err != nil {
				log.Error(op, "could not republish the roster after the landing",
					dlog.Context{"workspace": string(r.ws), "error": err.Error()})
			}
		}
		log.Debug(op, "a merge landed", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "commit": out.landed, "commits": len(out.commits)})
	}
	r.teardown(ctx, out)
	return nil
}

// abort ends a run that could not continue. It is the FAILED end reached by an
// error rather than by a verdict, and it takes the same teardown.
func (r *run) abort(ctx context.Context, summary string) {
	r.o.log(ctx, r.ws).Error("daemon.merge.abort", "a merge could not continue", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "summary": summary})
	if r.lease.ID != "" {
		r.head(&frontendv1.FeedMergeError{
			EndedAtMs: r.o.nowMS(),
			Reason:    &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: summary}},
		})
	}
	r.facts(StateFailed, "")
	facts, _ := r.o.Facts(r.ws)
	facts.Detail = summary
	r.o.publish(r.ws, facts)
	r.teardown(ctx, outcome{failed: summary})
}

// teardown releases everything a run held, in the one order that is safe, and
// fires the self-reload last.
func (r *run) teardown(ctx context.Context, out outcome) {
	const op = "daemon.merge.teardown"
	log := r.o.log(ctx, r.ws)

	// The session's rows go back to the root feed the moment the bubble stops
	// being where its output belongs.
	r.o.deps.Feed.SetOutputAddress(r.ws, nil)
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	if err := r.o.deps.DB.ReleaseLease(ctx, r.lease.ID); err != nil {
		log.Error(op, "could not release the merge lease", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)

	if err := r.o.deps.DB.RemoveMergeQueueEntry(ctx, r.repo, r.ws, terminalCause(out)); err != nil {
		log.Error(op, "could not drop the finished merge's queue entry", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
	}
	r.o.mu.Lock()
	delete(r.o.running, r.repo)
	delete(r.o.runsByWorkspace, r.ws)
	delete(r.o.repoOf, r.ws)
	// The bubble's ledger identity retires with the merge it addressed; a
	// later merge of the same workspace gets a bubble of its own.
	delete(r.o.ledgerOf, r.ws)
	r.o.mu.Unlock()
	r.o.clearOffer(r.ws)

	// The worktree goes only after the terminal was published, and only for a
	// merge that landed: a failed merge's branch still holds work.
	if out.failed == "" && out.landed != "" {
		if err := r.o.deps.Git.RemoveWorktree(ctx, string(r.repo), r.job.Layout.SourceDir); err != nil {
			log.Error(op, "could not remove the merged worktree", dlog.Context{
				"workspace": string(r.ws), "worktree": r.job.Layout.SourceDir, "error": err.Error()})
		}
	}
	r.resubmitDisplaced(ctx)
	if err := r.lock.Release(); err != nil {
		log.Error(op, "could not release the repository's queue lock", dlog.Context{
			"repo": string(r.repo), "error": err.Error()})
	}
	if err := r.o.republishQueue(ctx, r.repo); err != nil {
		log.Error(op, "could not republish the queue", dlog.Context{"repo": string(r.repo), "error": err.Error()})
	}
	r.selfReload(ctx, out)
	r.o.kick(r.repo)
}

// terminalCause names why a queue entry was dropped, which the queue's own log
// record keeps.
func terminalCause(out outcome) string {
	if out.failed != "" {
		return "failed"
	}
	return "merged"
}

// resubmitDisplaced puts back the user turn the merge displaced, EXACTLY ONCE.
// The capture is durable, so the resubmission survives a bounce; the record is
// cleared by the submission itself, so a second teardown cannot double it.
func (r *run) resubmitDisplaced(ctx context.Context) {
	if r.displaced == nil {
		return
	}
	turn := *r.displaced
	r.displaced = nil
	open, err := r.o.deps.DB.OpenTurns(ctx, r.ws)
	if err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.resubmit", "could not read the displaced turn",
			dlog.Context{"workspace": string(r.ws), "turn": string(turn), "error": err.Error()})
		return
	}
	for _, t := range open {
		if t.ID != turn || !t.Displaced {
			continue
		}
		if _, err := r.o.deps.Queue.Submit(ctx, promptqueue.Submission{
			WS: r.ws, Turn: wsm.NewTurnID(), Said: saidText(t.Text),
			Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME,
		}); err != nil {
			r.o.log(ctx, r.ws).Error("daemon.merge.resubmit", "could not resubmit the displaced turn",
				dlog.Context{"workspace": string(r.ws), "turn": string(turn), "error": err.Error()})
			return
		}
		if err := r.o.deps.DB.CloseTurn(ctx, turn, r.o.deps.Now(), wsm.CloseCompleted); err != nil {
			r.o.log(ctx, r.ws).Error("daemon.merge.resubmit", "could not retire the displaced turn's record",
				dlog.Context{"workspace": string(r.ws), "turn": string(turn), "error": err.Error()})
		}
		r.o.log(ctx, r.ws).Debug("daemon.merge.resubmit", "resubmitted the displaced turn",
			dlog.Context{"workspace": string(r.ws), "turn": string(turn)})
		return
	}
}

// selfReload fires the daemon's own redeploy, and ONLY for a merge that landed
// in the daemon's OWN CHECKOUT. A sibling worktree of the same repository is
// excluded: it shares the common dir but is not the tree the running binary was
// built from.
func (r *run) selfReload(ctx context.Context, out outcome) {
	const op = "daemon.merge.self_reload"
	if out.failed != "" || out.landed == "" || !r.selfCheckout || len(out.commits) == 0 {
		return
	}
	r.o.log(ctx, r.ws).Debug(op, "triggering the self-reload for a merge into this daemon's own checkout",
		dlog.Context{"workspace": string(r.ws), "commit": out.landed, "commits": len(out.commits)})
	if err := r.o.deps.Rollout.Trigger(ctx, out.commits); err != nil {
		r.o.log(ctx, r.ws).Error(op, "the self-reload trigger failed",
			dlog.Context{"workspace": string(r.ws), "error": err.Error()})
	}
}

// publishAbandoned draws the ABANDONED terminal: a merge taken off the queue
// before it ever reached the front. It has no commit, no failure and no
// teardown, because nothing of it ever ran.
// ledger is the bubble's ledger identity, captured before the caller dropped
// it: an empty one means no bubble was ever drawn and there is nothing to end.
//
// THE ABANDONED ARM IS THE ONLY EXPRESSIBLE CAUSE. `FeedMergeError` carries
// `failed` and `abandoned` and nothing else, so an evict, a dequeue release and
// a run's own give-up all end here with the same arm; distinguishing them is a
// landing-7 shape, and the daemon states what it CAN state rather than
// inventing an arm or leaving the bubble unterminated.
func (o *orchestrator) publishAbandoned(ctx context.Context, ws ids.WorkspaceID, ledger ids.LeaseID, summary string) {
	o.deps.Log.Global().Debug("daemon.merge.abandoned", "a merge left the queue without running",
		dlog.Context{"workspace": string(ws), "summary": summary})
	if ledger != "" {
		label := o.abandonedLabel(ctx, ws)
		o.deps.Feed.UpsertSynthesized(ws, feedid.Feed{Root: true}, headRow(ws, ledger, label, o.nowMS(),
			&frontendv1.FeedMergeError{
				EndedAtMs: o.nowMS(),
				// FeedMergeAbandoned is an EMPTY message: the cause has
				// nowhere to ride on the wire, so the summary stays in the
				// log record above and the arm carries only itself.
				Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{}},
			}))
	}
	// The surfaces come AFTER the terminal, in the teardown's own order: the
	// footer leaves its merging state and the roster sheds every merge arm
	// only once the bubble a reader is looking at has ended.
	o.forget(ws)
}

// abandonedLabel is the head's branch line for a merge that never ran. The
// geometry is read where it is available and the workspace's own name stands in
// where it is not: an unreadable label must not stop the bubble from ending.
func (o *orchestrator) abandonedLabel(ctx context.Context, ws ids.WorkspaceID) string {
	job, err := o.layoutFor(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Debug("daemon.merge.abandoned",
			"the abandoned merge's geometry could not be read for its label",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
		return string(ws)
	}
	return branchLabel(job.Layout.SourceBranch, job.Layout.TargetDir)
}

// OnInterrupt raises the dequeue offer for a workspace whose merge is queued.
// An interrupt no longer silently yanks a merge off the queue: the daemon asks.
func (o *orchestrator) OnInterrupt(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.merge.interrupt"
	if _, err := o.queueOf(ctx, ws); err != nil {
		return
	}
	record, err := o.deps.DB.Workspace(ctx, ws)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not compose the dequeue offer",
			dlog.Context{"workspace": string(ws), "error": err.Error()})
		return
	}
	o.mu.Lock()
	o.offers[ws] = true
	o.mu.Unlock()
	o.deps.Holds.SetOffer(ws, dequeueOffer(record.Name))
	o.log(ctx, ws).Debug(op, "raised the merge dequeue offer", dlog.Context{"workspace": string(ws)})
}

// AnswerDequeue answers the tray's offer: keep the queue slot, or release it.
func (o *orchestrator) AnswerDequeue(ctx context.Context, ws ids.WorkspaceID, keep bool) error {
	const op = "daemon.merge.answer_dequeue"
	o.mu.Lock()
	standing := o.offers[ws]
	o.mu.Unlock()
	if !standing {
		return refuse(ArmNoOfferStanding, ws, "no merge dequeue offer is standing for this workspace")
	}
	if keep {
		o.clearOffer(ws)
		o.log(ctx, ws).Debug(op, "kept a queued merge's slot", dlog.Context{"workspace": string(ws)})
		return nil
	}
	o.log(ctx, ws).Warn(op, "releasing a queued merge's slot on the user's answer", dlog.Context{"workspace": string(ws)})
	return o.dropQueued(ctx, ws, "dequeued", "the user released this merge's queue slot")
}

// clearOffer retires the dequeue offer, whether it was answered, superseded by
// the merge's own terminal, or ended by the merge leaving the queue.
func (o *orchestrator) clearOffer(ws ids.WorkspaceID) {
	o.mu.Lock()
	had := o.offers[ws]
	delete(o.offers, ws)
	o.mu.Unlock()
	if had {
		o.deps.Holds.SetOffer(ws, nil)
	}
}

// RouteParked delivers a submission that arrived while the lease stands parked.
func (o *orchestrator) RouteParked(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid) error {
	r, ok := o.runFor(ws)
	if !ok {
		return errNoRun
	}
	select {
	case r.guidance <- said:
	case <-ctx.Done():
		return ctx.Err()
	}
	select {
	case err := <-r.answered:
		return err
	case <-ctx.Done():
		return ctx.Err()
	}
}
