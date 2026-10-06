package merge

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// This file is a merge RESUMED across a daemon restart: the run rebuilt from
// its progress record, and the reattachment to an agent turn the dead run had
// submitted.
//
// THE RESUMED RUN IS THE SAME MERGE. It holds the same lease (adopted at
// boot), so it draws into the same bubble, continues the same tab rounds, and
// releases the lease at its end like any run. The steps it resumes at read
// their own state out of git and out of the turn store -- never a guess from
// what the tree happens to hold.

// register makes a run the slot's holder and its workspace's run. A run
// registered once the daemon's exit has begun is suspended at once: the drain
// has already taken its snapshot, so nothing waits for this run's work and it
// must not start any.
func (o *orchestrator) register(r *run) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.running[r.repo] = r
	o.runsByWorkspace[r.ws] = r
	if o.draining {
		r.suspending = true
		r.git.stop()
	}
}

// startResumed rebuilds a merge a restart interrupted from its progress
// record and runs it on from the step the record names.
//
// EVERY FAILURE FROM HERE ENDS THE RUN THROUGH ITS ONE CLASSIFICATION (end).
// The lease is still held from before the restart, so a resumed merge that
// failed to come back before it had a run would refuse its workspace's every
// prompt with nothing to release it.
func (o *orchestrator) startResumed(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID, lock *repoLock, doc progressDoc) (*run, error) {
	lease, held, leaseErr := o.deps.DB.Lease(ctx, ws)
	runCtx, cancel := context.WithCancelCause(ctx)
	r := &run{
		o: o, ws: ws, repo: repo, lease: lease, lock: lock,
		ctx: runCtx, cancel: cancel, finished: make(chan struct{}),
		startedMS: doc.StartedMS, queuedMS: doc.QueuedMS,
		rounds:     map[string]int{},
		openRounds: map[string]tabRound{},
		git:        newGatedGit(o.deps.Git, o.deps.Now),
		resume:     &doc,
	}
	r.restoreFrom(doc)
	o.register(r)
	switch {
	case leaseErr != nil:
		err := fmt.Errorf("could not read the lease the merge resumes under: %w", leaseErr)
		r.end(runCtx, err)
		return r, err
	case !held || lease.Holder != wsm.HolderMerge:
		err := fmt.Errorf("the merge lease it was recorded under is no longer held")
		r.end(runCtx, err)
		return r, err
	}
	return r, r.resumeRun(runCtx)
}

// resumeRun is startResumed's body once the run exists.
func (r *run) resumeRun(ctx context.Context) error {
	const op = "daemon.merge.resume"
	o, ws := r.o, r.ws
	entry, err := o.entryOf(ctx, r.repo, ws)
	if err != nil {
		r.end(ctx, fmt.Errorf("could not read what the resumed merge lands: %w", err))
		return err
	}
	r.source = entry.Source
	job, _, err := o.deps.DB.CreationJob(ctx, ws)
	if err != nil {
		r.end(ctx, fmt.Errorf("could not read the requester's creation job: %w", err))
		return err
	}
	r.job = job
	policy, err := o.policyFor(ctx, ws)
	if err != nil {
		r.end(ctx, fmt.Errorf("could not resolve the repository's merge policy: %w", err))
		return err
	}
	r.policy = policy
	if release, ok, err := o.deps.Occupy(ws, holderMerge); err != nil {
		r.end(ctx, fmt.Errorf("could not take the session's occupancy: %w", err))
		return err
	} else if ok {
		r.releaseOccupancy = release
	}
	// THE BUBBLE IS THE ONE THE MERGE ALREADY HAD, drawn live again, and the
	// footer shows the step the merge stands on before that step speaks.
	r.head(nil)
	o.publish(ws, r.factsNow())
	if err := o.republishQueue(ctx, r.repo); err != nil {
		r.end(ctx, fmt.Errorf("could not publish the queue: %w", err))
		return err
	}
	o.log(ctx, ws).Info(op, "resumed a merge a restart interrupted, at the step it recorded", dlog.Context{
		"workspace": string(ws), "repo": string(r.repo), "lease": string(r.lease.ID), "step": r.resume.Step,
		"round": r.resume.Active.N, "turn": r.resume.Turn, "branch": r.subject.branch, "target": r.subject.targetDir})
	if r.resume.Step == TabQueue {
		// THE MERGE HAD NOT LEFT THE QUEUE: it waits for its workspaces to
		// fall free exactly as a fresh admission does.
		if err := r.awaitFree(ctx, ws); err != nil {
			r.end(ctx, err)
			return err
		}
		if r.subject.other != "" {
			if err := r.awaitFree(ctx, r.subject.other); err != nil {
				r.end(ctx, err)
				return err
			}
		}
	}
	return r.execute(ctx)
}

// factsNow is the run's standing footer facts.
func (r *run) factsNow() footer.MergeFacts {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.facts
}

// reattachTurn answers how an agent turn the dead run submitted ended,
// waiting for it when it still runs.
//
// THE TURN STORE SAYS WHERE THE TURN IS. A turn with a recorded close ended
// while the daemon was down; a recorded open turn runs in the shim that
// outlived the daemon, and is waited for; a turn still HELD in the prompt
// queue is delivered by the queue and waited for the same way. A turn no store
// knows never reached the queue before the restart -- the record was written,
// the submission was not -- and is submitted now, under the same turn id.
func (r *run) reattachTurn(ctx context.Context, turn ids.TurnID, text string, origin conversationv1.PromptOrigin) (wsm.TurnClose, error) {
	const op = "daemon.merge.resume"
	fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "turn": string(turn)}
	if turn == "" {
		return wsm.CloseFailed, fmt.Errorf("merge: the progress record names no turn to reattach to")
	}
	closes, err := r.o.deps.DB.TurnCloses(ctx, r.ws, []ids.TurnID{turn})
	if err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: reading whether the merge's turn ended: %w", err)
	}
	if closed, ok := closes[turn]; ok {
		r.o.log(ctx, r.ws).Info(op, "the merge's turn ended while the daemon was down; the merge goes on from its end",
			withField(fields, "close", closed.How.String()))
		return closed.How, nil
	}
	recorded, err := r.o.deps.DB.RecordedTurns(ctx, r.ws, []ids.TurnID{turn})
	if err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: reading whether the merge's turn was delivered: %w", err)
	}
	if recorded[turn] {
		r.o.log(ctx, r.ws).Info(op, "reattached to the merge's turn, still running in the session", fields)
		return r.o.deps.AwaitTurnEnd(ctx, r.ws, turn)
	}
	held, found, err := r.o.deps.DB.HeldPromptByTurn(ctx, turn)
	if err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: reading whether the merge's turn is held: %w", err)
	}
	if found && held.Tombstone == nil {
		r.o.log(ctx, r.ws).Info(op, "the merge's turn is still held in the prompt queue; the merge waits for it", fields)
		if err := r.ensureSession(ctx); err != nil {
			return wsm.CloseFailed, fmt.Errorf("merge: starting a session for the merge's held turn: %w", err)
		}
		return r.o.deps.AwaitTurnEnd(ctx, r.ws, turn)
	}
	r.o.log(ctx, r.ws).Info(op, "the merge's turn never reached the session before the restart; it is submitted now", fields)
	if err := r.ensureSession(ctx); err != nil {
		return wsm.CloseFailed, fmt.Errorf("merge: starting a session for the merge's turn: %w", err)
	}
	return r.submit(ctx, turn, text, origin)
}

// suspend lets go of what a run holds IN THIS PROCESS when the daemon's exit
// took it away, and leaves its lease, its queue entry and its progress record
// to the next boot, which resumes it. It writes nothing durable: the state
// client is closing.
func (r *run) suspend(ctx context.Context, cause error) {
	step := r.progressStep()
	fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "step": step}
	if cause != nil {
		fields["cause"] = cause.Error()
	}
	r.o.deps.Log.Global().Info("daemon.merge.suspend",
		"a merge stopped at a stopping point for the daemon's exit; the next boot resumes it at the step it recorded", fields)
	r.releaseInProcess(ctx)
}

// progressStep is the step the run's record last named.
func (r *run) progressStep() string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.prog.Step
}

// contradiction is a resume that found the tree contradicting the merge's own
// record. It fails the merge, naming exactly what contradicted what.
func (r *run) contradiction(ctx context.Context, what string) *outcome {
	summary := "the merge could not resume after the restart: " + what
	r.o.log(ctx, r.ws).Error("daemon.merge.resume", "the tree contradicts the merge's progress record; the merge fails", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "step": r.progressStep(), "contradiction": what})
	out := failedIn(footer.FailedOther, summary)
	return &out
}
