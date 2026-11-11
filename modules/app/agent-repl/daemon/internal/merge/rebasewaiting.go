package merge

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/wsm"
)

// This file is the WAITING MEMBERS' REBASE (owner rulings, 2026-10-12 and
// 2026-11-11): every landing rebases each merge still waiting in line behind
// the head onto the target's new tip. A waiting member never runs tests; it is
// only kept close to the target, so that its turn at the head finds little
// left to replay.
//
// THE REBASE RUNS GIT ONLY. A replayed commit that conflicts is no prompt: the
// rebase is aborted, the branch keeps its base, and the member steps aside out
// of line for the RESOLVING-CONFLICTS SLOT (wsm.MergeAsideWaitingConflicts),
// holding it when nobody does or waiting for it in order (remediate.go). The
// slot's holder runs its RESOLVING REBASE (resolveWaiting): the same rebase,
// its conflicts resolved by its own session through the usual prompts and
// recorded resolutions, after which it returns to the line at the first
// waiting place and the slot passes on.
//
// ONE AT A TIME, IN QUEUE ORDER. A landing never fans out a rebase per member
// at once: one background rebase runs across the whole repository's queue,
// member by member. A landing during the pass asks for one more pass once
// this one ends; nothing is restarted mid-rebase.
//
// A MEMBER THAT IS BUSY IS SKIPPED, never waited for (owner ruling): a turn in
// flight, live detached work, a worktree with uncommitted changes or another
// branch checked out is the requester's own business, so that member keeps
// its base this round, said at INFO, and catches up at a later landing.
//
// THE REBASE HOLDS THE MERGE LEASE ONLY WHILE IT RUNS. A waiting sync merge's
// requester stays usable (no hold from enqueue); during its rebase the lease
// holds its prompts, because the rebase rewrites the worktree its session
// works in. The lease is released the moment the rebase settles. A background
// rebase records no progress: a restart in the middle simply leaves the
// member to catch up at the next landing, and a resolving rebase the restart
// cut runs again from its start once the boot takes its slot back.

// rebaseWaiting asks for a pass over repo's waiting members, once a merge
// landed: it starts one, or asks the pass running now for one more.
func (o *orchestrator) rebaseWaiting(repo wsm.RepoKey) {
	o.mu.Lock()
	if o.draining {
		o.mu.Unlock()
		return
	}
	if o.waitingPass[repo] {
		o.waitingAgain[repo] = true
		o.mu.Unlock()
		return
	}
	o.waitingPass[repo] = true
	o.mu.Unlock()
	o.live.Add(1)
	inflight.Standing(func() {
		defer o.live.Done()
		ctx := context.Background()
		for {
			o.rebaseWaitingPass(ctx, repo)
			if o.onWaitingPass != nil {
				o.onWaitingPass(repo)
			}
			o.mu.Lock()
			again := o.waitingAgain[repo] && !o.draining
			delete(o.waitingAgain, repo)
			if !again {
				delete(o.waitingPass, repo)
				o.mu.Unlock()
				return
			}
			o.mu.Unlock()
		}
	})
}

// rebaseWaitingPass rebases each waiting member of repo in queue order, one
// at a time.
func (o *orchestrator) rebaseWaitingPass(ctx context.Context, repo wsm.RepoKey) {
	const op = "daemon.merge.rebase_waiting"
	stored, err := o.deps.DB.MergeQueue(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read the queue to rebase its waiting merges", dlog.Context{"repo": string(repo), "error": err.Error()})
		return
	}
	paused, err := o.deps.DB.MergeQueuePaused(ctx, repo)
	if err != nil {
		o.deps.Log.Global().Error(op, "could not read whether the queue is paused to rebase its waiting merges", dlog.Context{"repo": string(repo), "error": err.Error()})
		return
	}
	if paused {
		o.deps.Log.Global().Info(op, "the queue is paused; its waiting merges are not rebased", dlog.Context{"repo": string(repo)})
		return
	}
	line := inLine(stored)
	if len(line) <= 1 {
		return
	}
	for _, entry := range line[1:] {
		if o.isDraining() {
			return
		}
		if entry.State != wsm.MergeQueued {
			continue
		}
		o.rebaseWaitingMember(ctx, repo, entry)
	}
}

// rebaseWaitingMember runs one waiting member's background rebase to its end.
//
// THE RUN IS REGISTERED BY ITS WORKSPACE ONLY, never as its repository's
// running merge nor in its chain: it is no head, and the admission never sees
// it. The registration by workspace is what an eviction
// and the shutdown drain find it by, and what keeps the admission from
// starting the member while its rebase runs.
func (o *orchestrator) rebaseWaitingMember(ctx context.Context, repo wsm.RepoKey, entry wsm.MergeQueueEntry) {
	const op = "daemon.merge.rebase_waiting"
	ws := entry.Workspace
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	if !o.deps.Freeness.Free(ws) {
		fields["blockers"] = o.deps.Freeness.Blockers(ws)
		log.Info(op, "a waiting merge's workspace is busy; it keeps its base this round and catches up at a later landing", fields)
		return
	}
	queuedMS, err := enqueuedMS(entry)
	if err != nil {
		log.Error(op, "could not read when a waiting merge was queued; it keeps its base this round", withField(fields, "error", err.Error()))
		return
	}
	runCtx, cancel := context.WithCancelCause(context.Background())
	defer cancel(nil)
	r := o.newBackgroundRun(runCtx, cancel, repo, ws, queuedMS)
	r.source = entry.Source
	if !o.registerBackground(r) {
		return
	}
	defer close(r.finished)
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, ws, o.mintLedger(ws), wsm.HolderMerge, wsm.PolicyHold)
	// A WORKSPACE ANOTHER HOLDER HOLDS IS BUSY, as one with work in flight is
	// above: freeness reads the session, not the lease, so a shim restart's
	// lease passes it (2026-10-25: a skills bounce held a waiting merge's
	// workspace, and the skip was stated at ERROR).
	if other, held := heldByAnother(err); held {
		fields["holder"], fields["lease"] = other.Holder.String(), string(other.Lease)
		log.Info(op, "a waiting merge's workspace is held by another lease; it keeps its base this round and catches up at a later landing", fields)
		o.unregisterBackground(r)
		return
	}
	if err != nil {
		log.Error(op, "could not take the merge lease to rebase a waiting merge", withField(fields, "error", err.Error()))
		o.unregisterBackground(r)
		return
	}
	r.lease = lease
	o.deps.Queue.OnLeaseChanged(ws)
	r.settleBackground(runCtx, r.rebaseInBackground(runCtx, fields))
}

// rebaseInBackground is a waiting member's rebase proper.
func (r *run) rebaseInBackground(ctx context.Context, fields dlog.Context) backgroundEnd {
	const op = "daemon.merge.rebase_waiting"
	o, log := r.o, r.o.log(ctx, r.ws)
	subject, err := o.resolveSubject(ctx, r)
	if err != nil {
		return backgroundEnd{err: fmt.Errorf("could not resolve what the waiting merge lands: %w", err)}
	}
	r.mu.Lock()
	r.subject = subject
	r.mu.Unlock()
	if skip := r.backgroundSkip(ctx); skip != "" {
		log.Info(op, "a waiting merge's worktree is not ready to rebase; it keeps its base this round", withField(fields, "why", skip))
		return backgroundEnd{}
	}
	if err := r.takeSession(ctx); err != nil {
		return backgroundEnd{err: err}
	}
	if err := r.openLedger(ctx); err != nil {
		return backgroundEnd{err: err}
	}
	targetBranch, err := r.git.CurrentBranch(ctx, subject.targetDir)
	if err != nil {
		return backgroundEnd{err: fmt.Errorf("merge: reading the branch checked out in %s: %w", subject.targetDir, err)}
	}
	tip, err := r.git.ResolveRef(ctx, subject.targetDir, "HEAD")
	if err != nil {
		return backgroundEnd{err: fmt.Errorf("merge: resolving %s's tip: %w", subject.targetDir, err)}
	}
	// A BRANCH THE TARGET ALREADY CONTAINS is left to the front, which
	// concludes it with nothing to land (alreadyOn).
	contained, err := r.git.IsAncestor(ctx, subject.dir, subject.branch, tip)
	if err != nil {
		return backgroundEnd{err: fmt.Errorf("merge: asking whether %s is already on %s: %w", subject.branch, targetBranch, err)}
	}
	if contained {
		return backgroundEnd{}
	}
	if r.resolving {
		log.Info(op, "the merge holds the resolving-conflicts slot; it rebases onto the target's tip, its session resolving the conflicts", withField(fields, "onto", tip))
	} else {
		log.Info(op, "a landing moved the target; the waiting merge rebases onto its new tip", withField(fields, "onto", tip))
	}
	gaveUp, err := r.rebase(ctx, tip, targetBranch)
	var conflicted *waitingConflict
	if errors.As(err, &conflicted) {
		return backgroundEnd{conflict: conflicted}
	}
	if err == nil && gaveUp == nil {
		log.Info(op, "the waiting merge stands on the target's new tip", fields)
	}
	return backgroundEnd{gaveUp: gaveUp, err: err}
}

// waitingConflict is the end of a git-only background rebase that met a
// conflict: the commit it conflicted on and the files.
type waitingConflict struct {
	commit string
	files  []string
}

func (c *waitingConflict) Error() string {
	return fmt.Sprintf("merge: the waiting merge's rebase conflicted on %s in %d file(s)", short(c.commit), len(c.files))
}

// backgroundEnd is how a waiting member's rebase ended: an error, an outcome
// its conflict resolution gave up on, a conflict a git-only rebase met, or
// none of them when it settled.
type backgroundEnd struct {
	gaveUp   *outcome
	err      error
	conflict *waitingConflict
}

// settleBackground lands a waiting member's rebase on its one end.
//
// THE DAEMON'S EXIT PUTS THE BRANCH BACK. A background rebase records nothing
// to resume from, so one the exit cut is aborted: the branch keeps its base
// and the member catches up at a later landing. Every other failure, and a
// conflict its session gave up on, ends the merge as it would at the front,
// because the rebase is the same step.
func (r *run) settleBackground(ctx context.Context, end backgroundEnd) {
	settle := context.WithoutCancel(ctx)
	switch {
	case r.exiting():
		r.standDownBackground(settle)
	case end.conflict != nil:
		r.stepAsideWaiting(settle, end.conflict)
	case end.err != nil:
		r.end(ctx, end.err)
	case end.gaveUp != nil:
		_ = r.finish(settle, *end.gaveUp)
	case r.resolving:
		r.giveBack(settle)
		if err := r.o.returnRemediated(settle, r.ws, r.repo); err != nil {
			r.o.log(settle, r.ws).Error(opRemediate, "could not return the merge whose resolving rebase settled to the line; it stays out of line, holding its slot, until the next boot",
				dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
		}
	default:
		r.keepInLine(settle)
	}
}

// stepAsideWaiting ends a git-only background rebase that met CONFLICT: the
// rebase is aborted so the branch keeps its base, the merge gives back what
// the rebase held, and it steps aside out of line for the resolving-conflicts
// slot, holding it or waiting for it (wsm.StepAsideMerge,
// MergeAsideWaitingConflicts). A step that fails is recorded at ERROR, and
// the merge keeps its base and its place, meeting the conflict at the head.
func (r *run) stepAsideWaiting(ctx context.Context, conflict *waitingConflict) {
	const op = "daemon.merge.rebase_waiting"
	o, log := r.o, r.o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "repo": string(r.repo), "commit": conflict.commit, "files": len(conflict.files)}
	if err := r.git.AbortRebase(ctx, r.subject.dir); err != nil {
		log.Error(op, "could not abort the waiting merge's conflicted rebase; it is left as git left it", withField(fields, "error", err.Error()))
		r.giveBack(ctx)
		return
	}
	r.giveBack(ctx)
	var slot wsm.MergeSlot
	if err := o.lineWrite(r.repo, func() error {
		var err error
		slot, err = o.deps.DB.StepAsideMerge(ctx, r.repo, r.ws, wsm.MergeAsideWaitingConflicts)
		return err
	}); err != nil {
		log.Error(op, "could not step the conflicted waiting merge aside; it keeps its base and its place, and meets the conflict at the head", withField(fields, "error", err.Error()))
		return
	}
	fields["slot"] = slot.String()
	o.registerRemediation(r.ws, &remediation{repo: r.repo, waiting: true})
	if slot.Awaiting() {
		log.Info(op, "the waiting merge's rebase conflicted; it was aborted, and the merge waits out of line for the resolving-conflicts slot, in order", fields)
	} else {
		log.Info(op, "the waiting merge's rebase conflicted; it was aborted, and the merge holds the resolving-conflicts slot", fields)
	}
	if err := o.republishQueue(ctx, r.repo); err != nil {
		log.Error(op, "could not republish the queue", withField(fields, "error", err.Error()))
	}
	o.armRepairs(ctx, r.repo)
	o.kick(r.repo)
}

// resolveWaiting is the resolving-conflicts slot's work for WS, a waiting
// merge whose background rebase conflicted: its RESOLVING REBASE, a background
// rebase whose session resolves its conflicts in place, after which the merge
// returns to the line (settleBackground). A workspace that is busy or held by
// another lease cannot be rebased now: the merge returns to the line at once,
// recorded at INFO, giving the slot on, and meets the conflict at the head.
func (o *orchestrator) resolveWaiting(ctx context.Context, repo wsm.RepoKey, ws ids.WorkspaceID) {
	const op = "daemon.merge.rebase_waiting"
	log := o.log(ctx, ws)
	fields := dlog.Context{"workspace": string(ws), "repo": string(repo)}
	settle := context.WithoutCancel(ctx)
	giveUp := func(message string) {
		log.Info(op, message, fields)
		if err := o.returnRemediated(settle, ws, repo); err != nil {
			log.Error(op, "could not return the merge to the line; it stays out of line, holding its slot, until the next boot", withField(fields, "error", err.Error()))
		}
	}
	entry, err := o.entryOf(ctx, repo, ws)
	if err != nil {
		log.Error(op, "could not read the entry of the merge holding the resolving-conflicts slot; it stays out of line until the next boot", withField(fields, "error", err.Error()))
		return
	}
	if !o.deps.Freeness.Free(ws) {
		fields["blockers"] = o.deps.Freeness.Blockers(ws)
		giveUp("the merge holding the resolving-conflicts slot has a busy workspace; it returns to the line and meets the conflict at the head")
		return
	}
	queuedMS, err := enqueuedMS(entry)
	if err != nil {
		log.Error(op, "could not read when the merge holding the resolving-conflicts slot was queued; it stays out of line until the next boot", withField(fields, "error", err.Error()))
		return
	}
	runCtx, cancel := context.WithCancelCause(context.Background())
	defer cancel(nil)
	r := o.newBackgroundRun(runCtx, cancel, repo, ws, queuedMS)
	r.resolving = true
	r.source = entry.Source
	if !o.registerBackground(r) {
		giveUp("the merge holding the resolving-conflicts slot has another run in this daemon; it returns to the line")
		return
	}
	defer close(r.finished)
	lease, err := o.deps.DB.AcquireLeaseAs(ctx, ws, o.mintLedger(ws), wsm.HolderMerge, wsm.PolicyHold)
	if other, held := heldByAnother(err); held {
		fields["holder"], fields["lease"] = other.Holder.String(), string(other.Lease)
		o.unregisterBackground(r)
		giveUp("the merge holding the resolving-conflicts slot has its workspace held by another lease; it returns to the line and meets the conflict at the head")
		return
	}
	if err != nil {
		o.unregisterBackground(r)
		log.Error(op, "could not take the merge lease for the resolving rebase; the merge stays out of line until the next boot", withField(fields, "error", err.Error()))
		return
	}
	r.lease = lease
	o.deps.Queue.OnLeaseChanged(ws)
	r.settleBackground(runCtx, r.rebaseInBackground(runCtx, fields))
}

// standDownBackground gives back what a waiting member's rebase holds when
// the daemon's exit cut it: the rebase standing in its worktree is aborted,
// through the git client itself because the run's own gate is stopped.
func (r *run) standDownBackground(ctx context.Context) {
	const op = "daemon.merge.rebase_waiting"
	log := r.o.log(ctx, r.ws)
	fields := dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID)}
	r.mu.Lock()
	dir := r.subject.dir
	r.mu.Unlock()
	if dir != "" {
		standing, err := r.o.deps.Git.RebaseInProgress(ctx, dir)
		switch {
		case err != nil:
			log.Error(op, "could not read whether the cut rebase of a waiting merge stands; it is left as git left it", withField(fields, "error", err.Error()))
		case standing:
			if err := r.o.deps.Git.AbortRebase(ctx, dir); err != nil {
				log.Error(op, "could not abort the cut rebase of a waiting merge; it is left as git left it", withField(fields, "error", err.Error()))
			} else {
				log.Info(op, "the daemon's exit cut a waiting merge's rebase; it was aborted, and the branch keeps its base", fields)
			}
		}
	}
	r.giveBack(ctx)
}

// registerBackground registers a waiting member's rebase by its workspace,
// and refuses one the drain or a merge already running there would race.
func (o *orchestrator) registerBackground(r *run) bool {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.draining || o.runsByWorkspace[r.ws] != nil || o.inChainLocked(r.repo, r.ws) || o.isTakenAwayLocked(r.ws) {
		return false
	}
	o.runsByWorkspace[r.ws] = r
	return true
}

// unregisterBackground retires a waiting member's registration.
func (o *orchestrator) unregisterBackground(r *run) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if o.runsByWorkspace[r.ws] == r {
		delete(o.runsByWorkspace, r.ws)
	}
}

// backgroundSkip answers why the member's worktree cannot be rebased now, or
// "" when it can: uncommitted changes, or another branch checked out, are the
// requester's own and are never touched.
func (r *run) backgroundSkip(ctx context.Context) string {
	clean, err := r.git.IsClean(ctx, r.subject.dir)
	if err != nil {
		return fmt.Sprintf("could not read whether the worktree is clean: %v", err)
	}
	if !clean {
		return "the worktree has uncommitted changes"
	}
	checkedOut, err := r.git.CurrentBranch(ctx, r.subject.dir)
	if err != nil {
		return fmt.Sprintf("could not read the branch checked out: %v", err)
	}
	if checkedOut != r.subject.branch {
		return fmt.Sprintf("the worktree has %q checked out, not %s", checkedOut, r.subject.branch)
	}
	return ""
}

// keepInLine ends a background rebase that settled: the member gives back what
// it held, and its entry stands where it stood, its bubble redrawn with the
// queue.
func (r *run) keepInLine(ctx context.Context) {
	const op = "daemon.merge.rebase_waiting"
	r.giveBack(ctx)
	if err := r.o.republishQueue(ctx, r.repo); err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not republish the queue", dlog.Context{"repo": string(r.repo), "error": err.Error()})
	}
	if r.o.onWaitingRebased != nil {
		r.o.onWaitingRebased(r.ws)
	}
	r.o.kick(r.repo)
}

// giveBack releases the occupancy, the lease and the registration a waiting
// member's rebase holds.
func (r *run) giveBack(ctx context.Context) {
	const op = "daemon.merge.rebase_waiting"
	if r.releaseOccupancy != nil {
		r.releaseOccupancy()
	}
	if err := r.o.deps.DB.ReleaseLease(ctx, r.lease.ID); err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not release the lease of a waiting merge's rebase", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	r.o.unregisterBackground(r)
}
