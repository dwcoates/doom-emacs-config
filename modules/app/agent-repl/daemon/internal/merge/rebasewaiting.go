package merge

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// This file is the WAITING MEMBERS' REBASE (owner ruling, 2026-10-12): every
// landing rebases each merge still waiting in line -- beyond the working
// window, whose two merges race to land (race.go) -- onto the target's new
// tip, with the same rebasing and conflict-resolution step the front takes:
// its conflicts are resolved by its own session through the usual prompts
// and recorded resolutions. A waiting member never runs tests; it is only
// kept close to the target, so that its turn at the front finds little left
// to replay.
//
// ONE AT A TIME, IN QUEUE ORDER. A landing never fans out a rebase and a
// conflict turn per member at once: one background rebase runs across the
// whole repository's queue, member by member. A landing during the pass asks
// for one more pass once this one ends; nothing is restarted mid-rebase.
//
// A MEMBER THAT IS BUSY IS SKIPPED, never waited for (owner ruling): a turn in
// flight, live detached work, a worktree with uncommitted changes or another
// branch checked out is the requester's own business, so that member keeps
// its base this round, said at INFO, and catches up at a later landing.
//
// THE REBASE HOLDS THE MERGE LEASE ONLY WHILE IT RUNS. A waiting sync merge's
// requester stays usable (no hold from enqueue); during its rebase the lease
// holds its prompts, because the rebase rewrites the worktree its session
// works in and its conflict turn runs in that session. The lease is released
// the moment the rebase settles, and the entry stays exactly where it stood
// in line. A background rebase records no progress: a restart in the middle
// simply leaves the member to catch up at the next landing.

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
	go func() {
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
	}()
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
	window := o.queueWindow()
	if len(line) <= window {
		return
	}
	for _, entry := range line[window:] {
		if o.isDraining() {
			return
		}
		if entry.State != wsm.MergeQueued || entry.Source.Kind == wsm.MergeSourceMergedUpstream {
			continue
		}
		o.rebaseWaitingMember(ctx, repo, entry)
	}
}

// rebaseWaitingMember runs one waiting member's background rebase to its end.
//
// THE RUN IS REGISTERED BY ITS WORKSPACE ONLY, never as its repository's
// running merge nor in its chain: it is no head, and the race to land and the
// admission never see it. The registration by workspace is what an eviction
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
	if err := o.deps.DB.OpenMergeLedger(ctx, r.ws, r.lease.ID); err != nil {
		return backgroundEnd{err: fmt.Errorf("could not open the merge ledger: %w", err)}
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
	log.Info(op, "a landing moved the target; the waiting merge rebases onto its new tip", withField(fields, "onto", tip))
	gaveUp, err := r.rebase(ctx, tip, targetBranch)
	if err == nil && gaveUp == nil {
		log.Info(op, "the waiting merge stands on the target's new tip; it keeps its place in line", fields)
	}
	return backgroundEnd{gaveUp: gaveUp, err: err}
}

// backgroundEnd is how a waiting member's rebase ended: an error, an outcome
// its conflict resolution gave up on, or neither when it settled.
type backgroundEnd struct {
	gaveUp *outcome
	err    error
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
	case end.err != nil:
		r.end(ctx, end.err)
	case end.gaveUp != nil:
		_ = r.finish(settle, *end.gaveUp)
	default:
		r.keepInLine(settle)
	}
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
	if o.draining || o.runsByWorkspace[r.ws] != nil || o.inChainLocked(r.repo, r.ws) || o.transferred[r.ws] {
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
