package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ctxend"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is the Emacs-repo method: the process a merge goes through once it
// is popped from the queue's head (owner, 2026-09-29/30).
//
//	rebasing   replay the branch onto the target's tip, commit by commit, in
//	           the worktree checked out on the branch;
//	conflicts  a replayed commit that conflicts is resolved by the requester's
//	           session; the rebase then continues. If the resolution gives up
//	           the merge FAILS (area conflicts) and the rebase is LEFT IN
//	           PROGRESS where it stopped, for the user to carry on;
//	testing    the gate runs on the rebased branch in its worktree;
//	fixing     failed suites are fixed by the requester's session, attempt x of
//	           MaxFixAttempts, then tested again; the last attempt failing
//	           FAILS the merge (area tests);
//	committing if the target moved since this attempt's rebase began, the
//	           whole process STARTS OVER on the new tip; otherwise the
//	           non-fast-forward merge commit of the gated branch is made onto
//	           the tip in a scratch tree, and the target is fast-forwarded to it.
//
// THE TARGET NEVER CARRIES AN UNGATED COMMIT. The branch is rebased onto the
// exact tip the merge commit is made on, so the merge commit's tree IS the tree
// the gate passed; a target that moved is never merged onto, only started over
// on; and the target moves by one fast-forward, which git refuses if anything
// moved it in between.

// process runs the Emacs-repo method, starting over for as long as the target
// moves under it.
func (r *run) process(ctx context.Context) (outcome, error) {
	// A MERGE BEHIND ITS HEAD TAKES ITS STEPS AHEAD OF ITS TURN (chain.go)
	// until the pump promotes it; a speculative step a restart interrupted is
	// finished first, whatever the merge's place is now.
	if r.isSpeculative() || (r.resume != nil && r.resume.Speculative) {
		gaveUp, err := r.speculate(ctx)
		if err != nil {
			return outcome{}, err
		}
		if gaveUp != nil {
			return *gaveUp, nil
		}
	}
	for {
		out, again, err := r.attempt(ctx)
		if err != nil || !again {
			return out, err
		}
	}
}

// attempt is one pass of the process on the target's tip as it stands now. It
// answers again=true when the target moved before committing, and the process
// starts over.
//
// A RESUMED RUN'S FIRST PASS IS THE DEAD RUN'S PASS: it goes on from the step
// the record names, on the tip the record names (resumeAttempt). Every later
// pass is a fresh one.
func (r *run) attempt(ctx context.Context) (outcome, bool, error) {
	if err := stillWanted(ctx); err != nil {
		return outcome{}, false, err
	}
	if res, ok := r.resumingAt(TabRebasing, TabConflicts, TabTests, TabFixes, TabCommitting); ok {
		return r.resumeAttempt(ctx, res)
	}
	r.doneResumingAt(TabPreDeploy)
	target := r.subject.targetDir
	targetBranch, err := r.git.CurrentBranch(ctx, target)
	if err != nil {
		return outcome{}, false, fmt.Errorf("merge: reading the branch checked out in %s: %w", target, err)
	}
	if targetBranch == "" {
		return outcome{}, false, fmt.Errorf("merge: the merge target %s has no branch checked out to land on", target)
	}
	tip, err := r.git.ResolveRef(ctx, target, "HEAD")
	if err != nil {
		return outcome{}, false, fmt.Errorf("merge: resolving %s's tip: %w", target, err)
	}
	r.o.noteTargetTip(r.repo, tip)
	// A BRANCH ALREADY ON THE TARGET HAS LANDED: there is nothing to rebase,
	// gate or commit.
	contained, err := r.git.IsAncestor(ctx, r.subject.dir, r.subject.branch, tip)
	if err != nil {
		return outcome{}, false, fmt.Errorf("merge: asking whether %s is already on %s: %w", r.subject.branch, targetBranch, err)
	}
	if contained {
		out, err := r.alreadyOn(ctx, targetBranch)
		return out, false, err
	}
	if gaveUp, err := r.rebase(ctx, tip, targetBranch); err != nil || gaveUp != nil {
		if gaveUp != nil {
			return *gaveUp, false, nil
		}
		return outcome{}, false, err
	}
	r.settledOn(tip)
	return r.afterRebase(ctx, tip, targetBranch, r.fixesUsed, false)
}

// targetTip resolves the merge target's tip as it stands now.
func (r *run) targetTip(ctx context.Context) (string, error) {
	target := r.subject.targetDir
	now, err := r.git.ResolveRef(ctx, target, "HEAD")
	if err != nil {
		return "", fmt.Errorf("merge: resolving %s's tip: %w", target, err)
	}
	r.o.noteTargetTip(r.repo, now)
	return now, nil
}

// goOnAfterResumedStep takes a resumed merge on once the step it resumed at
// finished on tip: a target that moved while the merge was suspended sends it
// back to the rebase step on the new tip first, since a gate on a tip the
// target left can never land (owner ruling, 2026-10-12); otherwise the gate
// runs on tip, from fixing attempt fromAttempt.
func (r *run) goOnAfterResumedStep(ctx context.Context, tip, targetBranch string, fromAttempt int) (outcome, bool, error) {
	now, err := r.targetTip(ctx)
	if err != nil {
		return outcome{}, false, err
	}
	if now != tip {
		return r.rebaseOntoMoved(ctx, tip, now, targetBranch, fromAttempt)
	}
	return r.afterRebase(ctx, tip, targetBranch, fromAttempt, false)
}

// rebaseOntoMoved takes a merge resumed after its rebase step, whose target
// moved from tip to now while it was suspended (a restart, a pause, a race to
// land it lost), back to the rebase step on now; the gate then runs on the new
// tip, keeping its fixing attempts.
func (r *run) rebaseOntoMoved(ctx context.Context, tip, now, targetBranch string, fromAttempt int) (outcome, bool, error) {
	r.o.log(ctx, r.ws).Info("daemon.merge.resume", "the target moved while the merge was suspended; it rebases onto the new tip before it goes on", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "target": targetBranch, "rebased_on": tip, "tip": now})
	if gaveUp, err := r.rebase(ctx, now, targetBranch); err != nil || gaveUp != nil {
		if gaveUp != nil {
			return *gaveUp, false, nil
		}
		return outcome{}, false, err
	}
	r.settledOn(now)
	return r.afterRebase(ctx, now, targetBranch, fromAttempt, false)
}

// afterRebase is the attempt once its branch stands on tip: the gate (with its
// fixing attempts from fromAttempt on), then the commit. resumeTests runs the
// first gate in the tests round the record left live rather than a new one.
func (r *run) afterRebase(ctx context.Context, tip, targetBranch string, fromAttempt int, resumeTests bool) (outcome, bool, error) {
	head, gaveUp, err := r.testAndFix(ctx, tip, targetBranch, fromAttempt, resumeTests)
	if err != nil || gaveUp != nil {
		if gaveUp != nil {
			return *gaveUp, false, nil
		}
		return outcome{}, false, err
	}
	return r.commit(ctx, tip, head, targetBranch)
}

// resumeAttempt goes on with the attempt a restart interrupted, from the step
// its record names.
func (r *run) resumeAttempt(ctx context.Context, res *progressDoc) (outcome, bool, error) {
	r.doneResuming()
	tip, targetBranch := res.Tip, res.TargetBranch
	if tip == "" || targetBranch == "" {
		return *r.contradiction(ctx, fmt.Sprintf("the record of its %s step names no tip or target branch", res.Step)), false, nil
	}
	p := &replay{round: r.activeRound(), commits: fromCommitDocs(res.Commits), done: res.Replayed}
	finish := func(gaveUp *outcome, err error) (outcome, bool, error, bool) {
		if gaveUp != nil {
			return *gaveUp, false, nil, true
		}
		if err != nil {
			return outcome{}, false, err, true
		}
		return outcome{}, false, nil, false
	}
	switch res.Step {
	case TabRebasing:
		if out, again, err, done := finish(r.resumeRebase(ctx, p, tip, targetBranch, res.BranchHead)); done {
			return out, again, err
		}
		r.settledOn(tip)
		return r.goOnAfterResumedStep(ctx, tip, targetBranch, r.fixesUsed)
	case TabConflicts:
		if out, again, err, done := finish(r.resumeConflict(ctx, p, tip, targetBranch, res)); done {
			return out, again, err
		}
		r.settledOn(tip)
		return r.goOnAfterResumedStep(ctx, tip, targetBranch, r.fixesUsed)
	case TabTests:
		if gaveUp, err := r.rebaseSettled(ctx, tip, targetBranch); err != nil || gaveUp != nil {
			out, again, err, _ := finish(gaveUp, err)
			return out, again, err
		}
		now, err := r.targetTip(ctx)
		if err != nil {
			return outcome{}, false, err
		}
		if now != tip {
			// A GATE ON A TIP THE TARGET LEFT can never land: the merge goes
			// back to the rebase step first (owner ruling, 2026-10-12).
			round := r.activeRound()
			const moved = "the daemon restarted under this run and the target moved; the branch rebases onto its new tip first"
			r.endRun(ctx, round, moved, nil, nil, "restarted")
			return r.rebaseOntoMoved(ctx, tip, now, targetBranch, res.FixAttempt)
		}
		r.o.log(ctx, r.ws).Info("daemon.merge.resume", "the test gate the restart cut runs again from its start", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "round": r.activeRound().n, "attempt": res.FixAttempt})
		return r.afterRebase(ctx, tip, targetBranch, res.FixAttempt, true)
	case TabFixes:
		if gaveUp, err := r.resumeFix(ctx, res, targetBranch); err != nil || gaveUp != nil {
			out, again, err, _ := finish(gaveUp, err)
			return out, again, err
		}
		return r.goOnAfterResumedStep(ctx, tip, targetBranch, res.FixAttempt)
	default:
		return r.resumeCommit(ctx, res)
	}
}

// alreadyOn concludes a merge whose branch the target already contains: it has
// landed, with nothing to merge and nothing to deploy, and says so.
func (r *run) alreadyOn(ctx context.Context, targetBranch string) (outcome, error) {
	round := r.openTab(ctx, TabRebasing)
	r.rebasing.Span.relive(round.started.UnixMilli())
	r.rebasing.Lines = append(r.rebasing.Lines, textLine(fmt.Sprintf("already on %s; nothing to merge", targetBranch)))
	r.rebasing.Span.settle(r.o.nowMS(), "")
	r.publishRebasing(false)
	r.closeTab(ctx, round, "succeeded")
	tip, err := r.git.ResolveRef(ctx, r.subject.dir, r.subject.branch)
	if err != nil {
		return outcome{}, fmt.Errorf("merge: resolving %s: %w", r.subject.branch, err)
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.rebase", "the branch is already on its target; the merge concludes with nothing to land", dlog.Context{
		"workspace": string(r.ws), "branch": r.subject.branch, "target": targetBranch, "tip": tip})
	return outcome{landed: tip, target: targetBranch, alreadyOn: targetBranch}, nil
}

// replay is one rebase's progress: the ledger round it stands in, the commits
// it replays and how many are done. What the bubble draws of it is the
// rebasing view (nested.go).
type replay struct {
	round   tabRound
	commits []gitclient.Commit
	done    int
}

// rebase replays the branch onto tip, commit by commit, in the branch's
// worktree. It answers the failed outcome when a conflict's resolution gave
// up, and an error when a rebase command failed outright; in both cases the
// rebase is left exactly where git left it.
func (r *run) rebase(ctx context.Context, tip, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.rebase"
	dir, branch := r.subject.dir, r.subject.branch
	clean, err := r.git.IsClean(ctx, dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading whether %s is clean: %w", dir, err)
	}
	if !clean {
		return nil, fmt.Errorf("merge: the worktree %s has uncommitted changes, so %s cannot be rebased", dir, branch)
	}
	checkedOut, err := r.git.CurrentBranch(ctx, dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading the branch checked out in %s: %w", dir, err)
	}
	if checkedOut != branch {
		return nil, fmt.Errorf("merge: the worktree %s has %q checked out, not %s", dir, checkedOut, branch)
	}
	// ONLY THE BRANCH'S OWN COMMITS ARE REPLAYED: those since its chain base,
	// so a merge ahead of it that did not land leaves nothing of its own
	// behind in this branch.
	commits, err := r.git.CommitsBetween(ctx, dir, r.ownBase(tip), branch)
	if err != nil {
		return nil, fmt.Errorf("merge: listing what %s replays onto %s: %w", branch, short(tip), err)
	}
	p := &replay{round: r.openTab(ctx, TabRebasing), commits: commits}
	r.rebasing.beginReplay(commits, p.round.started.UnixMilli())
	based, err := r.git.IsAncestor(ctx, dir, tip, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: asking whether %s is already on %s's tip: %w", branch, targetBranch, err)
	}
	if based && r.chainBase != "" && r.chainBase != tip {
		// A BRANCH STANDING ON A MERGE AHEAD that is not on tip is not on tip,
		// whatever tip it also descends from: that merge's commits must go.
		onTip, err := r.git.IsAncestor(ctx, dir, r.chainBase, tip)
		if err != nil {
			return nil, fmt.Errorf("merge: asking whether %s's chain base %s is on %s: %w", branch, short(r.chainBase), short(tip), err)
		}
		based = onTip
	}
	if based {
		// ALREADY ON THE TIP: the branch was rebased before (a start-over whose
		// target did not move after all, or an author who rebased by hand), so
		// there is nothing to replay.
		p.done = len(commits)
		now := r.o.nowMS()
		r.rebasing.reachCommit(len(commits), now)
		r.rebasing.Lines = append(r.rebasing.Lines, textLine(fmt.Sprintf("%s is already on %s's tip; nothing to replay", branch, targetBranch)))
		r.rebasing.Span.settle(now, "")
		r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) { f.Replayed, f.Total = p.done, len(commits) })
		r.publishReplay()
		r.closeTab(ctx, p.round, "succeeded")
		return nil, nil
	}
	if len(commits) == 0 {
		r.settleRebasing(ctx, p, "nothing to replay")
		return nil, fmt.Errorf("merge: %s is not on %s's tip, yet has no commit to replay onto it", branch, targetBranch)
	}
	head, err := r.git.ResolveRef(ctx, dir, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: resolving %s before its rebase: %w", branch, err)
	}
	replaying, err := r.replaying(ctx, len(commits), branch, targetBranch, tip)
	if err != nil {
		return nil, err
	}
	r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) {
		f.Total = len(commits)
		f.Line = rebaseCommandLine(pickLine(commits[0]))
	})
	r.rebasing.Lines = append(r.rebasing.Lines, replaying)
	r.rebasing.reachCommit(0, r.o.nowMS())
	r.publishReplay()
	// THE REBASE'S RECORD NAMES THE TIP, THE COMMITS AND THE BRANCH'S HEAD
	// BEFORE IT: a resume reads where git stopped against exactly these.
	if err := r.checkpoint(ctx, TabRebasing, func(d *progressDoc) {
		d.TargetBranch, d.Tip, d.BranchHead = targetBranch, tip, head
		d.Commits, d.Replayed = toCommitDocs(commits), 0
		d.Turn, d.ConflictFiles, d.FixAttempt, d.FailingSuites, d.Head, d.MergeCommit = "", nil, 0, nil, "", ""
		d.FailingArchive, d.FailingTail = "", ""
	}); err != nil {
		return nil, err
	}
	r.o.log(ctx, r.ws).Info(op, "rebasing the branch onto the target's tip", dlog.Context{
		"workspace": string(r.ws), "branch": branch, "worktree": dir, "onto": tip, "commits": len(commits)})
	step, err := r.git.StartRebase(ctx, dir, tip, shasOf(commits))
	return r.replayLoop(ctx, p, step, err, tip, targetBranch)
}

// shasOf answers the shas of commits, in order: what a rebase replays.
func shasOf(commits []gitclient.Commit) []string {
	shas := make([]string, len(commits))
	for i, c := range commits {
		shas[i] = c.SHA
	}
	return shas
}

// replayLoop follows a rebase from where one command left it to its end: each
// replayed commit recorded, each conflict handed to the requester's session.
func (r *run) replayLoop(ctx context.Context, p *replay, step gitclient.RebaseStep, err error, tip, targetBranch string) (*outcome, error) {
	dir, branch := r.subject.dir, r.subject.branch
	// resolvedAt is the commit the last step stopped on with rerere's
	// resolution staged, -1 when the last step was no such stop.
	resolvedAt := -1
	for {
		if err != nil {
			if errors.Is(err, errMergeStopping) {
				return nil, err
			}
			r.rebaseFailed(ctx, p, err)
			return nil, fmt.Errorf("merge: rebasing %s onto %s: %w", branch, short(tip), err)
		}
		if len(step.Conflicted) > 0 {
			resolvedAt = -1
			gaveUp, err := r.conflict(ctx, p, step.Conflicted, targetBranch)
			if err != nil || gaveUp != nil {
				return gaveUp, err
			}
			step, err = r.git.ContinueRebase(ctx, dir)
			continue
		}
		if len(step.Resolved) > 0 {
			// A CONFLICT ALREADY RESOLVED ONCE IS NEVER PROMPTED AGAIN: rerere
			// staged the resolution the session recorded for this same
			// conflict, and the rebase only waits to be continued.
			i := p.commitAt()
			commit := p.commits[i]
			// A CONTINUE THAT LEAVES THE REBASE STOPPED ON THE SAME RESOLVED
			// COMMIT MOVED NOTHING: git refused to commit the staged
			// resolution, and continuing again would only be refused again.
			if resolvedAt == i {
				err = fmt.Errorf("the rebase stayed stopped on %s after it was continued: git did not commit the resolution rerere staged in %s",
					short(commit.SHA), strings.Join(step.Resolved, ", "))
				r.o.log(ctx, r.ws).Error("daemon.merge.rerere", "continuing a rerere-resolved commit left the rebase where it stood", dlog.Context{
					"workspace": string(r.ws), "commit": commit.SHA, "files": strings.Join(step.Resolved, ", "), "worktree": dir})
				continue
			}
			resolvedAt = i
			r.rebasing.Commits[i].Lines = append(r.rebasing.Commits[i].Lines, fmt.Sprintf("conflicted in %d file(s), resolved as before", len(step.Resolved)))
			r.publishCommit(i, false)
			r.o.log(ctx, r.ws).Info("daemon.merge.rerere", "a replayed commit conflicted as before; the recorded resolution was reused and nothing is prompted", dlog.Context{
				"workspace": string(r.ws), "commit": commit.SHA, "files": strings.Join(step.Resolved, ", "), "worktree": dir})
			step, err = r.git.ContinueRebase(ctx, dir)
			continue
		}
		resolvedAt = -1
		r.replayed(ctx, p, step.Done)
		if step.Done {
			return nil, nil
		}
		if err := r.checkpoint(ctx, TabRebasing, func(d *progressDoc) { d.Replayed = p.done }); err != nil {
			return nil, err
		}
		if err := stillWanted(ctx); err != nil {
			return nil, err
		}
		step, err = r.git.ContinueRebase(ctx, dir)
	}
}

// resumeRebase goes on with a rebase a restart interrupted, from where GIT
// says it stopped -- which the record cannot know to the commit, because the
// command that moved it may have finished after the record was written.
//
// What a rebase step can leave is exactly this: a rebase standing in the
// worktree (stopped at a break, or at a conflict), the branch on the tip (the
// last command finished it), or the branch where it stood before the rebase
// (the first command never ran). Anything else contradicts the record.
func (r *run) resumeRebase(ctx context.Context, p *replay, tip, targetBranch, branchHead string) (*outcome, error) {
	const op = "daemon.merge.resume"
	dir, branch := r.subject.dir, r.subject.branch
	if len(p.commits) == 0 {
		return r.contradiction(ctx, "the record of the rebase names no commit it replays"), nil
	}
	standing, err := r.git.RebaseInProgress(ctx, dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading whether a rebase stands in %s: %w", dir, err)
	}
	if standing {
		files, err := r.git.ConflictedFiles(ctx, dir)
		if err != nil {
			return nil, fmt.Errorf("merge: reading what conflicts in %s: %w", dir, err)
		}
		onTip, err := r.git.CommitsBetween(ctx, dir, tip, "HEAD")
		if err != nil {
			return nil, fmt.Errorf("merge: counting what stands on %s in %s: %w", short(tip), dir, err)
		}
		if len(onTip) >= len(p.commits) {
			return r.contradiction(ctx, fmt.Sprintf("a rebase still stands in %s with %d commits on %s, but the record replays only %d",
				dir, len(onTip), short(tip), len(p.commits))), nil
		}
		p.done = len(onTip)
		r.o.log(ctx, r.ws).Info(op, "the rebase goes on where git stopped it", dlog.Context{
			"workspace": string(r.ws), "worktree": dir, "replayed": p.done, "total": len(p.commits), "conflicted": len(files)})
		r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) {
			f.Replayed, f.Total = p.done, len(p.commits)
			f.Line = rebaseCommandLine(pickLine(p.commits[p.done]))
		})
		r.resumeReplay(p)
		r.publishReplay()
		if len(files) > 0 {
			return r.replayLoop(ctx, p, gitclient.RebaseStep{Conflicted: files}, nil, tip, targetBranch)
		}
		step, err := r.git.ContinueRebase(ctx, dir)
		return r.replayLoop(ctx, p, step, err, tip, targetBranch)
	}
	onTip, err := r.git.IsAncestor(ctx, dir, tip, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: asking whether %s is on %s's tip: %w", branch, targetBranch, err)
	}
	if onTip {
		r.o.log(ctx, r.ws).Info(op, "the rebase had finished before the restart; the merge goes on to its tests", dlog.Context{
			"workspace": string(r.ws), "worktree": dir, "total": len(p.commits)})
		p.done = max(len(p.commits)-1, 0)
		r.resumeReplay(p)
		r.replayed(ctx, p, true)
		return nil, nil
	}
	head, err := r.git.ResolveRef(ctx, dir, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: resolving %s: %w", branch, err)
	}
	if head == branchHead && p.done == 0 {
		r.o.log(ctx, r.ws).Info(op, "the rebase had not begun before the restart; it begins now", dlog.Context{
			"workspace": string(r.ws), "worktree": dir, "total": len(p.commits)})
		r.resumeReplay(p)
		r.publishReplay()
		step, err := r.git.StartRebase(ctx, dir, tip, shasOf(p.commits))
		return r.replayLoop(ctx, p, step, err, tip, targetBranch)
	}
	return r.contradiction(ctx, fmt.Sprintf("no rebase stands in %s, and %s is at %s: neither on %s's tip %s nor at %s, where it stood before the recorded rebase",
		dir, branch, short(head), targetBranch, short(tip), short(branchHead))), nil
}

// rebaseSettled checks what a resumed step after the rebase stands on: no
// rebase in the worktree, and the branch on the tip it was rebased onto.
func (r *run) rebaseSettled(ctx context.Context, tip, targetBranch string) (*outcome, error) {
	dir, branch := r.subject.dir, r.subject.branch
	standing, err := r.git.RebaseInProgress(ctx, dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading whether a rebase stands in %s: %w", dir, err)
	}
	if standing {
		return r.contradiction(ctx, fmt.Sprintf("the record says the rebase of %s finished, but a rebase stands in %s", branch, dir)), nil
	}
	onTip, err := r.git.IsAncestor(ctx, dir, tip, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: asking whether %s is on %s's tip: %w", branch, targetBranch, err)
	}
	if !onTip {
		return r.contradiction(ctx, fmt.Sprintf("the record says %s was rebased onto %s's tip %s, but it is not on it", branch, targetBranch, short(tip))), nil
	}
	return nil, nil
}

// replayed records one commit replayed. A rebase that finished is complete
// whatever the count read: every commit is on the tip.
func (r *run) replayed(ctx context.Context, p *replay, done bool) {
	i := p.commitAt()
	p.done++
	if done {
		p.done = len(p.commits)
	}
	now := r.o.nowMS()
	current := &r.rebasing.Commits[i]
	current.Lines = append(current.Lines, fmt.Sprintf("replayed %d/%d", p.done, len(p.commits)))
	if !current.Span.begun() {
		current.Span.StartedMS = now
	}
	current.Span.settle(now, "")
	if done {
		r.rebasing.reachCommit(len(p.commits), now)
		r.rebasing.Span.settle(now, "")
		r.updateFacts(func(f *footer.MergeFacts) { f.Replayed, f.Line = p.done, nil })
		r.publishReplay()
		r.closeTab(ctx, p.round, "succeeded")
		return
	}
	r.rebasing.reachCommit(p.done, now)
	next := p.commits[p.done]
	r.updateFacts(func(f *footer.MergeFacts) { f.Replayed, f.Line = p.done, rebaseCommandLine(pickLine(next)) })
	r.publishRebasing(false)
	r.publishCommit(i, false)
	r.publishCommit(p.done, false)
}

// rebaseFailed settles the rebasing tab and the footer's line on a rebase
// command that failed outright, with its first error line.
func (r *run) rebaseFailed(ctx context.Context, p *replay, err error) {
	line := failureLine(err)
	r.updateFacts(func(f *footer.MergeFacts) { f.Line = rebaseFailureLine(line) })
	const summary = "the rebase failed"
	now := r.o.nowMS()
	if len(p.commits) > 0 {
		c := &r.rebasing.Commits[p.commitAt()]
		c.Lines = append(c.Lines, "failed: "+line)
		if !c.Span.begun() {
			c.Span.StartedMS = now
		}
		c.Span.settle(now, summary)
	} else {
		r.rebasing.Lines = append(r.rebasing.Lines, textLine("failed: "+line))
	}
	r.settleRebasing(ctx, p, summary)
}

// settleRebasing settles the rebasing tab and its ledger round as failed.
func (r *run) settleRebasing(ctx context.Context, p *replay, summary string) {
	r.rebasing.Span.settle(r.o.nowMS(), summary)
	r.publishReplay()
	r.closeTab(ctx, p.round, "failed")
}

// conflict hands a replayed commit's conflicts to the requester's session and
// answers the failed outcome when the resolution gave up. On a resolution the
// rebase continues in a new rebasing round.
func (r *run) conflict(ctx context.Context, p *replay, files []string, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.conflicts"
	i := p.commitAt()
	commit := p.commits[i]
	// THE ATTEMPTS COUNT PER COMMIT (owner ruling, 2026-10-12): a conflict on
	// a commit the rebase has not met before starts its count afresh, so a
	// rebase that advanced is never failed for the attempts an earlier commit
	// took.
	if r.conflictCommit != commit.SHA {
		r.conflictCommit, r.conflictAttempts = commit.SHA, 0
	}
	r.conflictAttempts++
	r.rebasing.Commits[i].Lines = append(r.rebasing.Commits[i].Lines, fmt.Sprintf("conflicted in %d file(s)", len(files)))
	// THE LEDGER'S REBASING ROUND ENDS AT THE CONFLICT, and the rebase goes on
	// in a new one once it is resolved. The bubble's rebasing tab and this
	// commit's subtab stay live across the resolution.
	r.closeTab(ctx, p.round, "failed")
	r.o.log(ctx, r.ws).Info(op, "a replayed commit conflicted; the requester's session resolves it", dlog.Context{
		"workspace": string(r.ws), "commit": commit.SHA, "files": strings.Join(files, ", "), "worktree": r.subject.dir,
		"attempt": r.conflictAttempts, "of": MaxConflictAttempts})
	return r.promptConflict(ctx, p, commit, files, targetBranch)
}

// promptConflict hands one conflict to the requester's session in a new
// conflicts round, and reads the resolution's end. It spends no attempt: the
// caller counted it, or -- a resolution a restart cut -- it is not counted.
func (r *run) promptConflict(ctx context.Context, p *replay, commit gitclient.Commit, files []string, targetBranch string) (*outcome, error) {
	i := p.commitAt()
	round := r.openTab(ctx, TabConflicts)
	r.rebasing.Commits[i].Conflict = &conflictView{Span: span{StartedMS: round.started.UnixMilli()}, Files: append([]string(nil), files...)}
	r.drawConflict(ctx, round, i, len(files))
	text, err := r.conflictBrief(commit, files, targetBranch)
	if err != nil {
		return nil, err
	}
	if err := r.ensureSession(ctx); err != nil {
		return nil, fmt.Errorf("merge: starting a session for the conflict resolution: %w", err)
	}
	turn := wsm.NewTurnID()
	if err := r.checkpoint(ctx, TabConflicts, func(d *progressDoc) {
		d.Replayed, d.ConflictFiles, d.Turn = p.done, files, string(turn)
	}); err != nil {
		return nil, err
	}
	close, err := r.submit(ctx, turn, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)
	if err != nil {
		return nil, err
	}
	return r.conflictSettled(ctx, p, round, close, commit, targetBranch, false)
}

// cutByRestart reports whether a merge turn ended because the session it ran
// in went away under a restart rather than by the agent's own hand: a turn the
// daemon reconciled with no terminal (orphaned), or -- read back on a resume --
// one whose agent died while the merge was suspended. Such a step reruns
// without spending an attempt.
func cutByRestart(close wsm.TurnClose, resumed bool) bool {
	return close == wsm.CloseOrphaned || (resumed && close == wsm.CloseAgentDied)
}

// stoppedByQueue reports whether a merge turn read back on a resume was
// killed by a hard stop of the queue (HardStop): the turn the merge recorded
// is the one the hard stop wrote down before it interrupted it. Like a cut by
// a restart, such a step reruns without spending an attempt.
func (r *run) stoppedByQueue(close wsm.TurnClose, resumed bool) bool {
	if !resumed || close != wsm.CloseKilled {
		return false
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.stoppedTurn != ""
}

// cutShort names what cut a merge turn short so that its step is prompted
// again without spending an attempt -- "a restart" (cutByRestart) or "the
// queue's hard stop" (stoppedByQueue) -- and answers "" for a turn that ended
// by its own hand.
func (r *run) cutShort(close wsm.TurnClose, resumed bool) string {
	switch {
	case cutByRestart(close, resumed):
		return "a restart"
	case r.stoppedByQueue(close, resumed):
		return "the queue's hard stop"
	}
	return ""
}

// MaxConflictAttempts is how many resolution attempts one commit's conflict
// gets IN A ROW before the merge fails in conflicts: one failure of the
// rebase-then-resolve cycle is one resolution attempt that left the commit
// unresolved (its turn failed, or files still conflict). A resolution that
// resolves starts the count over, and so does a conflict on another commit; an
// attempt a restart cut is prompted again and never counts. The bound is
// owner-ruled (2026-10-12): modules/app/agent-repl/AGENTS.md, "The merge
// queue's test gate and remediation contract".
const MaxConflictAttempts = 5

// conflictBrief composes the conflict resolution's brief.
func (r *run) conflictBrief(commit gitclient.Commit, files []string, targetBranch string) (string, error) {
	text, err := r.o.deps.Briefs(BriefConflictResolve, map[string]string{
		"conflict_commit":  commitLine(commit.SHA, commit.Subject),
		"source_branch":    r.subject.branch,
		"worktree_dir":     r.subject.dir,
		"target_branch":    r.briefTarget(targetBranch),
		"conflicted_files": strings.Join(files, ", "),
	})
	if err != nil {
		return "", fmt.Errorf("merge: composing the conflict brief: %w", err)
	}
	return text, nil
}

// conflictSettled reads the conflict resolution's end: the failed outcome when
// it gave up, or a new rebasing round the rebase continues in.
func (r *run) conflictSettled(ctx context.Context, p *replay, round tabRound, close wsm.TurnClose, commit gitclient.Commit, targetBranch string, resumed bool) (*outcome, error) {
	const op = "daemon.merge.conflicts"
	if err := stillWanted(ctx); err != nil {
		return nil, err
	}
	remaining, err := r.git.ConflictedFiles(ctx, r.subject.dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading what still conflicts in %s: %w", r.subject.dir, err)
	}
	// A RESOLUTION A RESTART CUT SHORT IS PROMPTED AGAIN AND NEVER COUNTS
	// (owner ruling, 2026-10-12): it did not fail, it was interrupted. So is
	// one a hard stop of the queue interrupted.
	cut := r.cutShort(close, resumed)
	stopped := r.stoppedByQueue(close, resumed)
	if cut != "" && len(remaining) > 0 {
		r.settleAgentic()
		r.settleConflictAttempt(p, cut+" cut this resolution short; it is prompted again")
		r.closeTab(ctx, round, "restarted")
		r.o.log(ctx, r.ws).Info(op, cut+" cut the conflict resolution short; it is prompted again and spends no attempt", dlog.Context{
			"workspace": string(r.ws), "commit": commit.SHA, "close": close.String(), "attempt": r.conflictAttempts, "of": MaxConflictAttempts})
		return r.promptConflict(ctx, p, commit, remaining, targetBranch)
	}
	why := ""
	switch {
	case close.Failed() && !stopped:
		why = fmt.Sprintf("the conflict resolution's turn ended %s", close)
	case len(remaining) > 0:
		why = fmt.Sprintf("%d file(s) still conflict: %s", len(remaining), strings.Join(remaining, ", "))
	}
	// A RESOLUTION MAY EDIT THE MERGE MACHINERY (owner ruling, 2026-10-14): a
	// conflict there is the branch's own change meeting master's, and only a
	// resolution in those files can land it. What it resolves is the branch's
	// work, and the gate tests it like any of the branch's work.
	//
	// A RESOLUTION THAT DID NOT RESOLVE IS TRIED AGAIN on the same commit, up
	// to MaxConflictAttempts in a row (owner ruling, 2026-10-12); only then
	// does the merge fail.
	if why != "" && r.conflictAttempts < MaxConflictAttempts {
		r.settleAgentic()
		retry := fmt.Sprintf("attempt %d of %d did not resolve it (%s); trying again", r.conflictAttempts, MaxConflictAttempts, why)
		r.settleConflictAttempt(p, retry)
		r.closeTab(ctx, round, "failed")
		r.o.log(ctx, r.ws).Info(op, "the conflict resolution did not resolve the commit; the session tries again", dlog.Context{
			"workspace": string(r.ws), "commit": commit.SHA, "attempt": r.conflictAttempts, "of": MaxConflictAttempts, "why": why})
		files := remaining
		if len(files) == 0 {
			files, err = r.git.ConflictedFiles(ctx, r.subject.dir)
			if err != nil {
				return nil, fmt.Errorf("merge: reading what still conflicts in %s: %w", r.subject.dir, err)
			}
		}
		return r.conflict(ctx, p, files, targetBranch)
	}
	if why != "" {
		summary := fmt.Sprintf("conflict resolution gave up: %s; the rebase is left in progress in %s", why, r.subject.dir)
		r.settleAgentic()
		i, now := p.commitAt(), r.o.nowMS()
		c := &r.rebasing.Commits[i]
		c.Conflict.Span.settle(now, summary)
		c.Span.settle(now, summary)
		r.rebasing.Span.settle(now, summary)
		r.publishRebasing(false)
		r.publishCommit(i, false)
		r.closeTab(ctx, round, "failed")
		r.o.log(ctx, r.ws).Info(op, "the conflict resolution gave up; the merge fails and the rebase is left in progress", dlog.Context{
			"workspace": string(r.ws), "why": why, "worktree": r.subject.dir})
		out := failedIn(footer.FailedConflicts, summary)
		return &out, nil
	}
	r.settleAgentic()
	i := p.commitAt()
	c := &r.rebasing.Commits[i]
	c.Conflict.Span.settle(r.o.nowMS(), "")
	c.Lines = append(c.Lines, "the conflict is resolved; the rebase continues")
	r.publishRebasing(false)
	r.publishCommit(i, false)
	r.closeTab(ctx, round, "succeeded")
	// A RESOLUTION THAT RESOLVED ENDS THE RUN OF FAILURES: the next conflict
	// starts its count afresh.
	r.conflictCommit, r.conflictAttempts = "", 0
	// ITS EDITS ARE THE BRANCH'S OWN from here on: the next test repair takes
	// the branch's merge machinery afresh (noteMachinery), so it is refused
	// only for machinery it adds itself.
	r.machinery = nil
	// THE REBASE CONTINUES IN A NEW LEDGER ROUND, drawn in the same rebasing
	// tab and the same commit's subtab.
	p.round = r.openTab(ctx, TabRebasing)
	r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) {
		f.Replayed, f.Total = p.done, len(p.commits)
		f.Line = rebaseCommandLine("git rebase --continue")
	})
	if err := r.checkpoint(ctx, TabRebasing, func(d *progressDoc) {
		d.Replayed, d.ConflictFiles, d.Turn = p.done, nil, ""
	}); err != nil {
		return nil, err
	}
	return nil, nil
}

// resumeConflict goes on with a conflict resolution a restart interrupted: the
// requester's session still runs (or ran) the resolution turn, so the merge
// reattaches to that turn, reads its end, and continues the rebase.
func (r *run) resumeConflict(ctx context.Context, p *replay, tip, targetBranch string, res *progressDoc) (*outcome, error) {
	dir := r.subject.dir
	standing, err := r.git.RebaseInProgress(ctx, dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading whether a rebase stands in %s: %w", dir, err)
	}
	if !standing {
		return r.contradiction(ctx, fmt.Sprintf("the record says a conflict of the rebase in %s was being resolved, but no rebase stands there", dir)), nil
	}
	if len(p.commits) == 0 {
		return r.contradiction(ctx, "the record of the conflict resolution names no commit the rebase replays"), nil
	}
	i := p.commitAt()
	commit := p.commits[i]
	round := r.activeRound()
	r.resumeReplay(p)
	if r.rebasing.Commits[i].Conflict == nil {
		r.rebasing.Commits[i].Conflict = &conflictView{Span: span{StartedMS: round.started.UnixMilli()}, Files: append([]string(nil), res.ConflictFiles...)}
	}
	r.publishReplay()
	r.drawConflict(ctx, round, i, len(res.ConflictFiles))
	text, err := r.conflictBrief(commit, res.ConflictFiles, targetBranch)
	if err != nil {
		return nil, err
	}
	close, err := r.reattachTurn(ctx, ids.TurnID(res.Turn), text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)
	if err != nil {
		return nil, err
	}
	if gaveUp, err := r.conflictSettled(ctx, p, round, close, commit, targetBranch, true); err != nil || gaveUp != nil {
		return gaveUp, err
	}
	step, err := r.git.ContinueRebase(ctx, dir)
	return r.replayLoop(ctx, p, step, err, tip, targetBranch)
}

// testAndFix gates the rebased branch, fixing and gating again up to
// MaxFixAttempts times, starting at fixing attempt fromAttempt. It answers the
// gated head, or the failed outcome. resumeTests runs the first gate in the
// tests round a restart left live.
func (r *run) testAndFix(ctx context.Context, tip, targetBranch string, fromAttempt int, resumeTests bool) (string, *outcome, error) {
	for attempt := fromAttempt; ; attempt++ {
		head, err := r.git.ResolveRef(ctx, r.subject.dir, r.subject.branch)
		if err != nil {
			return "", nil, fmt.Errorf("merge: resolving %s: %w", r.subject.branch, err)
		}
		if attempt == fromAttempt && !resumeTests {
			reused, err := r.reuseTested(ctx, tip, head)
			if err != nil {
				return "", nil, err
			}
			if reused {
				r.fixesUsed = 0
				return head, nil, nil
			}
		}
		var round tabRound
		if resumeTests && attempt == fromAttempt {
			round = r.activeRound()
		} else {
			round = r.openTab(ctx, TabTests)
		}
		// EVERY STEP'S RECORD NAMES ITS ATTEMPT'S TIP AND TARGET: a branch
		// already on the tip took no rebasing step to record them.
		if err := r.checkpoint(ctx, TabTests, func(d *progressDoc) {
			d.TargetBranch, d.Tip = targetBranch, tip
			d.FixAttempt, d.Turn, d.FailingSuites, d.FailingArchive, d.FailingTail = attempt, "", nil, "", ""
		}); err != nil {
			return "", nil, err
		}
		verdict, err := r.gate(ctx, round, tip, head, nil)
		if err == nil {
			err = stillWanted(ctx)
		}
		if err != nil {
			return "", nil, err
		}
		switch {
		case verdict.broken != "":
			out := failedIn(footer.FailedOther, verdict.broken)
			return "", &out, nil
		case verdict.result.Passed:
			// A PASSING GATE ENDS THE RUN OF FAILURES: a later failure (a
			// start-over on a moved target) starts its count afresh.
			r.fixesUsed = 0
			// THE PASS IS KEPT FOR A START-OVER (owner ruling, 2026-10-12): a
			// merge whose target moves before it commits rebases again, and
			// when that rebase gives the very tree that passed, the tests are
			// not run again (reuseTested).
			tree, err := r.git.TreeOf(ctx, r.subject.dir, head)
			if err != nil {
				return "", nil, fmt.Errorf("merge: reading the tree of %s: %w", short(head), err)
			}
			r.tested = &testedDoc{Base: tip, Head: head, Tree: tree, Own: true}
			return head, nil, nil
		case attempt == MaxFixAttempts:
			out := r.fixesExhausted(ctx, verdict.result.ArchivePath, nil)
			return "", &out, nil
		}
		if gaveUp, err := r.fix(ctx, attempt+1, verdict.result, targetBranch); err != nil || gaveUp != nil {
			return "", gaveUp, err
		}
		r.fixesUsed = attempt + 1
	}
}

// fixesExhausted fails a merge whose last fixing attempt left suites failing:
// the outcome, the record at INFO (with EXTRA's fields), and the tests tab
// settled failed, all with the one summary.
func (r *run) fixesExhausted(ctx context.Context, archive string, extra dlog.Context) outcome {
	summary := fmt.Sprintf("the tests still fail after %d fixing attempts; the whole run is archived at %s", MaxFixAttempts, archive)
	fields := dlog.Context{"workspace": string(r.ws), "attempts": MaxFixAttempts, "archive": archive}
	for k, v := range extra {
		fields[k] = v
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.fixes", "the last fixing attempt left suites failing; the merge fails", fields)
	r.settleTesting(summary)
	return failedIn(footer.FailedTests, summary)
}

// fix hands one failing run to the requester's session, which repairs the
// branch in its worktree: fixing attempt x of MaxFixAttempts. It answers the
// failed outcome when the agent escalated or changed the merge machinery.
func (r *run) fix(ctx context.Context, attempt int, failing GateResult, targetBranch string) (*outcome, error) {
	return r.promptFix(ctx, attempt, failedSuites(failing.Suites), failing.ArchivePath, failing.Tail, targetBranch)
}

// promptFix submits fixing attempt x's brief in a new fixes round, drawn as
// the sub-subtab of the run it follows, and reads its end.
func (r *run) promptFix(ctx context.Context, attempt int, suites []string, archive, tail, targetBranch string) (*outcome, error) {
	n := r.testsRound()
	round := r.openTab(ctx, TabFixes)
	r.drawFixing(ctx, round, n, attempt, suites)
	if err := r.noteMachinery(ctx, targetBranch); err != nil {
		return nil, err
	}
	// THE AGENT RUNS EXACTLY THE FAILING TESTS (owner ruling, 2026-10-12):
	// they are read off the failing run's archive into the worktree, and into
	// the brief itself, before the brief is sent.
	failingTests, err := r.writeFailingTests(ctx, archive)
	if err != nil {
		return nil, err
	}
	text, err := r.fixBrief(attempt, suites, archive, tail, failingTests, targetBranch)
	if err != nil {
		return nil, err
	}
	if err := r.ensureSession(ctx); err != nil {
		return nil, fmt.Errorf("merge: starting a session for the fixing attempt: %w", err)
	}
	turn := wsm.NewTurnID()
	if err := r.checkpoint(ctx, TabFixes, func(d *progressDoc) {
		d.FixAttempt, d.Turn, d.FailingSuites = attempt, string(turn), suites
		d.FailingArchive, d.FailingTail = archive, tail
	}); err != nil {
		return nil, err
	}
	close, err := r.submit(ctx, turn, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)
	if err != nil {
		return nil, err
	}
	return r.fixSettled(ctx, round, n, attempt, suites, archive, tail, targetBranch, close, false)
}

// fixBrief composes one fixing attempt's brief.
func (r *run) fixBrief(attempt int, suites []string, archive, tail, failingTests, targetBranch string) (string, error) {
	text, err := r.o.deps.Briefs(BriefTestFailureResolve, map[string]string{
		"source_branch":      r.subject.branch,
		"worktree_dir":       r.subject.dir,
		"target_branch":      r.briefTarget(targetBranch),
		"failing_suites":     strings.Join(suites, ", "),
		"archive_path":       archive,
		"failure_tail":       tail,
		"attempt":            fmt.Sprintf("%d", attempt),
		"max_attempts":       fmt.Sprintf("%d", MaxFixAttempts),
		"escalation_file":    EscalationFile,
		"escalation_marker":  EscalationMarker,
		"failing_tests_file": filepath.Join(r.subject.dir, FailingTestsFile),
		"failing_tests":      failingTests,
	})
	if err != nil {
		return "", fmt.Errorf("merge: composing the test-failure brief: %w", err)
	}
	return text, nil
}

// fixSettled reads a fixing attempt's end: the failed outcome when the agent
// escalated or changed the merge machinery, nil when the gate runs again. An
// attempt a restart cut short is prompted again as the same attempt, in the
// same run's fixing sub-subtab: it never counts (owner ruling, 2026-10-12).
func (r *run) fixSettled(ctx context.Context, round tabRound, n, attempt int, suites []string, archive, tail, targetBranch string, close wsm.TurnClose, resumed bool) (*outcome, error) {
	const op = "daemon.merge.fixes"
	if err := stillWanted(ctx); err != nil {
		return nil, err
	}
	if cut := r.cutShort(close, resumed); cut != "" {
		r.settleAgentic()
		r.publish(fixingKey(n), fixingTab(round.settled(r.o.nowMS(), cut+" cut this fixing attempt short; it is prompted again"), attempt))
		r.closeTab(ctx, round, "restarted")
		r.o.log(ctx, r.ws).Info(op, cut+" cut the fixing attempt short; it is prompted again and spends no attempt", dlog.Context{
			"workspace": string(r.ws), "attempt": attempt, "of": MaxFixAttempts, "close": close.String()})
		return r.promptFix(ctx, attempt, suites, archive, tail, targetBranch)
	}
	why := ""
	if escalated, gaveUp := r.consumeEscalation(ctx); gaveUp {
		why = "the agent escalated: " + firstLine(escalated)
	} else if line, refused, err := r.machineryChanged(ctx, targetBranch); err != nil {
		return nil, err
	} else if refused {
		why = line
	}
	if why != "" {
		r.settleAgentic()
		r.publish(fixingKey(n), fixingTab(round.settled(r.o.nowMS(), why), attempt))
		r.settleTesting(why)
		r.closeTab(ctx, round, "failed")
		r.o.log(ctx, r.ws).Info(op, "the fixing attempt gave up; the merge fails", dlog.Context{
			"workspace": string(r.ws), "attempt": attempt, "why": why})
		out := failedIn(footer.FailedTests, why)
		return &out, nil
	}
	r.settleAgentic()
	r.publish(fixingKey(n), fixingTab(round.settled(r.o.nowMS(), ""), attempt))
	r.publishTesting(false)
	r.closeTab(ctx, round, "succeeded")
	return nil, nil
}

// resumeFix goes on with a fixing attempt a restart interrupted: the merge
// reattaches to the fixing turn and reads its end.
func (r *run) resumeFix(ctx context.Context, res *progressDoc, targetBranch string) (*outcome, error) {
	round := r.activeRound()
	n := r.testsRound()
	r.drawFixing(ctx, round, n, res.FixAttempt, res.FailingSuites)
	// THE REATTACHED BRIEF CARRIES THE SAME FAILING TESTS: they are read off
	// the same archive again, so the file and the brief agree after a restart.
	failingTests, err := r.writeFailingTests(ctx, res.FailingArchive)
	if err != nil {
		return nil, err
	}
	text, err := r.fixBrief(res.FixAttempt, res.FailingSuites, res.FailingArchive, res.FailingTail, failingTests, targetBranch)
	if err != nil {
		return nil, err
	}
	close, err := r.reattachTurn(ctx, ids.TurnID(res.Turn), text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)
	if err != nil {
		return nil, err
	}
	return r.fixSettled(ctx, round, n, res.FixAttempt, res.FailingSuites, res.FailingArchive, res.FailingTail, targetBranch, close, true)
}

// failedSuites names the suites a run failed, in the gate's order.
func failedSuites(suites []*frontendv1.FeedMergeTestSuite) []string {
	var out []string
	for _, suite := range suites {
		if suite.GetFailed() != nil {
			out = append(out, suite.GetName())
		}
	}
	return out
}

// commit makes the merge commit of the gated head onto tip and fast-forwards
// the target to it -- unless the target moved since this attempt's rebase
// began, which starts the process over on its new tip.
func (r *run) commit(ctx context.Context, tip, head, targetBranch string) (outcome, bool, error) {
	const op = "daemon.merge.commit"
	// A HEAD THE MERGE BEHIND IT IS OVERTAKING NEVER COMMITS (race.go): it
	// waits for its stop, or goes on if the overtaking could not be recorded.
	if ok, overtaken := r.o.beginLanding(r); !ok {
		select {
		case <-overtaken:
			return outcome{}, true, nil
		case <-ctx.Done():
			return outcome{}, false, context.Cause(ctx)
		}
	}
	target := r.subject.targetDir
	now, err := r.git.ResolveRef(ctx, target, "HEAD")
	if err != nil {
		return outcome{}, false, fmt.Errorf("merge: resolving %s's tip before committing: %w", target, err)
	}
	if now != tip {
		r.o.log(ctx, r.ws).Info(op, "the target moved since the rebase began; the merge starts over on its new tip", dlog.Context{
			"workspace": string(r.ws), "target": targetBranch, "rebased_on": tip, "tip": now})
		return outcome{}, true, nil
	}
	round := r.openTab(ctx, TabCommitting)
	if err := r.checkpoint(ctx, TabCommitting, func(d *progressDoc) {
		d.TargetBranch, d.Tip, d.Head, d.MergeCommit, d.Turn = targetBranch, tip, head, "", ""
	}); err != nil {
		return outcome{}, false, err
	}
	return r.commitIn(ctx, round, tip, head, targetBranch)
}

// commitIn is the committing step in its round: the merge commit made in a
// scratch tree, RECORDED, and the target fast-forwarded to it.
func (r *run) commitIn(ctx context.Context, round tabRound, tip, head, targetBranch string) (outcome, bool, error) {
	const op = "daemon.merge.commit"
	target := r.subject.targetDir
	message := fmt.Sprintf("merge(%s): %s", targetBranch, r.subject.branch)
	r.setStep(ctx, footer.StepCommitting, func(f *footer.MergeFacts) { f.Line = committingLine(message) })
	r.upsert(round, committingTab(round.live(), message))
	settle := func(failure string) {
		r.upsert(round, committingTab(round.settled(r.o.nowMS(), failure), message))
		outcome := "succeeded"
		if failure != "" {
			outcome = "failed"
		}
		r.closeTab(ctx, round, outcome)
	}
	tree, err := r.makeTree(ctx, tip)
	if err != nil {
		settle("the merge commit's scratch tree could not be made")
		return outcome{}, false, err
	}
	defer r.dropTree(ctx)
	result, err := r.git.MergeNoFF(ctx, tree, head, message)
	if err != nil {
		settle("the merge commit could not be made")
		return outcome{}, false, fmt.Errorf("merge: making the merge commit of %s onto %s: %w", short(head), short(tip), err)
	}
	if result.Landed == nil {
		settle("the merge commit conflicted")
		return outcome{}, false, fmt.Errorf("merge: the merge commit of the rebased %s onto the tip it was rebased on conflicted in %s",
			r.subject.branch, strings.Join(result.Conflicted, ", "))
	}
	commit := result.Landed.SHA
	// AN ABANDONED MERGE NEVER MOVES THE TARGET, whatever its steps managed to
	// finish before the abandon reached them.
	if err := stillWanted(ctx); err != nil {
		settle("the merge was abandoned before the target moved")
		return outcome{}, false, err
	}
	// THE MERGE COMMIT IS RECORDED BEFORE THE TARGET MOVES TO IT: a resume
	// then knows whether the fast-forward it finds already happened.
	if err := r.checkpoint(ctx, TabCommitting, func(d *progressDoc) { d.MergeCommit = commit }); err != nil {
		settle("the merge's progress could not be recorded before the target moved")
		return outcome{}, false, err
	}
	if err := r.git.FastForward(ctx, target, commit); err != nil {
		settle("the target could not be fast-forwarded to the merge commit")
		return outcome{}, false, fmt.Errorf("merge: fast-forwarding %s to %s: %w", targetBranch, commit, err)
	}
	settle("")
	r.o.log(ctx, r.ws).Info(op, "the target was fast-forwarded to the merge commit of the gated branch", dlog.Context{
		"workspace": string(r.ws), "target": targetBranch, "commit": commit, "gated": head})
	out, err := r.landed(ctx, commit, tip, targetBranch)
	return out, false, err
}

// landed reads what a merge commit targetBranch now carries brought in, and
// tags it with the merge's version, counted from base, the tip it landed on.
//
// It answers an error only for the daemon's exit while tagging, which stops
// the step before its landing is recorded (tagVersion).
func (r *run) landed(ctx context.Context, commit, base, targetBranch string) (outcome, error) {
	r.o.noteTargetTip(r.repo, commit)
	version, err := r.tagVersion(ctx, commit, base)
	if err != nil {
		return outcome{}, err
	}
	commits, err := r.git.LandedRange(ctx, r.subject.targetDir, commit)
	if err != nil && (r.stopping(err) || r.cut(err)) {
		return outcome{landed: commit, target: targetBranch, version: version}, nil
	}
	if err != nil {
		// THE LANDING HAS HAPPENED. A range that will not read is a fault in
		// the account of it, not a failed merge: it is recorded, and the run
		// concludes as merged with no range (so no deploy is told of it).
		r.o.log(ctx, r.ws).Error("daemon.merge.commit", "the landed merge's range could not be read; no deploy is told of it", dlog.Context{
			"workspace": string(r.ws), "commit": commit, "error": err.Error()})
		commits = nil
	}
	return outcome{landed: commit, target: targetBranch, commits: commits, version: version}, nil
}

// resumeCommit goes on with a committing step a restart interrupted. What the
// step can leave is: the target at the recorded merge commit (the
// fast-forward happened), or the target still at the tip (it did not, and the
// commit is made again). A target that moved elsewhere moved under the merge,
// and the process starts over on it exactly as it would have before the
// restart.
func (r *run) resumeCommit(ctx context.Context, res *progressDoc) (outcome, bool, error) {
	const op = "daemon.merge.resume"
	target, branch := r.subject.targetDir, r.subject.branch
	round := r.activeRound()
	now, err := r.git.ResolveRef(ctx, target, "HEAD")
	if err != nil {
		return outcome{}, false, fmt.Errorf("merge: resolving %s's tip: %w", target, err)
	}
	message := fmt.Sprintf("merge(%s): %s", res.TargetBranch, branch)
	if res.MergeCommit != "" && now == res.MergeCommit {
		r.o.log(ctx, r.ws).Info(op, "the target was fast-forwarded to the merge commit before the restart; the merge has landed", dlog.Context{
			"workspace": string(r.ws), "target": res.TargetBranch, "commit": res.MergeCommit})
		r.upsert(round, committingTab(round.settled(r.o.nowMS(), ""), message))
		r.closeTab(ctx, round, "succeeded")
		out, err := r.landed(ctx, res.MergeCommit, res.Tip, res.TargetBranch)
		return out, false, err
	}
	head, err := r.git.ResolveRef(ctx, r.subject.dir, branch)
	if err != nil {
		return outcome{}, false, fmt.Errorf("merge: resolving %s: %w", branch, err)
	}
	if head != res.Head {
		return *r.contradiction(ctx, fmt.Sprintf("%s is at %s, not at the gated head %s the record commits", branch, short(head), short(res.Head))), false, nil
	}
	if now != res.Tip {
		r.o.log(ctx, r.ws).Info(op, "the target moved since the rebase began; the merge starts over on its new tip", dlog.Context{
			"workspace": string(r.ws), "target": res.TargetBranch, "rebased_on": res.Tip, "tip": now})
		r.upsert(round, committingTab(round.settled(r.o.nowMS(), "the target moved; the merge starts over on its new tip"), message))
		r.closeTab(ctx, round, "restarted")
		return outcome{}, true, nil
	}
	r.o.log(ctx, r.ws).Info(op, "the merge commit is made again: the target had not moved to it before the restart", dlog.Context{
		"workspace": string(r.ws), "target": res.TargetBranch, "tip": res.Tip, "gated": head})
	if err := r.checkpoint(ctx, TabCommitting, func(d *progressDoc) { d.MergeCommit = "" }); err != nil {
		return outcome{}, false, err
	}
	return r.commitIn(ctx, round, res.Tip, head, res.TargetBranch)
}

// stillWanted answers the reason a run must stop -- an abandon, the daemon's
// exit -- and nil while it may go on. Git and the shim stop on a cancelled
// context by themselves; this is the check at the steps that must not be
// taken at all once the run is no longer wanted.
func stillWanted(ctx context.Context) error {
	if ctx.Err() != nil {
		return context.Cause(ctx)
	}
	return nil
}

// mergeTreesDir is where the queue's scratch trees live, under the state root.
const mergeTreesDir = "merge-trees"

// treeFor names one scratch tree: one directory per lease and attempt, so no
// two attempts, and no two merges, ever share one.
func treeFor(stateDir string, lease ids.LeaseID, attempt int) string {
	return filepath.Join(stateDir, mergeTreesDir, fmt.Sprintf("%s-%d", lease, attempt))
}

// makeTree makes the merge commit's scratch tree: the target's tip, detached.
func (r *run) makeTree(ctx context.Context, base string) (string, error) {
	r.attempts++
	dir := treeFor(r.o.deps.StateDir, r.lease.ID, r.attempts)
	if err := os.MkdirAll(filepath.Dir(dir), 0o755); err != nil {
		return "", fmt.Errorf("merge: making the queue's tree directory: %w", err)
	}
	if err := r.git.AddDetachedWorktree(ctx, r.subject.targetDir, dir, base); err != nil {
		return "", fmt.Errorf("merge: making the queue's merge tree at %s: %w", dir, err)
	}
	r.tree = dir
	return dir, nil
}

// dropTree removes the scratch tree, if one stands. Its failure is recorded and
// does not end the run: the tree is the queue's own and holds nothing the
// target depends on, and the boot recovery sweeps what is left.
func (r *run) dropTree(ctx context.Context) {
	if r.tree == "" {
		return
	}
	tree := r.tree
	r.tree = ""
	// A SUSPENDED RUN LEAVES ITS TREE to the boot that resumes it, which sweeps
	// it: its gate starts no git.
	if r.exiting() {
		return
	}
	if err := r.git.RemoveWorktree(context.WithoutCancel(ctx), r.subject.targetDir, tree); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.tree", "could not remove the queue's merge tree", dlog.Context{
			"workspace": string(r.ws), "tree": tree, "error": err.Error()})
	}
}

// consumeEscalation reads the escalation record from the worktree the agent
// fixes in and REMOVES it, so a record is answered once.
func (r *run) consumeEscalation(ctx context.Context) (string, bool) {
	why, gaveUp := readEscalation(r.subject.dir)
	if !gaveUp {
		return "", false
	}
	path := filepath.Join(r.subject.dir, EscalationFile)
	if err := os.Remove(path); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.fixes", "could not remove the escalation record it read", dlog.Context{
			"workspace": string(r.ws), "path": path, "error": err.Error()})
	}
	return why, true
}

// mergeMachinery is what a repair may never change in the middle of a merge:
// the merge orchestrator itself and the gate's entrypoint (owner ruling,
// 2026-09-28). A fix to either goes through a branch of its own and a deploy.
var mergeMachinery = []string{
	"modules/app/agent-repl/daemon/internal/merge/",
	"modules/app/agent-repl/bin/test-all.sh",
	// The entry point's logic: test-all.sh builds and runs this.
	"modules/app/agent-repl/testrun/",
}

// isMachinery reports whether a repository path is merge machinery.
func isMachinery(path string) bool {
	for _, m := range mergeMachinery {
		if path == m || (strings.HasSuffix(m, "/") && strings.HasPrefix(path, m)) {
			return true
		}
	}
	return false
}

// machineryIn answers the merge machinery the branch changes against the
// target: `<target>...<branch>`, the branch's own contribution since the two
// diverged, so what the target itself brought in is never counted.
func (r *run) machineryIn(ctx context.Context, targetBranch string) (map[string]bool, error) {
	rangeSpec := targetBranch + "..." + r.subject.branch
	paths, err := r.git.ChangedPaths(ctx, r.subject.dir, rangeSpec)
	if err != nil {
		return nil, fmt.Errorf("merge: reading what %s changes: %w", rangeSpec, err)
	}
	found := map[string]bool{}
	for _, path := range paths {
		if isMachinery(path) {
			found[path] = true
		}
	}
	return found, nil
}

// noteMachinery records, before the first TEST repair turn since the branch
// last changed (a resolved conflict changes it), the merge machinery the
// branch already changes: that is the author's own work, which the merge is
// there to land. Only what a test repair adds to it is refused; a conflict
// resolution is never refused for it.
func (r *run) noteMachinery(ctx context.Context, targetBranch string) error {
	if r.machinery != nil {
		return nil
	}
	found, err := r.machineryIn(ctx, targetBranch)
	if err != nil {
		return err
	}
	r.machinery = found
	return nil
}

// machineryChanged answers why a repair that changed merge machinery the
// branch did not already change is refused, and whether one did.
func (r *run) machineryChanged(ctx context.Context, targetBranch string) (string, bool, error) {
	found, err := r.machineryIn(ctx, targetBranch)
	if err != nil {
		return "", false, err
	}
	var added []string
	for path := range found {
		if !r.machinery[path] {
			added = append(added, path)
		}
	}
	if len(added) == 0 {
		return "", false, nil
	}
	sort.Strings(added)
	line := fmt.Sprintf("the repair changed the merge machinery mid-merge (%s); a fix to the merge or its gate lands through a branch of its own",
		strings.Join(added, ", "))
	r.o.log(ctx, r.ws).Warn("daemon.merge.machinery", "refused a repair that changed the merge machinery mid-merge", dlog.Context{
		"workspace": string(r.ws), "branch": r.subject.branch, "paths": strings.Join(added, ", ")})
	return line, true, nil
}

// pickLine is the rebase command replaying one commit, as the footer draws it.
func pickLine(c gitclient.Commit) string {
	return "pick " + commitLine(c.SHA, c.Subject)
}

// failureLine is an error's first line, git's own words when git failed.
func failureLine(err error) string {
	var failure *gitclient.Error
	if errors.As(err, &failure) {
		if line := firstLine(failure.Stderr); line != "no reason was given" {
			return line
		}
	}
	return firstLine(err.Error())
}

// firstLine keeps a composed line to one line, whatever the agent wrote.
func firstLine(text string) string {
	line, _, _ := strings.Cut(strings.TrimSpace(text), "\n")
	if line == "" {
		return "no reason was given"
	}
	return line
}

// short renders a sha the way the narration names commits.
func short(sha string) string {
	if len(sha) > 12 {
		return sha[:12]
	}
	return sha
}

// stopping reports whether a step's error is the daemon's exit taking the run
// away, which is no fault of the step's and is recorded by the suspension
// alone.
func (r *run) stopping(err error) bool {
	return errors.Is(err, errMergeStopping) || r.exiting()
}

// cut reports whether a step's error is the run's own context ending under
// it: an abandon or the daemon's exit took the run away mid-command, so the
// error says nothing about the step, and the run's end records why.
func (r *run) cut(err error) bool {
	return cutUnder(r.ctx, err)
}

// cutUnder reports whether a step's error is ctx ending under it, so the
// error says nothing about the step: the run's own context (run.cut), or a
// narrower one a step ran under, such as a gate's.
func cutUnder(ctx context.Context, err error) bool {
	return ctxend.Is(err) && ctx.Err() != nil
}
