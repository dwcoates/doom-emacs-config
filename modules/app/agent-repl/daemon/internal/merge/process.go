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

	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
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
	r.doneResumingAt(TabPrePrompt)
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
	return r.afterRebase(ctx, tip, targetBranch, 0, false)
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
	p := &replay{round: r.activeRound(), commits: fromCommitDocs(res.Commits), done: res.Replayed, lines: res.Lines}
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
		return r.afterRebase(ctx, tip, targetBranch, 0, false)
	case TabConflicts:
		if out, again, err, done := finish(r.resumeConflict(ctx, p, tip, targetBranch, res)); done {
			return out, again, err
		}
		return r.afterRebase(ctx, tip, targetBranch, 0, false)
	case TabTests:
		if gaveUp, err := r.rebaseSettled(ctx, tip, targetBranch); err != nil || gaveUp != nil {
			out, again, err, _ := finish(gaveUp, err)
			return out, again, err
		}
		r.o.log(ctx, r.ws).Info("daemon.merge.resume", "the test gate the restart cut runs again from its start", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "round": r.activeRound().n, "attempt": res.FixAttempt})
		return r.afterRebase(ctx, tip, targetBranch, res.FixAttempt, true)
	case TabFixes:
		if gaveUp, err := r.resumeFix(ctx, res, targetBranch); err != nil || gaveUp != nil {
			out, again, err, _ := finish(gaveUp, err)
			return out, again, err
		}
		return r.afterRebase(ctx, tip, targetBranch, res.FixAttempt, false)
	default:
		return r.resumeCommit(ctx, res)
	}
}

// alreadyOn concludes a merge whose branch the target already contains: it has
// landed, with nothing to merge and nothing to deploy, and says so.
func (r *run) alreadyOn(ctx context.Context, targetBranch string) (outcome, error) {
	round := r.openTab(ctx, TabRebasing)
	line := fmt.Sprintf("already on %s; nothing to merge", targetBranch)
	r.upsert(round, rebasingTab(round.settled(r.o.nowMS(), ""), 0, 0, []string{line}))
	r.closeTab(ctx, round, "succeeded")
	tip, err := r.git.ResolveRef(ctx, r.subject.dir, r.subject.branch)
	if err != nil {
		return outcome{}, fmt.Errorf("merge: resolving %s: %w", r.subject.branch, err)
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.rebase", "the branch is already on its target; the merge concludes with nothing to land", dlog.Context{
		"workspace": string(r.ws), "branch": r.subject.branch, "target": targetBranch, "tip": tip})
	return outcome{landed: tip, alreadyOn: targetBranch}, nil
}

// replay is one rebase's progress: the commits it replays, how many are done,
// and the narration its tab draws.
type replay struct {
	round   tabRound
	commits []gitclient.Commit
	done    int
	lines   []string
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
	commits, err := r.git.CommitsBetween(ctx, dir, tip, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: listing what %s replays onto %s: %w", branch, short(tip), err)
	}
	p := &replay{round: r.openTab(ctx, TabRebasing), commits: commits}
	based, err := r.git.IsAncestor(ctx, dir, tip, branch)
	if err != nil {
		return nil, fmt.Errorf("merge: asking whether %s is already on %s's tip: %w", branch, targetBranch, err)
	}
	if based {
		// ALREADY ON THE TIP: the branch was rebased before (a start-over whose
		// target did not move after all, or an author who rebased by hand), so
		// there is nothing to replay.
		p.done = len(commits)
		p.lines = append(p.lines, fmt.Sprintf("%s is already on %s's tip; nothing to replay", branch, targetBranch))
		r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) { f.Replayed, f.Total = p.done, len(commits) })
		r.upsert(p.round, rebasingTab(p.round.settled(r.o.nowMS(), ""), p.done, len(commits), p.lines))
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
	r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) {
		f.Total = len(commits)
		f.Line = rebaseCommandLine(pickLine(commits[0]))
	})
	p.lines = append(p.lines, fmt.Sprintf("replaying %d commits of %s onto %s at %s", len(commits), branch, targetBranch, short(tip)))
	r.upsert(p.round, rebasingTab(p.round.live(), 0, len(commits), p.lines))
	// THE REBASE'S RECORD NAMES THE TIP, THE COMMITS AND THE BRANCH'S HEAD
	// BEFORE IT: a resume reads where git stopped against exactly these.
	if err := r.checkpoint(ctx, TabRebasing, func(d *progressDoc) {
		d.TargetBranch, d.Tip, d.BranchHead = targetBranch, tip, head
		d.Commits, d.Replayed, d.Lines = toCommitDocs(commits), 0, p.lines
		d.Turn, d.ConflictFiles, d.FixAttempt, d.FailingSuites, d.Head, d.MergeCommit = "", nil, 0, nil, "", ""
		d.FailingArchive, d.FailingTail = "", ""
	}); err != nil {
		return nil, err
	}
	shas := make([]string, len(commits))
	for i, c := range commits {
		shas[i] = c.SHA
	}
	r.o.log(ctx, r.ws).Info(op, "rebasing the branch onto the target's tip", dlog.Context{
		"workspace": string(r.ws), "branch": branch, "worktree": dir, "onto": tip, "commits": len(commits)})
	step, err := r.git.StartRebase(ctx, dir, tip, shas)
	return r.replayLoop(ctx, p, step, err, tip, targetBranch)
}

// replayLoop follows a rebase from where one command left it to its end: each
// replayed commit recorded, each conflict handed to the requester's session.
func (r *run) replayLoop(ctx context.Context, p *replay, step gitclient.RebaseStep, err error, tip, targetBranch string) (*outcome, error) {
	dir, branch := r.subject.dir, r.subject.branch
	for {
		if err != nil {
			if errors.Is(err, errMergeStopping) {
				return nil, err
			}
			r.rebaseFailed(ctx, p, err)
			return nil, fmt.Errorf("merge: rebasing %s onto %s: %w", branch, short(tip), err)
		}
		if len(step.Conflicted) > 0 {
			gaveUp, err := r.conflict(ctx, p, step.Conflicted, targetBranch)
			if err != nil || gaveUp != nil {
				return gaveUp, err
			}
			step, err = r.git.ContinueRebase(ctx, dir)
			continue
		}
		r.replayed(ctx, p, step.Done)
		if step.Done {
			return nil, nil
		}
		if err := r.checkpoint(ctx, TabRebasing, func(d *progressDoc) { d.Replayed, d.Lines = p.done, p.lines }); err != nil {
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
		r.upsert(p.round, rebasingTab(p.round.live(), p.done, len(p.commits), p.lines))
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
		shas := make([]string, len(p.commits))
		for i, c := range p.commits {
			shas[i] = c.SHA
		}
		step, err := r.git.StartRebase(ctx, dir, tip, shas)
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
	current := p.commits[min(p.done, len(p.commits)-1)]
	p.done++
	if done {
		p.done = len(p.commits)
	}
	p.lines = append(p.lines, fmt.Sprintf("replayed %d/%d · %s", p.done, len(p.commits), current.Subject))
	if done {
		r.updateFacts(func(f *footer.MergeFacts) { f.Replayed, f.Line = p.done, nil })
		r.upsert(p.round, rebasingTab(p.round.settled(r.o.nowMS(), ""), p.done, len(p.commits), p.lines))
		r.closeTab(ctx, p.round, "succeeded")
		return
	}
	next := p.commits[p.done]
	r.updateFacts(func(f *footer.MergeFacts) { f.Replayed, f.Line = p.done, rebaseCommandLine(pickLine(next)) })
	r.upsert(p.round, rebasingTab(p.round.live(), p.done, len(p.commits), p.lines))
}

// rebaseFailed settles the rebasing tab and the footer's line on a rebase
// command that failed outright, with its first error line.
func (r *run) rebaseFailed(ctx context.Context, p *replay, err error) {
	line := failureLine(err)
	r.updateFacts(func(f *footer.MergeFacts) { f.Line = rebaseFailureLine(line) })
	p.lines = append(p.lines, "failed: "+line)
	r.settleRebasing(ctx, p, "the rebase failed")
}

// settleRebasing settles a rebasing round as failed.
func (r *run) settleRebasing(ctx context.Context, p *replay, summary string) {
	r.upsert(p.round, rebasingTab(p.round.settled(r.o.nowMS(), summary), p.done, len(p.commits), p.lines))
	r.closeTab(ctx, p.round, "failed")
}

// conflict hands a replayed commit's conflicts to the requester's session and
// answers the failed outcome when the resolution gave up. On a resolution the
// rebase continues in a new rebasing round.
func (r *run) conflict(ctx context.Context, p *replay, files []string, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.conflicts"
	commit := p.commits[min(p.done, len(p.commits)-1)]
	p.lines = append(p.lines, fmt.Sprintf("%s conflicted in %d file(s)", commitLine(commit.SHA, commit.Subject), len(files)))
	r.settleRebasing(ctx, p, "the rebase stopped on a conflict")
	r.o.log(ctx, r.ws).Info(op, "a replayed commit conflicted; the requester's session resolves it", dlog.Context{
		"workspace": string(r.ws), "commit": commit.SHA, "files": strings.Join(files, ", "), "worktree": r.subject.dir})
	round := r.openTab(ctx, TabConflicts)
	r.address(round)
	r.setStep(ctx, footer.StepConflictResolution, func(f *footer.MergeFacts) { f.Line = conflictLine(commit.Subject, len(files)) })
	r.upsert(round, conflictsTab(round.live()))
	if err := r.noteMachinery(ctx, targetBranch); err != nil {
		return nil, err
	}
	text, err := r.conflictBrief(commit, files, targetBranch)
	if err != nil {
		return nil, err
	}
	if err := r.ensureSession(ctx); err != nil {
		return nil, fmt.Errorf("merge: starting a session for the conflict resolution: %w", err)
	}
	turn := wsm.NewTurnID()
	if err := r.checkpoint(ctx, TabConflicts, func(d *progressDoc) {
		d.Replayed, d.Lines, d.ConflictFiles, d.Turn = p.done, p.lines, files, string(turn)
	}); err != nil {
		return nil, err
	}
	close, err := r.submit(ctx, turn, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)
	if err != nil {
		return nil, err
	}
	return r.conflictSettled(ctx, p, round, close, commit, targetBranch)
}

// conflictBrief composes the conflict resolution's brief.
func (r *run) conflictBrief(commit gitclient.Commit, files []string, targetBranch string) (string, error) {
	text, err := r.o.deps.Briefs(BriefConflictResolve, map[string]string{
		"conflict_commit":  commitLine(commit.SHA, commit.Subject),
		"source_branch":    r.subject.branch,
		"worktree_dir":     r.subject.dir,
		"target_branch":    targetBranch,
		"conflicted_files": strings.Join(files, ", "),
	})
	if err != nil {
		return "", fmt.Errorf("merge: composing the conflict brief: %w", err)
	}
	return text, nil
}

// conflictSettled reads the conflict resolution's end: the failed outcome when
// it gave up, or a new rebasing round the rebase continues in.
func (r *run) conflictSettled(ctx context.Context, p *replay, round tabRound, close wsm.TurnClose, commit gitclient.Commit, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.conflicts"
	if err := stillWanted(ctx); err != nil {
		return nil, err
	}
	remaining, err := r.git.ConflictedFiles(ctx, r.subject.dir)
	if err != nil {
		return nil, fmt.Errorf("merge: reading what still conflicts in %s: %w", r.subject.dir, err)
	}
	why := ""
	switch {
	case close.Failed():
		why = fmt.Sprintf("the conflict resolution's turn ended %s", close)
	case len(remaining) > 0:
		why = fmt.Sprintf("%d file(s) still conflict: %s", len(remaining), strings.Join(remaining, ", "))
	}
	if why == "" {
		if line, refused, err := r.machineryChanged(ctx, targetBranch); err != nil {
			return nil, err
		} else if refused {
			why = line
		}
	}
	if why != "" {
		summary := fmt.Sprintf("conflict resolution gave up: %s; the rebase is left in progress in %s", why, r.subject.dir)
		r.upsert(round, conflictsTab(round.settled(r.o.nowMS(), summary)))
		r.closeTab(ctx, round, "failed")
		r.o.log(ctx, r.ws).Info(op, "the conflict resolution gave up; the merge fails and the rebase is left in progress", dlog.Context{
			"workspace": string(r.ws), "why": why, "worktree": r.subject.dir})
		out := failedIn(footer.FailedConflicts, summary)
		return &out, nil
	}
	r.upsert(round, conflictsTab(round.settled(r.o.nowMS(), "")))
	r.closeTab(ctx, round, "succeeded")
	// THE REBASE CONTINUES IN A NEW ROUND: tabs never reopen.
	p.round = r.openTab(ctx, TabRebasing)
	p.lines = []string{fmt.Sprintf("the conflict in %s is resolved; the rebase continues", commit.Subject)}
	r.setStep(ctx, footer.StepRebasing, func(f *footer.MergeFacts) {
		f.Replayed, f.Total = p.done, len(p.commits)
		f.Line = rebaseCommandLine("git rebase --continue")
	})
	r.upsert(p.round, rebasingTab(p.round.live(), p.done, len(p.commits), p.lines))
	if err := r.checkpoint(ctx, TabRebasing, func(d *progressDoc) {
		d.Replayed, d.Lines, d.ConflictFiles, d.Turn = p.done, p.lines, nil, ""
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
	commit := p.commits[min(p.done, len(p.commits)-1)]
	round := r.activeRound()
	r.address(round)
	r.setStep(ctx, footer.StepConflictResolution, func(f *footer.MergeFacts) { f.Line = conflictLine(commit.Subject, len(res.ConflictFiles)) })
	r.upsert(round, conflictsTab(round.live()))
	text, err := r.conflictBrief(commit, res.ConflictFiles, targetBranch)
	if err != nil {
		return nil, err
	}
	close, err := r.reattachTurn(ctx, ids.TurnID(res.Turn), text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR)
	if err != nil {
		return nil, err
	}
	if gaveUp, err := r.conflictSettled(ctx, p, round, close, commit, targetBranch); err != nil || gaveUp != nil {
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
		var round tabRound
		if resumeTests && attempt == fromAttempt {
			round = r.activeRound()
		} else {
			round = r.openTab(ctx, TabTests)
		}
		if err := r.checkpoint(ctx, TabTests, func(d *progressDoc) {
			d.FixAttempt, d.Turn, d.FailingSuites, d.FailingArchive, d.FailingTail = attempt, "", nil, "", ""
		}); err != nil {
			return "", nil, err
		}
		verdict, err := r.gate(ctx, round, tip, head)
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
			return head, nil, nil
		case attempt == MaxFixAttempts:
			summary := fmt.Sprintf("the tests still fail after %d fixing attempts; the whole run is archived at %s", MaxFixAttempts, verdict.result.ArchivePath)
			r.o.log(ctx, r.ws).Info("daemon.merge.fixes", "the last fixing attempt left suites failing; the merge fails", dlog.Context{
				"workspace": string(r.ws), "attempts": MaxFixAttempts, "archive": verdict.result.ArchivePath})
			out := failedIn(footer.FailedTests, summary)
			return "", &out, nil
		}
		if gaveUp, err := r.fix(ctx, attempt+1, verdict.result, targetBranch); err != nil || gaveUp != nil {
			return "", gaveUp, err
		}
	}
}

// fix hands one failing run to the requester's session, which repairs the
// branch in its worktree: fixing attempt x of MaxFixAttempts. It answers the
// failed outcome when the agent escalated or changed the merge machinery.
func (r *run) fix(ctx context.Context, attempt int, failing GateResult, targetBranch string) (*outcome, error) {
	suites := failedSuites(failing.Suites)
	round := r.openTab(ctx, TabFixes)
	r.address(round)
	r.setStep(ctx, footer.StepFixing, func(f *footer.MergeFacts) {
		f.Attempt, f.MaxAttempts = attempt, MaxFixAttempts
		f.Line = fixingLine(suites)
	})
	r.upsert(round, fixesTab(round.live(), attempt))
	if err := r.noteMachinery(ctx, targetBranch); err != nil {
		return nil, err
	}
	text, err := r.fixBrief(attempt, suites, failing.ArchivePath, failing.Tail, targetBranch)
	if err != nil {
		return nil, err
	}
	if err := r.ensureSession(ctx); err != nil {
		return nil, fmt.Errorf("merge: starting a session for the fixing attempt: %w", err)
	}
	turn := wsm.NewTurnID()
	if err := r.checkpoint(ctx, TabFixes, func(d *progressDoc) {
		d.FixAttempt, d.Turn, d.FailingSuites = attempt, string(turn), suites
		d.FailingArchive, d.FailingTail = failing.ArchivePath, failing.Tail
	}); err != nil {
		return nil, err
	}
	if _, err := r.submit(ctx, turn, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); err != nil {
		return nil, err
	}
	return r.fixSettled(ctx, round, attempt, targetBranch)
}

// fixBrief composes one fixing attempt's brief.
func (r *run) fixBrief(attempt int, suites []string, archive, tail, targetBranch string) (string, error) {
	text, err := r.o.deps.Briefs(BriefTestFailureResolve, map[string]string{
		"source_branch":     r.subject.branch,
		"worktree_dir":      r.subject.dir,
		"target_branch":     targetBranch,
		"failing_suites":    strings.Join(suites, ", "),
		"archive_path":      archive,
		"failure_tail":      tail,
		"attempt":           fmt.Sprintf("%d", attempt),
		"max_attempts":      fmt.Sprintf("%d", MaxFixAttempts),
		"escalation_file":   EscalationFile,
		"escalation_marker": EscalationMarker,
	})
	if err != nil {
		return "", fmt.Errorf("merge: composing the test-failure brief: %w", err)
	}
	return text, nil
}

// fixSettled reads a fixing attempt's end: the failed outcome when the agent
// escalated or changed the merge machinery, nil when the gate runs again.
func (r *run) fixSettled(ctx context.Context, round tabRound, attempt int, targetBranch string) (*outcome, error) {
	const op = "daemon.merge.fixes"
	if err := stillWanted(ctx); err != nil {
		return nil, err
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
		r.upsert(round, fixesTab(round.settled(r.o.nowMS(), why), attempt))
		r.closeTab(ctx, round, "failed")
		r.o.log(ctx, r.ws).Info(op, "the fixing attempt gave up; the merge fails", dlog.Context{
			"workspace": string(r.ws), "attempt": attempt, "why": why})
		out := failedIn(footer.FailedTests, why)
		return &out, nil
	}
	r.upsert(round, fixesTab(round.settled(r.o.nowMS(), ""), attempt))
	r.closeTab(ctx, round, "succeeded")
	return nil, nil
}

// resumeFix goes on with a fixing attempt a restart interrupted: the merge
// reattaches to the fixing turn and reads its end.
func (r *run) resumeFix(ctx context.Context, res *progressDoc, targetBranch string) (*outcome, error) {
	round := r.activeRound()
	r.address(round)
	r.setStep(ctx, footer.StepFixing, func(f *footer.MergeFacts) {
		f.Attempt, f.MaxAttempts = res.FixAttempt, MaxFixAttempts
		f.Line = fixingLine(res.FailingSuites)
	})
	r.upsert(round, fixesTab(round.live(), res.FixAttempt))
	text, err := r.fixBrief(res.FixAttempt, res.FailingSuites, res.FailingArchive, res.FailingTail, targetBranch)
	if err != nil {
		return nil, err
	}
	if _, err := r.reattachTurn(ctx, ids.TurnID(res.Turn), text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); err != nil {
		return nil, err
	}
	return r.fixSettled(ctx, round, res.FixAttempt, targetBranch)
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
	if err := r.checkpoint(ctx, TabCommitting, func(d *progressDoc) { d.Head, d.MergeCommit, d.Turn = head, "", "" }); err != nil {
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
	return r.landed(ctx, commit), false, nil
}

// landed reads what a merge commit the target now carries brought in.
func (r *run) landed(ctx context.Context, commit string) outcome {
	commits, err := r.git.LandedRange(ctx, r.subject.targetDir, commit)
	if err != nil && r.stopping(err) {
		return outcome{landed: commit}
	}
	if err != nil {
		// THE LANDING HAS HAPPENED. A range that will not read is a fault in
		// the account of it, not a failed merge: it is recorded, and the run
		// concludes as merged with no range (so no deploy is told of it).
		r.o.log(ctx, r.ws).Error("daemon.merge.commit", "the landed merge's range could not be read; no deploy is told of it", dlog.Context{
			"workspace": string(r.ws), "commit": commit, "error": err.Error()})
		commits = nil
	}
	return outcome{landed: commit, commits: commits}
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
		return r.landed(ctx, res.MergeCommit), false, nil
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

// updateMain lands a branch ALREADY MERGED UPSTREAM: the default branch in the
// repository's main worktree is fetched and fast-forwarded to upstream. The
// daemon does not re-check the assertion (owner ruling, 2026-09-29); a pull
// that cannot happen -- a diverged or unchecked-out default branch, no
// network -- fails the merge (area other).
//
// A RESUMED UPDATE RUNS AGAIN in the round it was in: the fetch and the
// fast-forward are idempotent, and the tip recorded before the first run moved
// it keeps the account of what landed.
func (r *run) updateMain(ctx context.Context) (outcome, error) {
	const op = "daemon.merge.updating_main"
	main := r.subject.targetDir
	recordedBefore := ""
	resumed := false
	if res, ok := r.resumingAt(TabUpdatingMain); ok {
		recordedBefore, resumed = res.Before, true
		r.doneResuming()
	}
	defaultBranch, err := r.git.DefaultBranch(ctx, main)
	if err != nil {
		return outcome{}, fmt.Errorf("merge: resolving the default branch of %s: %w", main, err)
	}
	checkedOut, err := r.git.CurrentBranch(ctx, main)
	if err != nil {
		return outcome{}, fmt.Errorf("merge: reading the branch checked out in %s: %w", main, err)
	}
	if checkedOut != defaultBranch {
		return outcome{}, fmt.Errorf("merge: the main worktree %s has %q checked out, not the default branch %s", main, checkedOut, defaultBranch)
	}
	var round tabRound
	if resumed {
		round = r.activeRound()
	} else {
		round = r.openTab(ctx, TabUpdatingMain)
	}
	r.setStep(ctx, footer.StepUpdatingMain, func(f *footer.MergeFacts) { f.Line = fetchingLine() })
	r.upsert(round, updatingMainTab(round.live(), ""))
	settle := func(commit, failure string) {
		r.upsert(round, updatingMainTab(round.settled(r.o.nowMS(), failure), commit))
		outcome := "succeeded"
		if failure != "" {
			outcome = "failed"
		}
		r.closeTab(ctx, round, outcome)
	}
	before, err := r.git.ResolveRef(ctx, main, "HEAD")
	if err != nil {
		settle("", "the main worktree's tip could not be read")
		return outcome{}, fmt.Errorf("merge: resolving %s's tip: %w", main, err)
	}
	if recordedBefore != "" {
		before = recordedBefore
	}
	if err := r.checkpoint(ctx, TabUpdatingMain, func(d *progressDoc) { d.Before, d.Turn = before, "" }); err != nil {
		return outcome{}, err
	}
	if err := r.git.Fetch(ctx, main, "origin"); err != nil {
		settle("", "the fetch from upstream failed")
		return outcome{}, fmt.Errorf("merge: fetching upstream into %s: %w", main, err)
	}
	upstream, err := r.git.ResolveRef(ctx, main, "refs/remotes/origin/"+defaultBranch)
	if err != nil {
		settle("", "the upstream default branch could not be read")
		return outcome{}, fmt.Errorf("merge: resolving origin/%s: %w", defaultBranch, err)
	}
	current, err := r.git.ResolveRef(ctx, main, "HEAD")
	if err != nil {
		settle("", "the main worktree's tip could not be read")
		return outcome{}, fmt.Errorf("merge: resolving %s's tip: %w", main, err)
	}
	r.updateFacts(func(f *footer.MergeFacts) { f.Line = fastForwardingLine(short(upstream)) })
	r.upsert(round, updatingMainTab(round.live(), short(upstream)))
	if err := stillWanted(ctx); err != nil {
		settle(short(upstream), "the merge was abandoned before the default branch moved")
		return outcome{}, err
	}
	if current != upstream {
		if err := r.git.FastForward(ctx, main, upstream); err != nil {
			settle(short(upstream), "the default branch could not be fast-forwarded to upstream")
			return outcome{}, fmt.Errorf("merge: fast-forwarding %s to origin/%s: %w", defaultBranch, defaultBranch, err)
		}
	}
	settle(short(upstream), "")
	commits, err := r.git.CommitsBetween(ctx, main, before, upstream)
	if err != nil && r.stopping(err) {
		return outcome{}, err
	}
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "the updated range could not be read; no deploy is told of it", dlog.Context{
			"workspace": string(r.ws), "from": before, "to": upstream, "error": err.Error()})
		commits = nil
	}
	r.o.log(ctx, r.ws).Info(op, "the default branch was fast-forwarded to upstream", dlog.Context{
		"workspace": string(r.ws), "main": main, "branch": defaultBranch, "from": before, "to": upstream, "commits": len(commits)})
	return outcome{landed: upstream, commits: commits}, nil
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

// noteMachinery records, before the FIRST repair turn, the merge machinery the
// branch already changed: that is the author's own work, which the merge is
// there to land. Only what a repair adds to it is refused.
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
