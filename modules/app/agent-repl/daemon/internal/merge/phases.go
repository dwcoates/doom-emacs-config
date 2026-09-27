package merge

import (
	"context"
	"fmt"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// This file holds the Emacs-repo method's three working phases: the landing,
// the conflicts it can stop on, and the test gate with its fixes loop.

// mergeTab lands the source branch on the target's default branch as a NO-FF
// MERGE COMMIT — one commit to apply and one to revert — narrating the landing
// as daemon-composed lines. A conflict opens the conflicts tab, and the commit
// is concluded there.
//
// The merge happens IN THE TARGET DIRECTORY. There is no temporary worktree and
// no replay: a merge commit carries the whole branch, so there is no per-commit
// state for anything to go wrong in the middle of.
func (r *run) mergeTab(ctx context.Context) (string, bool, error) {
	const op = "daemon.merge.merge_tab"
	round := r.openTab(ctx, TabMerge)
	target := r.job.Layout.TargetDir
	branch, err := r.o.deps.Git.DefaultBranch(ctx, target)
	if err != nil {
		return "", false, fmt.Errorf("merge: resolving %s's default branch: %w", target, err)
	}
	lines := []string{fmt.Sprintf("merging %s into %s", r.job.Layout.SourceBranch, branch)}
	r.upsert(TabMerge, round, mergeTabRow(nil, lines, 0, ""))

	message := fmt.Sprintf("merge(%s): %s", branch, r.job.Layout.SourceBranch)
	result, err := r.o.deps.Git.MergeNoFF(ctx, target, r.job.Layout.SourceBranch, message)
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "the merge could not be attempted", dlog.Context{
			"workspace": string(r.ws), "target": target, "branch": r.job.Layout.SourceBranch, "error": err.Error()})
		lines = append(lines, fmt.Sprintf("merge failed: %v", err))
		r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), "the merge could not be attempted"))
		r.closeTab(ctx, TabMerge, round, "failed")
		return "", false, err
	}
	if result.Landed != nil {
		lines = append(lines, fmt.Sprintf("merged cleanly · %s", short(result.Landed.SHA)))
		r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), ""))
		r.closeTab(ctx, TabMerge, round, "succeeded")
		r.o.log(ctx, r.ws).Debug(op, "the merge landed cleanly", dlog.Context{
			"workspace": string(r.ws), "commit": result.Landed.SHA})
		return result.Landed.SHA, false, nil
	}
	lines = append(lines, fmt.Sprintf("conflicted in %d file(s)", len(result.Conflicted)))
	r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), "the merge conflicted"))
	r.closeTab(ctx, TabMerge, round, "conflicted")
	r.o.log(ctx, r.ws).Warn(op, "the merge conflicted", dlog.Context{
		"workspace": string(r.ws), "target": target, "files": strings.Join(result.Conflicted, ", ")})
	return r.conflicts(ctx, result.Conflicted, message)
}

// conflicts hands the conflict to the agent EXACTLY ONCE and then parks.
//
// ONCE IS THE WHOLE POLICY. An agent that could not resolve a conflict on the
// facts it was given will not do better on the same facts, and a second
// unattended pass over a half-resolved index is how a merge corrupts a target.
// So the loop is: brief once, look at the index the agent left, and either
// conclude the commit or hand the merge to a human.
func (r *run) conflicts(ctx context.Context, files []string, message string) (string, bool, error) {
	const op = "daemon.merge.conflicts"
	round := r.openTab(ctx, TabConflicts)
	r.address(TabConflicts, round)
	r.upsert(TabConflicts, round, conflictsTabRow(&frontendv1.FeedMergeTabLive{}, "", 0, ""))

	tip, err := r.o.deps.Git.ResolveRef(ctx, r.job.Layout.SourceDir, r.job.Layout.SourceBranch)
	if err != nil {
		return "", false, fmt.Errorf("merge: resolving %s: %w", r.job.Layout.SourceBranch, err)
	}
	r.mu.Lock()
	briefed := r.conflictBriefed[tip]
	r.conflictBriefed[tip] = true
	r.mu.Unlock()
	if !briefed {
		text, err := r.o.deps.Briefs(BriefConflictResolve, map[string]string{
			"conflict_commit": short(tip),
			"source_branch":   r.job.Layout.SourceBranch,
			"target_dir":      r.job.Layout.TargetDir,
		})
		if err != nil {
			return "", false, fmt.Errorf("merge: composing the conflict brief: %w", err)
		}
		if _, err := r.submit(ctx, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); err != nil {
			return "", false, err
		}
	}
	for {
		commit, resolved, err := r.concludeConflict(ctx, round, message)
		if err != nil {
			return "", false, err
		}
		if resolved {
			return commit, false, nil
		}
		line := r.conflictLine(ctx)
		r.o.log(ctx, r.ws).Warn(op, "parked a merge on unresolved conflicts", dlog.Context{
			"workspace": string(r.ws), "files": strings.Join(files, ", "), "line": line})
		r.upsert(TabConflicts, round, conflictsTabRow(nil, line, 0, ""))
		guidance, ok := r.park(ctx, line)
		if !ok {
			return "", true, nil
		}
		r.upsert(TabConflicts, round, conflictsTabRow(&frontendv1.FeedMergeTabLive{}, "", 0, ""))
		if err := r.deliverGuidance(ctx, guidance); err != nil {
			return "", false, err
		}
	}
}

// concludeConflict inspects the index the agent left. A CLEAN INDEX means the
// resolution is staged and the daemon concludes the merge commit itself; the
// brief tells the agent to stop before committing precisely so this step exists.
func (r *run) concludeConflict(ctx context.Context, round int, message string) (string, bool, error) {
	remaining, err := r.o.deps.Git.ConflictedFiles(ctx, r.job.Layout.TargetDir)
	if err != nil {
		return "", false, fmt.Errorf("merge: reading the conflicted files: %w", err)
	}
	if len(remaining) > 0 {
		return "", false, nil
	}
	sha, err := r.o.deps.Git.Commit(ctx, r.job.Layout.TargetDir, message)
	if err != nil {
		return "", false, fmt.Errorf("merge: concluding the merge commit: %w", err)
	}
	r.upsert(TabConflicts, round, conflictsTabRow(nil, "", r.o.nowMS(), ""))
	r.closeTab(ctx, TabConflicts, round, "succeeded")
	r.o.log(ctx, r.ws).Debug("daemon.merge.conflicts", "the agent's resolution concluded the merge",
		dlog.Context{"workspace": string(r.ws), "commit": sha})
	return sha, true, nil
}

// conflictLine composes the parked merge's standing line. The footer draws it
// verbatim and the parked tab shows the same sentence, so a parked merge has
// ONE account rather than two that can disagree.
func (r *run) conflictLine(ctx context.Context) string {
	files, err := r.o.deps.Git.ConflictedFiles(ctx, r.job.Layout.TargetDir)
	if err != nil || len(files) == 0 {
		return "parked for your input — the merge's conflicts are unresolved"
	}
	if len(files) == 1 {
		return fmt.Sprintf("parked for your input — 1 conflict remains in %s", files[0])
	}
	return fmt.Sprintf("parked for your input — %d conflicts remain in %s", len(files), strings.Join(files[:min(len(files), 3)], ", "))
}

// gate runs the test gate on the tree the merge produced, and its fixes loop.
//
// The gate reports a failure summary, whether the run parked, and any error.
// THERE IS NO FLAKE RE-RUN: the loop's only exits are a passing suite and the
// agent's own escalation record.
func (r *run) gate(ctx context.Context, commit string) (string, bool, error) {
	const op = "daemon.merge.tests"
	rangeSpec := fmt.Sprintf("%s^1..%s", commit, commit)
	paths, err := r.o.deps.Git.ChangedPaths(ctx, r.job.Layout.TargetDir, rangeSpec)
	if err != nil {
		r.o.log(ctx, r.ws).Warn(op, "could not read the landed range's paths; every suite runs",
			dlog.Context{"workspace": string(r.ws), "range": rangeSpec, "error": err.Error()})
		paths = nil
	}
	selection := SelectSuites(paths)
	r.o.log(ctx, r.ws).Debug(op, "selected the merge's suites", dlog.Context{
		"workspace": string(r.ws), "suites": strings.Join(selection.Suites, ","),
		"full": selection.Full, "reason": selection.Reason})

	for {
		round := r.openTab(ctx, TabTests)
		r.upsert(TabTests, round, testsTabRow(&frontendv1.FeedMergeTabLive{}, nil, 0, ""))
		result, err := r.o.runGate(ctx, r.lease.ID, round, r.job.Layout.TargetDir, selection)
		if err != nil {
			r.upsert(TabTests, round, testsTabRow(nil, nil, r.o.nowMS(), "the test gate could not run"))
			r.closeTab(ctx, TabTests, round, "failed")
			return "", false, err
		}
		if result.Passed {
			r.upsert(TabTests, round, testsTabRow(nil, result.Suites, r.o.nowMS(), ""))
			r.closeTab(ctx, TabTests, round, "succeeded")
			r.o.log(ctx, r.ws).Debug(op, "the merge's suites passed", dlog.Context{
				"workspace": string(r.ws), "round": round, "archive": result.ArchivePath})
			return "", false, nil
		}
		summary := fmt.Sprintf("the test suite failed (exit %d); the whole run is archived at %s", result.ExitCode, result.ArchivePath)
		r.upsert(TabTests, round, testsTabRow(nil, result.Suites, r.o.nowMS(), summary))
		r.closeTab(ctx, TabTests, round, "failed")
		r.o.log(ctx, r.ws).Warn(op, "the merge's suites failed", dlog.Context{
			"workspace": string(r.ws), "round": round, "exit_code": result.ExitCode, "archive": result.ArchivePath})

		done, parked, err := r.fixes(ctx, result)
		if err != nil || parked {
			return summary, parked, err
		}
		if done {
			return summary, false, nil
		}
	}
}

// fixes hands one failing run to the agent and commits what it staged.
//
// The loop has NO ATTEMPT LIMIT by design: an agent that is making progress is
// asked again with the new failing output. Its one non-passing exit is the
// agent's OWN JUDGEMENT, recorded as the escalation file — which parks the
// merge for a human rather than failing it silently.
func (r *run) fixes(ctx context.Context, failing GateResult) (bool, bool, error) {
	const op = "daemon.merge.fixes"
	round := r.openTab(ctx, TabFixes)
	r.address(TabFixes, round)
	r.upsert(TabFixes, round, fixesTabRow(&frontendv1.FeedMergeTabLive{}, "", 0, ""))

	text, err := r.o.deps.Briefs(BriefTestFailureResolve, map[string]string{
		"source_branch":     r.job.Layout.SourceBranch,
		"target_dir":        r.job.Layout.TargetDir,
		"failure_tail":      failing.Tail,
		"escalation_file":   EscalationFile,
		"escalation_marker": EscalationMarker,
	})
	if err != nil {
		return false, false, fmt.Errorf("merge: composing the test-failure brief: %w", err)
	}
	if _, err := r.submit(ctx, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); err != nil {
		return false, false, err
	}
	if why, gaveUp := readEscalation(r.job.Layout.TargetDir); gaveUp {
		line := fmt.Sprintf("parked for your input — the agent escalated: %s", firstLine(why))
		r.o.log(ctx, r.ws).Warn(op, "the fixes agent escalated the merge", dlog.Context{
			"workspace": string(r.ws), "round": round, "reason": why})
		r.upsert(TabFixes, round, fixesTabRow(nil, line, 0, ""))
		r.closeTab(ctx, TabFixes, round, "escalated")
		r.park(ctx, line)
		return false, true, nil
	}
	if _, err := r.o.deps.Git.Commit(ctx, r.job.Layout.TargetDir,
		fmt.Sprintf("fix(merge): repair the suite after merging %s", r.job.Layout.SourceBranch)); err != nil {
		r.o.log(ctx, r.ws).Warn(op, "the fixes turn staged nothing to commit", dlog.Context{
			"workspace": string(r.ws), "round": round, "error": err.Error()})
	}
	r.upsert(TabFixes, round, fixesTabRow(nil, "", r.o.nowMS(), ""))
	r.closeTab(ctx, TabFixes, round, "succeeded")
	return false, false, nil
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

// park stops the run for the user: the lease's policy flips to PARKED, so the
// prompt queue stops refusing this workspace's submissions and routes them here
// instead. THE LEASE STATE IS THE RECOGNITION — no classifier and no content
// inspection decides that a prompt is guidance.
//
// It blocks until guidance arrives or the context ends. The bool is false when
// the run should stop holding on.
func (r *run) park(ctx context.Context, line string) (guidance, bool) {
	if err := r.o.deps.DB.SetLeasePolicy(ctx, r.lease.ID, wsm.PolicyParked); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.park", "could not move the lease to parked",
			dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	r.facts(StateParked, line)
	if r.o.onPark != nil {
		r.o.onPark(r.ws)
	}
	select {
	case g := <-r.guidance:
		return g, true
	case <-ctx.Done():
		return guidance{}, false
	}
}

// guidance is one parked submission on its way to the resolution agent: what
// was said, and the submission's own turn, which the guidance runs under.
type guidance struct {
	turn ids.TurnID
	said *conversationv1.UserSaid
}

// deliverGuidance hands one parked submission to the resolution agent and waits
// for its turn to end. The answer travels back to RouteParked, so a caller
// learns whether its guidance was accepted.
func (r *run) deliverGuidance(ctx context.Context, g guidance) error {
	err := r.o.deps.ParkedRoute(ctx, r.ws, g.turn, g.said)
	r.answered <- err
	if err != nil {
		return nil
	}
	if err := r.o.deps.DB.SetLeasePolicy(ctx, r.lease.ID, wsm.PolicyRefuse); err != nil {
		return err
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	r.facts(StateMerging, "")
	_, err = r.o.deps.AwaitTurnEnd(ctx, r.ws, g.turn)
	return err
}

// min is the smallest of two ints.
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
