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
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// This file holds the Emacs-repo method's ATTEMPT -- the merge made in the
// queue's own tree, its conflicts, the gate, the repair and the landing -- and
// the park every give-up-to-human path ends in.

// step is what one attempt asks for next: the run's conclusion (done), a park
// for the user (park), or -- neither set -- another attempt on the target's
// tip as it then stands.
type step struct {
	done *outcome
	park *parking
}

// parking is one park: the tab round it stands on and its standing line.
type parking struct {
	tab   string
	round int
	line  string
}

// parkAt builds the step that parks on one tab round.
func parkAt(tab string, round int, line string) step {
	return step{park: &parking{tab: tab, round: round, line: line}}
}

// attempt makes the merge ONCE, in a scratch tree of the queue's own, and
// moves the target only when the gate passed there.
//
// THE TARGET IS NEVER A WORKSPACE FOR THE MERGE. The tree is checked out,
// detached, at the target's tip; the no-ff merge, the gate and anything a
// failure leaves behind live in it and go with it. The target moves by one
// fast-forward to the commit the gate passed, and not at all otherwise -- so
// a failed, parked or abandoned merge leaves it exactly as it was, and a tip
// that moved while the gate ran is merged onto afresh rather than overwritten.
func (r *run) attempt(ctx context.Context) (step, error) {
	const op = "daemon.merge.merge_tab"
	target := r.job.Layout.TargetDir
	branch, err := r.o.deps.Git.CurrentBranch(ctx, target)
	if err != nil {
		return step{}, fmt.Errorf("merge: reading the branch checked out in %s: %w", target, err)
	}
	if branch == "" {
		return step{}, fmt.Errorf("merge: the merge target %s has no branch checked out to land on", target)
	}
	base, err := r.o.deps.Git.ResolveRef(ctx, target, "HEAD")
	if err != nil {
		return step{}, fmt.Errorf("merge: resolving %s's tip: %w", target, err)
	}
	round := r.openTab(ctx, TabMerge)
	lines := []string{fmt.Sprintf("merging %s into %s", r.job.Layout.SourceBranch, branch)}
	r.upsert(TabMerge, round, mergeTabRow(nil, lines, 0, ""))

	// A BRANCH ALREADY ON THE TARGET HAS LANDED. A no-ff merge of it makes
	// nothing and answers the target's own tip, a one-parent commit whose
	// landed range git cannot even read; that aborted a merge that had landed
	// and drew it failed (2026-09-28 15:28:24, lease 9cf657a4654d4c93).
	contained, err := r.o.deps.Git.IsAncestor(ctx, target, r.job.Layout.SourceBranch, base)
	if err != nil {
		r.settleMergeTab(ctx, round, lines, fmt.Sprintf("could not tell whether %s is already on %s", r.job.Layout.SourceBranch, branch))
		return step{}, fmt.Errorf("merge: asking whether %s is already on %s: %w", r.job.Layout.SourceBranch, branch, err)
	}
	if contained {
		return r.alreadyOn(ctx, round, lines, branch)
	}

	tree, err := r.makeTree(ctx, base)
	if err != nil {
		r.settleMergeTab(ctx, round, lines, "the queue's merge tree could not be made")
		return step{}, err
	}
	message := fmt.Sprintf("merge(%s): %s", branch, r.job.Layout.SourceBranch)
	result, err := r.o.deps.Git.MergeNoFF(ctx, tree, r.job.Layout.SourceBranch, message)
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "the merge could not be attempted", dlog.Context{
			"workspace": string(r.ws), "tree": tree, "branch": r.job.Layout.SourceBranch, "error": err.Error()})
		lines = append(lines, fmt.Sprintf("merge failed: %v", err))
		r.settleMergeTab(ctx, round, lines, "the merge could not be attempted")
		r.dropTree(ctx)
		return step{}, err
	}
	if result.Landed == nil {
		lines = append(lines, fmt.Sprintf("conflicted in %d file(s)", len(result.Conflicted)))
		r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), "the merge conflicted"))
		r.closeTab(ctx, TabMerge, round, "conflicted")
		r.o.log(ctx, r.ws).Warn(op, "the merge conflicted", dlog.Context{
			"workspace": string(r.ws), "target": branch, "files": strings.Join(result.Conflicted, ", ")})
		r.dropTree(ctx)
		return r.conflicts(ctx, result.Conflicted, branch)
	}
	commit := result.Landed.SHA
	lines = append(lines, fmt.Sprintf("merged cleanly · %s", short(commit)))
	r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), ""))
	r.closeTab(ctx, TabMerge, round, "succeeded")
	r.o.log(ctx, r.ws).Debug(op, "the merge was made in the queue's tree", dlog.Context{
		"workspace": string(r.ws), "commit": commit, "tree": tree})

	verdict, err := r.gate(ctx, tree, commit)
	if err != nil {
		r.dropTree(ctx)
		return step{}, err
	}
	switch {
	case verdict.broken != "":
		r.dropTree(ctx)
		return parkAt(TabTests, verdict.round, verdict.broken), nil
	case !verdict.result.Passed:
		next, err := r.fixes(ctx, verdict.result, tree, branch)
		r.dropTree(ctx)
		return next, err
	}
	return r.land(ctx, base, commit, branch)
}

// alreadyOn concludes a merge whose branch the target already contains: it has
// landed, with nothing to merge and nothing to deploy, and says so.
func (r *run) alreadyOn(ctx context.Context, round int, lines []string, branch string) (step, error) {
	summary := fmt.Sprintf("already on %s; nothing to merge", branch)
	lines = append(lines, summary)
	r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), ""))
	r.closeTab(ctx, TabMerge, round, "succeeded")
	tip, err := r.o.deps.Git.ResolveRef(ctx, r.job.Layout.TargetDir, r.job.Layout.SourceBranch)
	if err != nil {
		return step{}, fmt.Errorf("merge: resolving %s: %w", r.job.Layout.SourceBranch, err)
	}
	r.o.log(ctx, r.ws).Info("daemon.merge.merge_tab", "the branch is already on its target; the merge concludes with nothing to land", dlog.Context{
		"workspace": string(r.ws), "branch": r.job.Layout.SourceBranch, "target": branch, "tip": tip})
	return step{done: &outcome{landed: tip, alreadyOn: branch}}, nil
}

// land moves the target to the commit the gate passed, by fast-forward only.
// A target that moved while the gate ran is NOT landed on: the merge is made
// again on its new tip, so what lands is always what was tested.
func (r *run) land(ctx context.Context, base, commit, branch string) (step, error) {
	const op = "daemon.merge.land"
	target := r.job.Layout.TargetDir
	defer r.dropTree(ctx)
	now, err := r.o.deps.Git.ResolveRef(ctx, target, "HEAD")
	if err != nil {
		return step{}, fmt.Errorf("merge: resolving %s's tip before landing: %w", target, err)
	}
	if now != base {
		r.o.log(ctx, r.ws).Info(op, "the target moved while the merge was tested; the merge is made again on its new tip", dlog.Context{
			"workspace": string(r.ws), "target": branch, "tested_on": base, "tip": now})
		return step{}, nil
	}
	if err := r.o.deps.Git.FastForward(ctx, target, commit); err != nil {
		return step{}, fmt.Errorf("merge: fast-forwarding %s to %s: %w", branch, commit, err)
	}
	r.o.log(ctx, r.ws).Info(op, "the target was fast-forwarded to the merge its gate passed", dlog.Context{
		"workspace": string(r.ws), "target": branch, "commit": commit})
	commits, err := r.o.deps.Git.LandedRange(ctx, target, commit)
	if err != nil {
		// THE LANDING HAS HAPPENED. A range that will not read is a fault in
		// the account of it, not a failed merge: it is recorded, and the run
		// concludes as merged with no range (so no deploy is told of it).
		r.o.log(ctx, r.ws).Error(op, "the landed merge's range could not be read; no deploy is told of it", dlog.Context{
			"workspace": string(r.ws), "commit": commit, "error": err.Error()})
		commits = nil
	}
	return step{done: &outcome{landed: commit, commits: commits}}, nil
}

// settleMergeTab settles an attempt's merge tab as failed.
func (r *run) settleMergeTab(ctx context.Context, round int, lines []string, failure string) {
	r.upsert(TabMerge, round, mergeTabRow(nil, lines, r.o.nowMS(), failure))
	r.closeTab(ctx, TabMerge, round, "failed")
}

// mergeTreesDir is where the queue's scratch trees live, under the state root.
const mergeTreesDir = "merge-trees"

// treeFor names one attempt's scratch tree: one directory per lease and
// attempt, so no two attempts, and no two merges, ever share one.
func treeFor(stateDir string, lease ids.LeaseID, attempt int) string {
	return filepath.Join(stateDir, mergeTreesDir, fmt.Sprintf("%s-%d", lease, attempt))
}

// makeTree makes the attempt's scratch tree: the target's tip, detached.
func (r *run) makeTree(ctx context.Context, base string) (string, error) {
	r.attempts++
	dir := treeFor(r.o.deps.StateDir, r.lease.ID, r.attempts)
	if err := os.MkdirAll(filepath.Dir(dir), 0o755); err != nil {
		return "", fmt.Errorf("merge: making the queue's tree directory: %w", err)
	}
	if err := r.o.deps.Git.AddDetachedWorktree(ctx, r.job.Layout.TargetDir, dir, base); err != nil {
		return "", fmt.Errorf("merge: making the queue's merge tree at %s: %w", dir, err)
	}
	r.tree = dir
	return dir, nil
}

// dropTree removes the attempt's scratch tree, if one stands. Its failure is
// recorded and does not end the run: the tree is the queue's own and holds
// nothing the target depends on, and the boot recovery sweeps what is left.
func (r *run) dropTree(ctx context.Context) {
	if r.tree == "" {
		return
	}
	tree := r.tree
	r.tree = ""
	if err := r.o.deps.Git.RemoveWorktree(context.WithoutCancel(ctx), r.job.Layout.TargetDir, tree); err != nil {
		r.o.log(ctx, r.ws).Error("daemon.merge.tree", "could not remove the queue's merge tree", dlog.Context{
			"workspace": string(r.ws), "tree": tree, "error": err.Error()})
	}
}

// conflicts hands the conflict to the agent EXACTLY ONCE per source tip, and
// parks when the same tip conflicts again.
//
// ONCE IS THE WHOLE POLICY. An agent that could not resolve a conflict on the
// facts it was given will not do better on the same facts. So the agent is
// briefed to bring ITS OWN BRANCH up to date with the target, in its own
// worktree, and the next attempt is the merge made again: a tip that moved is
// a new set of facts, a tip that did not is a conflict for a human.
func (r *run) conflicts(ctx context.Context, files []string, targetBranch string) (step, error) {
	const op = "daemon.merge.conflicts"
	round := r.openTab(ctx, TabConflicts)
	r.address(TabConflicts, round)
	tip, err := r.o.deps.Git.ResolveRef(ctx, r.job.Layout.SourceDir, r.job.Layout.SourceBranch)
	if err != nil {
		return step{}, fmt.Errorf("merge: resolving %s: %w", r.job.Layout.SourceBranch, err)
	}
	r.mu.Lock()
	briefed := r.conflictBriefed[tip]
	r.conflictBriefed[tip] = true
	r.mu.Unlock()
	if briefed {
		line := conflictLine(files)
		r.o.log(ctx, r.ws).Warn(op, "parked a merge on unresolved conflicts", dlog.Context{
			"workspace": string(r.ws), "files": strings.Join(files, ", "), "line": line})
		r.upsert(TabConflicts, round, conflictsTabRow(nil, line, 0, ""))
		return parkAt(TabConflicts, round, line), nil
	}
	r.upsert(TabConflicts, round, conflictsTabRow(&frontendv1.FeedMergeTabLive{}, "", 0, ""))
	if err := r.noteMachinery(ctx, targetBranch); err != nil {
		return step{}, err
	}
	text, err := r.o.deps.Briefs(BriefConflictResolve, map[string]string{
		"conflict_commit":  short(tip),
		"source_branch":    r.job.Layout.SourceBranch,
		"source_dir":       r.job.Layout.SourceDir,
		"target_branch":    targetBranch,
		"target_dir":       r.job.Layout.TargetDir,
		"conflicted_files": strings.Join(files, ", "),
	})
	if err != nil {
		return step{}, fmt.Errorf("merge: composing the conflict brief: %w", err)
	}
	if _, err := r.submit(ctx, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); err != nil {
		return step{}, err
	}
	line, refused, err := r.machineryChanged(ctx, targetBranch)
	if err != nil {
		return step{}, err
	}
	if refused {
		r.upsert(TabConflicts, round, conflictsTabRow(nil, line, 0, ""))
		return parkAt(TabConflicts, round, line), nil
	}
	r.upsert(TabConflicts, round, conflictsTabRow(nil, "", r.o.nowMS(), ""))
	r.closeTab(ctx, TabConflicts, round, "succeeded")
	r.o.log(ctx, r.ws).Debug(op, "the agent's conflict turn ended; the merge is made again", dlog.Context{
		"workspace": string(r.ws), "briefed_tip": tip})
	return step{}, nil
}

// conflictLine composes the parked merge's standing line. The footer draws it
// verbatim and the parked tab shows the same sentence, so a parked merge has
// ONE account rather than two that can disagree.
func conflictLine(files []string) string {
	switch len(files) {
	case 0:
		return "parked for your input — the merge's conflicts are unresolved"
	case 1:
		return fmt.Sprintf("parked for your input — 1 conflict remains in %s", files[0])
	}
	return fmt.Sprintf("parked for your input — %d conflicts remain in %s", len(files), strings.Join(files[:min(len(files), 3)], ", "))
}

// gateVerdict is one gate run's answer: the run itself, or -- broken set -- a
// gate that could not run at all.
type gateVerdict struct {
	result GateResult
	round  int
	// broken is the parked line of a gate that FAILED TO RUN, empty when it
	// ran (to a pass or a failure).
	broken string
}

// gate runs the test gate on the queue's tree.
//
// THERE IS NO FLAKE RE-RUN: a failure goes to the fixes loop, whose only exits
// are a passing suite and the agent's own escalation. And A BROKEN GATE IS NOT
// A TEST FAILURE: a gate that could not run -- its script is not there, it
// could not be started, or the shell could not find or execute its command
// (exit 127 or 126) -- is not something the branch's agent can repair from
// inside the merge the gate is judging, so it parks at once with a plain line
// and no repair round (2026-09-28: three repair rounds on an exit-127 gate).
func (r *run) gate(ctx context.Context, tree, commit string) (gateVerdict, error) {
	const op = "daemon.merge.tests"
	rangeSpec := fmt.Sprintf("%s^1..%s", commit, commit)
	paths, err := r.o.deps.Git.ChangedPaths(ctx, tree, rangeSpec)
	if err != nil {
		r.o.log(ctx, r.ws).Warn(op, "could not read the landed range's paths; every suite runs",
			dlog.Context{"workspace": string(r.ws), "range": rangeSpec, "error": err.Error()})
		paths = nil
	}
	selection := SelectSuites(paths)
	r.o.log(ctx, r.ws).Debug(op, "selected the merge's suites", dlog.Context{
		"workspace": string(r.ws), "suites": strings.Join(selection.Suites, ","),
		"full": selection.Full, "reason": selection.Reason})

	round := r.openTab(ctx, TabTests)
	r.upsert(TabTests, round, testsTabRow(&frontendv1.FeedMergeTabLive{}, nil, 0, ""))
	argv := r.o.deps.TestCommand(tree)
	if len(argv) == 0 {
		return r.brokenGate(ctx, round, GateResult{}, "no test command is configured for it"), nil
	}
	script := argv[len(argv)-1]
	if _, err := os.Stat(script); err != nil {
		return r.brokenGate(ctx, round, GateResult{}, fmt.Sprintf("its script %s is not there", script)), nil
	}
	result, err := r.o.runGate(ctx, r.lease.ID, round, tree, argv, selection)
	if err != nil {
		// A GATE THE DAEMON'S OWN EXIT STOPPED FROM STARTING IS NOT BROKEN: a
		// cancelled run or a draining daemon takes the ordinary error path,
		// which records the exit rather than a fault in the gate.
		var unstarted *gateUnstartedError
		if errors.As(err, &unstarted) && ctx.Err() == nil && !r.o.isDraining() {
			return r.brokenGate(ctx, round, GateResult{}, fmt.Sprintf("it could not be started (%v)", unstarted.err)), nil
		}
		r.upsert(TabTests, round, testsTabRow(nil, nil, r.o.nowMS(), "the test gate could not run"))
		r.closeTab(ctx, TabTests, round, "failed")
		return gateVerdict{}, err
	}
	if why, broken := gateDidNotRun(result.ExitCode); broken {
		return r.brokenGate(ctx, round, result, fmt.Sprintf("%s; the whole run is archived at %s", why, result.ArchivePath)), nil
	}
	if result.Passed {
		r.upsert(TabTests, round, testsTabRow(nil, result.Suites, r.o.nowMS(), ""))
		r.closeTab(ctx, TabTests, round, "succeeded")
		r.o.log(ctx, r.ws).Debug(op, "the merge's suites passed", dlog.Context{
			"workspace": string(r.ws), "round": round, "archive": result.ArchivePath})
		return gateVerdict{result: result, round: round}, nil
	}
	summary := fmt.Sprintf("the test suite failed (exit %d); the whole run is archived at %s", result.ExitCode, result.ArchivePath)
	r.upsert(TabTests, round, testsTabRow(nil, result.Suites, r.o.nowMS(), summary))
	r.closeTab(ctx, TabTests, round, "failed")
	r.o.log(ctx, r.ws).Warn(op, "the merge's suites failed", dlog.Context{
		"workspace": string(r.ws), "round": round, "exit_code": result.ExitCode, "archive": result.ArchivePath})
	return gateVerdict{result: result, round: round}, nil
}

// gateDidNotRun reads the exit statuses that mean the gate's command never
// ran: the shell's own "command not found" (127) and "not executable" (126).
func gateDidNotRun(code int) (string, bool) {
	switch code {
	case 127:
		return "its command was not found (exit 127)", true
	case 126:
		return "its command could not be executed (exit 126)", true
	}
	return "", false
}

// brokenGate settles the tests tab on a gate that failed to run and answers
// the park its line names.
func (r *run) brokenGate(ctx context.Context, round int, result GateResult, why string) gateVerdict {
	line := "parked for your input — the test gate itself failed to run: " + why
	r.upsert(TabTests, round, testsTabRow(nil, result.Suites, r.o.nowMS(), line))
	r.closeTab(ctx, TabTests, round, "gate_broken")
	r.o.log(ctx, r.ws).Warn("daemon.merge.tests", "the test gate itself failed to run; the merge parks with no repair round", dlog.Context{
		"workspace": string(r.ws), "round": round, "why": why, "exit_code": result.ExitCode, "archive": result.ArchivePath})
	return gateVerdict{result: result, round: round, broken: line}
}

// fixes hands one failing run to the agent, which repairs ITS OWN BRANCH in its
// own worktree; the next attempt merges that branch again.
//
// The loop has NO ATTEMPT LIMIT by design: an agent that is making progress is
// asked again with the new failing output. Its one non-passing exit is the
// agent's OWN JUDGEMENT, recorded as the escalation file — which parks the
// merge for a human rather than failing it silently. A repair that changed
// the merge machinery is REFUSED and parks too.
func (r *run) fixes(ctx context.Context, failing GateResult, tree, targetBranch string) (step, error) {
	const op = "daemon.merge.fixes"
	round := r.openTab(ctx, TabFixes)
	r.address(TabFixes, round)
	r.upsert(TabFixes, round, fixesTabRow(&frontendv1.FeedMergeTabLive{}, "", 0, ""))
	if err := r.noteMachinery(ctx, targetBranch); err != nil {
		return step{}, err
	}
	before, err := r.o.deps.Git.ResolveRef(ctx, r.job.Layout.SourceDir, r.job.Layout.SourceBranch)
	if err != nil {
		return step{}, fmt.Errorf("merge: resolving %s: %w", r.job.Layout.SourceBranch, err)
	}
	text, err := r.o.deps.Briefs(BriefTestFailureResolve, map[string]string{
		"source_branch":     r.job.Layout.SourceBranch,
		"source_dir":        r.job.Layout.SourceDir,
		"target_branch":     targetBranch,
		"target_dir":        r.job.Layout.TargetDir,
		"queue_dir":         tree,
		"archive_path":      failing.ArchivePath,
		"failure_tail":      failing.Tail,
		"escalation_file":   EscalationFile,
		"escalation_marker": EscalationMarker,
	})
	if err != nil {
		return step{}, fmt.Errorf("merge: composing the test-failure brief: %w", err)
	}
	if _, err := r.submit(ctx, text, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); err != nil {
		return step{}, err
	}
	if why, gaveUp := r.consumeEscalation(ctx); gaveUp {
		line := fmt.Sprintf("parked for your input — the agent escalated: %s", firstLine(why))
		r.o.log(ctx, r.ws).Warn(op, "the fixes agent escalated the merge", dlog.Context{
			"workspace": string(r.ws), "round": round, "reason": why})
		r.upsert(TabFixes, round, fixesTabRow(nil, line, 0, ""))
		return parkAt(TabFixes, round, line), nil
	}
	line, refused, err := r.machineryChanged(ctx, targetBranch)
	if err != nil {
		return step{}, err
	}
	if refused {
		r.upsert(TabFixes, round, fixesTabRow(nil, line, 0, ""))
		return parkAt(TabFixes, round, line), nil
	}
	after, err := r.o.deps.Git.ResolveRef(ctx, r.job.Layout.SourceDir, r.job.Layout.SourceBranch)
	if err != nil {
		return step{}, fmt.Errorf("merge: resolving %s: %w", r.job.Layout.SourceBranch, err)
	}
	if after == before {
		r.o.log(ctx, r.ws).Warn(op, "the fixes turn committed nothing on the branch; the suite runs again on it as it stands", dlog.Context{
			"workspace": string(r.ws), "round": round, "branch": r.job.Layout.SourceBranch})
	}
	r.upsert(TabFixes, round, fixesTabRow(nil, "", r.o.nowMS(), ""))
	r.closeTab(ctx, TabFixes, round, "succeeded")
	return step{}, nil
}

// consumeEscalation reads the escalation record from the agent's own worktree
// and REMOVES it, so a record is answered once: it neither stands in a tree
// for ever nor parks the next attempt after the user's guidance.
func (r *run) consumeEscalation(ctx context.Context) (string, bool) {
	why, gaveUp := readEscalation(r.job.Layout.SourceDir)
	if !gaveUp {
		return "", false
	}
	path := filepath.Join(r.job.Layout.SourceDir, EscalationFile)
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
	rangeSpec := targetBranch + "..." + r.job.Layout.SourceBranch
	paths, err := r.o.deps.Git.ChangedPaths(ctx, r.job.Layout.SourceDir, rangeSpec)
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

// noteMachinery records, before the FIRST repair or guidance turn, the merge
// machinery the branch already changed: that is the author's own work, which
// the merge is there to land. Only what a repair adds to it is refused.
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

// machineryChanged answers the parked line for a repair that changed merge
// machinery the branch did not already change, and whether one did.
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
	line := fmt.Sprintf("parked for your input — the repair changed the merge machinery mid-merge (%s); a fix to the merge or its gate lands through a branch of its own",
		strings.Join(added, ", "))
	r.o.log(ctx, r.ws).Warn("daemon.merge.machinery", "refused a repair that changed the merge machinery mid-merge", dlog.Context{
		"workspace": string(r.ws), "branch": r.job.Layout.SourceBranch, "paths": strings.Join(added, ", ")})
	return line, true, nil
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

// parkUntilResumed stops the run for the user until the user resumes it.
//
// The lease's policy flips to PARKED, so the prompt queue stops refusing this
// workspace's submissions and routes them here instead; THE LEASE STATE IS THE
// RECOGNITION. The run YIELDS its repository's slot, so the merges behind it
// proceed (owner ruling, 2026-09-28). It then waits -- for as many prompts as
// it takes -- until one is DELIVERED to the workspace's agent and ANSWERED to
// its caller, and returns once that guidance turn has ended: the caller then
// asks for the slot back and makes the merge afresh. Every other resolution
// (an abandon, the daemon's exit) ends the run's context, which is returned.
//
// Before this the guidance a park received was discarded and the run
// returned, so nothing was left to deliver or answer the next prompt: the
// user's prompts timed out, and the parked run held the repository's queue
// until the daemon restarted (2026-09-28, lease c8a3a664006f46c1).
func (r *run) parkUntilResumed(ctx context.Context, p parking) error {
	const op = "daemon.merge.park"
	if err := r.o.deps.DB.SetLeasePolicy(ctx, r.lease.ID, wsm.PolicyParked); err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not move the lease to parked",
			dlog.Context{"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	r.mu.Lock()
	r.parked = true
	r.mu.Unlock()
	r.facts(StateParked, p.line)
	r.o.yieldSlot(ctx, r)
	if err := r.o.republishQueue(ctx, r.repo); err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not republish the queue a parked merge left", dlog.Context{
			"workspace": string(r.ws), "repo": string(r.repo), "error": err.Error()})
	}
	r.o.log(ctx, r.ws).Info(op, "parked a merge for the user; its repository's queue proceeds without it", dlog.Context{
		"workspace": string(r.ws), "lease": string(r.lease.ID), "tab": p.tab, "line": p.line})
	if r.o.onPark != nil {
		r.o.onPark(r.ws)
	}
	for {
		select {
		case g := <-r.guidance:
			if !r.deliverGuidance(ctx, g) {
				continue
			}
			r.mu.Lock()
			r.parked = false
			r.mu.Unlock()
			r.closeTab(ctx, p.tab, p.round, "resumed")
			if _, err := r.o.deps.AwaitTurnEnd(ctx, r.ws, g.turn); err != nil {
				return err
			}
			r.o.log(ctx, r.ws).Info(op, "the guidance turn of a parked merge ended; the merge resumes", dlog.Context{
				"workspace": string(r.ws), "turn": string(g.turn)})
			return nil
		case <-ctx.Done():
			return context.Cause(ctx)
		}
	}
}

// guidance is one parked submission on its way to the resolution agent: what
// was said, the submission's own turn, which the guidance runs under, and the
// reply its caller waits on. The reply is buffered, so a caller that stopped
// listening never holds the run.
type guidance struct {
	turn  ids.TurnID
	said  *conversationv1.UserSaid
	reply chan error
}

// deliverGuidance hands one parked submission to the workspace agent's own
// session and ANSWERS its caller either way. It reports whether the guidance
// was delivered: a refused route leaves the run parked, still listening, and
// its caller holds the refusal.
func (r *run) deliverGuidance(ctx context.Context, g guidance) bool {
	const op = "daemon.merge.park"
	err := r.o.deps.ParkedRoute(ctx, r.ws, g.turn, g.said)
	g.reply <- err
	if err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not deliver the user's guidance to the parked merge's agent; the merge stays parked", dlog.Context{
			"workspace": string(r.ws), "turn": string(g.turn), "error": err.Error()})
		return false
	}
	if err := r.o.deps.DB.SetLeasePolicy(ctx, r.lease.ID, wsm.PolicyRefuse); err != nil {
		r.o.log(ctx, r.ws).Error(op, "could not move the lease back from parked", dlog.Context{
			"workspace": string(r.ws), "lease": string(r.lease.ID), "error": err.Error()})
	}
	r.o.deps.Queue.OnLeaseChanged(r.ws)
	r.facts(StateMerging, "")
	r.o.log(ctx, r.ws).Info(op, "delivered the user's guidance to the parked merge's agent", dlog.Context{
		"workspace": string(r.ws), "turn": string(g.turn)})
	return true
}

// min is the smallest of two ints.
func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}
