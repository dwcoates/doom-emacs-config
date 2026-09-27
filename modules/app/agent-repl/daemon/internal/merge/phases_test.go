package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// TestEmacsRepoTabSequence covers the method a merge into this daemon's own
// repository takes: the landing, then the gate.
func TestEmacsRepoTabSequence(t *testing.T) {
	// Arrange: a clean merge whose suites pass.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	want := []string{TabQueue, TabMerge, TabTests}
	if got := h.feed.tabSequence(); !equal(got, want) {
		t.Fatalf("the tab sequence is %v, want %v", got, want)
	}
}

// TestOtherRepoTabSequence covers the other method: landing, tests and PR work
// are the configured prompts' job there, so nothing between them runs.
func TestOtherRepoTabSequence(t *testing.T) {
	// Arrange: a workspace of a repository that is not the daemon's, with both
	// prompts configured.
	h := newHarness(t)
	h.configureActions([]string{"before"}, []string{"after"})
	h.briefs["before"] = nil
	h.briefs["after"] = nil
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	want := []string{TabQueue, TabPrePrompt, TabPostPrompt}
	if got := h.feed.tabSequence(); !equal(got, want) {
		t.Fatalf("the tab sequence is %v, want %v", got, want)
	}
}

// TestOtherRepoNeverTouchesGitsMergeVerb covers the same ruling from the git
// side: a repository that lands through a CI merge queue is never merged
// locally, because that would duplicate the commits CI owns.
func TestOtherRepoNeverTouchesGitsMergeVerb(t *testing.T) {
	// Arrange: a non-self repository with no configured prompts.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if h.git.seen("merge_no_ff") {
		t.Fatal("a merge outside the daemon's own repository ran the local merge")
	}
}

// TestPrePromptFailureFailsTheRun covers the configured before-action: it is
// the workspace's own precondition, so merging past a failed one would land
// work its author said was not ready.
func TestPrePromptFailureFailsTheRun(t *testing.T) {
	// Arrange: a before-action whose turn fails.
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	if h.git.seen("merge_no_ff") {
		t.Fatal("the merge ran after its before-action failed")
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want %q", facts.State, StateFailed)
	}
}

// TestAPrePromptWhoseAgentProcessDiedFailsTheRun covers the other failed
// close: a before-action whose turn the agent process cut by dying did not
// complete either.
func TestAPrePromptWhoseAgentProcessDiedFailsTheRun(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseAgentDied}
	enqueue(t, h)

	// Act
	_ = h.admit(context.Background())

	// Assert
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want %q", facts.State, StateFailed)
	}
}

// TestPostPromptFailureRidesTheTerminal covers the configured after-action:
// every commit has landed by the time it runs, so there is nothing left to
// refuse and its failure never fails the run.
func TestPostPromptFailureRidesTheTerminal(t *testing.T) {
	// Arrange: a landed merge whose after-action's turn fails.
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions(nil, []string{"after"})
	h.briefs["after"] = nil
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge reported a failure for a post-prompt: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the merge is %q, want it to have landed anyway", facts.State)
	}
}

// TestSessionlessWorkspaceWithAConfiguredPromptIsRevived covers
// revival-is-implicit: a configured prompt needs a session, so one is started
// under the lease rather than the merge refusing.
func TestSessionlessWorkspaceWithAConfiguredPromptIsRevived(t *testing.T) {
	// Arrange: a workspace with no session and a before-action.
	h := newHarness(t)
	h.configureActions([]string{"before"}, nil)
	h.briefs["before"] = nil
	h.db.mu.Lock()
	delete(h.db.sessions, theWorkspace)
	h.db.mu.Unlock()
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.startedSessions) != 1 || h.startedSessions[0] != theWorkspace {
		t.Fatalf("sessions started: %v, want exactly this workspace's", h.startedSessions)
	}
}

// TestSessionlessWorkspaceWithNoPromptsMergesSessionless covers the other half:
// only a workspace with no configured prompts merges truly sessionless.
func TestSessionlessWorkspaceWithNoPromptsMergesSessionless(t *testing.T) {
	// Arrange: no session and no configured actions.
	h := newHarness(t)
	h.db.mu.Lock()
	delete(h.db.sessions, theWorkspace)
	h.db.mu.Unlock()
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.startedSessions) != 0 {
		t.Fatalf("a sessionless merge with no prompts revived %v", h.startedSessions)
	}
}

// TestConflictBriefsTheAgentExactlyOnce covers the give-up rule: an agent that
// could not resolve a conflict on the facts it was given will not do better on
// the same facts.
func TestConflictBriefsTheAgentExactlyOnce(t *testing.T) {
	// Arrange: a conflicted merge the agent does not resolve.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{"a.go"}, {"a.go"}}
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)

	// Act.
	waitForParked(t, h)

	// Assert.
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); n != 1 {
		t.Fatalf("the conflict brief was sent %d times, want exactly once", n)
	}
	cancel()
	<-done
}

// TestUnresolvedConflictParks covers where an unresolved conflict ends: with a
// human, not with another unattended pass.
func TestUnresolvedConflictParks(t *testing.T) {
	// Arrange: a conflict the agent leaves unresolved.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go", "b.go"))
	h.git.conflicted = [][]string{{"a.go", "b.go"}, {"a.go", "b.go"}}
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)

	// Act.
	waitForParked(t, h)

	// Assert.
	facts := h.footer.last()
	if facts.State != StateParked {
		t.Fatalf("the footer says %q, want %q", facts.State, StateParked)
	}
	if !strings.Contains(facts.ParkedLine, "2 conflicts") {
		t.Fatalf("the parked line is %q, want it to name the remaining conflicts", facts.ParkedLine)
	}
	if h.db.policyOf(h.leaseID(t)) != wsm.PolicyParked {
		t.Fatal("the lease did not move to the parked policy")
	}
	cancel()
	<-done
}

// TestResolvedConflictConcludesTheCommit covers the other conflict outcome: a
// clean index means the resolution is staged, and the daemon concludes the
// commit the brief told the agent to leave alone.
func TestResolvedConflictConcludesTheCommit(t *testing.T) {
	// Arrange: a conflict the agent resolves.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{}}
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	want := []string{TabQueue, TabMerge, TabConflicts, TabTests}
	if got := h.feed.tabSequence(); !equal(got, want) {
		t.Fatalf("the tab sequence is %v, want %v", got, want)
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the merge is %q, want it to have landed", facts.State)
	}
}

// TestResolvedConflictCommitsThroughTheGitLeaf pins the seam: the conclusion
// goes through gitclient.Git.Commit, not a local shell-out of the merge
// package's own.
func TestResolvedConflictCommitsThroughTheGitLeaf(t *testing.T) {
	// Arrange: a conflict the agent resolves.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{}}
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.commitMessages) != 1 {
		t.Fatalf("the git leaf saw %d commits, want the resolution's one", len(h.git.commitMessages))
	}
}

// TestAFailedConclusionSurfacesTheGitFailure covers the other half of the
// seam: the leaf's failure is the merge's failure, never swallowed.
func TestAFailedConclusionSurfacesTheGitFailure(t *testing.T) {
	// Arrange: a resolved conflict the git leaf refuses to commit.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{}}
	h.git.commitErr = errors.New("nothing to commit")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert: the failure is the MERGE's, stated on its own facts. The pump's
	// return is no longer where it surfaces: one merge ending badly must not
	// stop the queue behind it, so the run's terminal is the report.
	facts, _ := h.o.Facts(theWorkspace)
	if facts.State != StateFailed {
		t.Fatalf("the merge is %q after a refused conclusion, want it failed", facts.State)
	}
	if !strings.Contains(facts.Detail, "nothing to commit") {
		t.Fatalf("the merge's detail = %q, want git's own account of the refusal", facts.Detail)
	}
}

// TestParkedSubmissionRoutesToTheResolutionAgent covers the parked policy: the
// lease state is the recognition, and what the user types is delivered as
// guidance rather than started as a turn of the session's own.
func TestParkedSubmissionRoutesToTheResolutionAgent(t *testing.T) {
	// Arrange: a parked merge.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{"a.go"}, {"a.go"}, {}}
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)
	waitForParked(t, h)

	// Act.
	err := h.o.RouteParked(ctx, theWorkspace, "guidance-turn", saidText("try resolving it this way"))

	// Assert.
	if err != nil {
		t.Fatalf("RouteParked failed: %v", err)
	}
	<-done
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.parkedSaid) != 1 {
		t.Fatalf("%d submissions were routed as guidance, want 1", len(h.parkedSaid))
	}
}

// TestParkedGuidanceRunsUnderTheSubmissionsTurn pins that the guidance is
// routed under the turn RouteParked was handed and the run resumes on THAT
// turn's end, so a re-driven retry of the submission is the start the shim
// already took.
func TestParkedGuidanceRunsUnderTheSubmissionsTurn(t *testing.T) {
	// Arrange: a parked merge.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{"a.go"}, {"a.go"}, {}}
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)
	waitForParked(t, h)

	// Act.
	err := h.o.RouteParked(ctx, theWorkspace, "submitted-turn", saidText("try resolving it this way"))

	// Assert.
	if err != nil {
		t.Fatalf("RouteParked failed: %v", err)
	}
	<-done
	h.mu.Lock()
	defer h.mu.Unlock()
	if len(h.parkedTurns) != 1 || h.parkedTurns[0] != "submitted-turn" {
		t.Fatalf("guidance routed under %v, want the submission's own turn", h.parkedTurns)
	}
	if !slices.Contains(h.awaitedTurns, "submitted-turn") {
		t.Fatalf("awaited turns = %v, want the run to resume on the guidance turn's end", h.awaitedTurns)
	}
}

// TestRouteParkedRefusesAWorkspaceWithNoRun covers the seam's own guard: a route
// without a run in flight means the caller lost track of the merge.
func TestRouteParkedRefusesAWorkspaceWithNoRun(t *testing.T) {
	// Arrange: no merge in flight.
	h := newHarness(t)

	// Act.
	err := h.o.RouteParked(context.Background(), theWorkspace, "guidance-turn", saidText("hello"))

	// Assert.
	if err != errNoRun {
		t.Fatalf("RouteParked answered %v, want the no-run failure", err)
	}
}

// TestGateSelectsSuitesFromTheLandedRange covers the narrowing: the gate runs
// the suites the landed paths can break, and passes them to the script.
func TestGateSelectsSuitesFromTheLandedRange(t *testing.T) {
	// Arrange: a merge touching only the webapp.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/webapp/src/App.tsx"}
	h.gatePasses("build-frontend-harness", "webapp")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if len(h.runner.argv) != 1 {
		t.Fatalf("the gate ran %d times, want once", len(h.runner.argv))
	}
	got := strings.Join(h.runner.argv[0], " ")
	if !strings.Contains(got, "--suites build-frontend-harness,webapp") {
		t.Fatalf("the gate was invoked as %q, want the narrowed suites", got)
	}
}

// TestGatePassesNoNarrowingForTheFullSet covers the conservative answer: running
// everything is the script's own default, so a full selection hands it no flag.
func TestGatePassesNoNarrowingForTheFullSet(t *testing.T) {
	// Arrange: a merge touching a path no rule maps.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"tools/unknown/main.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if strings.Contains(strings.Join(h.runner.argv[0], " "), "--suites") {
		t.Fatalf("a full selection was narrowed: %v", h.runner.argv[0])
	}
}

// TestGateRunsInTheTargetWorktree covers where the gate runs: on the tree the
// merge commit produced, which is the target rather than the source.
func TestGateRunsInTheTargetWorktree(t *testing.T) {
	// Arrange: a landed merge.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.runner.mu.Lock()
	defer h.runner.mu.Unlock()
	if h.runner.dirs[0] != h.targetD {
		t.Fatalf("the gate ran in %q, want the merge target %q", h.runner.dirs[0], h.targetD)
	}
}

// TestGateNeverReRunsAFailure covers the deliberate reversal: a failure is an
// error to remediate, and a second green would paper over the first red.
func TestGateNeverReRunsAFailure(t *testing.T) {
	// Arrange: a failing gate whose fixes agent escalates at once.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.escalate("a redesign is needed")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)
	waitForParked(t, h)

	// Act.
	h.runner.mu.Lock()
	runs := len(h.runner.argv)
	h.runner.mu.Unlock()

	// Assert.
	if runs != 1 {
		t.Fatalf("the gate ran %d times for one failure, want exactly once", runs)
	}
	cancel()
	<-done
}

// TestFixesLoopExitsOnAPass covers the loop's ordinary exit: the agent's fix is
// committed and the suite is re-run as a NEW round.
func TestFixesLoopExitsOnAPass(t *testing.T) {
	// Arrange: a gate that fails once and then passes.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	want := []string{TabQueue, TabMerge, TabTests, TabFixes}
	if got := h.feed.tabSequence(); !equal(got, want) {
		t.Fatalf("the tab sequence is %v, want %v", got, want)
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the merge is %q, want it to have landed", facts.State)
	}
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); n != 1 {
		t.Fatalf("the fixes brief was sent %d times, want once", n)
	}
}

// TestFixesLoopOpensASecondTestsTab covers the append-only rule: a re-run is a
// SECOND tab carrying its round, never a reopened first tab.
func TestFixesLoopOpensASecondTestsTab(t *testing.T) {
	// Arrange: a gate that fails once and then passes.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	rounds := h.feed.roundsOfKind(TabTests)
	if len(rounds) != 2 || rounds[0] != 1 || rounds[1] != 2 {
		t.Fatalf("the tests tab drew rounds %v, want a first and a second", rounds)
	}
}

// TestFixesLoopExitsOnTheEscalationRecord covers the loop's only non-passing
// exit: the agent's own judgement, which parks the merge for a human.
func TestFixesLoopExitsOnTheEscalationRecord(t *testing.T) {
	// Arrange: a failing gate and an escalation record.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.escalate("the storage layer needs redesigning")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)

	// Act.
	waitForParked(t, h)

	// Assert.
	facts := h.footer.last()
	if facts.State != StateParked {
		t.Fatalf("the footer says %q, want %q", facts.State, StateParked)
	}
	if !strings.Contains(facts.ParkedLine, "the storage layer needs redesigning") {
		t.Fatalf("the parked line is %q, want the agent's own reason", facts.ParkedLine)
	}
	cancel()
	<-done
}

// TestEscalationNeedsTheExactMarker covers the wire format of the loop's exit: a
// file whose first line is not the substituted marker is not an escalation.
func TestEscalationNeedsTheExactMarker(t *testing.T) {
	tests := []struct {
		name string
		body string
		want bool
	}{
		{name: "the marker exactly", body: EscalationMarker + "\nbecause", want: true},
		{name: "a different first line", body: "I give up\n" + EscalationMarker, want: false},
		{name: "an empty file", body: "", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a target carrying the candidate record.
			dir := t.TempDir()
			if err := os.WriteFile(filepath.Join(dir, EscalationFile), []byte(tc.body), 0o644); err != nil {
				t.Fatalf("writing the record: %v", err)
			}

			// Act.
			_, got := readEscalation(dir)

			// Assert.
			if got != tc.want {
				t.Fatalf("readEscalation answered %v, want %v", got, tc.want)
			}
		})
	}
}

// TestFixesBriefCarriesTheEscalationConstants covers the substitution: the
// daemon parses exactly the constants it supplied, so an edited brief cannot
// send an agent to write a record nothing reads.
func TestFixesBriefCarriesTheEscalationConstants(t *testing.T) {
	// Arrange: a failing gate whose agent escalates.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.escalate("no local fix is correct")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)
	waitForParked(t, h)

	// Act.
	h.mu.Lock()
	values := h.briefValues
	h.mu.Unlock()

	// Assert.
	if len(values) == 0 {
		t.Fatal("no brief was composed")
	}
	last := values[len(values)-1]
	if last["escalation_file"] != EscalationFile || last["escalation_marker"] != EscalationMarker {
		t.Fatalf("the brief was spliced with %v, want the daemon's own constants", last)
	}
	cancel()
	<-done
}

// TestTestsTabCarriesPaintedSpans covers the tab's content: the daemon parses
// the terminal's escapes once and the client paints classes.
func TestTestsTabCarriesPaintedSpans(t *testing.T) {
	// Arrange: a passing gate with output.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	// The sandbox image is the one region whose blast radius is a SINGLE
	// suite, which is what keeps this test about one painted span rather than
	// about the selector's fan-out.
	h.git.changed = []string{"modules/app/agent-repl/e2e/sandbox/Dockerfile"}
	h.gatePasses("e2e-emacs")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	tab := h.feed.lastTabOfKind(TabTests)
	suites := tab.GetTests().GetSuites()
	if len(suites) != 1 || suites[0].GetName() != "e2e-emacs" {
		t.Fatalf("the tests tab drew %d suite(s), want the one that ran", len(suites))
	}
	if _, passed := suites[0].GetState().(*frontendv1.FeedMergeTestSuite_Passed); !passed {
		t.Fatalf("the suite's state is %T, want passed", suites[0].GetState())
	}
	if len(suites[0].GetOutput()) == 0 {
		t.Fatal("the suite carried no painted output")
	}
}

// TestGateArchivesEveryRun covers the archive: a multi-suite runner buries a
// failure's own output, so the whole run is kept and named.
func TestGateArchivesEveryRun(t *testing.T) {
	// Arrange: a passing gate.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	entries, err := os.ReadDir(filepath.Join(h.stateDir, "merge-logs"))
	if err != nil {
		t.Fatalf("reading the archive directory: %v", err)
	}
	if len(entries) != 1 || !strings.Contains(entries[0].Name(), "-tests-1.log") {
		t.Fatalf("the archive holds %v, want one round-numbered log", names(entries))
	}
}

// TestParkedMergeKeepsItsLease covers what parked means: the merge is stopped,
// not finished, so nothing of it is torn down while it waits.
func TestParkedMergeKeepsItsLease(t *testing.T) {
	// Arrange: a parked merge.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.git.conflicted = [][]string{{"a.go"}, {"a.go"}}
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := admitAsync(h, ctx)

	// Act.
	waitForParked(t, h)

	// Assert.
	h.db.mu.Lock()
	released := len(h.db.releasedLeases)
	h.db.mu.Unlock()
	if released != 0 {
		t.Fatalf("%d lease(s) were released while the merge was parked", released)
	}
	if h.git.seen("remove_worktree") {
		t.Fatal("a parked merge's worktree was removed")
	}
	cancel()
	<-done
}

// TestFirstLineKeepsAComposedLineToOneLine covers the parked line's shape: it is
// a line, whatever the agent wrote.
func TestFirstLineKeepsAComposedLineToOneLine(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "several lines", text: "the reason\nand more detail", want: "the reason"},
		{name: "nothing at all", text: "   ", want: "no reason was given"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the agent's text.
			text := tc.text

			// Act.
			got := firstLine(text)

			// Assert.
			if got != tc.want {
				t.Fatalf("firstLine(%q) = %q, want %q", text, got, tc.want)
			}
		})
	}
}

// TestShortRendersACommitTheWayNarrationNamesIt covers the merge tab's lines.
func TestShortRendersACommitTheWayNarrationNamesIt(t *testing.T) {
	// Arrange: a full sha.
	sha := "abcdef0123456789abcdef0123456789abcdef01"

	// Act.
	got := short(sha)

	// Assert.
	if got != "abcdef012345" {
		t.Fatalf("short(%q) = %q, want the narration's own length", sha, got)
	}
}

// TestPostPromptFailureRidesTheTerminalOutsideTheDaemonsRepo covers the same
// ruling for EVERY OTHER repository, the case the two methods once spelled
// differently: an after-action failure there aborted a merge the contract says
// had already succeeded.
func TestPostPromptFailureRidesTheTerminalOutsideTheDaemonsRepo(t *testing.T) {
	// Arrange: a non-self repository whose after-action's turn fails.
	h := newHarness(t)
	h.configureActions(nil, []string{"after"})
	h.briefs["after"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge reported a failure for a post-prompt: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the merge is %q, want it to have succeeded anyway", facts.State)
	}
}

// TestPostPromptFailureWarnsOutsideTheDaemonsRepo covers what the swallowed
// failure becomes: the canonical WARN, carrying the failure's own text, so the
// run's success is never silent about it.
func TestPostPromptFailureWarnsOutsideTheDaemonsRepo(t *testing.T) {
	// Arrange: a non-self repository whose after-action's turn fails.
	h := newHarness(t)
	h.configureActions(nil, []string{"after"})
	h.briefs["after"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	var warned *dlog.Record
	for i, rec := range h.logs.Records() {
		if rec.Operation == "daemon.merge.post_prompt" && rec.Level == "warn" {
			warned = &h.logs.Records()[i]
		}
	}
	if warned == nil {
		t.Fatal("no daemon.merge.post_prompt WARN records the after-action's failure")
	}
	if text, _ := warned.Context["error"].(string); !strings.Contains(text, "after-merge prompt") {
		t.Fatalf("the WARN's error is %q, want the failure's own text", warned.Context["error"])
	}
}

// TestPostPromptFailedTabSettlesOutsideTheDaemonsRepo covers the feed's half of
// the same failure: the post_prompt tab still settles failed with its composed
// summary even though the run succeeded.
func TestPostPromptFailedTabSettlesOutsideTheDaemonsRepo(t *testing.T) {
	// Arrange: a non-self repository whose after-action's turn fails.
	h := newHarness(t)
	h.configureActions(nil, []string{"after"})
	h.briefs["after"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	want := []string{TabQueue, TabPostPrompt}
	if got := h.feed.tabSequence(); !equal(got, want) {
		t.Fatalf("the tab sequence is %v, want %v", got, want)
	}
	summary := h.feed.lastTabOfKind(TabPostPrompt).GetPostPrompt().GetSettled().GetFailed().GetSummary()
	if got := summary; !strings.Contains(got, "after-merge prompt") {
		t.Fatalf("the settled post-prompt tab reads %q, want the composed summary", got)
	}
}

// TestPostPromptNeverParks covers the parked-versus-failed distinction the one
// after-action helper turns on: a park stops the run and holds its lease, and
// the after-action has no parking arm to reach for.
func TestPostPromptNeverParks(t *testing.T) {
	// Arrange: a non-self repository whose after-action's turn fails.
	h := newHarness(t)
	h.configureActions(nil, []string{"after"})
	h.briefs["after"] = nil
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State == StateParked {
		t.Fatalf("the merge parked on a failed after-action, want %q", StateMerged)
	}
}

// ---- The repository's own merge policy ----

// submittedWith answers the texts submitted under one merge origin.
func submittedWith(h *harness, origin conversationv1.PromptOrigin) []string {
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	var out []string
	for _, sub := range h.queue.submissions {
		if sub.Origin != origin {
			continue
		}
		for _, block := range sub.Said.GetContent().GetBlocks() {
			if text := block.GetText(); text != nil {
				out = append(out, text.GetText())
			}
		}
	}
	return out
}

// TestRepositoryPolicyFillsTheEmptyBeforeSlot covers the owner's ruling that a
// repository states its own merge policy in its tree: a workspace that
// configured no before-action runs the repository's `merge-before` brief.
func TestRepositoryPolicyFillsTheEmptyBeforeSlot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.policy[prompts.PolicyMergeBefore] = "run the repository's checklist"
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	got := submittedWith(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION)
	if len(got) != 1 || got[0] != "run the repository's checklist" {
		t.Fatalf("before-merge submissions = %v, want the repository's own brief", got)
	}
}

// TestRepositoryPolicyFillsTheEmptyAfterSlot covers the same for the
// after-merge action.
func TestRepositoryPolicyFillsTheEmptyAfterSlot(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.policy[prompts.PolicyMergeAfter] = "announce the landing"
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	got := submittedWith(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION)
	if len(got) != 1 || got[0] != "announce the landing" {
		t.Fatalf("after-merge submissions = %v, want the repository's own brief", got)
	}
}

// TestConfiguredActionBeatsTheRepositoryPolicy covers the precedence: the file
// fills an EMPTY slot and never overrides what a create configured.
func TestConfiguredActionBeatsTheRepositoryPolicy(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.configureActions([]string{"the workspace's own precondition"}, nil)
	h.policy[prompts.PolicyMergeBefore] = "the repository's default"
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	got := submittedWith(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION)
	if len(got) != 1 || got[0] != "the workspace's own precondition" {
		t.Fatalf("before-merge submissions = %v, want the configured action alone", got)
	}
}

// TestAbsentRepositoryPolicyRunsNothing covers the other half of the ruling:
// a repository that states no merge policy is what every repository looked
// like before the policy existed, and that is not an error.
func TestAbsentRepositoryPolicyRunsNothing(t *testing.T) {
	// Arrange: a repository whose policy directory holds nothing.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	before := submittedWith(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION)
	after := submittedWith(h, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION)
	if len(before) != 0 || len(after) != 0 {
		t.Fatalf("merge-action submissions = (%v, %v), want none", before, after)
	}
}

// TestAStatedBeforePolicyThatWillNotReadFailsTheRun covers the distinction an
// absent brief does not: a policy the repository STATED and that cannot be
// read is a fault, never a silent skip.
func TestAStatedBeforePolicyThatWillNotReadFailsTheRun(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.policy[prompts.PolicyMergeBefore] = "unreadable"
	h.policyErr[prompts.PolicyMergeBefore] = errors.New("the header does not parse")
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the merge is %q, want %q", facts.State, StateFailed)
	}
	if h.git.seen("merge_no_ff") {
		t.Fatal("the merge ran despite an unreadable before-merge policy")
	}
}
