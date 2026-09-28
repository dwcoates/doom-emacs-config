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
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
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

// TestAConflictTheAgentResolvesOnItsBranchLandsOnTheNextAttempt covers the
// other conflict outcome. The agent brings its OWN branch up to date in its own
// worktree, and the next attempt is the merge made again, which now lands.
// (It replaces the daemon concluding a commit the agent staged in the target:
// the queue never merges in the target any more.)
func TestAConflictTheAgentResolvesOnItsBranchLandsOnTheNextAttempt(t *testing.T) {
	// Arrange: a merge that conflicts, then lands once the agent's turn ended.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.landsCleanly("abc123def4567")
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

// TestAResolvedConflictCommitsNothingOfTheDaemonsOwn pins who writes a
// resolution: the agent, on its own branch. The daemon records no commit of its
// own anywhere, so nothing it made can land on the target unreviewed.
func TestAResolvedConflictCommitsNothingOfTheDaemonsOwn(t *testing.T) {
	// Arrange: a conflict the agent resolves on its branch.
	h := newHarness(t)
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.landsCleanly("abc123def4567")
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
	if len(h.git.commitMessages) != 0 {
		t.Fatalf("the daemon made commits %v, want none of its own", h.git.commitMessages)
	}
}

// TestAMergeGitRefusesSurfacesTheGitFailure covers the seam: the leaf's
// failure to make the merge at all is the merge's failure, never swallowed.
func TestAMergeGitRefusesSurfacesTheGitFailure(t *testing.T) {
	// Arrange: a merge git refuses outright.
	h := newHarness(t)
	h.emacsRepo()
	h.git.mergeErr = errors.New("refusing to merge unrelated histories")
	enqueue(t, h)

	// Act.
	_ = h.admit(context.Background())

	// Assert: the failure is the MERGE's, stated on its own facts. The pump's
	// return is not where it surfaces: one merge ending badly must not stop
	// the queue behind it, so the run's terminal is the report.
	facts, _ := h.o.Facts(theWorkspace)
	if facts.State != StateFailed {
		t.Fatalf("the merge is %q after git refused it, want it failed", facts.State)
	}
	if !strings.Contains(facts.Detail, "unrelated histories") {
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
	err := route(h, ctx, "guidance-turn", "try resolving it this way")

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
	err := route(h, ctx, "submitted-turn", "try resolving it this way")

	// Assert.
	if err != nil {
		t.Fatalf("RouteParked failed: %v", err)
	}
	<-done
	// The run asks for its slot back only once the guidance turn has ended.
	<-h.waiting
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

// TestGateRunsInTheQueuesOwnTree covers where the gate runs: on the tree the
// merge commit produced, which is the queue's own scratch tree, never the
// target checkout.
func TestGateRunsInTheQueuesOwnTree(t *testing.T) {
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
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.queueTrees) != 1 || h.runner.dirs[0] != h.git.queueTrees[0] {
		t.Fatalf("the gate ran in %v, want the queue's tree %v", h.runner.dirs, h.git.queueTrees)
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

// --- a park stays alive until it is resolved (2026-09-28, lease c8a3a664006f46c1)

// TestGuidanceWhileParkedIsDeliveredToTheWorkspacesAgent covers the first half
// of "every guidance is delivered": the park that received a prompt used to
// discard it, so the agent never saw it.
func TestGuidanceWhileParkedIsDeliveredToTheWorkspacesAgent(t *testing.T) {
	// Arrange: a merge parked on its gate.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 2)

	// Act.
	err := route(h, ctx, "g-1", "the gate is broken on master; drop it")

	// Assert.
	h.mu.Lock()
	defer h.mu.Unlock()
	if err != nil || len(h.parkedTurns) != 1 || h.parkedTurns[0] != "g-1" {
		t.Fatalf("RouteParked = %v, routed %v; want the guidance delivered under its own turn", err, h.parkedTurns)
	}
}

// TestAParkedMergeAnswersEveryPromptNotOnlyTheFirst covers the second half:
// after a resume the merge parks again, and the next prompt still has a
// receiver that delivers and answers it (the owner's second prompt had none).
func TestAParkedMergeAnswersEveryPromptNotOnlyTheFirst(t *testing.T) {
	// Arrange: a merge parked on a gate that stays broken, guided once and
	// resumed, so it parks a second time.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 2)
	if err := route(h, ctx, "g-1", "try again"); err != nil {
		t.Fatalf("the first guidance: %v", err)
	}
	resume(t, h, ctx)
	waitForParked(t, h)

	// Act.
	err := route(h, ctx, "g-2", "and again")

	// Assert.
	h.mu.Lock()
	defer h.mu.Unlock()
	if err != nil || len(h.parkedTurns) != 2 || h.parkedTurns[1] != "g-2" {
		t.Fatalf("RouteParked = %v, routed %v; want the second prompt delivered too", err, h.parkedTurns)
	}
}

// TestARefusedGuidanceRouteIsAnsweredAndTheMergeStaysParked covers the refusal:
// the caller is told, and the park keeps listening for the next prompt.
func TestARefusedGuidanceRouteIsAnsweredAndTheMergeStaysParked(t *testing.T) {
	// Arrange: a parked merge whose session refuses the route once.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 2)
	h.mu.Lock()
	h.parkedErr = errors.New("the workspace has no live session")
	h.mu.Unlock()
	if err := route(h, ctx, "g-1", "first"); err == nil {
		t.Fatal("a refused route was answered as delivered")
	}
	h.mu.Lock()
	h.parkedErr = nil
	h.mu.Unlock()

	// Act.
	err := route(h, ctx, "g-2", "second")

	// Assert.
	if err != nil {
		t.Fatalf("the prompt after a refused route = %v, want it delivered by the park still listening", err)
	}
}

// TestAParkedMergeResumesByMakingItsMergeAgain covers what resuming is: on the
// guidance turn's end, the merge is made afresh rather than carried on from a
// stale tree.
func TestAParkedMergeResumesByMakingItsMergeAgain(t *testing.T) {
	// Arrange: a parked merge whose gate passes after the guidance.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	parkOnABrokenGate(t, h, ctx, 1)
	h.gatePasses("daemon")
	if err := route(h, ctx, "g-1", "the gate is fixed now"); err != nil {
		t.Fatalf("the guidance: %v", err)
	}

	// Act.
	resume(t, h, ctx)

	// Assert.
	h.git.mu.Lock()
	merges := len(h.git.mergeDirs)
	h.git.mu.Unlock()
	facts, _ := h.o.Facts(theWorkspace)
	if merges != 2 || facts.State != StateMerged {
		t.Fatalf("merges made = %d, state = %q; want the merge made again and landed", merges, facts.State)
	}
}

// --- a broken gate is not a test failure --------------------------------

// TestAGateThatFailedToRunParksWithNoRepairRound covers every way a gate can
// fail to run: none of them is a failure the branch's agent can repair.
func TestAGateThatFailedToRunParksWithNoRepairRound(t *testing.T) {
	tests := []struct {
		name  string
		setup func(h *harness)
	}{
		{name: "the shell could not find the command (exit 127)", setup: func(h *harness) { h.gateBroken(1) }},
		{name: "the shell could not execute the command (exit 126)", setup: func(h *harness) {
			h.runner.runs = append(h.runner.runs, scriptedRun{Code: 126})
		}},
		{name: "the gate's script is not there", setup: func(h *harness) { h.script = filepath.Join(h.stateDir, "absent.sh") }},
		{name: "the gate could not be started", setup: func(h *harness) {
			h.runner.runs = append(h.runner.runs, scriptedRun{Err: errors.New("fork/exec bash: resource temporarily unavailable")})
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.emacsRepo()
			h.landsCleanly("abc123def4567")
			h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
			tc.setup(h)
			enqueue(t, h)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			// Act.
			if err := h.admit(ctx); err != nil {
				t.Fatalf("admitting: %v", err)
			}

			// Assert.
			facts := h.footer.last()
			repairs := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR)
			if facts.State != StateParked || repairs != 0 {
				t.Fatalf("state = %q with %d repair round(s), want parked with none", facts.State, repairs)
			}
		})
	}
}

// TestABrokenGateParksWithAPlainLine pins the line the user reads.
func TestABrokenGateParksWithAPlainLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	parkOnABrokenGate(t, h, ctx, 1)

	// Assert.
	if line := h.footer.last().ParkedLine; !strings.Contains(line, "the test gate itself failed to run") {
		t.Fatalf("the parked line is %q, want it to say the gate itself failed to run", line)
	}
}

// TestAFailingSuiteIsStillATestFailure is the other side of the line: a gate
// that RAN and failed goes to the repair round as before.
func TestAFailingSuiteIsStillATestFailure(t *testing.T) {
	// Arrange: a gate that ran and failed, then passes.
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
	if n := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); n != 1 {
		t.Fatalf("%d repair round(s) for a failing suite, want 1", n)
	}
}

// --- the queue owns the target ------------------------------------------

// TestTheMergeIsNeverMadeInTheTarget covers where the merge is made: the
// queue's own tree, so the live checkout never carries a half-made merge.
func TestTheMergeIsNeverMadeInTheTarget(t *testing.T) {
	// Arrange.
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
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if slices.Contains(h.git.mergeDirs, h.targetD) || !equal(h.git.mergeDirs, h.git.queueTrees) {
		t.Fatalf("merges were made in %v, want only the queue's trees %v", h.git.mergeDirs, h.git.queueTrees)
	}
}

// TestAPassingGateFastForwardsTheTargetToTheTestedCommit covers the landing:
// one fast-forward, to exactly the commit the gate passed.
func TestAPassingGateFastForwardsTheTargetToTheTestedCommit(t *testing.T) {
	// Arrange.
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
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if want := []string{h.targetD + "@abc123def4567"}; !equal(h.git.fastForwards, want) {
		t.Fatalf("fast-forwards = %v, want %v", h.git.fastForwards, want)
	}
}

// TestAFailedMergeLeavesTheTargetUntouched covers each way a merge can stop
// short of landing: none moves the target.
func TestAFailedMergeLeavesTheTargetUntouched(t *testing.T) {
	tests := []struct {
		name  string
		setup func(h *harness)
	}{
		{name: "the agent escalates a failing gate", setup: func(h *harness) {
			h.landsCleanly("abc123def4567")
			h.gateFails("daemon")
			h.escalate("the storage layer needs redesigning")
		}},
		{name: "the merge conflicts and the agent resolves nothing", setup: func(h *harness) {
			h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
		}},
		{name: "the gate itself is broken", setup: func(h *harness) {
			h.landsCleanly("abc123def4567")
			h.gateBroken(1)
		}},
		{name: "git refuses the merge", setup: func(h *harness) {
			h.git.mergeErr = errors.New("refusing to merge unrelated histories")
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.emacsRepo()
			h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
			tc.setup(h)
			enqueue(t, h)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			// Act.
			if err := h.admit(ctx); err != nil {
				t.Fatalf("admitting: %v", err)
			}

			// Assert.
			h.git.mu.Lock()
			defer h.git.mu.Unlock()
			if len(h.git.fastForwards) != 0 || slices.Contains(h.git.mergeDirs, h.targetD) || len(h.git.commitMessages) != 0 {
				t.Fatalf("the target was touched: fast-forwards %v, merges in %v, commits %v",
					h.git.fastForwards, h.git.mergeDirs, h.git.commitMessages)
			}
		})
	}
}

// TestATargetThatMovedUnderTheGateIsMergedOntoAgain covers the landing's
// guard: a tip that moved while the merge was tested is never overwritten; the
// merge is made again on it and tested again.
func TestATargetThatMovedUnderTheGateIsMergedOntoAgain(t *testing.T) {
	// Arrange: the target's tip reads "base" for the first attempt, then moves.
	h := newHarness(t)
	h.emacsRepo()
	h.git.refSeqs["HEAD"] = []string{"base", "moved", "moved"}
	h.git.outcomes = append(h.git.outcomes,
		gitclient.MergeOutcome{Landed: &gitclient.Commit{SHA: "first-merge"}},
		gitclient.MergeOutcome{Landed: &gitclient.Commit{SHA: "second-merge"}})
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if want := []string{h.targetD + "@second-merge"}; !equal(h.git.fastForwards, want) || !equal(h.git.queueBases, []string{"base", "moved"}) {
		t.Fatalf("fast-forwards %v on bases %v, want only the merge made on the moved tip", h.git.fastForwards, h.git.queueBases)
	}
}

// TestEveryAttemptsTreeIsRemoved covers the scratch trees' lifetime: none
// outlives its attempt.
func TestEveryAttemptsTreeIsRemoved(t *testing.T) {
	// Arrange: a failing gate, a repair, and a landing: two attempts.
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
	h.git.mu.Lock()
	defer h.git.mu.Unlock()
	if len(h.git.queueTrees) != 2 || !equal(h.git.removedQueueTrees, h.git.queueTrees) {
		t.Fatalf("trees made %v, removed %v; want every attempt's tree removed", h.git.queueTrees, h.git.removedQueueTrees)
	}
}

// TestALandingWhoseRangeWillNotReadStillConcludesAsMerged covers the account
// of a landing that has happened: a fault in reading it is not a failed merge.
func TestALandingWhoseRangeWillNotReadStillConcludesAsMerged(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.landedErr = errors.New("fatal: bad revision")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the merge is %q, want merged: the target had already moved", facts.State)
	}
}

// --- a branch already on the target (2026-09-28 15:28:24, lease 9cf657a4654d4c93)

// TestABranchAlreadyOnTheTargetConcludesAsMerged covers the no-op merge: it
// landed long ago, and says so honestly rather than failing.
func TestABranchAlreadyOnTheTargetConcludesAsMerged(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.git.contained = true
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	facts, _ := h.o.Facts(theWorkspace)
	if facts.State != StateMerged || facts.Detail != "already on master; nothing to merge" {
		t.Fatalf("facts = %q / %q, want merged, already on master", facts.State, facts.Detail)
	}
}

// TestABranchAlreadyOnTheTargetMakesNoMerge covers what the no-op merge does
// NOT do: no merge commit, no landed-range read of a one-parent commit, no
// fast-forward.
func TestABranchAlreadyOnTheTargetMakesNoMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.git.contained = true
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if h.git.seen("merge_no_ff") || h.git.seen("landed_range") || h.git.seen("fast_forward") {
		t.Fatalf("git was asked %v, want no merge, range or fast-forward", h.git.calls)
	}
}

// --- a repair never changes the merge machinery mid-merge ----------------

// TestARepairThatChangesTheMergeMachineryIsRefusedAndParks covers the owner's
// ruling: the gate judging the merge must not be edited by the merge.
func TestARepairThatChangesTheMergeMachineryIsRefusedAndParks(t *testing.T) {
	// Arrange: the branch changes no machinery before the repair, and the
	// repair adds a change to the merge package.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.git.changedIn["master...feature"] = [][]string{
		{"modules/app/agent-repl/webapp/src/a.ts"},
		{"modules/app/agent-repl/webapp/src/a.ts", "modules/app/agent-repl/daemon/internal/merge/testgate.go"},
	}
	h.gateFails("daemon")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	if err := h.admit(ctx); err != nil {
		t.Fatalf("admitting: %v", err)
	}

	// Assert.
	facts := h.footer.last()
	if facts.State != StateParked || !strings.Contains(facts.ParkedLine, "internal/merge/testgate.go") {
		t.Fatalf("state = %q, line = %q; want parked naming the machinery the repair changed", facts.State, facts.ParkedLine)
	}
}

// TestMachineryTheBranchAlreadyChangedIsNotARepairsChange covers the other
// side: a branch whose own work is in the merge package still merges.
func TestMachineryTheBranchAlreadyChangedIsNotARepairsChange(t *testing.T) {
	// Arrange: the branch's own work touches the merge package throughout.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/internal/merge/run.go"}
	h.git.changedIn["master...feature"] = [][]string{{"modules/app/agent-repl/daemon/internal/merge/run.go"}}
	h.gateFails("daemon")
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the merge failed: %v", err)
	}

	// Assert.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the merge is %q, want the author's own machinery change landed", facts.State)
	}
}

// TestTheRepairBriefForbidsChangingTheMergeMachinery pins the brief's half of
// the ruling: the agent is told before it is refused.
func TestTheRepairBriefForbidsChangingTheMergeMachinery(t *testing.T) {
	tests := []struct{ brief string }{{brief: BriefConflictResolve}, {brief: BriefTestFailureResolve}}
	for _, tc := range tests {
		t.Run(tc.brief, func(t *testing.T) {
			// Arrange.
			dir := filepath.Join("..", "..", "..", "prompts")

			// Act.
			brief, err := LoadBrief(dir, tc.brief)

			// Assert.
			if err != nil || !strings.Contains(brief.Body, "daemon/internal/merge/") || !strings.Contains(brief.Body, "bin/test-all.sh") {
				t.Fatalf("the %s brief (err %v) does not forbid changing the merge machinery", tc.brief, err)
			}
		})
	}
}

// --- the escalation record is answered once ------------------------------

// TestTheEscalationRecordIsRemovedOnceRead covers the record's lifetime: it is
// consumed, so it neither lingers in a tree nor re-parks the next attempt.
func TestTheEscalationRecordIsRemovedOnceRead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.escalate("the storage layer needs redesigning")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	if err := h.admit(ctx); err != nil {
		t.Fatalf("admitting: %v", err)
	}

	// Assert.
	if _, err := os.Stat(filepath.Join(h.sourceD, EscalationFile)); !os.IsNotExist(err) {
		t.Fatalf("the escalation record is still there (stat err %v), want it consumed", err)
	}
}

// --- the shim that resolves a merge is the workspace agent's own ----------

// TestEveryMergeDeliveryReachesTheWorkspacesOwnSession is the owner's stated
// invariant (2026-09-28): every brief and every guidance a merge sends goes to
// the merging workspace's own session, never another.
func TestEveryMergeDeliveryReachesTheWorkspacesOwnSession(t *testing.T) {
	// Arrange: another workspace registered beside it, a conflict brief, a
	// fixes brief and a guidance.
	h := newHarness(t)
	h.register("ws-2", "ws-two")
	h.emacsRepo()
	h.git.outcomes = append(h.git.outcomes, mergeConflicted("a.go"))
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gateFails("daemon")
	h.escalate("needs a human")
	enqueue(t, h)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if err := h.admit(ctx); err != nil {
		t.Fatalf("admitting: %v", err)
	}

	// Act.
	if err := route(h, ctx, "g-1", "carry on"); err != nil {
		t.Fatalf("the guidance: %v", err)
	}

	// Assert.
	h.queue.mu.Lock()
	var reached []ids.WorkspaceID
	for _, sub := range h.queue.submissions {
		reached = append(reached, sub.WS)
	}
	h.queue.mu.Unlock()
	h.mu.Lock()
	reached = append(reached, h.parkedWS...)
	h.mu.Unlock()
	if len(reached) != 3 {
		t.Fatalf("deliveries reached %v, want the conflict brief, the fixes brief and the guidance", reached)
	}
	for _, ws := range reached {
		if ws != theWorkspace {
			t.Fatalf("a merge delivery reached %s, want only the merging workspace's own session", ws)
		}
	}
}
