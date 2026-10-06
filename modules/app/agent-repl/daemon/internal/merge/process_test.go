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
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

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

// --- a broken gate is not a test failure --------------------------------

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
			if err != nil {
				t.Fatalf("loading the %s brief: %v", tc.brief, err)
			}
			for _, m := range mergeMachinery {
				if !strings.Contains(brief.Body, "`"+m+"`") {
					t.Errorf("the %s brief does not forbid changing the merge machinery %s", tc.brief, m)
				}
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

// landing arranges the harness's Emacs-repo merge to land: n commits to
// replay, a clean merge commit, and a passing gate.
func landing(h *harness, n int) {
	h.emacsRepo()
	h.git.between = commits(n)
	h.landsCleanly("merge0000commit")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
}

// admitted enqueues the harness's merge as the user asks and runs it to its
// end.
func admitted(t *testing.T, h *harness) {
	t.Helper()
	enqueue(t, h)
	if err := h.admit(context.Background()); err != nil {
		t.Logf("the merge ended on: %v", err)
	}
}

func TestTheEmacsRepoMethodWalksItsStepsInOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)

	// Act.
	admitted(t, h)

	// Assert.
	want := []string{"enqueued", "rebasing", "testing", "committing", StateMerged}
	if got := h.footer.steps(); !equal(got, want) {
		t.Fatalf("steps = %v, want %v", got, want)
	}
}

func TestTheEmacsRepoMethodDrawsItsTabsInOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)

	// Act.
	admitted(t, h)

	// Assert.
	want := []string{TabQueue, TabRebasing, TabTests, TabCommitting}
	if got := h.feed.tabSequence(); !equal(got, want) {
		t.Fatalf("tab sequence = %v, want %v", got, want)
	}
}

func TestTheRebaseReportsEachCommitReplayed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 3)

	// Act.
	admitted(t, h)

	// Assert: 0/3 as it starts, then 1/3, 2/3, 3/3.
	var got []int
	for _, f := range h.footer.all() {
		if f.Step == "rebasing" && (len(got) == 0 || got[len(got)-1] != f.Replayed) {
			got = append(got, f.Replayed)
		}
		if f.Step == "rebasing" && f.Total != 3 {
			t.Fatalf("rebasing total = %d, want 3", f.Total)
		}
	}
	if want := []int{0, 1, 2, 3}; !slices.Equal(got, want) {
		t.Fatalf("replayed = %v, want %v", got, want)
	}
}

func TestTheRebasesLineIsTheCommandReplayingTheCurrentCommit(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)

	// Act.
	admitted(t, h)

	// Assert.
	var lines []string
	for _, f := range h.footer.all() {
		if running := f.Line.GetRebasing().GetRunning(); running != nil {
			lines = append(lines, running.GetText())
		}
	}
	if len(lines) < 2 || lines[0] != "pick c00000000000 commit 1" || lines[len(lines)-1] != "pick c00000000000 commit 2" {
		t.Fatalf("rebase lines = %v, want the pick of commit 1 then commit 2", lines)
	}
}

func TestTheRebaseReplaysOntoTheTargetsTip(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.refs["HEAD"] = "tip0000000000"

	// Act.
	admitted(t, h)

	// Assert.
	if len(h.git.rebaseCommands) == 0 || h.git.rebaseCommands[0] != "start tip0000000000" {
		t.Fatalf("rebase commands = %v, want a start onto the tip", h.git.rebaseCommands)
	}
}

func TestABranchAlreadyOnTheTipReplaysNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)
	h.git.ancestry["0000000000000000000000000000000000000000>feature"] = true

	// Act.
	admitted(t, h)

	// Assert.
	if h.git.seen("start_rebase") {
		t.Fatal("a branch already on the tip was rebased")
	}
	if got := h.footer.last().State; got != StateMerged {
		t.Fatalf("state = %q, want merged", got)
	}
}

func TestAWorktreeWithUncommittedChangesFailsTheMergeBeforeTheRebase(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.clean = false

	// Act.
	admitted(t, h)

	// Assert.
	if h.git.seen("start_rebase") {
		t.Fatal("a dirty worktree was rebased")
	}
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" {
		t.Fatalf("facts = %+v, want merge failed in other", got)
	}
}

func TestAWorktreeOffItsCreationBranchMergesTheBranchCheckedOut(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.mu.Lock()
	h.git.branches[h.sourceD] = "renamed"
	h.git.mu.Unlock()

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateMerged {
		t.Fatalf("facts = %+v, want merged", got)
	}
}

func TestARequestRecordedWithNoBranchMergesTheBranchCheckedOutAtAdmission(t *testing.T) {
	// Arrange: a request an earlier build recorded, before branches were.
	h := newHarness(t)
	landing(h, 1)
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if _, err := h.db.QueueMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Logf("the merge ended on: %v", err)
	}

	// Assert.
	if got := h.footer.last(); got.State != StateMerged {
		t.Fatalf("facts = %+v, want merged", got)
	}
	if _, logged := h.recordFor("info", "daemon.merge.subject"); !logged {
		t.Fatalf("reading the branch at admission was not recorded: %+v", h.logs.Records())
	}
}

func TestAWorktreeSwitchedToAnotherBranchAfterTheRequestFailsTheMerge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	enqueue(t, h)
	h.git.mu.Lock()
	h.git.branches[h.sourceD] = "something-else"
	h.git.mu.Unlock()

	// Act.
	if err := h.admit(context.Background()); err != nil {
		t.Logf("the merge ended on: %v", err)
	}

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" {
		t.Fatalf("facts = %+v, want merge failed in other", got)
	}
}

func TestARebaseCommandThatFailsFailsTheMergeWithGitsLine(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseErr = &gitclient.Error{Stderr: "error: could not apply c1\nmore\n", ExitCode: 1}

	// Act.
	admitted(t, h)

	// Assert.
	var failure string
	for _, f := range h.footer.all() {
		if failed := f.Line.GetRebasing().GetFailed(); failed != nil {
			failure = failed.GetText()
		}
	}
	if failure != "error: could not apply c1" {
		t.Fatalf("rebase failure line = %q, want git's first line", failure)
	}
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" {
		t.Fatalf("facts = %+v, want merge failed in other", got)
	}
}

func TestAConflictIsResolvedByTheRequestersSessionAndTheRebaseContinues(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)
	h.git.rebaseConflicts[2] = []string{"a.go", "b.go"}

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); got != 1 {
		t.Fatalf("conflict briefs = %d, want 1", got)
	}
	if got := h.footer.last().State; got != StateMerged {
		t.Fatalf("state = %q, want merged", got)
	}
	if rounds := h.feed.roundsOfKind(TabRebasing); !slices.Equal(rounds, []int{1, 2}) {
		t.Fatalf("rebasing rounds = %v, want the rebase continued in a second round", rounds)
	}
}

func TestTheConflictResolutionsLineNamesTheCommitAndTheFileCount(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)
	h.git.rebaseConflicts[2] = []string{"a.go", "b.go"}

	// Act.
	admitted(t, h)

	// Assert.
	var line *frontendv1.FooterMergeStepConflict
	for _, f := range h.footer.all() {
		if c := f.Line.GetConflictResolution(); c != nil {
			line = c
		}
	}
	if line.GetCommitSubject() != "commit 2" || line.GetFiles() != 2 {
		t.Fatalf("conflict line = %+v, want commit 2 with 2 files", line)
	}
}

func TestTheConflictBriefNamesTheWorktreeItResolvesIn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}

	// Act.
	admitted(t, h)

	// Assert.
	if len(h.briefValues) == 0 || h.briefValues[0]["worktree_dir"] != h.sourceD {
		t.Fatalf("brief values = %+v, want the worktree %s", h.briefValues, h.sourceD)
	}
}

func TestAConflictResolutionThatGivesUpFailsTheMergeInConflicts(t *testing.T) {
	// Arrange: the files still conflict when the turn ends.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = [][]string{{"a.go"}}

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "conflicts" {
		t.Fatalf("facts = %+v, want merge failed in conflicts", got)
	}
	if len(h.git.fastForwards) != 0 {
		t.Fatalf("a failed merge moved the target: %v", h.git.fastForwards)
	}
}

func TestAConflictResolutionThatGivesUpLeavesTheRebaseInProgress(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 2)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = [][]string{{"a.go"}}

	// Act.
	admitted(t, h)

	// Assert: no command after the conflict, and nothing aborted.
	if !slices.Equal(h.git.rebaseCommands, []string{"start 0000000000000000000000000000000000000000"}) {
		t.Fatalf("rebase commands = %v, want only the start that stopped", h.git.rebaseCommands)
	}
	if h.git.seen("abort_merge") {
		t.Fatal("the rebase was aborted")
	}
}

func TestAConflictResolutionWhoseTurnFailedGivesUp(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "conflicts" {
		t.Fatalf("facts = %+v, want merge failed in conflicts", got)
	}
}

func TestAGivenUpConflictIsAnOutcomeRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}
	h.git.conflicted = [][]string{{"a.go"}}

	// Act.
	admitted(t, h)

	// Assert: a give-up is the merge's ordinary failed outcome, never a fault.
	if _, logged := h.recordFor("info", "daemon.merge.conflicts"); !logged {
		t.Fatalf("the give-up was not recorded at INFO: %+v", h.logs.Records())
	}
	for _, record := range h.logs.Records() {
		if record.Level == "warn" || record.Level == "error" {
			t.Fatalf("a given-up conflict produced a %s record: %+v", record.Level, record)
		}
	}
}

func TestAConflictRepairStandsItsAddressAtTheConflictsTab(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.rebaseConflicts[1] = []string{"a.go"}

	// Act.
	admitted(t, h)

	// Assert: the merge's own turns are recorded at the tab, and only there.
	for _, addr := range h.feed.installed() {
		if addr == nil {
			continue
		}
		want := tabRef(theWorkspace, *addr.Feed.Merge, TabConflicts, 1)
		if addr.Parent == nil || addr.Parent.Row != want.Row || addr.Parent.Feed.Merge == nil || *addr.Parent.Feed.Merge != *want.Feed.Merge {
			t.Fatalf("address = %+v, want the conflicts tab %+v", addr, want)
		}
		return
	}
	t.Fatalf("addresses = %+v, want the conflicts tab addressed", h.feed.installed())
}

func TestFixingAttemptsAreBoundedByTheOneBoundAndThenFailTheMergeInTests(t *testing.T) {
	// Arrange: every gate fails.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	for i := 0; i <= MaxFixAttempts; i++ {
		h.gateFails("daemon")
	}

	// Act.
	admitted(t, h)

	// Assert.
	var attempts []int
	for _, f := range h.footer.all() {
		if f.Step == "fixing" && (len(attempts) == 0 || attempts[len(attempts)-1] != f.Attempt) {
			attempts = append(attempts, f.Attempt)
			if f.MaxAttempts != MaxFixAttempts {
				t.Fatalf("fixing max = %d, want %d", f.MaxAttempts, MaxFixAttempts)
			}
		}
	}
	if !slices.Equal(attempts, []int{1, 2, 3}) {
		t.Fatalf("fixing attempts = %v, want 1, 2, 3", attempts)
	}
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "tests" {
		t.Fatalf("facts = %+v, want merge failed in tests", got)
	}
}

func TestAFixingAttemptThatMakesTheGatePassLands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last().State; got != StateMerged {
		t.Fatalf("state = %q, want merged", got)
	}
	if got := h.queue.countOrigin(conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); got != 1 {
		t.Fatalf("fixing briefs = %d, want 1", got)
	}
}

func TestTheFixingLineNamesTheSuitesBeingFixed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	var suites []string
	for _, f := range h.footer.all() {
		if fixing := f.Line.GetFixing(); fixing != nil {
			suites = fixing.GetSuites()
		}
	}
	if !slices.Equal(suites, []string{"daemon"}) {
		t.Fatalf("fixing line = %v, want [daemon]", suites)
	}
}

func TestTheFixesTabCarriesItsAttemptAndTheBound(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	attempt := h.feed.lastTabOfKind(TabFixes).GetFixes().GetAttempt()
	if attempt.GetAttempt() != 1 || attempt.GetMaxAttempts() != MaxFixAttempts {
		t.Fatalf("fixes attempt = %+v, want 1 of %d", attempt, MaxFixAttempts)
	}
}

func TestAnEscalatedFixFailsTheMergeInTests(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.escalate("this needs a redesign")

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "tests" {
		t.Fatalf("facts = %+v, want merge failed in tests", got)
	}
}

func TestATargetThatMovedBeforeCommittingStartsTheProcessOver(t *testing.T) {
	// Arrange: the tip reads "old" for the first attempt's rebase and its
	// committing check reads "new"; the second attempt stands on "new".
	h := newHarness(t)
	landing(h, 1)
	h.gatePasses("daemon")
	h.git.refSeqs["HEAD"] = []string{"old", "new", "new", "new"}

	// Act.
	admitted(t, h)

	// Assert.
	if !slices.Equal(h.git.rebaseCommands, []string{"start old", "start new"}) {
		t.Fatalf("rebase commands = %v, want the rebase again onto the new tip", h.git.rebaseCommands)
	}
	if len(h.git.queueBases) != 1 || h.git.queueBases[0] != "new" {
		t.Fatalf("merge commits made on %v, want one on the new tip", h.git.queueBases)
	}
}

func TestTheMergeCommitIsMadeOfTheGatedHeadInAScratchTree(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.git.refs["feature"] = "gated0000head"

	// Act.
	admitted(t, h)

	// Assert.
	if len(h.git.mergeDirs) != 1 || h.git.mergeDirs[0] == h.targetD || h.git.mergeDirs[0] == h.sourceD {
		t.Fatalf("merge commits made in %v, want one scratch tree", h.git.mergeDirs)
	}
	if len(h.git.fastForwards) != 1 || h.git.fastForwards[0] != h.targetD+"@merge0000commit" {
		t.Fatalf("fast-forwards = %v, want the target to the merge commit", h.git.fastForwards)
	}
}

func TestTheCommittingLineIsTheMergeCommitsSubject(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	var subject string
	for _, f := range h.footer.all() {
		if c := f.Line.GetCommitting(); c != nil {
			subject = c.GetSubject()
		}
	}
	if subject != "merge(master): feature" {
		t.Fatalf("committing line = %q, want the merge commit's subject", subject)
	}
}

// mergedUpstream arranges the harness's merge of its branch already merged
// upstream: the main worktree on its default branch, upstream ahead.
func mergedUpstream(h *harness) {
	h.git.branches[h.sourceD] = "master"
	h.git.refs["refs/remotes/origin/master"] = "upstream00000"
	h.git.refs["HEAD"] = "local00000000"
	h.db.mu.Lock()
	h.db.queues[h.repoKey()] = nil
	h.db.mu.Unlock()
}

func TestABranchMergedUpstreamFetchesAndFastForwardsTheDefaultBranch(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	mergedUpstream(h)

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert.
	if len(h.git.fetches) != 1 || h.git.fetches[0] != h.sourceD+"<origin" {
		t.Fatalf("fetches = %v, want one of origin in the main worktree", h.git.fetches)
	}
	if !slices.Equal(h.git.fastForwards, []string{h.sourceD + "@upstream00000"}) {
		t.Fatalf("fast-forwards = %v, want the default branch to upstream", h.git.fastForwards)
	}
	if want := []string{TabQueue, TabUpdatingMain}; !equal(h.feed.tabSequence(), want) {
		t.Fatalf("tabs = %v, want %v", h.feed.tabSequence(), want)
	}
}

func TestABranchMergedUpstreamClosesTheRequester(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	mergedUpstream(h)

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("admit: %v", err)
	}

	// Assert.
	if !h.db.closed[theWorkspace] {
		t.Fatal("the requester was not closed once its branch was on the default branch")
	}
}

func TestUpdatingMainSaysFetchingThenFastForwarding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	mergedUpstream(h)

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	_ = h.admit(context.Background())

	// Assert.
	var steps []string
	for _, f := range h.footer.all() {
		switch {
		case f.Line.GetUpdatingMain().GetFetching() != nil:
			steps = append(steps, "fetching")
		case f.Line.GetUpdatingMain().GetFastForwarding() != nil:
			steps = append(steps, "fast-forwarding "+f.Line.GetUpdatingMain().GetFastForwarding().GetCommit())
		}
	}
	if !slices.Equal(steps, []string{"fetching", "fast-forwarding upstream0000"}) {
		t.Fatalf("updating main lines = %v", steps)
	}
}

func TestUpdatingMainFailsWhenTheFetchFails(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	mergedUpstream(h)
	h.git.fetchErr = errors.New("could not read from remote")

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	_ = h.admit(context.Background())

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" {
		t.Fatalf("facts = %+v, want merge failed in other", got)
	}
	if h.db.closed[theWorkspace] {
		t.Fatal("a failed update closed the requester")
	}
}

func TestUpdatingMainFailsWhenTheMainWorktreeIsOffItsDefaultBranch(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	mergedUpstream(h)
	h.git.branches[h.sourceD] = "feature"

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	_ = h.admit(context.Background())

	// Assert.
	if h.git.seen("fetch") {
		t.Fatal("a main worktree off its default branch was fetched into")
	}
	if got := h.footer.last(); got.State != StateFailed {
		t.Fatalf("facts = %+v, want merge failed", got)
	}
}

func TestUpdatingMainAlreadyAtUpstreamMovesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	mergedUpstream(h)
	h.git.refs["HEAD"] = "upstream00000"

	// Act.
	if err := h.request(t, wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, RequestedByUser); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	_ = h.admit(context.Background())

	// Assert.
	if len(h.git.fastForwards) != 0 {
		t.Fatalf("fast-forwards = %v, want none", h.git.fastForwards)
	}
	if got := h.footer.last().State; got != StateMerged {
		t.Fatalf("state = %q, want merged", got)
	}
}

// TestIsMachinery covers what a repair may not change: the merge itself, the
// gate's entry point, and the runner the entry point builds.
func TestIsMachinery(t *testing.T) {
	tests := []struct {
		path string
		want bool
	}{
		{"modules/app/agent-repl/daemon/internal/merge/gate.go", true},
		{"modules/app/agent-repl/bin/test-all.sh", true},
		{"modules/app/agent-repl/testrun/internal/sched/plan.go", true},
		{"modules/app/agent-repl/bin/test-e2e.sh", false},
		{"modules/app/agent-repl/daemon/internal/server/server.go", false},
	}
	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			// Act / Assert.
			if got := isMachinery(tt.path); got != tt.want {
				t.Fatalf("isMachinery(%q) = %v, want %v", tt.path, got, tt.want)
			}
		})
	}
}

func TestShasOfAnswersTheCommitsShasInOrder(t *testing.T) {
	// Arrange, Act.
	got := shasOf(commits(3))

	// Assert.
	if want := []string{"c000000000001", "c000000000002", "c000000000003"}; !slices.Equal(got, want) {
		t.Fatalf("shasOf = %v, want %v", got, want)
	}
}

// TestEveryRebaseIsStartedFromShasOf is the drift guard on the shared shape.
func TestEveryRebaseIsStartedFromShasOf(t *testing.T) {
	// Arrange.
	body, err := os.ReadFile("process.go")
	if err != nil {
		t.Fatalf("reading process.go: %v", err)
	}

	// Act.
	starts := strings.Count(string(body), "r.git.StartRebase(")
	shared := strings.Count(string(body), "r.git.StartRebase(ctx, dir, tip, shasOf(")

	// Assert.
	if starts == 0 || starts != shared {
		t.Fatalf("%d rebase starts, %d through shasOf, want every one through it", starts, shared)
	}
}

func TestDrawFixingStandsTheAttemptLive(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	r := ledgeredRun(t, h)
	round := r.openTab(context.Background(), TabFixes)

	// Act.
	r.drawFixing(context.Background(), round, 2, []string{"daemon"})

	// Assert.
	facts := h.footer.last()
	if facts.Attempt != 2 || facts.MaxAttempts != MaxFixAttempts || h.feed.lastTabOfKind(TabFixes).GetFixes().GetAttempt().GetAttempt() != 2 {
		t.Fatalf("facts = %+v, want attempt 2 of %d drawn", facts, MaxFixAttempts)
	}
}
