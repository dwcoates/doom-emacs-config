package merge

import (
	"context"
	"errors"
	"os"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/testsclient"
	"agent-repld/internal/wsm"
)

// ruleSuitesOfTheTests are the rule suites the rule check tests name.
var ruleSuitesOfTheTests = []string{"repo-scans", "shellcheck"}

// ruleCheckHarness answers a harness whose repository's deploy runs
// agent-repl's suites (an unversioned local merge) and whose roster names rule
// suites, so an agent's merge runs them before it is admitted.
func ruleCheckHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	config := localMergeConfig()
	config.GetPreDeploy().GetLocalMerge().VersionTag = nil
	h.mu.Lock()
	h.config = config
	h.ruleSuites = ruleSuitesOfTheTests
	h.mu.Unlock()
	return h
}

// agentFinish asks for the harness workspace's own deploy, as its agent.
func agentFinish(h *harness) error {
	return enqueueErr(h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: ownBranch, By: RequestedByAgent, Asker: theWorkspace}))
}

// agentAsyncMerge asks for the async merge of asyncBranch made by the harness
// workspace, as its agent.
func agentAsyncMerge(h *harness) error {
	return enqueueErr(h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByAgent, Asker: theWorkspace,
		Detached: &DetachedRun{Maker: theWorkspace, Name: "reconnect-fix"}}))
}

// ruleStarts answers every run the rule check started on the test service.
func (h *harness) ruleStarts() []testsclient.Spec {
	h.tests.mu.Lock()
	defer h.tests.mu.Unlock()
	var out []testsclient.Spec
	for _, spec := range h.tests.starts {
		if strings.HasSuffix(spec.Label, " rule checks") {
			out = append(out, spec)
		}
	}
	return out
}

// workspaceDir answers the harness workspace's worktree.
func (h *harness) workspaceDir() string {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	return h.db.workspaces[theWorkspace].Dir
}

func TestAnAgentsFinishRunsTheRuleSuitesInItsWorktreeBeforeItIsAdmitted(t *testing.T) {
	// Arrange
	h := ruleCheckHarness(t)

	// Act
	err := agentFinish(h)

	// Assert
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	starts := h.ruleStarts()
	if len(starts) != 1 || starts[0].Dir != h.workspaceDir() {
		t.Fatalf("rule check runs = %+v, want one in the workspace's worktree %s", starts, h.workspaceDir())
	}
	argv := starts[0].Argv
	if !slices.Equal(argv[len(argv)-2:], []string{"--suites", "repo-scans,shellcheck"}) {
		t.Fatalf("argv = %v, want the rule suites named last", argv)
	}
	if state, ok := h.entryState(); !ok || state != wsm.MergeRequested {
		t.Fatalf("queue entry = %v, %v; want the merge recorded once its rule suites passed", state, ok)
	}
	if passed := h.recordsFor("info", opRuleCheck, "the merge's rule suites passed; the merge goes on to be admitted"); len(passed) != 1 {
		t.Fatalf("pass records = %v, want one", passed)
	}
}

func TestAFinishWhoseRuleSuitesFailIsRefusedNamingTheFailingCases(t *testing.T) {
	// Arrange
	h := ruleCheckHarness(t)
	h.runner.runs = []scriptedRun{{Output: failedUnit("repo-scans:daemonrules#00", "repo-scans", "--- FAIL: TestEveryGoFileIsGofmtClean (0.01s)"), Code: 1}}

	// Act
	err := agentFinish(h)

	// Assert
	refusal, ok := Refused(err)
	if !ok || refusal.Arm != ArmRuleChecksFailed {
		t.Fatalf("Enqueue = %v, want the rule_checks_failed refusal", err)
	}
	if refusal.Fields["worktree"] != h.workspaceDir() || !slices.Equal(refusal.Fields["suites"].([]string), ruleSuitesOfTheTests) {
		t.Fatalf("refusal fields = %v, want the worktree and the rule suites", refusal.Fields)
	}
	if failing := refusal.Fields["failing_tests"].(string); !strings.Contains(failing, "TestEveryGoFileIsGofmtClean") || !strings.Contains(failing, "run on this branch before its merge was admitted") {
		t.Fatalf("failing tests = %q, want the failing case under the rule check's own intro", failing)
	}
	archived, readErr := os.ReadFile(refusal.Fields["log"].(string))
	if readErr != nil || !strings.Contains(string(archived), "TestEveryGoFileIsGofmtClean") {
		t.Fatalf("archived log = %q, %v; want the run's whole output", archived, readErr)
	}
	if _, ok := h.entryState(); ok {
		t.Fatal("a merge whose rule suites failed was recorded")
	}
	h.db.mu.Lock()
	recorded := len(h.db.testCases[theWorkspace])
	h.db.mu.Unlock()
	if recorded != 1 {
		t.Fatalf("recorded cases = %d, want the failing case kept for the maker's -rerun", recorded)
	}
	if refused := h.recordsFor("info", opRuleCheck, "refused a merge whose branch failed its rule suites"); len(refused) != 1 || refused[0].Context["archive"] != refusal.Fields["log"] {
		t.Fatalf("refusal records = %v, want one naming the archive", refused)
	}
}

func TestAnAsyncMergeRunsItsRuleSuitesInTheWorktreeHoldingItsBranch(t *testing.T) {
	// Arrange
	h := ruleCheckHarness(t)
	h.git.mu.Lock()
	h.git.worktrees = []gitclient.Worktree{{Dir: "/w/other", Branch: "other"}, {Dir: "/w/agent-fix", Branch: asyncBranch}}
	h.git.mu.Unlock()

	// Act
	err := agentAsyncMerge(h)

	// Assert
	if err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	if starts := h.ruleStarts(); len(starts) != 1 || starts[0].Dir != "/w/agent-fix" {
		t.Fatalf("rule check runs = %+v, want one in the worktree holding %s", starts, asyncBranch)
	}
}

func TestAnAsyncMergeWhoseBranchNoWorktreeHoldsRunsNoRuleCheck(t *testing.T) {
	// Arrange: the queue makes the branch a worktree when its merge starts,
	// and the gate checks it there.
	h := ruleCheckHarness(t)
	h.git.mu.Lock()
	h.git.worktrees = []gitclient.Worktree{{Dir: "/w/other", Branch: "other"}}
	h.git.mu.Unlock()

	// Act
	err := agentAsyncMerge(h)

	// Assert
	if err != nil {
		t.Fatalf("Enqueue = %v, want the merge admitted", err)
	}
	if starts := h.ruleStarts(); len(starts) != 0 {
		t.Fatalf("rule check runs = %+v, want none", starts)
	}
	if skipped := h.recordsFor("info", opRuleCheck, "no worktree holds the merge's branch, so its rule suites run in the gate alone"); len(skipped) != 1 || skipped[0].Context["branch"] != asyncBranch {
		t.Fatalf("skip records = %v, want one naming the branch", skipped)
	}
}

func TestNoRuleCheckRunsWhereItIsNotAsked(t *testing.T) {
	tests := []struct {
		name   string
		suites []string
		by     Requester
	}{
		{name: "the user's own finish", suites: ruleSuitesOfTheTests, by: RequestedByUser},
		{name: "a roster naming no rule suite", suites: nil, by: RequestedByAgent},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := ruleCheckHarness(t)
			h.mu.Lock()
			h.ruleSuites = tt.suites
			h.mu.Unlock()

			// Act
			err := h.request(t, ownBranch, tt.by)

			// Assert
			if err != nil {
				t.Fatalf("Enqueue: %v", err)
			}
			if starts := h.ruleStarts(); len(starts) != 0 {
				t.Fatalf("rule check runs = %+v, want none", starts)
			}
		})
	}
}

func TestARuleCheckThatCannotRunAdmitsNothingAndSaysWhy(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "the run cannot start", arrange: func(h *harness) { h.tests.startErr = errors.New("the service is down") },
			message: "the rule check's run could not be started on the test service; the merge is not admitted"},
		{name: "the run cannot be watched", arrange: func(h *harness) { h.tests.watchErr = errors.New("the stream broke") },
			message: "could not watch the rule check's run; it was stopped and the merge is not admitted"},
		{name: "the service loses the run", arrange: func(h *harness) {
			h.tests.onStart = func(run string) { h.tests.lose(run, "the service restarted") }
		}, message: "the test service lost the rule check's run; the merge is not admitted"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := ruleCheckHarness(t)
			tt.arrange(h)

			// Act
			err := agentFinish(h)

			// Assert
			if _, refused := Refused(err); err == nil || refused {
				t.Fatalf("Enqueue = %v, want an error, not a refusal", err)
			}
			if _, ok := h.entryState(); ok {
				t.Fatal("a merge whose rule check could not run was recorded")
			}
			if errs := h.recordsFor("error", opRuleCheck, tt.message); len(errs) != 1 || errs[0].Context["worktree"] != h.workspaceDir() {
				t.Fatalf("error records = %v, want one naming the worktree", errs)
			}
		})
	}
}

func TestARuleCheckRunThatOutlivesItsBoundIsStoppedAndFails(t *testing.T) {
	// Arrange: the run is held, so it never ends on its own.
	h := ruleCheckHarness(t)
	entered, release := h.tests.holds.arm("ws rule checks")
	defer close(release)
	go func() { <-entered }()

	// Act
	_, _, err := h.o.runRuleCheck(context.Background(), h.o.log(context.Background(), theWorkspace), "/w/t", []string{"bash", "x"}, "ws", time.Millisecond, dlog.Context{"worktree": "/w/t"})

	// Assert
	if !errors.Is(err, errRuleCheckTimedOut) {
		t.Fatalf("runRuleCheck = %v, want the bound's error", err)
	}
	h.tests.mu.Lock()
	stops := append([]string(nil), h.tests.stops...)
	h.tests.mu.Unlock()
	if len(stops) != 1 {
		t.Fatalf("stopped runs = %v, want the run the bound cut", stops)
	}
	if errs := h.recordsFor("error", opRuleCheck, "the rule check's run outlived its bound and was stopped; the merge is not admitted"); len(errs) != 1 {
		t.Fatalf("error records = %v, want one", errs)
	}
}
