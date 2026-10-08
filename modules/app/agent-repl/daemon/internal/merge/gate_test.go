package merge

import (
	frontendv1 "agentrepl/proto/frontend/v1"
	"os"
	"strings"
	"testing"
)

// streamingGate scripts one passing gate run that announces each suite.
func (h *harness) streamingGate(suites ...string) {
	out := ""
	for _, suite := range suites {
		out += "[agent-repl-tests] " + suite + ": starting\n"
		out += "[agent-repl-tests] " + suite + ": passed in 2s\n"
	}
	h.runner.runs = append(h.runner.runs, scriptedRun{Output: out, Code: 0})
}

func TestTheTestsTabFollowsEachSuiteFromRunningToPassed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	var states []string
	h.feed.mu.Lock()
	for _, row := range h.feed.rows {
		for _, suite := range row.Row.GetMergeTab().GetTests().GetSuites() {
			if suite.GetName() != "daemon" {
				continue
			}
			name := "running"
			if suite.GetPassed() != nil {
				name = "passed"
			}
			if len(states) == 0 || states[len(states)-1] != name {
				states = append(states, name)
			}
		}
	}
	h.feed.mu.Unlock()
	if want := []string{"running", "passed"}; !equal(states, want) {
		t.Fatalf("tab suite states = %v, want %v", states, want)
	}
}

func TestTheTestingLineIsEachSuitesEdge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	var edges []string
	for _, f := range h.footer.all() {
		if suite := f.Line.GetTesting(); suite != nil {
			edge := "started"
			if suite.GetPassed() != nil {
				edge = "passed"
			}
			if len(edges) == 0 || edges[len(edges)-1] != edge {
				edges = append(edges, edge)
			}
		}
	}
	if want := []string{"started", "passed"}; !equal(edges, want) {
		t.Fatalf("testing lines = %v, want %v", edges, want)
	}
}

func TestTheTestsTabLinksTheRoundsLog(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	link := h.feed.lastTabOfKind(TabTests).GetTests().GetLog()
	lease := h.db.ledger[theWorkspace][0].Lease
	if link.GetToken().GetValue() != string(lease)+"/1" {
		t.Fatalf("log token = %q, want the lease and round", link.GetToken().GetValue())
	}
	if !strings.HasSuffix(link.GetLabel().GetText(), string(lease)+"-tests-1.log") {
		t.Fatalf("log label = %q, want the log's path", link.GetLabel().GetText())
	}
}

func TestTheRoundsLogHoldsTheWholeRun(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	lease := h.db.ledger[theWorkspace][0].Lease
	body, err := os.ReadFile(h.o.testLog(lease, 1).path)
	if err != nil || !strings.Contains(string(body), "daemon: starting") || !strings.Contains(string(body), "daemon: passed in 2s") {
		t.Fatalf("log = (%q, %v), want the whole run", body, err)
	}
}

func TestABrokenGateFailsTheMergeWithNoFixingAttempt(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateBroken(1)

	// Act.
	admitted(t, h)

	// Assert.
	if got := h.footer.last(); got.State != StateFailed || got.FailedArea != "other" {
		t.Fatalf("facts = %+v, want merge failed in other", got)
	}
	for _, f := range h.footer.all() {
		if f.Step == "fixing" {
			t.Fatal("a broken gate had a fixing attempt")
		}
	}
}

func TestSuiteEdgeReadsTheScriptsLines(t *testing.T) {
	tests := []struct {
		line  string
		name  string
		state suiteState
		ok    bool
	}{
		{line: "[agent-repl-tests] daemon: starting", name: "daemon", state: suiteStateRunning, ok: true},
		{line: "[agent-repl-tests] daemon: passed in 3s", name: "daemon", state: suiteStatePassed, ok: true},
		{line: "[agent-repl-tests] ERROR: webapp failed after 4s with exit code 1", name: "webapp", state: suiteStateFailed, ok: true},
		{line: "[agent-repl-tests] e2e-emacs: DECLINED after 0.125s — its precondition is not met (exit 77)", name: "e2e-emacs", state: suiteStateDeclined, ok: true},
		{line: "some output", ok: false},
	}
	for _, tt := range tests {
		t.Run(tt.line, func(t *testing.T) {
			// Act.
			name, state, ok := suiteEdge(tt.line)

			// Assert.
			if ok != tt.ok || (ok && (name != tt.name || state != tt.state)) {
				t.Fatalf("suiteEdge = (%q, %v, %v), want (%q, %v, %v)", name, state, ok, tt.name, tt.state, tt.ok)
			}
		})
	}
}

func TestDeclinedSuiteUsesTheNonFailurePresentation(t *testing.T) {
	// Arrange
	g := &gateRun{states: map[string]suiteState{}, counts: newSuiteCounts()}

	// Act
	g.setTabSuite("e2e-emacs", suiteStateDeclined)
	line := suiteLine("e2e-emacs", suiteStateDeclined).GetTesting()

	// Assert
	if len(g.suites) != 1 {
		t.Fatalf("tab suites = %d, want one", len(g.suites))
	}
	if _, passed := g.suites[0].GetState().(*frontendv1.FeedMergeTestSuite_Passed); !passed {
		t.Fatalf("tab state = %T, want passed/non-failure", g.suites[0].GetState())
	}
	if _, passed := line.GetEdge().(*frontendv1.FooterMergeStepSuite_Passed); !passed {
		t.Fatalf("footer edge = %T, want passed/non-failure", line.GetEdge())
	}
}
