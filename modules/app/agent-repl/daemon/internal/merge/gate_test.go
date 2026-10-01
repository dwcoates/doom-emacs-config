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

func TestTheMergeTestsPanelFollowsEachSuiteFromWaitingToPassed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.streamingGate("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	var states []string
	for _, f := range h.footer.all() {
		if f.Step != "testing" {
			continue
		}
		var state *frontendv1.FooterMergeTestRowState
		for _, row := range f.Tests {
			if row.GetName().GetText() == "daemon" {
				state = row.GetState()
			}
		}
		name := ""
		switch {
		case state.GetWaiting() != nil:
			name = "waiting"
		case state.GetRunning() != nil:
			name = "running"
		case state.GetPassed() != nil:
			name = "passed"
		}
		if len(states) == 0 || states[len(states)-1] != name {
			states = append(states, name)
		}
	}
	if want := []string{"waiting", "running", "passed"}; !equal(states, want) {
		t.Fatalf("panel states = %v, want %v", states, want)
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

func TestEachTestingRoundIsANewTestsRound(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = nil
	h.gateFails("daemon")
	h.gatePasses("daemon")

	// Act.
	admitted(t, h)

	// Assert.
	max := 0
	for _, f := range h.footer.all() {
		if f.TestsRound > max {
			max = f.TestsRound
		}
	}
	if max != 2 {
		t.Fatalf("tests rounds reached %d, want 2", max)
	}
}

func TestThePanelEmptiesWhenTestingEnds(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	for _, f := range h.footer.all() {
		if f.Step != "testing" && len(f.Tests) != 0 {
			t.Fatalf("facts on step %q carry %d test rows, want none outside testing", f.Step, len(f.Tests))
		}
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
