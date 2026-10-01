package merge

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// TestParseSuiteStatesReadsTheScriptsOwnLines covers the parse contract: the
// gate reads the per-suite lines bin/test-all.sh prints, whatever log prefix
// decorates them.
func TestParseSuiteStatesReadsTheScriptsOwnLines(t *testing.T) {
	tests := []struct {
		name  string
		line  string
		suite string
		want  suiteState
	}{
		{
			name:  "a pass line",
			line:  "[agent-repl-tests] daemon: passed in 12s",
			suite: "daemon",
			want:  suiteStatePassed,
		},
		{
			name:  "a failure line with its exit code",
			line:  "[agent-repl-tests] ERROR: daemon failed after 12s with exit code 1",
			suite: "daemon",
			want:  suiteStateFailed,
		},
		{
			// The script has always printed milliseconds; a whole-seconds-only
			// pattern never matched a real run.
			name:  "a pass line in the script's own decimal seconds",
			line:  "[agent-repl-tests] daemon: passed in 28.392s",
			suite: "daemon",
			want:  suiteStatePassed,
		},
		{
			name:  "a failure line in the script's own decimal seconds",
			line:  "[agent-repl-tests] ERROR: webapp failed after 196.514s with exit code 1",
			suite: "webapp",
			want:  suiteStateFailed,
		},
		{
			name:  "a failure of a unit that could not start",
			line:  "[agent-repl-tests] ERROR: shim failed after 0.000s with exit code -1",
			suite: "shim",
			want:  suiteStateFailed,
		},
		{
			name:  "a skip line",
			line:  "[agent-repl-tests] webapp: not selected by --suites, skipping",
			suite: "webapp",
			want:  suiteStateSkipped,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: one line of the script's output.
			output := tc.line + "\n"

			// Act.
			states := parseSuiteStates(output)

			// Assert.
			if states[tc.suite] != tc.want {
				t.Fatalf("%q parsed to %v, want %v", tc.line, states[tc.suite], tc.want)
			}
		})
	}
}

// TestParseSuiteStatesLetsALaterLineWin covers a suite reported skipped and then
// run: the run is what happened.
func TestParseSuiteStatesLetsALaterLineWin(t *testing.T) {
	// Arrange: a skip followed by a pass.
	output := "[agent-repl-tests] daemon: not selected by --suites, skipping\n" +
		"[agent-repl-tests] daemon: passed in 3s\n"

	// Act.
	states := parseSuiteStates(output)

	// Assert.
	if states["daemon"] != suiteStatePassed {
		t.Fatalf("the suite reads %v, want the later pass to win", states["daemon"])
	}
}

// TestUnsettledSuiteStaysRunning covers a selected suite the output never
// settled: the script was killed or never reached it, and neither is a pass.
func TestUnsettledSuiteStaysRunning(t *testing.T) {
	// Arrange: a gate whose output settles only one of two selected suites.
	h := newHarness(t)
	rows, err := h.o.paintSuites([]string{"daemon", "webapp"},
		"[agent-repl-tests] daemon: passed in 3s\n")
	if err != nil {
		t.Fatalf("painting: %v", err)
	}

	// Act.
	var webapp *frontendv1.FeedMergeTestSuite
	for _, row := range rows {
		if row.GetName() == "webapp" {
			webapp = row
		}
	}

	// Assert.
	if webapp == nil {
		t.Fatal("the unsettled suite drew no row")
	}
	if _, running := webapp.GetState().(*frontendv1.FeedMergeTestSuite_Running); !running {
		t.Fatalf("the unsettled suite is %T, want running", webapp.GetState())
	}
}

// TestSkippedSuiteDrawsNoRow covers the other side: a suite --suites left out was
// never selected, so it is not a state the tab draws.
func TestSkippedSuiteDrawsNoRow(t *testing.T) {
	// Arrange: a run that skipped one of the named suites.
	h := newHarness(t)

	// Act.
	rows, err := h.o.paintSuites([]string{"daemon", "webapp"},
		"[agent-repl-tests] daemon: passed in 3s\n[agent-repl-tests] webapp: not selected by --suites, skipping\n")

	// Assert.
	if err != nil {
		t.Fatalf("painting: %v", err)
	}
	if len(rows) != 1 || rows[0].GetName() != "daemon" {
		t.Fatalf("the tab drew %d row(s), want the run suite alone", len(rows))
	}
}

// TestSuiteOutputIsAttributedToTheSuiteItPrecedes covers the split for a run
// that prints no unit brackets: a line belongs to the suite whose terminal
// comes next.
func TestSuiteOutputIsAttributedToTheSuiteItPrecedes(t *testing.T) {
	// Arrange: two suites' output, interleaved by nothing.
	output := "building daemon\n[agent-repl-tests] daemon: passed in 3s\n" +
		"building webapp\n[agent-repl-tests] webapp: passed in 4s\n"

	// Act.
	sections := splitSuiteOutput(output, []string{"daemon", "webapp"})

	// Assert.
	if !strings.Contains(sections["daemon"], "building daemon") || strings.Contains(sections["daemon"], "building webapp") {
		t.Fatalf("the daemon section is %q, want only its own lines", sections["daemon"])
	}
	if !strings.Contains(sections["webapp"], "building webapp") {
		t.Fatalf("the webapp section is %q, want its own lines", sections["webapp"])
	}
}

// TestInterleavedUnitBlocksAreAttributedToTheirOwnSuites covers the runner's
// real shape: units of different suites finish interleaved, each block
// bracketed by its begin line and its own verdict line.
func TestInterleavedUnitBlocksAreAttributedToTheirOwnSuites(t *testing.T) {
	// Arrange: daemon and webapp units interleaved, a cancelled e2e chunk, and
	// planner noise that names no suite.
	output := strings.Join([]string{
		"[agent-repl-tests] plan: 3 units on 14 core slots",
		"[agent-repl-tests] daemon: starting",
		"[agent-repl-tests] webapp: starting",
		"[agent-repl-tests] unit webapp#00 [webapp] output:",
		"webapp chunk zero",
		"[agent-repl-tests] unit webapp#00 [webapp] ok, 3.000s wall, 2.000s cpu",
		"[agent-repl-tests] unit daemon:internal/x [daemon] output:",
		"daemon package x",
		"[agent-repl-tests] ERROR: unit daemon:internal/x [daemon] FAILED with exit code 1 after 2.000s",
		"[agent-repl-tests] ERROR: daemon failed after 2.000s with exit code 1",
		"[agent-repl-tests] unit webapp#01 [webapp] output:",
		"webapp chunk one",
		"[agent-repl-tests] unit webapp#01 [webapp] ok, 4.000s wall, 2.000s cpu",
		"[agent-repl-tests] webapp: passed in 4.000s",
		"[agent-repl-tests] ERROR: unit e2e#00 [e2e] NOT RUN: its dependency e2e:build did not pass",
	}, "\n")

	// Act
	sections := splitSuiteOutput(output, []string{"daemon", "webapp", "e2e"})

	// Assert
	for suite, want := range map[string][]string{
		"daemon": {"daemon: starting", "daemon package x", "daemon failed after"},
		"webapp": {"webapp: starting", "webapp chunk zero", "webapp chunk one", "webapp: passed in"},
		"e2e":    {"unit e2e#00 [e2e] NOT RUN"},
	} {
		for _, w := range want {
			if !strings.Contains(sections[suite], w) {
				t.Errorf("the %s section lacks %q:\n%s", suite, w, sections[suite])
			}
		}
	}
	if strings.Contains(sections["daemon"], "webapp chunk") || strings.Contains(sections["webapp"], "daemon package") {
		t.Fatalf("a section carries another suite's block:\ndaemon:\n%s\nwebapp:\n%s", sections["daemon"], sections["webapp"])
	}
}

// TestClampTailStartsOnALineBoundary covers the excerpt the brief carries: it
// starts at a line rather than mid-word.
func TestClampTailStartsOnALineBoundary(t *testing.T) {
	// Arrange: output longer than the clamp.
	output := "first line that is long enough to be cut\nsecond line\nthird line\n"

	// Act.
	tail := clampTail(output, 25)

	// Assert.
	if strings.HasPrefix(tail, "first") {
		t.Fatalf("the tail is %q, want it clamped", tail)
	}
	if strings.Contains(tail, "cut\n") {
		t.Fatalf("the tail is %q, want it to start on a line boundary", tail)
	}
}

// TestClampTailKeepsShortOutputWhole covers the ordinary case: nothing is cut
// when nothing needs to be.
func TestClampTailKeepsShortOutputWhole(t *testing.T) {
	// Arrange: output within the clamp.
	output := "all of it\n"

	// Act.
	tail := clampTail(output, 4000)

	// Assert.
	if tail != output {
		t.Fatalf("the tail is %q, want the whole output", tail)
	}
}
