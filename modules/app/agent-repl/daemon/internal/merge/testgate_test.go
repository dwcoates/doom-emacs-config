package merge

import (
	"strings"
	"testing"

	"google.golang.org/protobuf/proto"

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
		{
			name:  "a declined line",
			line:  "[agent-repl-tests] e2e-emacs: DECLINED after 0.125s — its precondition is not met (exit 77); see its message above",
			suite: "e2e-emacs",
			want:  suiteStateDeclined,
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
	tests := []struct {
		name, output string
		want         suiteState
	}{
		{name: "pass after skip", output: "[agent-repl-tests] daemon: not selected by --suites, skipping\n[agent-repl-tests] daemon: passed in 3s\n", want: suiteStatePassed},
		{name: "skip after pass", output: "[agent-repl-tests] daemon: passed in 3s\n[agent-repl-tests] daemon: not selected by --suites, skipping\n", want: suiteStateSkipped},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			states := parseSuiteStates(tt.output)

			// Assert
			if states["daemon"] != tt.want {
				t.Fatalf("the suite reads %v, want %v", states["daemon"], tt.want)
			}
		})
	}
}

func TestDeclinedSuiteSettlesItsTabRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	output := "sandbox unavailable\n[agent-repl-tests] e2e-emacs: DECLINED after 0.125s — its precondition is not met (exit 77); see its message above\n"

	// Act
	rows, err := h.o.paintSuites([]string{"e2e-emacs"}, output)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(rows) != 1 {
		t.Fatalf("rows = %d, want one", len(rows))
	}
	if _, passed := rows[0].GetState().(*frontendv1.FeedMergeTestSuite_Passed); !passed {
		t.Fatalf("declined suite state = %T, want non-failure terminal", rows[0].GetState())
	}
	var painted string
	for _, span := range rows[0].GetOutput() {
		painted += span.GetText()
	}
	if !strings.Contains(painted, "DECLINED") {
		t.Fatalf("declined suite output = %q", painted)
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

// countsOutput is a run of webapp's three units: one passed, one failed, one
// still to report.
const countsOutput = "[agent-repl-tests] webapp: starting\n" +
	"[agent-repl-tests] webapp: 3 units planned\n" +
	"[agent-repl-tests] unit webapp#00 [webapp] ok, 3.000s wall, 2.000s cpu\n" +
	"[agent-repl-tests] ERROR: unit webapp#01 [webapp] FAILED with exit code 1 after 2.000s\n"

// TestSuiteCountsReadTheRunnersUnitVerdicts covers the counts each suite row
// carries: the planned total and the unit verdicts the runner printed.
func TestSuiteCountsReadTheRunnersUnitVerdicts(t *testing.T) {
	tests := []struct {
		name   string
		output string
		want   *frontendv1.FeedMergeTestCounts
	}{
		{
			name:   "no planned line leaves the counts unknown",
			output: "[agent-repl-tests] webapp: starting\n",
			want:   nil,
		},
		{
			name:   "the planned line alone counts nothing yet",
			output: "[agent-repl-tests] webapp: starting\n[agent-repl-tests] webapp: 3 units planned\n",
			want:   &frontendv1.FeedMergeTestCounts{Total: 3},
		},
		{
			name:   "an ok and a FAILED unit count one each",
			output: countsOutput,
			want:   &frontendv1.FeedMergeTestCounts{Passed: 1, Failed: 1, Total: 3},
		},
		{
			name:   "a unit the runner did not run counts as failed",
			output: "[agent-repl-tests] webapp: 1 units planned\n[agent-repl-tests] ERROR: unit webapp#00 [webapp] NOT RUN: its dependency x did not pass\n",
			want:   &frontendv1.FeedMergeTestCounts{Failed: 1, Total: 1},
		},
		{
			name:   "a declined unit counts as neither",
			output: "[agent-repl-tests] webapp: 1 units planned\n[agent-repl-tests] unit webapp#00 [webapp] declined (exit 77) after 0.100s\n",
			want:   &frontendv1.FeedMergeTestCounts{Total: 1},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			rows, err := h.o.paintSuites([]string{"webapp"}, tc.output)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if got := rows[0].GetCounts(); !proto.Equal(got, tc.want) {
				t.Fatalf("counts = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestARunningSuiteSaysWhatItsTestsHaveSaidSoFar covers the running arm the
// suite's dot is drawn from.
func TestARunningSuiteSaysWhatItsTestsHaveSaidSoFar(t *testing.T) {
	tests := []struct {
		name   string
		output string
		want   string
	}{
		{name: "nothing reported", output: "[agent-repl-tests] webapp: starting\n", want: "unreported"},
		{name: "planned, nothing reported", output: "[agent-repl-tests] webapp: 3 units planned\n", want: "unreported"},
		{name: "every verdict a pass", output: "[agent-repl-tests] webapp: 3 units planned\n[agent-repl-tests] unit webapp#00 [webapp] ok, 1.000s wall, 1.000s cpu\n", want: "passing"},
		{name: "a failure among them", output: countsOutput, want: "failing"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			rows, err := h.o.paintSuites([]string{"webapp"}, tc.output)

			// Assert
			if err != nil {
				t.Fatal(err)
			}
			if got := runningArm(rows[0].GetRunning()); got != tc.want {
				t.Fatalf("running arm = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestTheTestsTabCountsEachUnitAsItsVerdictLands covers the live stream: each
// unit verdict line moves the tab row's counts as the script writes it.
func TestTheTestsTabCountsEachUnitAsItsVerdictLands(t *testing.T) {
	// Arrange
	h := newHarness(t)
	landing(h, 1)
	h.runner.runs = []scriptedRun{{Output: "[agent-repl-tests] daemon: starting\n" +
		"[agent-repl-tests] daemon: 2 units planned\n" +
		"[agent-repl-tests] unit d1 [daemon] ok, 1.000s wall, 1.000s cpu\n" +
		"[agent-repl-tests] unit d2 [daemon] ok, 1.000s wall, 1.000s cpu\n" +
		"[agent-repl-tests] daemon: passed in 2s\n", Code: 0}}

	// Act
	admitted(t, h)

	// Assert
	var passed []uint32
	h.feed.mu.Lock()
	for _, row := range h.feed.rows {
		for _, suite := range row.Row.GetMergeTab().GetTests().GetSuites() {
			if c := suite.GetCounts(); c != nil && (len(passed) == 0 || passed[len(passed)-1] != c.GetPassed()) {
				passed = append(passed, c.GetPassed())
			}
		}
	}
	h.feed.mu.Unlock()
	if want := []uint32{0, 1, 2}; !equalCounts(passed, want) {
		t.Fatalf("passed counts = %v, want %v", passed, want)
	}
}

// runningArm names a running suite's so-far arm.
func runningArm(r *frontendv1.FeedMergeTestSuiteRunning) string {
	switch r.GetSoFar().(type) {
	case *frontendv1.FeedMergeTestSuiteRunning_Unreported:
		return "unreported"
	case *frontendv1.FeedMergeTestSuiteRunning_Passing:
		return "passing"
	case *frontendv1.FeedMergeTestSuiteRunning_Failing:
		return "failing"
	default:
		return "unset"
	}
}

// equalCounts reports whether two count sequences match.
func equalCounts(a, b []uint32) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// unreadableTotal is a planned line whose total overflows an int.
const unreadableTotal = "[agent-repl-tests] webapp: 99999999999999999999999 units planned\n"

func TestAPlannedTotalThatDoesNotReadFailsThePaint(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.o.paintSuites([]string{"webapp"}, unreadableTotal)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "planned unit total") {
		t.Fatalf("err = %v, want the unreadable total named", err)
	}
}

func TestALiveLineThatDoesNotCountIsRecordedAtWarn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	g := &gateRun{r: &run{o: h.o, ws: theWorkspace}, states: map[string]suiteState{}, counts: newSuiteCounts()}

	// Act
	g.edge(strings.TrimSuffix(unreadableTotal, "\n"))

	// Assert
	warned := false
	for _, rec := range h.logs.Records() {
		if rec.Operation == "daemon.merge.tests" && rec.Level == "warn" {
			warned = true
		}
	}
	if !warned {
		t.Fatal("no daemon.merge.tests WARN records the line that could not be counted")
	}
}
