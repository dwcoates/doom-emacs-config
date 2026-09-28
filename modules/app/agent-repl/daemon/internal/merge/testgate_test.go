package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
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

// TestSuiteOutputIsAttributedToTheSuiteItPrecedes covers the split: the script
// runs suites one at a time and announces each terminal, so a line belongs to
// the suite whose terminal comes next.
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

// TestGateArchivesTheWholeRunNotTheTail covers the archive's reason: the tail
// carries whatever the last suites printed, and the diagnosis is further back.
func TestGateArchivesTheWholeRunNotTheTail(t *testing.T) {
	// Arrange: a long run whose failure is far from the end.
	h := newHarness(t)
	output := "the real failure is here\n" + strings.Repeat("coverage table row\n", 500)
	h.runner.runs = append(h.runner.runs, scriptedRun{Output: output, Code: 1})

	// Act.
	result, err := h.o.runGate(context.Background(), "lease-1", 1, h.targetD, h.o.deps.TestCommand(h.targetD),
		SuiteSelection{Suites: []string{"daemon"}})

	// Assert.
	if err != nil {
		t.Fatalf("the gate errored: %v", err)
	}
	archived, err := os.ReadFile(result.ArchivePath)
	if err != nil {
		t.Fatalf("reading the archive: %v", err)
	}
	if !strings.Contains(string(archived), "the real failure is here") {
		t.Fatal("the archive lost the failure the tail could not carry")
	}
	if strings.Contains(result.Tail, "the real failure is here") {
		t.Fatal("the test's premise is wrong: the tail still holds the failure")
	}
}

// TestGateArchiveIsNamedByLeaseAndRound covers the naming: a run is findable
// from the ledger alone.
func TestGateArchiveIsNamedByLeaseAndRound(t *testing.T) {
	// Arrange: a gate run under a known lease and round.
	h := newHarness(t)
	h.runner.runs = append(h.runner.runs, scriptedRun{Code: 0})

	// Act.
	result, err := h.o.runGate(context.Background(), "lease-7", 3, h.targetD, h.o.deps.TestCommand(h.targetD),
		SuiteSelection{Suites: []string{"daemon"}})

	// Assert.
	if err != nil {
		t.Fatalf("the gate errored: %v", err)
	}
	if filepath.Base(result.ArchivePath) != "lease-7-tests-3.log" {
		t.Fatalf("the archive is %q, want it named by lease and round", result.ArchivePath)
	}
}

// TestGateRefusesAnUnknownSuiteName covers the roster-drift guard at the gate
// itself: the script would refuse the name, and a merge failing on that has told
// the user nothing.
func TestGateRefusesAnUnknownSuiteName(t *testing.T) {
	// Arrange: a selection naming a suite the roster does not declare.
	h := newHarness(t)

	// Act.
	_, err := h.o.runGate(context.Background(), "lease-1", 1, h.targetD, h.o.deps.TestCommand(h.targetD),
		SuiteSelection{Suites: []string{"not-a-suite"}})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "not-a-suite") {
		t.Fatalf("the gate accepted an unknown suite, err = %v", err)
	}
}

// TestGateReportsAFailingSuiteAsAVerdict covers the classification: a suite that
// ran and failed is an answer, never an error.
func TestGateReportsAFailingSuiteAsAVerdict(t *testing.T) {
	// Arrange: a failing run.
	h := newHarness(t)
	h.runner.runs = append(h.runner.runs, scriptedRun{
		Output: "[agent-repl-tests] ERROR: daemon failed after 4s with exit code 2\n", Code: 2})

	// Act.
	result, err := h.o.runGate(context.Background(), "lease-1", 1, h.targetD, h.o.deps.TestCommand(h.targetD),
		SuiteSelection{Suites: []string{"daemon"}})

	// Assert.
	if err != nil {
		t.Fatalf("a failing suite was reported as an error: %v", err)
	}
	if result.Passed || result.ExitCode != 2 {
		t.Fatalf("the verdict is %+v, want a failure carrying exit 2", result)
	}
}

// TestGateSurfacesAnUnrunnableScript covers the other classification: a script
// that could not be spawned is an error, because no verdict exists.
func TestGateSurfacesAnUnrunnableScript(t *testing.T) {
	// Arrange: a runner that cannot spawn.
	h := newHarness(t)
	boom := errors.New("no such script")
	h.runner.runs = append(h.runner.runs, scriptedRun{Err: boom})

	// Act.
	_, err := h.o.runGate(context.Background(), "lease-1", 1, h.targetD, h.o.deps.TestCommand(h.targetD),
		SuiteSelection{Suites: []string{"daemon"}})

	// Assert.
	if !errors.Is(err, boom) {
		t.Fatalf("the gate answered %v, want the spawn failure", err)
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
