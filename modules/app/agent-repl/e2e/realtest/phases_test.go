//go:build realtest

package realtest

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

// The phase reader's unit tests. Each one writes a module log under
// t.TempDir(), reads it, and asserts one thing about the timeline.

func phaseFixture(t *testing.T, lines ...string) (string, time.Time) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "doom-agent-repl.log")
	writeLines(t, path, lines...)
	spawned, err := time.Parse(time.RFC3339Nano, "2026-09-10T12:00:00.000000-04:00")
	if err != nil {
		t.Fatalf("parse the fixture spawn time: %v", err)
	}
	return path, spawned
}

func elisp(timestamp, level, message string, extra string) string {
	return rec(timestamp, "emacs", level, "agent-repl.derived", message, extra)
}

func measurementFor(measurements []Measurement, phase PhaseName, workspace string) (Measurement, bool) {
	for _, m := range measurements {
		if m.Phase == phase && m.Workspace == workspace {
			return m, true
		}
	}
	return Measurement{}, false
}

func TestPhasesMeasureDoomBootFromTheFirstModuleRecord(t *testing.T) {
	// Arrange: the module's first record is the earliest evidence the process
	// reached lisp at all.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:04.500000-04:00", "debug", "elisp.core.loaded", ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}
	measurements := phases.Measure()

	// Assert.
	got, ok := measurementFor(measurements, PhaseDoomBoot, GlobalWorkspace)
	if !ok {
		t.Fatalf("no %s measurement: %+v", PhaseDoomBoot, measurements)
	}
	if got.Elapsed != 4500*time.Millisecond {
		t.Errorf("%s took %s from spawn, want 4.5s", PhaseDoomBoot, got.Elapsed)
	}
}

func TestPhasesReportDoomBootAsNotObservedWhenTheModuleWroteNothing(t *testing.T) {
	// Arrange: an empty log after the spawn. A zero elapsed would read as an
	// instantaneous boot, which is the wrong answer to give.
	path, spawned := phaseFixture(t)

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseDoomBoot, GlobalWorkspace)
	if !ok {
		t.Fatalf("no %s measurement at all", PhaseDoomBoot)
	}
	if got.Note == "" {
		t.Errorf("%s reported %s rather than saying it was not observed", PhaseDoomBoot, got.Elapsed)
	}
}

func TestPhasesReadTheDaemonAdoptionPath(t *testing.T) {
	// Arrange.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:03.000000-04:00", "info", "elisp.daemon.ensure-command", ""),
		elisp("2026-09-10T12:00:03.400000-04:00", "info", `elisp.daemon.adopted address="127.0.0.1:1234" health=healthy`, ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if phases.DaemonPath != "adopted" {
		t.Errorf("the daemon path reads %q, want \"adopted\"", phases.DaemonPath)
	}
	got, ok := measurementFor(phases.Measure(), PhaseDaemonAnswered, GlobalWorkspace)
	if !ok || got.Elapsed != 3400*time.Millisecond {
		t.Errorf("%s measured %+v, want 3.4s", PhaseDaemonAnswered, got)
	}
}

func TestPhasesReadTheDaemonBootPath(t *testing.T) {
	// Arrange: a spawned daemon is different work from an adopted one, and
	// comparing their times would be comparing different things.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:09.000000-04:00", "info", `elisp.daemon.booted address="127.0.0.1:1234"`, ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if phases.DaemonPath != "booted" {
		t.Errorf("the daemon path reads %q, want \"booted\"", phases.DaemonPath)
	}
}

func TestPhasesMeasureATabPerWorkspace(t *testing.T) {
	// Arrange: two workspaces, each with its own tab-open record.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", `"workspace_id":"aaaa"`),
		elisp("2026-09-10T12:00:07.500000-04:00", "info", "elisp.roster.tab-open: ws=two id=bbbb dir=/tmp/two", `"workspace_id":"bbbb"`))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}
	measurements := phases.Measure()

	// Assert.
	first, ok := measurementFor(measurements, PhaseTabDrawn, "aaaa")
	if !ok || first.Elapsed != 6*time.Second {
		t.Errorf("workspace aaaa's tab measured %+v, want 6s", first)
	}
	second, ok := measurementFor(measurements, PhaseTabDrawn, "bbbb")
	if !ok || second.Elapsed != 7500*time.Millisecond {
		t.Errorf("workspace bbbb's tab measured %+v, want 7.5s", second)
	}
	drawn := phases.DrawnWorkspaces()
	if len(drawn) != 2 || drawn[0] != "aaaa" || drawn[1] != "bbbb" {
		t.Errorf("the drawn workspaces are %v, want [aaaa bbbb]", drawn)
	}
}

func TestPhasesReadATabsWorkspaceFromTheMessageWhenTheFieldIsAbsent(t *testing.T) {
	// Arrange: tab-open is the one marker that can be written before the
	// workspace's sink exists, so its `id=` token is the fallback.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	drawn := phases.DrawnWorkspaces()
	if len(drawn) != 1 || drawn[0] != "aaaa" {
		t.Errorf("the drawn workspaces are %v, want [aaaa]", drawn)
	}
}

func TestPhasesMeasureAPanelPaintedPerWorkspace(t *testing.T) {
	// Arrange: the webview's own load-finished event, which is the only
	// signal in the startup that comes from the page.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:11.000000-04:00", "debug", "elisp.frontend.watch-load: load-changed ws=one", `"workspace_id":"aaaa"`))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhasePanelPainted, "aaaa")
	if !ok || got.Elapsed != 11*time.Second {
		t.Errorf("workspace aaaa's panel measured %+v, want 11s", got)
	}
	if painted := phases.PaintedWorkspaces(); len(painted) != 1 || painted[0] != "aaaa" {
		t.Errorf("the painted workspaces are %v, want [aaaa]", painted)
	}
}

func TestPhasesTakeTheFirstOccurrenceOfAOncePerRunMarker(t *testing.T) {
	// Arrange: a link that comes up, drops and comes back reports
	// `elisp.link.up` twice. The startup is the first one; the second is a
	// reconnect.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="a"`, ""),
		elisp("2026-09-10T12:00:40.000000-04:00", "info", `elisp.link.up address="a"`, ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace)
	if !ok || got.Elapsed != 5*time.Second {
		t.Errorf("%s measured %+v, want the first occurrence at 5s", PhaseLinkUp, got)
	}
}

func TestPhasesIgnoreRecordsWrittenBeforeTheSpawn(t *testing.T) {
	// Arrange: the outgoing Emacs can write a straggler after the snapshot
	// offset was taken and before this one was spawned.
	path, spawned := phaseFixture(t,
		elisp("2026-09-09T23:00:00.000000-04:00", "info", `elisp.link.up address="stale"`, ""),
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="fresh"`, ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace)
	if !ok || got.Elapsed != 5*time.Second {
		t.Errorf("%s measured %+v, want only the record after the spawn", PhaseLinkUp, got)
	}
}

func TestPhasesReadFromTheSnapshotOffset(t *testing.T) {
	// Arrange: everything before the offset belongs to a previous run, even
	// when its timestamps are indistinguishable.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:02.000000-04:00", "info", `elisp.link.up address="previous run"`, ""))
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat the fixture: %v", err)
	}
	appendLines(t, path, elisp("2026-09-10T12:00:08.000000-04:00", "info", `elisp.link.up address="this run"`, ""))

	// Act.
	phases, err := ReadPhases(path, info.Size(), spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace)
	if !ok || got.Elapsed != 8*time.Second {
		t.Errorf("%s measured %+v, want only the record past the offset", PhaseLinkUp, got)
	}
}

func TestPhasesMeasureTheTotalToTheLastMarker(t *testing.T) {
	// Arrange: usable is when the last thing the startup produces has
	// happened, which here is the second workspace's panel.
	path, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", ""),
		elisp("2026-09-10T12:00:13.250000-04:00", "debug", "elisp.frontend.watch-load: load-changed ws=one", `"workspace_id":"aaaa"`))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseTotal, GlobalWorkspace)
	if !ok || got.Elapsed != 13250*time.Millisecond {
		t.Errorf("%s measured %+v, want 13.25s", PhaseTotal, got)
	}
}

func TestPhasesSkipALineThatIsNotARecord(t *testing.T) {
	// Arrange: the harvester reports the malformed line; the phase reader's
	// job is the timeline, and a line it cannot parse carries no marker.
	path, spawned := phaseFixture(t,
		"not a record at all",
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="a"`, ""))

	// Act.
	phases, err := ReadPhases(path, 0, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if _, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace); !ok {
		t.Errorf("a malformed line ahead of the marker lost the marker")
	}
}

func TestBudgetsAreAllUnmeasuredUntilTheFirstRun(t *testing.T) {
	// Arrange/Act: this is a standing assertion about the table's own state,
	// and it is what makes the two-phase arrangement visible. When the first
	// authorized measurement lands and the numbers are set, THIS TEST is the
	// one that has to be updated, which is the point: the change is deliberate
	// and reviewed rather than incidental.
	unmeasuredPhases := UnmeasuredBudgets()

	// Assert.
	if len(unmeasuredPhases) != len(budgets) {
		t.Logf("some budgets now carry measured values: %d of %d are still unmeasured",
			len(unmeasuredPhases), len(budgets))
	}
	for _, budget := range budgets {
		if budget.Limit != unmeasured && budget.Basis == "" {
			t.Errorf("phase %s carries a budget of %s with no basis recorded; "+
				"a bound without the measurement behind it is a guess (AGENTS.md, \"Test wait/timeout bounds are measured, not guessed\")",
				budget.Phase, budget.Limit)
		}
	}
}

func TestCheckBudgetsNamesThePhaseThatWentOver(t *testing.T) {
	// Arrange: the plan requires that a phase over budget fails NAMING the
	// phase, so the message is asserted, not just the count.
	measurements := []Measurement{
		{Phase: PhaseLinkUp, Workspace: GlobalWorkspace, Elapsed: 30 * time.Second},
	}
	restore := budgets
	budgets = []Budget{{Phase: PhaseLinkUp, Limit: 5 * time.Second, Basis: "fixture"}}
	defer func() { budgets = restore }()

	// Act.
	over := CheckBudgets(measurements)

	// Assert.
	if len(over) != 1 {
		t.Fatalf("CheckBudgets reported %v, want one breach", over)
	}
	if !contains(over[0], string(PhaseLinkUp)) {
		t.Errorf("the breach message %q does not name the phase", over[0])
	}
}

func contains(haystack, needle string) bool {
	return len(haystack) >= len(needle) && (haystack == needle || indexOf(haystack, needle) >= 0)
}

func indexOf(haystack, needle string) int {
	for i := 0; i+len(needle) <= len(haystack); i++ {
		if haystack[i:i+len(needle)] == needle {
			return i
		}
	}
	return -1
}
