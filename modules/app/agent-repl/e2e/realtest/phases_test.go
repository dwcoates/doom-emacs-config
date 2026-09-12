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

// phaseFixture writes one global Emacs sink and returns the source, a snapshot
// taken while the file did not yet exist, and the fixture spawn time. The
// snapshot precedes the write on purpose: the run's snapshot is always taken
// before the cold start writes anything, so a zero offset here reads the whole
// fixture exactly as a real cold start reads its own fresh records.
func phaseFixture(t *testing.T, lines ...string) (Source, Snapshot, time.Time) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "doom-agent-repl.log")
	src := Source{Name: "emacs.global", Path: path, Kind: KindJSONL}
	snap := TakeSnapshot([]Source{src})
	writeLines(t, path, lines...)
	spawned, err := time.Parse(time.RFC3339Nano, "2026-09-10T12:00:00.000000-04:00")
	if err != nil {
		t.Fatalf("parse the fixture spawn time: %v", err)
	}
	return src, snap, spawned
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
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:04.500000-04:00", "debug", "elisp.core.loaded", ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
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
	src, snap, spawned := phaseFixture(t)

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
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
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:03.000000-04:00", "info", "elisp.daemon.ensure-scheduled idle=0", ""),
		elisp("2026-09-10T12:00:03.400000-04:00", "info", `elisp.daemon.adopted address="127.0.0.1:1234" health=healthy`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if phases.DaemonPath != "adopted" {
		t.Errorf("the daemon path reads %q, want \"adopted\"", phases.DaemonPath)
	}
	got, ok := measurementFor(phases.Measure(), PhaseDaemonSpawned, GlobalWorkspace)
	if !ok || got.Elapsed != 3400*time.Millisecond {
		t.Errorf("%s measured %+v, want 3.4s", PhaseDaemonSpawned, got)
	}
}

func TestPhasesReadTheDaemonSpawnPath(t *testing.T) {
	// Arrange: a spawned daemon is different work from an adopted one, and
	// comparing their times would be comparing different things.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:09.000000-04:00", "info", `elisp.daemon.started argv=("claude-repld") state-dir="/s"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if phases.DaemonPath != "spawned" {
		t.Errorf("the daemon path reads %q, want \"spawned\"", phases.DaemonPath)
	}
	got, ok := measurementFor(phases.Measure(), PhaseDaemonSpawned, GlobalWorkspace)
	if !ok || got.Elapsed != 9*time.Second {
		t.Errorf("%s measured %+v, want 9s", PhaseDaemonSpawned, got)
	}
}

func TestPhasesDoNotCreditAnUnhealthyAdoptionToTheDaemonSpawnedPhase(t *testing.T) {
	// Arrange: `elisp.daemon.adopted-unhealthy` is the OPPOSITE of the phase a
	// word-boundary match would credit it to.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:03.000000-04:00", "warn", `elisp.daemon.adopted-unhealthy address="a"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if _, ok := measurementFor(phases.Measure(), PhaseDaemonSpawned, GlobalWorkspace); ok {
		t.Errorf("an unhealthy adoption was measured as %s", PhaseDaemonSpawned)
	}
}

func TestPhasesEndDaemonAnsweredAtTheLaterOfLinkUpAndTheRosterSubscription(t *testing.T) {
	// Arrange: a link with no roster has nothing to draw, so the phase ends at
	// the subscription when that is the later of the two.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:10.000000-04:00", "info", `elisp.link.reconnected address="a"`, ""),
		elisp("2026-09-10T12:00:13.500000-04:00", "info", `elisp.roster.subscribed method="push" address="a"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseDaemonAnswered, GlobalWorkspace)
	if !ok || got.Elapsed != 13500*time.Millisecond {
		t.Errorf("%s measured %+v, want 13.5s", PhaseDaemonAnswered, got)
	}
}

func TestPhasesDoNotEndDaemonAnsweredOnABootClaim(t *testing.T) {
	// Arrange: realtest 1 run 1 wrote `elisp.daemon.booted` three milliseconds
	// after the spawn against a stale address file, while the link came up ten
	// seconds later. A phase that ends here measures the claim.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:00.003000-04:00", "info", `elisp.daemon.booted address="127.0.0.1:1234"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseDaemonAnswered, GlobalWorkspace)
	if !ok {
		t.Fatalf("no %s measurement at all", PhaseDaemonAnswered)
	}
	if got.Note == "" {
		t.Errorf("%s reported %s off a boot claim alone", PhaseDaemonAnswered, got.Elapsed)
	}
}

func TestPhasesSayWhichHalfOfDaemonAnsweredIsMissing(t *testing.T) {
	// Arrange: a link and no roster subscription.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:10.000000-04:00", "info", `elisp.link.up address="a"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, _ := measurementFor(phases.Measure(), PhaseDaemonAnswered, GlobalWorkspace)
	if !contains(got.Note, "roster subscription was never accepted") {
		t.Errorf("the note reads %q, and does not say which half is missing", got.Note)
	}
}

func TestPhasesMeasureLinkUpFromAHostLinkUpRecord(t *testing.T) {
	// Arrange: `elisp.host.link-up-skipped` must not be credited as a link
	// coming up, and `elisp.host.link-up` must be.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:04.000000-04:00", "warn", `elisp.host.link-up-skipped ws=none`, ""),
		elisp("2026-09-10T12:00:11.000000-04:00", "info", `elisp.host.link-up workspaces=3 selection="a"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace)
	if !ok || got.Elapsed != 11*time.Second {
		t.Errorf("%s measured %+v, want 11s from the link-up record", PhaseLinkUp, got)
	}
}

func TestPhasesMeasureATabPerWorkspace(t *testing.T) {
	// Arrange: two workspaces, each with its own tab-open record.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", `"workspace_id":"aaaa"`),
		elisp("2026-09-10T12:00:07.500000-04:00", "info", "elisp.roster.tab-open: ws=two id=bbbb dir=/tmp/two", `"workspace_id":"bbbb"`))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
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
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
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
	// Arrange: the harness's own hidden-to-shown wait (spawn to focus-edge) is
	// deliberately huge here, so the test fails if PhasePanelPainted's elapsed
	// leaks any of it. The panel's real load happens a short interval AFTER
	// the focus edge.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", `"workspace_id":"aaaa"`),
		elisp("2026-09-10T12:00:40.000000-04:00", "info", "elisp.webview-recovery.precreate-drained-on-focus queued=1 reason=focus-edge", ""),
		elisp("2026-09-10T12:00:40.400000-04:00", "debug", "elisp.frontend.watch-load: load-changed ws=one", `"workspace_id":"aaaa"`))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert: the panel's elapsed is the INTRINSIC cost, focus-edge to load
	// (400ms), never spawn to load (34.4s) — the owner ruling this file is
	// named for is precisely that the ~34s hidden-window wait above must
	// never be reported as latency.
	got, ok := measurementFor(phases.Measure(), PhasePanelPainted, "aaaa")
	if !ok || got.Elapsed != 400*time.Millisecond {
		t.Errorf("workspace aaaa's panel measured %+v, want 400ms (focus-edge to load), not spawn to load", got)
	}
	if painted := phases.PaintedWorkspaces(); len(painted) != 1 || painted[0] != "aaaa" {
		t.Errorf("the painted workspaces are %v, want [aaaa]", painted)
	}
}

func TestPhasesReportPanelPaintedAsNotObservedWithNoFocusEdge(t *testing.T) {
	// Arrange: a load-changed record with no focus-edge marker in the run —
	// the panel's intrinsic cost has nothing to be measured from.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:11.000000-04:00", "debug", "elisp.frontend.watch-load: load-changed ws=one", `"workspace_id":"aaaa"`))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert: no elapsed is reported at all, spawn-based or otherwise.
	got, ok := measurementFor(phases.Measure(), PhasePanelPainted, "aaaa")
	if !ok {
		t.Fatalf("expected a panel-painted measurement (as a not-observed note), got none")
	}
	if got.Elapsed != 0 || got.Note == "" {
		t.Errorf("workspace aaaa's panel measured %+v, want Elapsed 0 and a not-observed note", got)
	}
}

func TestPhasesMeasureTheFocusEdgeFromSpawn(t *testing.T) {
	// Arrange: the harness bringing Emacs forward for the key self-test is a
	// real observed edge in its own right, and it is measured from spawn like
	// every phase except PhasePanelPainted.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:34.000000-04:00", "info", "elisp.webview-recovery.precreate-drained-on-focus queued=1 reason=focus-edge", ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseFocusEdge, GlobalWorkspace)
	if !ok || got.Elapsed != 34*time.Second {
		t.Errorf("%s measured %+v, want 34s from spawn", PhaseFocusEdge, got)
	}
}

func TestPhasesTakeTheFirstOccurrenceOfAOncePerRunMarker(t *testing.T) {
	// Arrange: a link that comes up, drops and comes back reports
	// `elisp.link.up` twice. The startup is the first one; the second is a
	// reconnect.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="a"`, ""),
		elisp("2026-09-10T12:00:40.000000-04:00", "info", `elisp.link.up address="a"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
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
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-09T23:00:00.000000-04:00", "info", `elisp.link.up address="stale"`, ""),
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="fresh"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace)
	if !ok || got.Elapsed != 5*time.Second {
		t.Errorf("%s measured %+v, want only the record after the spawn", PhaseLinkUp, got)
	}
}

func TestPhasesReadPerWorkspaceMarkersFromTheWorkspaceSink(t *testing.T) {
	// Arrange: this is the run-3 defect. The workspace-owned tab-open and
	// panel-painted records land in the workspace's own emacs.log sink, not in
	// the global module log, so a phase reader that opened only the global log
	// would report a startup that drew every tab as "no tab drawn".
	dir := t.TempDir()
	global := filepath.Join(dir, "doom-agent-repl.log")
	globalSrc := Source{Name: "emacs.global", Path: global, Kind: KindJSONL}
	link := filepath.Join(dir, "ws", ".claude", "emacs", "emacs.log")
	target := filepath.Join(dir, "ws-target.log")
	wsSrc := Source{Name: "workspace.emacs.log", Path: link, Kind: KindJSONL, Workspace: "aaaa"}
	sources := []Source{globalSrc, wsSrc}
	snap := TakeSnapshot(sources)

	writeLines(t, global,
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="a"`, ""))
	writeLines(t, target,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", `"workspace_id":"aaaa"`),
		elisp("2026-09-10T12:00:07.000000-04:00", "debug", "elisp.frontend.watch-load: load-changed ws=one", `"workspace_id":"aaaa"`))
	if err := os.MkdirAll(filepath.Dir(link), 0o755); err != nil {
		t.Fatalf("create the workspace sink directory: %v", err)
	}
	if err := os.Symlink(target, link); err != nil {
		t.Fatalf("install the workspace sink link: %v", err)
	}
	spawned, err := time.Parse(time.RFC3339Nano, "2026-09-10T12:00:00.000000-04:00")
	if err != nil {
		t.Fatalf("parse the spawn time: %v", err)
	}

	// Act.
	phases, err := ReadPhases(sources, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert: the marker in the global log is read, and so are the two in the
	// workspace sink.
	if _, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace); !ok {
		t.Errorf("the global link-up marker was not read")
	}
	if drawn := phases.DrawnWorkspaces(); len(drawn) != 1 || drawn[0] != "aaaa" {
		t.Errorf("the drawn workspaces are %v, want [aaaa] read from the workspace sink", drawn)
	}
	if painted := phases.PaintedWorkspaces(); len(painted) != 1 || painted[0] != "aaaa" {
		t.Errorf("the painted workspaces are %v, want [aaaa] read from the workspace sink", painted)
	}
}

func TestPhasesFollowARelinkedWorkspaceSink(t *testing.T) {
	// Arrange: the workspace sink's canonical link is replaced mid-run, exactly
	// as a new Emacs instance or a cap rotation replaces it, and the tab-open
	// record is in the NEW target. A reader keyed to the snapshot offset of the
	// old target would seek past the end of the new one and see nothing.
	dir := t.TempDir()
	link := filepath.Join(dir, "ws", ".claude", "emacs", "emacs.log")
	first := filepath.Join(dir, "target-1.log")
	second := filepath.Join(dir, "target-2.log")
	writeLines(t, first,
		elisp("2026-09-10T12:00:01.000000-04:00", "info", "elisp.roster.reconcile: tabs=0", ""))
	if err := os.MkdirAll(filepath.Dir(link), 0o755); err != nil {
		t.Fatalf("create the workspace sink directory: %v", err)
	}
	if err := os.Symlink(first, link); err != nil {
		t.Fatalf("install the workspace sink link: %v", err)
	}
	wsSrc := Source{Name: "workspace.emacs.log", Path: link, Kind: KindJSONL, Workspace: "aaaa"}
	sources := []Source{wsSrc}
	snap := TakeSnapshot(sources)

	writeLines(t, second,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", `"workspace_id":"aaaa"`))
	if err := os.Remove(link); err != nil {
		t.Fatalf("remove the old link: %v", err)
	}
	if err := os.Symlink(second, link); err != nil {
		t.Fatalf("install the replaced link: %v", err)
	}
	spawned, err := time.Parse(time.RFC3339Nano, "2026-09-10T12:00:00.000000-04:00")
	if err != nil {
		t.Fatalf("parse the spawn time: %v", err)
	}

	// Act.
	phases, err := ReadPhases(sources, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases across a relink: %v", err)
	}

	// Assert.
	if drawn := phases.DrawnWorkspaces(); len(drawn) != 1 || drawn[0] != "aaaa" {
		t.Errorf("the drawn workspaces are %v, want [aaaa] read from the relinked sink", drawn)
	}
}

func TestPhasesReadFromTheSnapshotOffset(t *testing.T) {
	// Arrange: everything at or before the snapshot offset belongs to a
	// previous run, even when its timestamps are indistinguishable. The
	// snapshot is taken AFTER the previous run's line and BEFORE this run's, so
	// the offset it records is the inode's size at that moment.
	path := filepath.Join(t.TempDir(), "doom-agent-repl.log")
	src := Source{Name: "emacs.global", Path: path, Kind: KindJSONL}
	writeLines(t, path,
		elisp("2026-09-10T12:00:02.000000-04:00", "info", `elisp.link.up address="previous run"`, ""))
	snap := TakeSnapshot([]Source{src})
	appendLines(t, path, elisp("2026-09-10T12:00:08.000000-04:00", "info", `elisp.link.up address="this run"`, ""))
	spawned, err := time.Parse(time.RFC3339Nano, "2026-09-10T12:00:00.000000-04:00")
	if err != nil {
		t.Fatalf("parse the spawn time: %v", err)
	}

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace)
	if !ok || got.Elapsed != 8*time.Second {
		t.Errorf("%s measured %+v, want only the record past the offset", PhaseLinkUp, got)
	}
}

func TestPhasesMeasureTheTotalToTheLastUsableEdge(t *testing.T) {
	// Arrange: startup-usable (PhaseTotal) is the latest of the hidden-window
	// edges, which here is link-up at 8s — tab-drawn at 6s is earlier. The
	// panel-painted record is much later still (a large, deliberately
	// harness-shaped gap, standing in for the hidden-to-shown wait), and it
	// must NOT be able to inflate PhaseTotal: the owner ruling this test
	// guards is precisely that no reported figure equals spawn-to-load.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:06.000000-04:00", "info", "elisp.roster.tab-open: ws=one id=aaaa dir=/tmp/one", ""),
		elisp("2026-09-10T12:00:08.000000-04:00", "info", `elisp.link.up address="a"`, ""),
		elisp("2026-09-10T12:00:40.000000-04:00", "info", "elisp.webview-recovery.precreate-drained-on-focus queued=1 reason=focus-edge", ""),
		elisp("2026-09-10T12:00:40.400000-04:00", "debug", "elisp.frontend.watch-load: load-changed ws=one", `"workspace_id":"aaaa"`))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}
	measurements := phases.Measure()

	// Assert: PhaseTotal is 8s (spawn to link-up, the later of the two
	// usable edges present), not 40.4s (spawn to the panel painting).
	got, ok := measurementFor(measurements, PhaseTotal, GlobalWorkspace)
	if !ok || got.Elapsed != 8*time.Second {
		t.Errorf("%s measured %+v, want 8s (spawn to the latest usable edge), not spawn to the panel paint", PhaseTotal, got)
	}
	for _, m := range measurements {
		if m.Note == "" && m.Elapsed == 40400*time.Millisecond {
			t.Errorf("phase %s (workspace %s) reported %s, the inflated spawn-to-load delta; "+
				"no reported figure may equal spawn to load", m.Phase, m.Workspace, m.Elapsed)
		}
	}

	// And panel-painted itself is the intrinsic 400ms, not part of the total.
	painted, ok := measurementFor(measurements, PhasePanelPainted, "aaaa")
	if !ok || painted.Elapsed != 400*time.Millisecond {
		t.Errorf("workspace aaaa's panel measured %+v, want 400ms", painted)
	}
}

func TestPhasesSkipALineThatIsNotARecord(t *testing.T) {
	// Arrange: the harvester reports the malformed line; the phase reader's
	// job is the timeline, and a line it cannot parse carries no marker.
	src, snap, spawned := phaseFixture(t,
		"not a record at all",
		elisp("2026-09-10T12:00:05.000000-04:00", "info", `elisp.link.up address="a"`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if _, ok := measurementFor(phases.Measure(), PhaseLinkUp, GlobalWorkspace); !ok {
		t.Errorf("a malformed line ahead of the marker lost the marker")
	}
}

func TestPhasesMeasureModuleLoadedFromTheScheduledEnsure(t *testing.T) {
	// Arrange: `elisp.daemon.ensure-scheduled` is what the startup path
	// actually writes — config.el registers agent-repl-daemon-schedule-ensure
	// on emacs-startup-hook — so it is what ends this phase.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:02.900000-04:00", "info", "elisp.daemon.ensure-scheduled idle=0", ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseModuleLoaded, GlobalWorkspace)
	if !ok || got.Elapsed != 2900*time.Millisecond {
		t.Errorf("%s measured %+v, want 2.9s from the scheduled ensure", PhaseModuleLoaded, got)
	}
}

func TestPhasesIgnoreTheInteractiveEnsureCommand(t *testing.T) {
	// Arrange: `elisp.daemon.ensure-command` is the record the INTERACTIVE
	// retry writes, never the startup. Crediting it to module-loaded would
	// report a person's keypress as module-load latency.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:41.000000-04:00", "info", "elisp.daemon.ensure-command", ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	if got, ok := measurementFor(phases.Measure(), PhaseModuleLoaded, GlobalWorkspace); ok {
		t.Errorf("the interactive ensure was credited to %s as %+v", PhaseModuleLoaded, got)
	}
}

func TestPhasesMeasureFirstRosterFromTheReconcileRecord(t *testing.T) {
	// Arrange: the reconcile record is the end of the first-roster phase, and
	// it is emitted at INFO by roster.el so it reaches the durable sink at the
	// default log level.
	src, snap, spawned := phaseFixture(t,
		elisp("2026-09-10T12:00:03.250000-04:00", "info", `elisp.roster.reconcile: tabs=2 order=("one" "two")`, ""))

	// Act.
	phases, err := ReadPhases([]Source{src}, snap, spawned)
	if err != nil {
		t.Fatalf("read the phases: %v", err)
	}

	// Assert.
	got, ok := measurementFor(phases.Measure(), PhaseFirstRoster, GlobalWorkspace)
	if !ok || got.Elapsed != 3250*time.Millisecond {
		t.Errorf("%s measured %+v, want 3.25s from the reconcile record", PhaseFirstRoster, got)
	}
}

func TestBudgetsCarryNoUnmeasuredRow(t *testing.T) {
	// Arrange/Act: a run refuses to start while any row still carries the
	// sentinel, so an unmeasured row blocks the whole realtest section.

	// Assert.
	if left := UnmeasuredBudgets(); len(left) > 0 {
		t.Errorf("these phases still have no measured budget and will block every run: %v", left)
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
