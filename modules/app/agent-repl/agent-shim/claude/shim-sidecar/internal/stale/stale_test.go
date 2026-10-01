package stale

import (
	"io"
	"os"
	"strings"
	"testing"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

type sliceWriter struct{ lines *[]string }

func (w sliceWriter) Write(p []byte) (int, error) {
	*w.lines = append(*w.lines, string(p))
	return len(p), nil
}

func tracker(t *testing.T, opt Options) (*Tracker, *[]string) {
	t.Helper()
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "stale-test"})
	return New(opt, log), &logs
}

const (
	nowMs   = int64(1_000_000)
	shellMs = int64(30 * 60 * 1000)
	// bootMs is before the fixtures' activity, so the sweep's boot arm is inert
	// in every test that is not about it.
	bootMs = nowMs - int64(500_000)
)

func shellRun(path string, lastActivityMs int64) Work {
	return Work{Path: path, TaskID: "b1", Kind: tail.KindShellSpool, RunActivityID: "call-1", LastActivityMs: lastActivityMs}
}

func TestObserveTracksARun(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})

	// Act.
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Assert.
	if !tr.Open("/private/tmp/b1.output") {
		t.Fatal("an observed run is not being tracked")
	}
}

func TestObserveRejectsARunWithNoFile(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a run with no file was tracked")
		}
	}()

	// Act.
	tr.Observe(Work{TaskID: "b1"}, nowMs)
}

func TestReObserveLearnsTheOwner(t *testing.T) {
	// Arrange: the spawn is observed after the spool is.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Observe(Work{Path: "/private/tmp/b1.output", OwnerAgentID: "agent-1"}, nowMs)
	lost := tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].OwnerAgentID != "agent-1" {
		t.Fatalf("swept %+v, want the owner learned on re-observation", lost)
	}
}

func TestSilentRunIsLost(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonWentSilent {
		t.Fatalf("swept %+v, want one went_silent conclusion", lost)
	}
}

func TestActivityKeepsARunAlive(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act: the file grew just before the window would have expired.
	tr.Activity("/private/tmp/b1.output", nowMs+shellMs-1)
	lost := tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("swept %+v, want a growing file left alone", lost)
	}
}

func TestVanishedRunIsLostAfterGrace(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, "")

	// Act.
	lost := tr.Sweep(bootMs, nowMs+1000, allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonFileVanished {
		t.Fatalf("swept %+v, want one file_vanished conclusion", lost)
	}
}

func TestVanishedRunSurvivesInsideGrace(t *testing.T) {
	// Arrange: the ordinary rename/replace race.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, "")

	// Act.
	lost := tr.Sweep(bootMs, nowMs+999, allSeen)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("swept %+v inside the grace window", lost)
	}
}

func TestAReturningFileClearsTheVanish(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, "")

	// Act.
	tr.Activity("/private/tmp/b1.output", nowMs+500)
	lost := tr.Sweep(bootMs, nowMs+1000, allSeen)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("swept %+v, want the returned file's grace clock cleared", lost)
	}
}

func TestSettledRunIsNeverSwept(t *testing.T) {
	// Arrange: a spool whose EXIT marker the reader actually read.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Settle("/private/tmp/b1.output")
	lost := tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert: LOST is only ever the answer for a run we stopped seeing.
	if len(lost) != 0 {
		t.Fatalf("swept %+v, want a run we saw finish left alone", lost)
	}
}

func TestBootSweepConcludesPreBootRuns(t *testing.T) {
	// Arrange: a file untouched since before the machine booted.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs-10_000), nowMs)

	// Act.
	lost := tr.BootSweep(nowMs, nowMs)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonSweptUp {
		t.Fatalf("swept %+v, want one swept_up conclusion", lost)
	}
}

func TestBootSweepSparesPostBootRuns(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs+10_000), nowMs)

	// Act.
	lost := tr.BootSweep(nowMs, nowMs)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("swept %+v, want a run written since the boot left alone", lost)
	}
}

func TestBootSweepWithoutABootTimeIsLoud(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs-10_000), nowMs)

	// Act.
	lost := tr.BootSweep(0, nowMs)

	// Assert: a sweep that cannot run is a real loss, stated rather than passed over.
	if len(lost) != 0 {
		t.Fatalf("swept %+v without a boot time", lost)
	}
	requireOnceIn(t, parseLogLines(t, *logs), "boot-sweep", "warn")
}

func TestSweepStopsTrackingWhatItConcluded(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert: a run is concluded once, never on every later sweep.
	if tr.Open("/private/tmp/b1.output") {
		t.Fatal("a concluded run is still tracked")
	}
}

// TestASilenceConclusionIsStatedAtInfo pins the reason that is NOT a fault.
//
// The file plane cannot tell a quiet DEAD run from a quiet LIVE one: the
// vendor's launch result carries no pid, the spool carries no heartbeat, and a
// terminator is the only end it ever writes — so a silence window expiring is
// this reader losing sight of a run, exactly as the record's own sentence says,
// and never a fault an operator must act on. The owner's own polling background
// shells produce it on purpose. The conclusion, the wire arm and the terminal
// are unchanged; only the level moves. (A file unlinked under a STANDING
// directory keeps its warn — TestAnUnlinkUnderAStandingDirectoryStillWarnsItsConclusion.)
func TestASilenceConclusionIsStatedAtInfo(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert.
	records := parseLogLines(t, *logs)
	requireNoneIn(t, records, "lost-policy", "warn")
	rec := requireConclusion(t, records)
	if got := ctxString(t, rec, "reason"); got != string(ReasonWentSilent) {
		t.Fatalf("conclusion reason = %q, want the wire arm %q", got, ReasonWentSilent)
	}
	if got := ctxString(t, rec, "task_id"); got != "b1" {
		t.Fatalf("the conclusion's task_id = %q, want b1", got)
	}
}

func TestSilenceWindowIsPerKind(t *testing.T) {
	tests := []struct {
		name    string
		kind    tail.Kind
		elapsed int64
		want    int
	}{
		{name: "shell inside its window", kind: tail.KindShellSpool, elapsed: shellMs - 1, want: 0},
		{name: "shell past its window", kind: tail.KindShellSpool, elapsed: shellMs, want: 1},
		{name: "agent inside the shell window", kind: tail.KindAgentTranscript, elapsed: shellMs, want: 0},
		{name: "agent past its own window", kind: tail.KindAgentTranscript, elapsed: 60 * 60 * 1000, want: 1},
		{name: "workflow past its own window", kind: tail.KindWorkflowJournal, elapsed: 60 * 60 * 1000, want: 1},
		{name: "residue spool on the shell window", kind: tail.KindResidueSpool, elapsed: shellMs, want: 1},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			tr, _ := tracker(t, Options{})
			tr.Observe(Work{Path: "/private/tmp/x.output", TaskID: "x", Kind: tc.kind, LastActivityMs: nowMs}, nowMs)

			// Act.
			lost := tr.Sweep(bootMs, nowMs+tc.elapsed, allSeen)

			// Assert.
			if len(lost) != tc.want {
				t.Fatalf("swept %d run(s), want %d", len(lost), tc.want)
			}
		})
	}
}

func TestSweepOrdersConclusionsByPath(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b2.output", nowMs), nowMs)
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert: a sweep's records must be stable across runs.
	if len(lost) != 2 || lost[0].Path != "/private/tmp/b1.output" {
		t.Fatalf("swept %+v, want path order", lost)
	}
}

func TestActivityForAnUntrackedPathIsANoOp(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})

	// Act.
	tr.Activity("/private/tmp/unknown.output", nowMs)

	// Assert.
	if tr.Open("/private/tmp/unknown.output") {
		t.Fatal("activity on an untracked path started tracking it")
	}
}

func TestWindowsReportsTheDefaultsWhenOptionsAreZero(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})

	// Act.
	got := tr.Windows()

	// Assert.
	want := Options{
		Grace:           DefaultGrace,
		ShellSilence:    DefaultShellSilence,
		AgentSilence:    DefaultAgentSilence,
		WorkflowSilence: DefaultWorkflowSilence,
	}
	if got != want {
		t.Fatalf("windows = %+v, want the package defaults %+v", got, want)
	}
}

func TestWindowsReportsTheOptionsTheCallerChose(t *testing.T) {
	// Arrange: the four windows the sidecar's flags carry, all distinct.
	opt := Options{
		Grace:           10 * time.Millisecond,
		ShellSilence:    20 * time.Millisecond,
		AgentSilence:    30 * time.Millisecond,
		WorkflowSilence: 40 * time.Millisecond,
	}
	tr, _ := tracker(t, opt)

	// Act.
	got := tr.Windows()

	// Assert.
	if got != opt {
		t.Fatalf("windows = %+v, want the caller's %+v", got, opt)
	}
}

func TestSweepConcludesAPreBootRunSweptUp(t *testing.T) {
	// Arrange: a run whose file was last written before the machine booted, and
	// which the boot pass never saw because it was discovered afterwards.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", bootMs-1), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs, allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonSweptUp {
		t.Fatalf("sweep concluded %+v, want one swept_up run", lost)
	}
}

func TestSweepPrefersSweptUpOverWentSilentForAPreBootRun(t *testing.T) {
	// Arrange: the run is both pre-boot and long silent; swept_up says HOW we
	// know, which is the more informative statement.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", bootMs-1), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs+shellMs, allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonSweptUp {
		t.Fatalf("sweep concluded %+v, want swept_up rather than went_silent", lost)
	}
}

func TestSweepLeavesAPostBootRunToItsSilenceWindow(t *testing.T) {
	// Arrange: the file was written after boot and is still inside its window.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs+1, allSeen)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("sweep concluded %+v, want nothing for a post-boot run inside its window", lost)
	}
}

func TestSweepWithoutABootTimeCannotConcludeSweptUp(t *testing.T) {
	// Arrange: no boot time, and a run whose file predates any plausible one.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", 1), nowMs)

	// Act.
	lost := tr.Sweep(0, nowMs+shellMs, allSeen)

	// Assert: it is concluded by its SILENCE, never by a boot rule that has no
	// boot time to apply.
	if len(lost) != 1 || lost[0].Reason != ReasonWentSilent {
		t.Fatalf("sweep concluded %+v, want went_silent when no boot time is known", lost)
	}
}

func TestSweepStatesAnUnknownBootTimeOnce(t *testing.T) {
	// Arrange: the statement is verbose, so verbose emission is on.
	tr, logs := tracker(t, Options{})

	// Act: the sweep runs on a timer, so the statement must not repeat.
	tr.Sweep(0, nowMs, allSeen)
	tr.Sweep(0, nowMs, allSeen)

	// Assert.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "lost-policy", "debug")
	if rec.Verbosity != "verbose" {
		t.Fatalf("the inert-boot-arm statement was recorded at verbosity %q, want verbose", rec.Verbosity)
	}
}

func TestAVanishedRunIsStillJudgedByItsGraceWindowWhenItPredatesBoot(t *testing.T) {
	// Arrange: watching the file disappear is a better statement than the boot
	// rule, so the grace window keeps deciding.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", bootMs-1), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, "")

	// Act.
	lost := tr.Sweep(bootMs, nowMs+DefaultGrace.Milliseconds(), allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonFileVanished {
		t.Fatalf("sweep concluded %+v, want file_vanished", lost)
	}
}

// startMs is the sidecar's process-start boundary for the catch-up subjects: a
// run whose last activity predates it is backlog, one after it is steady state.
// It sits after nowMs so the plain fixtures above (which observe at nowMs) count
// as backlog when a test opts into the boundary, and a steady-state run is one
// stamped past it.
const startMs = nowMs + int64(1000)

// trackerFromStart builds a tracker whose startup catch-up boundary is set, so
// the sweep can tell backlog from a steady-state conclusion.
func trackerFromStart(t *testing.T, opt Options, processStartMs int64) (*Tracker, *[]string) {
	t.Helper()
	tr, logs := tracker(t, opt)
	tr.SetProcessStart(processStartMs)
	return tr, logs
}

func TestStartupCatchUpSummarizesABacklogOfSilentRunsAsOneRecord(t *testing.T) {
	// Arrange: three runs that were already silent before the sidecar started.
	tr, logs := trackerFromStart(t, Options{}, startMs)
	for _, path := range []string{"/private/tmp/b1.output", "/private/tmp/b2.output", "/private/tmp/b3.output"} {
		tr.Observe(Work{Path: path, TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: nowMs - 10_000}, nowMs)
	}

	// Act: the first sweep catches up on the whole backlog at once.
	tr.Sweep(bootMs, startMs+shellMs, allSeen)

	// Assert: one summary at info naming the class and the count, never three
	// per-item warnings.
	records := parseLogLines(t, *logs)
	rec := requireOnceIn(t, records, "catchup-summary", "info")
	if got := ctxString(t, rec, "reason"); got != string(ReasonWentSilent) {
		t.Fatalf("summary reason = %q, want %q", got, ReasonWentSilent)
	}
	if got := ctxInt(t, rec, "repeat_count"); got != 3 {
		t.Fatalf("summary repeat_count = %d, want 3", got)
	}
	if got := len(opsAt(records, "lost-policy", "warn")); got != 0 {
		t.Fatalf("catch-up stated %d per-item warnings, want none (the summary carries the total)", got)
	}
}

func TestStartupCatchUpStatesEachBacklogRunAtDebug(t *testing.T) {
	// Arrange: two backlog runs.
	tr, logs := trackerFromStart(t, Options{}, startMs)
	tr.Observe(Work{Path: "/private/tmp/b1.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: nowMs - 10_000}, nowMs)
	tr.Observe(Work{Path: "/private/tmp/b2.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: nowMs - 10_000}, nowMs)

	// Act.
	tr.Sweep(bootMs, startMs+shellMs, allSeen)

	// Assert: nothing is silenced — each backlog conclusion is still stated, at
	// debug, so the per-file detail is retrievable behind the summary.
	if got := len(opsAt(parseLogLines(t, *logs), "lost-policy", "debug")); got != 2 {
		t.Fatalf("catch-up stated %d per-item debug records, want one per backlog run (2)", got)
	}
}

func TestASteadyStateRunAfterCatchUpIsStatedPerItem(t *testing.T) {
	// Arrange: a run that grew AFTER the sidecar started, then went silent.
	tr, logs := trackerFromStart(t, Options{}, startMs)
	tr.Observe(Work{Path: "/private/tmp/b1.output", TaskID: "b1", Kind: tail.KindShellSpool, RunActivityID: "call-1", LastActivityMs: startMs + 1}, startMs+1)

	// Act.
	tr.Sweep(bootMs, startMs+1+shellMs, allSeen)

	// Assert: a newly-arising conclusion is stated per item, never folded into a
	// catch-up summary.
	records := parseLogLines(t, *logs)
	requireConclusion(t, records)
	if got := len(opsAt(records, "catchup-summary", "")); got != 0 {
		t.Fatalf("a steady-state run produced %d catch-up summaries, want none", got)
	}
}

func TestAnEmptyBacklogEmitsNoCatchUpSummary(t *testing.T) {
	// Arrange: the boundary is set but nothing is being tracked.
	tr, logs := trackerFromStart(t, Options{}, startMs)

	// Act.
	tr.Sweep(bootMs, startMs+shellMs, allSeen)

	// Assert.
	if got := len(opsAt(parseLogLines(t, *logs), "catchup-summary", "")); got != 0 {
		t.Fatalf("an empty backlog emitted %d catch-up summaries, want none", got)
	}
}

func TestStartupCatchUpSummarizesEachClassSeparately(t *testing.T) {
	// Arrange: one backlog run that went silent and one that predates the reboot.
	tr, logs := trackerFromStart(t, Options{}, startMs)
	tr.Observe(Work{Path: "/private/tmp/b1.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: nowMs - 10_000}, nowMs)
	tr.Observe(Work{Path: "/private/tmp/b2.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: bootMs - 1}, nowMs)

	// Act.
	tr.Sweep(bootMs, startMs+shellMs, allSeen)

	// Assert: the arm IS the class, so each gets its own one-line summary.
	records := parseLogLines(t, *logs)
	summaries := map[string]int{}
	for _, r := range opsAt(records, "catchup-summary", "info") {
		summaries[ctxString(t, r, "reason")] = ctxInt(t, r, "repeat_count")
	}
	want := map[string]int{string(ReasonWentSilent): 1, string(ReasonSweptUp): 1}
	for reason, count := range want {
		if summaries[reason] != count {
			t.Fatalf("summary for %q counted %d, want %d; summaries=%v", reason, summaries[reason], count, summaries)
		}
	}
	if len(summaries) != len(want) {
		t.Fatalf("catch-up emitted %d class summaries, want %d: %v", len(summaries), len(want), summaries)
	}
}

func TestABootSweepBacklogIsSummarizedNotStatedPerRun(t *testing.T) {
	// Arrange: two runs whose files predate the machine boot.
	tr, logs := trackerFromStart(t, Options{}, startMs)
	tr.Observe(Work{Path: "/private/tmp/b1.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: bootMs - 1}, nowMs)
	tr.Observe(Work{Path: "/private/tmp/b2.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: bootMs - 1}, nowMs)

	// Act: the boot sweep is itself a catch-up pass.
	tr.BootSweep(bootMs, startMs)

	// Assert.
	records := parseLogLines(t, *logs)
	rec := requireOnceIn(t, records, "catchup-summary", "info")
	if got := ctxString(t, rec, "reason"); got != string(ReasonSweptUp) {
		t.Fatalf("boot-sweep summary reason = %q, want %q", got, ReasonSweptUp)
	}
	if got := ctxInt(t, rec, "repeat_count"); got != 2 {
		t.Fatalf("boot-sweep summary counted %d, want 2", got)
	}
	if got := len(opsAt(records, "lost-policy", "warn")); got != 0 {
		t.Fatalf("the boot-sweep backlog stated %d per-item warnings, want none", got)
	}
}

func TestProcessStartBoundaryIsSetOnlyOnce(t *testing.T) {
	// Arrange: a store bounce re-enters the first cycle and would move the
	// boundary forward, reclassifying runs a prior cycle already summarized.
	tr, _ := tracker(t, Options{})

	// Act.
	tr.SetProcessStart(startMs)
	tr.SetProcessStart(startMs + 1_000_000)

	// Assert: a run stamped just before the FIRST boundary is still backlog.
	tr.Observe(Work{Path: "/private/tmp/b1.output", TaskID: "b", Kind: tail.KindShellSpool, LastActivityMs: startMs - 1}, nowMs)
	if tr.processStartMs != startMs {
		t.Fatalf("process start = %d, want it pinned to the first value %d", tr.processStartMs, startMs)
	}
}

func TestABacklogConclusionIsHandedToTheCallerAsCatchUp(t *testing.T) {
	// Arrange: a run that was already silent before this sidecar started.
	tr, _ := trackerFromStart(t, Options{}, startMs)
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs-10_000), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, startMs+shellMs, allSeen)

	// Assert: the classification rides out, so the TERMINAL's own record can
	// follow it instead of restating summarized backlog as a fresh warning.
	if len(lost) != 1 {
		t.Fatalf("concluded %d run(s), want 1", len(lost))
	}
	if !lost[0].Catchup {
		t.Fatal("a run stale before the process start was not handed out as catch-up")
	}
}

func TestAConclusionReachedWhileWatchingIsNotCatchUp(t *testing.T) {
	// Arrange: a run that was still growing after this sidecar started and only
	// then went quiet.
	tr, _ := trackerFromStart(t, Options{}, startMs)
	tr.Observe(shellRun("/private/tmp/b2.output", startMs+1000), nowMs)

	// Act.
	lost := tr.Sweep(bootMs, startMs+1000+shellMs, allSeen)

	// Assert.
	if len(lost) != 1 {
		t.Fatalf("concluded %d run(s), want 1", len(lost))
	}
	if lost[0].Catchup {
		t.Fatal("a newly-arising conclusion was classified as startup backlog")
	}
}

// --- a file that went with its whole tree ----------------------------------

func TestATreeRemovedVanishStatesTheGraceClockWithoutWarning(t *testing.T) {
	// Arrange: the directory holding the file was removed on purpose.
	tr, logs := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.MarkVanished("/private/tmp/b1.output", nowMs, BenignTreeRemoved)

	// Assert.
	requireNoneIn(t, parseLogLines(t, *logs), "lost-policy", "warn")
}

func TestAnUnlinkUnderAStandingDirectoryStillWarnsTheGraceClock(t *testing.T) {
	// Arrange: the file went, its directory did not.
	tr, logs := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.MarkVanished("/private/tmp/b1.output", nowMs, "")

	// Assert.
	requireOnceIn(t, parseLogLines(t, *logs), "lost-policy", "warn")
}

func TestATreeRemovedConclusionIsStatedAtInfo(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, BenignTreeRemoved)

	// Act.
	tr.Sweep(bootMs, nowMs+1000, allSeen)

	// Assert: no warning anywhere on the path, and the conclusion still joins to
	// its terminal on the wire's own arm.
	records := parseLogLines(t, *logs)
	requireNoneIn(t, records, "lost-policy", "warn")
	rec := requireConclusion(t, records)
	if got := ctxString(t, rec, "reason"); got != string(ReasonFileVanished) {
		t.Fatalf("conclusion reason = %q, want the wire arm %q", got, ReasonFileVanished)
	}
}

func TestATreeRemovedRunIsStillConcludedLostOnTheFileVanishedArm(t *testing.T) {
	// Arrange: the level changes, the conclusion does not.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, BenignTreeRemoved)

	// Act.
	lost := tr.Sweep(bootMs, nowMs+1000, allSeen)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonFileVanished || lost[0].BenignEnd != BenignTreeRemoved {
		t.Fatalf("swept %+v, want one file_vanished conclusion carrying the tree removal", lost)
	}
}

func TestAnUnlinkUnderAStandingDirectoryStillWarnsItsConclusion(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs, "")

	// Act.
	tr.Sweep(bootMs, nowMs+1000, allSeen)

	// Assert: two warnings — the grace clock and the conclusion — is the old
	// behavior, and the conclusion is the one this asserts.
	warnings := opsAt(parseLogLines(t, *logs), "lost-policy", "warn")
	if len(warnings) != 2 || !strings.Contains(warnings[1].Message, "run concluded LOST") {
		t.Fatalf("lost-policy warnings were %v, want the grace clock and then the conclusion", warnings)
	}
}

// requireConclusion answers the ONE lost-policy record that states a
// conclusion. The path writes several informational records for one run (the
// observation, the grace clock), so a level filter alone cannot name it.
func requireConclusion(t *testing.T, records []logRecord) logRecord {
	t.Helper()
	var found []logRecord
	for _, r := range records {
		if r.Operation == "lost-policy" && strings.Contains(r.Message, "run concluded LOST") {
			found = append(found, r)
		}
	}
	if len(found) != 1 {
		t.Fatalf("the log holds %d LOST conclusions, want exactly one; it held %v", len(found), operationLevels(records))
	}
	if found[0].Level != "info" {
		t.Fatalf("the conclusion was recorded at level %q, want info", found[0].Level)
	}
	return found[0]
}

// --- a file that vanished after everything in it was read -------------------

// TestEveryBenignEndStatesItsConclusionWithoutWarning covers the vocabulary the
// tree removal opened: whatever the reason a vanish took nothing with it, the
// grace clock and the conclusion are stated rather than warned.
func TestEveryBenignEndStatesItsConclusionWithoutWarning(t *testing.T) {
	cases := []struct {
		name   string
		benign string
	}{
		{name: "the spool had already written its terminator", benign: BenignEnded},
		{name: "the committed offset was the whole file", benign: BenignFullyRead},
		{name: "the whole directory went with it", benign: BenignTreeRemoved},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			tr, logs := tracker(t, Options{Grace: time.Second})
			tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
			tr.MarkVanished("/private/tmp/b1.output", nowMs, tc.benign)

			// Act.
			lost := tr.Sweep(bootMs, nowMs+1000, allSeen)

			// Assert: no warning anywhere, and the wire arm is untouched.
			requireNoneIn(t, parseLogLines(t, *logs), "lost-policy", "warn")
			if len(lost) != 1 || lost[0].Reason != ReasonFileVanished || lost[0].BenignEnd != tc.benign {
				t.Fatalf("swept %+v, want one file_vanished conclusion carrying %q", lost, tc.benign)
			}
		})
	}
}

// ---- a settled run is never tracked again ----

// requirePanic runs act and fails unless it panics.
func requirePanic(t *testing.T, what string, act func()) {
	t.Helper()
	defer func() {
		if recover() == nil {
			t.Fatalf("%s did not panic", what)
		}
	}()
	act()
}

func TestSettleAnswersWhetherTheRunWasTracked(t *testing.T) {
	tests := []struct {
		name    string
		observe bool
		want    bool
	}{
		{name: "a tracked run", observe: true, want: true},
		{name: "an untracked run", observe: false, want: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			tr, _ := tracker(t, Options{})
			if test.observe {
				tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
			}

			// Act.
			got := tr.Settle("/private/tmp/b1.output")

			// Assert.
			if got != test.want {
				t.Fatalf("Settle = %t, want %t", got, test.want)
			}
		})
	}
}

func TestSettleStatesATrackedRunAtInfo(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Settle("/private/tmp/b1.output")

	// Assert.
	rec := requireSettleRecord(t, parseLogLines(t, *logs), "run settled by a terminal read")
	if rec.Level != "info" {
		t.Fatalf("the settle was recorded at %q, want info", rec.Level)
	}
}

func TestSettleRecordsAnUntrackedRunAsSettled(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})

	// Act.
	tr.Settle("/private/tmp/b1.output")

	// Assert: a watcher rebuilt later in the process never tracks it again.
	if !tr.Settled("/private/tmp/b1.output") {
		t.Fatal("an untracked run that settled is not recorded as settled")
	}
}

func TestObservingASettledRunPanics(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.Settle("/private/tmp/b1.output")

	// Act / Assert.
	requirePanic(t, "observing a settled run", func() {
		tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	})
}

func TestASweepConclusionIsNotASettleUntilTheCallerSettlesIt(t *testing.T) {
	tests := []struct {
		name  string
		sweep func(tr *Tracker)
	}{
		{name: "the silence sweep", sweep: func(tr *Tracker) { tr.Sweep(bootMs, nowMs+shellMs, allSeen) }},
		{name: "the boot sweep", sweep: func(tr *Tracker) { tr.BootSweep(nowMs+1, nowMs) }},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: a conclusion whose LOST write fails must be restated by
			// the next cycle, so the sweep alone does not settle it.
			tr, _ := tracker(t, Options{})
			tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

			// Act.
			test.sweep(tr)

			// Assert.
			if tr.Open("/private/tmp/b1.output") || tr.Settled("/private/tmp/b1.output") {
				t.Fatalf("open=%t settled=%t, want a concluded run untracked and not yet settled",
					tr.Open("/private/tmp/b1.output"), tr.Settled("/private/tmp/b1.output"))
			}
		})
	}
}

func TestObservingAConcludedRunOnceItsLostIsSettledPanics(t *testing.T) {
	// Arrange: the caller made the LOST durable and settled it.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.Sweep(bootMs, nowMs+shellMs, allSeen)
	tr.Settle("/private/tmp/b1.output")

	// Act / Assert.
	requirePanic(t, "observing a run whose LOST was settled", func() {
		tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	})
}

func TestSettledByRecordNeverTracksTheRun(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})

	// Act.
	tr.SettledByRecord(shellRun("/private/tmp/b1.output", nowMs), 42)

	// Assert.
	if tr.Open("/private/tmp/b1.output") || !tr.Settled("/private/tmp/b1.output") {
		t.Fatal("a run the record holds as ended must be settled and untracked")
	}
	if lost := tr.Sweep(bootMs, nowMs+24*shellMs, allSeen); len(lost) != 0 {
		t.Fatalf("swept %+v, want nothing: a settled run is never concluded LOST", lost)
	}
}

func TestSettledByRecordStatesTheRunOnceAtInfo(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{})

	// Act.
	tr.SettledByRecord(shellRun("/private/tmp/b1.output", nowMs), 42)

	// Assert.
	rec := requireSettleRecord(t, parseLogLines(t, *logs), "run already settled per the store")
	if rec.Level != "info" || !strings.Contains(rec.Message, "ended_at_ms=42") {
		t.Fatalf("record = %+v, want one info record naming ended_at_ms=42", rec)
	}
	if got := ctxString(t, rec, "activity_id"); got != "call-1" {
		t.Fatalf("activity_id = %q, want call-1", got)
	}
}

func TestSettledByRecordOfATrackedRunPanics(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act / Assert.
	requirePanic(t, "settling a tracked run by the record", func() {
		tr.SettledByRecord(shellRun("/private/tmp/b1.output", nowMs), 42)
	})
}

// requireSettleRecord finds the one lost-policy record whose message holds
// substring.
func requireSettleRecord(t *testing.T, records []logRecord, substring string) logRecord {
	t.Helper()
	var found []logRecord
	for _, r := range records {
		if r.Operation == "lost-policy" && strings.Contains(r.Message, substring) {
			found = append(found, r)
		}
	}
	if len(found) != 1 {
		t.Fatalf("the log holds %d lost-policy records saying %q, want exactly one; it held %v", len(found), substring, operationLevels(records))
	}
	return found[0]
}

// allSeen is a reader that has seen every byte of every file.
func allSeen(string) bool { return false }

// TestASilentRunWithUnseenBytesIsNotConcluded covers a reader that lagged
// behind the window: the file's unseen bytes may be its own terminator, so it
// is not silent until the reader has seen them.
func TestASilentRunWithUnseenBytesIsNotConcluded(t *testing.T) {
	tests := []struct {
		name          string
		unseen        bool
		wantConcluded bool
	}{
		{"unseen bytes keep the run open", true, false},
		{"a fully seen quiet file is silent", false, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			tr, _ := tracker(t, Options{})
			tr.Observe(shellRun("/spool", nowMs), nowMs)

			// Act
			lost := tr.Sweep(bootMs, nowMs+shellMs, func(string) bool { return tt.unseen })

			// Assert
			if (len(lost) == 1) != tt.wantConcluded {
				t.Fatalf("lost = %+v, want concluded=%v", lost, tt.wantConcluded)
			}
			if tr.Open("/spool") == tt.wantConcluded {
				t.Fatalf("open = %v, want the run open only when not concluded", tr.Open("/spool"))
			}
		})
	}
}
