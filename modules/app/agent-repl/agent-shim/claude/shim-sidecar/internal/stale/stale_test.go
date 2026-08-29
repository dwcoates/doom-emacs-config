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
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
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
	lost := tr.Sweep(bootMs, nowMs + shellMs)

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
	lost := tr.Sweep(bootMs, nowMs + shellMs)

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
	lost := tr.Sweep(bootMs, nowMs + shellMs)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("swept %+v, want a growing file left alone", lost)
	}
}

func TestVanishedRunIsLostAfterGrace(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs + 1000)

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonFileVanished {
		t.Fatalf("swept %+v, want one file_vanished conclusion", lost)
	}
}

func TestVanishedRunSurvivesInsideGrace(t *testing.T) {
	// Arrange: the ordinary rename/replace race.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs + 999)

	// Assert.
	if len(lost) != 0 {
		t.Fatalf("swept %+v inside the grace window", lost)
	}
}

func TestAReturningFileClearsTheVanish(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{Grace: time.Second})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs)

	// Act.
	tr.Activity("/private/tmp/b1.output", nowMs+500)
	lost := tr.Sweep(bootMs, nowMs + 1000)

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
	lost := tr.Sweep(bootMs, nowMs + shellMs)

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
	if !strings.Contains(strings.Join(*logs, "\n"), "boot time unavailable") {
		t.Fatalf("an unrunnable boot sweep was silent; got %v", *logs)
	}
}

func TestSweepStopsTrackingWhatItConcluded(t *testing.T) {
	// Arrange.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Sweep(bootMs, nowMs + shellMs)

	// Assert: a run is concluded once, never on every later sweep.
	if tr.Open("/private/tmp/b1.output") {
		t.Fatal("a concluded run is still tracked")
	}
}

func TestConclusionIsLoggedAsAWarning(t *testing.T) {
	// Arrange.
	tr, logs := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", nowMs), nowMs)

	// Act.
	tr.Sweep(bootMs, nowMs + shellMs)

	// Assert.
	joined := strings.Join(*logs, "\n")
	if !strings.Contains(joined, "concluded LOST") || !strings.Contains(joined, `"level":"warn"`) {
		t.Fatalf("the LOST conclusion was not stated loudly; got %v", *logs)
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
			lost := tr.Sweep(bootMs, nowMs + tc.elapsed)

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
	lost := tr.Sweep(bootMs, nowMs + shellMs)

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
	lost := tr.Sweep(bootMs, nowMs)

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
	lost := tr.Sweep(bootMs, nowMs+shellMs)

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
	lost := tr.Sweep(bootMs, nowMs+1)

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
	lost := tr.Sweep(0, nowMs+shellMs)

	// Assert: it is concluded by its SILENCE, never by a boot rule that has no
	// boot time to apply.
	if len(lost) != 1 || lost[0].Reason != ReasonWentSilent {
		t.Fatalf("sweep concluded %+v, want went_silent when no boot time is known", lost)
	}
}

func TestSweepStatesAnUnknownBootTimeOnce(t *testing.T) {
	// Arrange: the statement is verbose, so verbose emission is on.
	t.Setenv("AGENT_REPL_LOG_VERBOSE", "1")
	tr, logs := tracker(t, Options{})

	// Act: the sweep runs on a timer, so the statement must not repeat.
	tr.Sweep(0, nowMs)
	tr.Sweep(0, nowMs)

	// Assert.
	if got := strings.Count(strings.Join(*logs, "\n"), "boot time unavailable"); got != 1 {
		t.Fatalf("the unknown-boot-time statement appears %d time(s), want exactly 1", got)
	}
}

func TestAVanishedRunIsStillJudgedByItsGraceWindowWhenItPredatesBoot(t *testing.T) {
	// Arrange: watching the file disappear is a better statement than the boot
	// rule, so the grace window keeps deciding.
	tr, _ := tracker(t, Options{})
	tr.Observe(shellRun("/private/tmp/b1.output", bootMs-1), nowMs)
	tr.MarkVanished("/private/tmp/b1.output", nowMs)

	// Act.
	lost := tr.Sweep(bootMs, nowMs+DefaultGrace.Milliseconds())

	// Assert.
	if len(lost) != 1 || lost[0].Reason != ReasonFileVanished {
		t.Fatalf("sweep concluded %+v, want file_vanished", lost)
	}
}
