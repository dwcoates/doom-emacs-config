package main

import (
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
)

func TestCycleBeginsOnlyAfterASuccessfulCursorRead(t *testing.T) {
	// Arrange: a store that refuses cursor recovery.
	h := newHarness(t, &fakeStore{cursorsFail: "database is locked"})
	h.transcript(t, "sess-1", promptLine)

	// Act.
	h.sc.attempt()

	// Assert: production may not begin without the store's own answer.
	if h.sc.cursors != nil {
		t.Fatal("production began on a refused cursor read")
	}
}

func TestSuspendedCycleReadsNothing(t *testing.T) {
	// Arrange: no store is listening at all.
	h := newHarness(t, nil)
	h.transcript(t, "sess-1", promptLine)

	// Act.
	h.sc.attempt()
	h.sc.producing(h.sc.pollAll)

	// Assert.
	if len(h.sc.watchers) != 0 {
		t.Fatalf("watching %d file(s) with production suspended", len(h.sc.watchers))
	}
}

func TestSuspensionIsStatedOnce(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: two failing writes in one pass surface the same dead store twice.
	store.writeFail = "transaction rolled back"
	h.sc.suspend("first", nil)
	h.sc.suspend("second", nil)

	// Assert.
	if got := strings.Count(h.logText(), "production suspended"); got != 1 {
		t.Fatalf("suspension stated %d times, want once", got)
	}
}

func TestBeginCycleStartsReading(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-1", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: recovery is followed by a rescan, in that order.
	if _, ok := h.sc.watchers[path]; !ok {
		t.Fatalf("the transcript is not being watched; watchers=%v", h.sc.watchers)
	}
}

func TestTailerResumesFromTheStoresCursor(t *testing.T) {
	// Arrange: two complete turns, the store's cursor at the end of the file.
	h := newHarness(t, &fakeStore{})
	turn := promptLine + "\n" + assistantLine + "\n"
	path := h.transcript(t, "sess-1", promptLine, assistantLine, promptLine, assistantLine)
	h.store.cursors = []*storev1.CursorState{{FileId: "1:2", Path: path, Offset: int64(2 * len(turn))}}

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: the position came from the store (rewound to the last turn start),
	// never from zero.
	if got := h.sc.watchers[path].tailer.Offset(); got != int64(len(turn)) {
		t.Fatalf("resumed offset = %d, want %d (the last turn start below the store's cursor)", got, int64(len(turn)))
	}
}

func TestAFileTheStoreHoldsNoCursorForStartsAtZero(t *testing.T) {
	// Arrange: the honest backfill path — a REACHED store that genuinely holds
	// no cursor for a newly discovered transcript.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-1", promptLine)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if got := h.sc.watchers[path].tailer.Offset(); got != 0 {
		t.Fatalf("offset = %d, want 0 for a file with no stored cursor", got)
	}
}

func TestTheBootRewindHappensOncePerFile(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	turn := promptLine + "\n" + assistantLine + "\n"
	path := h.transcript(t, "sess-1", promptLine, assistantLine, promptLine, assistantLine)
	h.store.cursors = []*storev1.CursorState{{FileId: "1:2", Path: path, Offset: int64(2 * len(turn))}}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: a reconnect rebuilds the tailer from the same stored cursor.
	h.sc.suspend("test", nil)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("second beginCycle: %v", err)
	}

	// Assert: one bounded backward scan per file per boot, not per reconnect.
	if got := strings.Count(h.logText(), "rewound the restored cursor"); got != 1 {
		t.Fatalf("rewind ran %d times, want once per file per boot", got)
	}
}

func TestRescanWithoutCursorsPanics(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	defer func() {
		// Assert: a tailer built without a recovered cursor starts at 0, and
		// that silent cold start is what the whole cycle exists to prevent.
		if recover() == nil {
			t.Fatal("rescan ran with production suspended")
		}
	}()

	// Act.
	h.sc.rescan()
}

func TestWriteFailureSuspendsProduction(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "transaction rolled back"

	// Act.
	h.sc.pollAll()

	// Assert.
	if h.sc.cursors != nil {
		t.Fatal("a refused batch left production running")
	}
}

func TestWriteFailureLeavesTheCursorUnchanged(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	tailer := h.sc.watchers[path].tailer
	store.writeFail = "transaction rolled back"

	// Act.
	h.sc.pollAll()

	// Assert: nothing was committed, so the reader must re-read the same bytes.
	if got := tailer.Offset(); got != 0 {
		t.Fatalf("committed offset = %d, want 0 after a failed write", got)
	}
}

func TestWriteFailureAbandonsTheRestOfThePass(t *testing.T) {
	// Arrange: two files, one failing write.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	h.transcript(t, "sess-2", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "transaction rolled back"
	store.writeCalls = 0

	// Act.
	h.sc.pollAll()

	// Assert: reading the second file would mean reading with nowhere to put it.
	if store.writeCalls != 1 {
		t.Fatalf("write calls = %d, want 1 (the pass must stop at the first failure)", store.writeCalls)
	}
}

func TestSuccessfulWriteCommitsTheCursor(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	path := h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.pollAll()

	// Assert.
	if got := h.sc.watchers[path].tailer.Offset(); got != int64(len(promptLine)+1) {
		t.Fatalf("committed offset = %d, want the whole line", got)
	}
}

func TestWriteCarriesTheCursorAdvance(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.pollAll()

	// Assert: the position must become durable in the same transaction.
	if len(store.writes) != 1 || store.writes[0].GetCursorAdvance() == nil {
		t.Fatalf("batch = %+v, want one batch carrying its cursor advance", store.writes)
	}
}

func TestSuspensionDropsEveryTailer(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.suspend("test", nil)

	// Assert: a tailer that outlived its cycle would resume from a position the
	// next cycle's store never handed us.
	if len(h.sc.watchers) != 0 {
		t.Fatalf("%d tailer(s) survived the suspension", len(h.sc.watchers))
	}
}

func TestRecoveryReReadsCursorsThenRescans(t *testing.T) {
	// Arrange: a cycle that has been suspended.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.suspend("test", nil)
	store.cursorsCalls = 0

	// Act.
	h.sc.attempt()

	// Assert.
	if store.cursorsCalls != 1 {
		t.Fatalf("cursor reads on recovery = %d, want 1", store.cursorsCalls)
	}
	if len(h.sc.watchers) != 1 {
		t.Fatalf("watchers after recovery = %d, want the rescan to have rebuilt them", len(h.sc.watchers))
	}
}

func TestFailedAttemptArmsABackoff(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()

	// Assert.
	if !h.sc.nextAttemptAt.After(h.clock) {
		t.Fatalf("next attempt at %s, want it armed after the failure", h.sc.nextAttemptAt)
	}
}

func TestBackoffIsBoundedAndClimbs(t *testing.T) {
	tests := []struct {
		name string
		from time.Duration
		want time.Duration
	}{
		{name: "first failure", from: 0, want: recoverBackoffMin},
		{name: "doubling", from: recoverBackoffMin, want: 2 * recoverBackoffMin},
		{name: "at the ceiling", from: recoverBackoffMax, want: recoverBackoffMax},
		{name: "past the ceiling", from: 2 * recoverBackoffMax, want: recoverBackoffMax},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := nextBackoff(tc.from)

			// Assert.
			if got != tc.want {
				t.Fatalf("nextBackoff(%s) = %s, want %s", tc.from, got, tc.want)
			}
		})
	}
}

func TestAttemptIsNotDueBeforeItsDeadline(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.sc.nextAttemptAt = h.clock.Add(time.Second)

	// Act.
	h.sc.attemptDue()

	// Assert.
	if h.store.cursorsCalls != 0 {
		t.Fatalf("cursor reads = %d, want none before the armed deadline", h.store.cursorsCalls)
	}
}

func TestAttemptIsDueAtItsDeadline(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.sc.nextAttemptAt = h.clock.Add(time.Second)
	h.advance(time.Second)

	// Act.
	h.sc.attemptDue()

	// Assert.
	if h.store.cursorsCalls != 1 {
		t.Fatalf("cursor reads = %d, want 1 once the deadline passed", h.store.cursorsCalls)
	}
}

func TestARunningCycleDoesNotReattempt(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.store.cursorsCalls = 0

	// Act.
	h.sc.attemptDue()

	// Assert.
	if h.store.cursorsCalls != 0 {
		t.Fatalf("cursor reads = %d, want none while production is live", h.store.cursorsCalls)
	}
}

func TestResumeReportsTheOutage(t *testing.T) {
	// Arrange: one failed attempt, then a reachable store.
	h := newHarness(t, &fakeStore{})
	h.sc.attempts = 2
	h.sc.suspendedSince = h.clock.Add(-5 * time.Second)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if !strings.Contains(h.logText(), "production resumed") {
		t.Fatalf("the outage window was never closed in the log; got %s", h.logText())
	}
}

func TestAFirstAttemptCycleReportsNoOutage(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert.
	if strings.Contains(h.logText(), "production resumed") {
		t.Fatalf("a cycle that began immediately reported an outage; got %s", h.logText())
	}
}

func TestNoHeartbeatPathRemains(t *testing.T) {
	// Arrange: the store declares no health verb by design, so nothing may probe
	// one. This is the executable form of that ruling.
	h := newHarness(t, &fakeStore{})

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Assert.
	for _, retired := range []string{"heartbeat", "health"} {
		if strings.Contains(strings.ToLower(h.logText()), retired) {
			t.Fatalf("the retired %q path is still exercised; got %s", retired, h.logText())
		}
	}
}

// TestTheConfiguredLostWindowsReachTheTracker asserts the flag wiring lands:
// a window that never reaches internal/stale is a flag that does nothing.
func TestTheConfiguredLostWindowsReachTheTracker(t *testing.T) {
	// Arrange: windows no production default could be confused with.
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "sidecar-test"})
	options := Options{
		StoreSocket: filepath.Join(os.TempDir(), "ar-unused.sock"),
		Stale: stale.Options{
			Grace:           11 * time.Millisecond,
			ShellSilence:    22 * time.Millisecond,
			AgentSilence:    33 * time.Millisecond,
			WorkflowSilence: 44 * time.Millisecond,
		},
	}

	// Act.
	sc := newSidecar(options, log)

	// Assert.
	if got := sc.tracker.Windows(); got != options.Stale {
		t.Fatalf("the tracker runs with %+v, want the configured %+v", got, options.Stale)
	}
}

// TestTheConfiguredHoldWindowReachesTheHeldIndex asserts the same wiring for the
// unclaimed-spool wait, which is the other wall-clock window a caller sits out.
func TestTheConfiguredHoldWindowReachesTheHeldIndex(t *testing.T) {
	// Arrange.
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "sidecar-test"})
	options := Options{
		StoreSocket:        filepath.Join(os.TempDir(), "ar-unused.sock"),
		UnownedSpoolWindow: 12 * time.Millisecond,
	}

	// Act.
	sc := newSidecar(options, log)

	// Assert.
	if sc.held.window != options.UnownedSpoolWindow {
		t.Fatalf("the held index runs with %s, want the configured %s", sc.held.window, options.UnownedSpoolWindow)
	}
}

func TestFileActivityMsReportsWhenTheFileLastGrew(t *testing.T) {
	// Arrange: our own read is not activity — the file's mtime is.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")

	// Act.
	got := fileActivityMs(spool, h.clock.Add(time.Hour).UnixMilli())

	// Assert.
	if got != h.clock.UnixMilli() {
		t.Fatalf("activity = %d, want the file's mtime %d rather than the caller's clock", got, h.clock.UnixMilli())
	}
}

func TestFileActivityMsFallsBackWhenTheFileCannotBeStatted(t *testing.T) {
	// Arrange.
	fallback := int64(1234)

	// Act.
	got := fileActivityMs(filepath.Join(os.TempDir(), "ar-no-such-file.output"), fallback)

	// Assert.
	if got != fallback {
		t.Fatalf("activity = %d, want the fallback %d", got, fallback)
	}
}

func TestAVanishedFileKeepsItsTailerSoItsTerminalCanBeSpelled(t *testing.T) {
	// Arrange: a claimed spool being tailed.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "", "")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	if err := os.Remove(spool); err != nil {
		t.Fatalf("remove %s: %v", spool, err)
	}

	// Act.
	h.sc.pollAll()

	// Assert: the handler is the only converter that can spell this run's
	// terminal, so it must outlive the file.
	if _, ok := h.sc.watchers[spool]; !ok {
		t.Fatal("the vanished file's tailer was dropped, so its LOST terminal could never be spelled")
	}
}

func TestAVanishedFileIsStatedOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "", "")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	if err := os.Remove(spool); err != nil {
		t.Fatalf("remove %s: %v", spool, err)
	}

	// Act: the poll loop keeps running while the grace window decides.
	h.sc.pollAll()
	h.sc.pollAll()

	// Assert.
	if got := strings.Count(h.logText(), "the watched file vanished"); got != 1 {
		t.Fatalf("the disappearance is stated %d time(s), want exactly 1", got)
	}
}

func TestAFirstCycleThatNeverBeganStatesTheOutage(t *testing.T) {
	// Arrange. A process that starts with no store is in exactly the outage a
	// process whose store died mid-run is in, and it never makes the live-cycle
	// transition suspend() reports — so without this the boot outage was silent
	// and the file plane stopped with nothing said at normal verbosity.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()

	// Assert.
	if got := strings.Count(h.logText(), "production suspended"); got != 1 {
		t.Fatalf("the boot outage was stated %d times, want exactly once: %s", got, h.logText())
	}
}

func TestRepeatedFailedAttemptsRestateNothing(t *testing.T) {
	// Arrange. The ladder runs forever; a WARNING per attempt would bury the one
	// record that matters.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()
	h.advance(time.Minute)
	h.sc.attemptDue()
	h.advance(time.Minute)
	h.sc.attemptDue()

	// Assert.
	if got := strings.Count(h.logText(), "production suspended"); got != 1 {
		t.Fatalf("the outage was stated %d times across three attempts, want once", got)
	}
}

func TestASecondOutageIsStatedAgain(t *testing.T) {
	// Arrange. "Stated once" is per OUTAGE, not per process: a store that dies,
	// recovers and dies again is two outages and owes two records.
	store := &fakeStore{}
	h := newHarness(t, store)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.suspend("first outage", nil)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("second beginCycle: %v", err)
	}
	h.sc.suspend("second outage", nil)

	// Assert.
	if got := strings.Count(h.logText(), "production suspended"); got != 2 {
		t.Fatalf("two outages were stated %d times, want twice", got)
	}
}

func TestTheSuspensionRecordNamesTheStoreItIsWaitingOn(t *testing.T) {
	// Arrange. The dependency the file plane stopped for is a correlation key,
	// not prose: an operator filters on store_socket to find every reader that
	// lost the same store.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()

	// Assert.
	var found bool
	for _, line := range *h.logs {
		if !strings.Contains(line, "production suspended") {
			continue
		}
		found = true
		if !strings.Contains(line, `"store_socket":"`+h.socket+`"`) {
			t.Fatalf("the suspension record names no store_socket: %s", line)
		}
	}
	if !found {
		t.Fatal("no suspension record was written at all")
	}
}

func TestASpoolThatReadItsOwnExitMarkerIsNoLongerTracked(t *testing.T) {
	// Arrange. LOST means "we stopped seeing it". A run whose EXIT marker we
	// READ is a run we watched finish, so leaving it tracked would have the
	// staleness sweep eventually restate a completed run as LOST.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1settled", "work\nEXIT=0\n")
	h.sc.TaskSpawned("b1settled", "toolu_settled_run", "", spool)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Assert.
	if h.sc.tracker.Open(spool) {
		t.Fatalf("the run is still tracked after its own terminal was read: %s", h.logText())
	}
}

func TestARunIsSettledOnlyOnceItsTerminalIsDurable(t *testing.T) {
	// Arrange. A terminal whose batch the store refused is a terminal the store
	// never saw; untracking the run against it would leave a run that is neither
	// tracked nor recorded as ended.
	store := &fakeStore{writeFail: "the store is refusing everything"}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1refused", "work\nEXIT=0\n")
	h.sc.TaskSpawned("b1refused", "toolu_refused_run", "", spool)

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Assert.
	if !h.sc.tracker.Open(spool) {
		t.Fatal("a run whose terminal was never committed must stay tracked")
	}
}

func TestASettledRunIsNeverConcludedLost(t *testing.T) {
	// Arrange. The sweep is what would restate the finished run, so the subject
	// is the sweep itself, run past every silence window.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1swept", "work\nEXIT=0\n")
	h.sc.TaskSpawned("b1swept", "toolu_swept_run", "", spool)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Act: long past any silence window.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert.
	if strings.Contains(h.logText(), "run concluded LOST") {
		t.Fatalf("a run that ended on its own EXIT marker was restated LOST: %s", h.logText())
	}
}
