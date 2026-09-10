package main

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
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
	h.requireOnce(t, "production-suspended", "warn")
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
	h.store.cursors = []*storev1.CursorState{{FileId: identityOf(t, path), Path: path, Offset: int64(2 * len(turn))}}

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

// identityOf answers a file's own dev:inode — the key the store's cursor row is
// under, and therefore the key a stored cursor must carry to be found again.
//
// A PLACEHOLDER USED TO DO. It worked only while the recovered-cursor index was
// keyed by PATH, which is precisely the defect a vendor rename exposed: the new
// path found no cursor and the whole file was re-read from zero.
func identityOf(t *testing.T, path string) string {
	t.Helper()
	id, err := tail.Identity(path)
	if err != nil {
		t.Fatalf("reading the identity of %s: %v", path, err)
	}
	return id
}

// TestARenamedFileResumesFromTheStoresCursor pins the rename-proof cursor
// identity: the store's row names where the file USED TO BE, and the tailer for
// its new path must still resume from that row rather than from zero.
func TestARenamedFileResumesFromTheStoresCursor(t *testing.T) {
	// Arrange: the file is where it is now; the cursor row still says the old
	// name, which is exactly what the store holds between a rename and the next
	// successful write.
	h := newHarness(t, &fakeStore{})
	turn := promptLine + "\n" + assistantLine + "\n"
	path := h.transcript(t, "sess-1", promptLine, assistantLine, promptLine, assistantLine)
	h.store.cursors = []*storev1.CursorState{{
		FileId: identityOf(t, path),
		Path:   filepath.Join(filepath.Dir(path), "sess-0.jsonl"),
		Offset: int64(2 * len(turn)),
	}}

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: the stored position was found by the file's identity.
	if got := h.sc.watchers[path].tailer.Offset(); got != int64(len(turn)) {
		t.Fatalf("resumed offset = %d, want %d; a renamed file must be found by its file_id, never by the path the cursor row happens to name",
			got, int64(len(turn)))
	}
}

// TestACursorWithNoFileIdIsNotIndexed asserts the index refuses a row that names
// no identity: it keys nothing, so restoring a tailer from it would be restoring
// from a position no file can be matched to.
func TestACursorWithNoFileIdIsNotIndexed(t *testing.T) {
	// Arrange.
	rows := []*storev1.CursorState{
		{FileId: "", Path: "/tmp/nameless.jsonl", Offset: 10},
		{FileId: "1:2", Path: "/tmp/named.jsonl", Offset: 20},
	}

	// Act.
	index := indexCursorsByFileID(rows)

	// Assert.
	if len(index) != 1 {
		t.Fatalf("the index holds %d rows, want only the one naming an identity", len(index))
	}
	if got := index["1:2"].GetOffset(); got != 20 {
		t.Fatalf("the indexed row states offset %d, want 20", got)
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
	h.store.cursors = []*storev1.CursorState{{FileId: identityOf(t, path), Path: path, Offset: int64(2 * len(turn))}}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: a reconnect rebuilds the tailer from the same stored cursor.
	h.sc.suspend("test", nil)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("second beginCycle: %v", err)
	}

	// Assert: one bounded backward scan per file per boot, not per reconnect.
	h.requireOnce(t, "boot-rewind", "info")
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

	// Assert: the cycle's own wholesale recovery, plus ONE per-identity read for
	// the file the snapshot did not name — this store holds no cursor for it, so
	// it is a snapshot MISS, and a miss asks the store rather than assuming zero.
	if store.cursorsCalls != 2 {
		t.Fatalf("cursor reads on recovery = %d, want 2 (the cycle's, then one for the file the snapshot did not name)", store.cursorsCalls)
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
			got := nextBackoff(tc.from, recoverBackoffMin, recoverBackoffMax)

			// Assert.
			if got != tc.want {
				t.Fatalf("nextBackoff(%s) = %s, want %s", tc.from, got, tc.want)
			}
		})
	}
}

// TestBackoffClimbsFromAConfiguredFloor asserts the ladder's first rung is the
// floor it was CONFIGURED with, not the package default.
func TestBackoffClimbsFromAConfiguredFloor(t *testing.T) {
	// Arrange, Act.
	got := nextBackoff(0, 5*time.Millisecond, time.Second)

	// Assert.
	if got != 5*time.Millisecond {
		t.Fatalf("nextBackoff(0, 5ms, 1s) = %s, want the configured floor 5ms", got)
	}
}

// TestBackoffHoldsAConfiguredCeiling asserts the doubling stops at the ceiling
// it was CONFIGURED with, not the package default.
func TestBackoffHoldsAConfiguredCeiling(t *testing.T) {
	// Arrange, Act.
	got := nextBackoff(30*time.Millisecond, 5*time.Millisecond, 40*time.Millisecond)

	// Assert.
	if got != 40*time.Millisecond {
		t.Fatalf("nextBackoff(30ms, 5ms, 40ms) = %s, want the configured ceiling 40ms", got)
	}
}

// TestAnUnsetLadderKeepsThePackageDefaults asserts zero means "unset" for both
// rungs, which is the only meaning zero has anywhere in this process's options.
func TestAnUnsetLadderKeepsThePackageDefaults(t *testing.T) {
	// Arrange, Act.
	min, max := resolveBackoff(0, 0)

	// Assert.
	if min != recoverBackoffMin || max != recoverBackoffMax {
		t.Fatalf("resolveBackoff(0, 0) = (%s, %s), want the package defaults (%s, %s)",
			min, max, recoverBackoffMin, recoverBackoffMax)
	}
}

// TestAConfiguredFloorKeepsTheDefaultCeiling asserts the two rungs are resolved
// independently: setting one must not silently reset the other.
func TestAConfiguredFloorKeepsTheDefaultCeiling(t *testing.T) {
	// Arrange, Act.
	min, max := resolveBackoff(5*time.Millisecond, 0)

	// Assert.
	if min != 5*time.Millisecond || max != recoverBackoffMax {
		t.Fatalf("resolveBackoff(5ms, 0) = (%s, %s), want (5ms, %s)", min, max, recoverBackoffMax)
	}
}

// TestALadderBuiltWithACeilingBelowItsFloorCannotDescend asserts the clamp that
// keeps a ladder assembled in code (main refuses this combination at bootstrap)
// from climbing DOWN from its own first rung.
func TestALadderBuiltWithACeilingBelowItsFloorCannotDescend(t *testing.T) {
	// Arrange, Act.
	min, max := resolveBackoff(time.Second, time.Millisecond)

	// Assert.
	if min != time.Second || max != time.Second {
		t.Fatalf("resolveBackoff(1s, 1ms) = (%s, %s), want the ceiling raised to the floor (1s, 1s)", min, max)
	}
}

// TestTheConfiguredRecoveryLadderReachesTheCycle asserts the same wiring for the
// store-recovery ladder: the resolution happens ONCE, at construction, so
// nothing downstream has to know what zero means.
func TestTheConfiguredRecoveryLadderReachesTheCycle(t *testing.T) {
	// Arrange.
	var logs []string
	log := logging.New(sliceWriter{lines: &logs}, io.Discard).With(logging.Context{Component: "sidecar-test"})
	options := Options{
		StoreSocket:       filepath.Join(os.TempDir(), "ar-unused.sock"),
		RecoverBackoffMin: 3 * time.Millisecond,
		RecoverBackoffMax: 7 * time.Millisecond,
	}

	// Act.
	sc := newSidecar(options, log)

	// Assert.
	if sc.backoffMin != options.RecoverBackoffMin || sc.backoffMax != options.RecoverBackoffMax {
		t.Fatalf("the cycle's ladder is (%s, %s), want the configured (%s, %s)",
			sc.backoffMin, sc.backoffMax, options.RecoverBackoffMin, options.RecoverBackoffMax)
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

	// Assert: the outage window is closed by its own operation, naming the store
	// it waited on and how many attempts it took.
	rec := h.requireOnce(t, "production-resumed", "info")
	if got := ctxString(t, rec, "store_socket"); got != h.socket {
		t.Fatalf("store_socket = %q, want the store the outage was against", got)
	}
	if got, ok := rec.Context["attempt"]; !ok || got.(float64) != 2 {
		t.Fatalf("attempt = %v, want the failed-attempt count the outage accrued", rec.Context["attempt"])
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
	h.requireNone(t, "production-resumed", "")
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
	// The retired path would be its OWN operation and its own rpc, so both
	// vocabularies are checked rather than the prose that would have named it.
	for _, r := range h.records(t) {
		for _, retired := range []string{"heartbeat", "health"} {
			if strings.Contains(strings.ToLower(r.Operation), retired) {
				t.Errorf("the retired %q path is still exercised: operation=%q", retired, r.Operation)
			}
			if rpc, ok := r.Context["rpc"].(string); ok && strings.Contains(strings.ToLower(rpc), retired) {
				t.Errorf("the retired %q verb is still called: rpc=%q", retired, rpc)
			}
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
	h.sc.TaskSpawned("b1", "call-1", "", "", false)
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
	h.sc.TaskSpawned("b1", "call-1", "", "", false)
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
	h.requireOnce(t, "file-vanished", "warn")
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
	h.requireOnce(t, "production-suspended", "warn")
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
	h.requireOnce(t, "production-suspended", "warn")
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
	if got := opsAt(h.records(t), "production-suspended", "warn"); len(got) != 2 {
		t.Fatalf("two outages were stated %d times, want twice; the log held %v", len(got), operationLevels(h.records(t)))
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
	rec := h.requireOnce(t, "production-suspended", "warn")
	if got := ctxString(t, rec, "store_socket"); got != h.socket {
		t.Fatalf("store_socket = %q, want the store the file plane is waiting on", got)
	}
}

func TestASpoolThatReadItsOwnExitMarkerIsNoLongerTracked(t *testing.T) {
	// Arrange. LOST means "we stopped seeing it". A run whose EXIT marker we
	// READ is a run we watched finish, so leaving it tracked would have the
	// staleness sweep eventually restate a completed run as LOST.
	store := &fakeStore{}
	h := newHarness(t, store)
	spool := h.spoolFile(t, "b1settled", "work\nEXIT=0\n")
	h.sc.TaskSpawned("b1settled", "toolu_settled_run", "", spool, false)

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
	h.sc.TaskSpawned("b1refused", "toolu_refused_run", "", spool, false)

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
	h.sc.TaskSpawned("b1swept", "toolu_swept_run", "", spool, false)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()

	// Act: long past any silence window.
	h.advance(24 * time.Hour)
	h.sc.sweep()

	// Assert: a LOST conclusion is the warn-level `lost-policy` record, and the
	// sweep reached none.
	h.requireNone(t, "lost-policy", "warn")
}

// ---- ruling R-S2: the refusal's KIND decides what the cycle does about it ----

func TestAnInvalidRequestParksOnlyTheOffendingFile(t *testing.T) {
	// Arrange. THE REFUSAL IS A PRODUCER DEFECT, NOT AN OUTAGE: the store is
	// reachable and answering, so suspending the whole file plane over one
	// malformed batch would stop every other file for a defect in one.
	store := &fakeStore{}
	h := newHarness(t, store)
	first := h.transcript(t, "sess-1", promptLine)
	second := h.transcript(t, "sess-2", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "entry 0 carries no upsert_key"
	store.writeInvalidField = "batch.entries[0].upsert_key"

	// Act: one pass in which both files offer a batch.
	h.sc.pollAll()

	// Assert: production is NOT suspended, and the other file was still read.
	if h.sc.cursors == nil {
		t.Fatal("an invalid_request suspended production; it is a producer defect, not an outage")
	}
	if len(h.sc.parked) != 2 {
		t.Fatalf("parked %v, want both files parked once each refused its own batch", h.sc.parked)
	}
	if _, watched := h.sc.watchers[first]; !watched {
		t.Fatalf("the refused file's tailer was dropped; its cursor must stay where the store has it")
	}
	if _, watched := h.sc.watchers[second]; !watched {
		t.Fatal("the second file's tailer was dropped by the first file's refusal")
	}
}

func TestAParkedFileIsNeverReadAgain(t *testing.T) {
	// Arrange. Re-reading the same durable bytes re-mints the SAME rejected
	// batch, forever: a tight identical replay loop that makes no progress and
	// drowns the log.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "entry 0 carries no upsert_key"
	store.writeInvalidField = "batch.entries[0].upsert_key"
	h.sc.pollAll()
	refusedAt := store.writeCalls

	// Act: two further passes over the same unchanged bytes.
	h.sc.pollAll()
	h.sc.pollAll()

	// Assert.
	if store.writeCalls != refusedAt {
		t.Fatalf("the parked file was written %d more time(s); a rejected batch must never be replayed identically",
			store.writeCalls-refusedAt)
	}
}

func TestParkingSurvivesAStoreOutage(t *testing.T) {
	// Arrange. A suspension drops every tailer, and a resumed cycle rebuilds
	// them — but an outage in between does not make a producer defect go away,
	// so the parked file must not be quietly un-parked and replayed.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "entry 0 carries no upsert_key"
	store.writeInvalidField = "batch.entries[0].upsert_key"
	h.sc.pollAll()
	store.writeFail, store.writeInvalidField = "", ""

	// Act: an outage, then a full recovery.
	h.sc.suspend("a store bounce", errors.New("connection refused"))
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle after the outage: %v", err)
	}
	before := store.writeCalls
	h.sc.pollAll()

	// Assert.
	if !h.sc.parked[path] {
		t.Fatal("the outage un-parked a file the store said it can never accept")
	}
	if store.writeCalls != before {
		t.Fatalf("the parked file was replayed %d time(s) after the outage", store.writeCalls-before)
	}
}

func TestTheProducerDefectIsStatedWithTheStoresOwnField(t *testing.T) {
	// Arrange. The field the store named, the write ids it rejected and the file
	// position they were read at are the ONLY way anyone finds the bug, so they
	// ride dedicated keys rather than prose.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "entry 0 carries no upsert_key"
	store.writeInvalidField = "batch.entries[0].upsert_key"

	// Act.
	h.sc.pollAll()

	// Assert: exactly one record, carrying the arm, the field and the whole
	// refused batch's write ids on dedicated keys.
	rec := h.requireOnce(t, "producer-defect", "error")
	if got := ctxString(t, rec, "refusal_kind"); got != "invalid_request" {
		t.Errorf("refusal_kind = %q, want the arm that says a retry cannot help", got)
	}
	if got := ctxString(t, rec, "field"); got != "batch.entries[0].upsert_key" {
		t.Errorf("field = %q, want the offending field the store named", got)
	}
	ids, ok := rec.Context["write_ids"].([]any)
	if !ok || len(ids) == 0 {
		t.Errorf("write_ids = %v, want the write ids of the whole refused batch", rec.Context["write_ids"])
	}
}

func TestAStorageFailureStillSuspendsProduction(t *testing.T) {
	// Arrange. The other arm: the transaction failed in the DATABASE and a retry
	// may succeed, which is the outage the recover-cursors-then-rescan cycle
	// exists for. Parking a file over it would abandon a file the store will
	// happily take next time.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "database is locked"

	// Act.
	h.sc.pollAll()

	// Assert.
	if h.sc.cursors != nil {
		t.Fatal("a storage_failure did not suspend production")
	}
	if h.sc.parked[path] {
		t.Fatal("a storage_failure parked the file; a retry of those same bytes may well succeed")
	}
}

func TestAKindLessFailureIsStatedAsAContractViolationAndSuspends(t *testing.T) {
	// Arrange. An unset oneof is illegal here — the kind is the arm that says
	// whether a retry can help — so a store that omits it has told the producer
	// nothing actionable. It is stated at ERROR and then treated as the
	// RECOVERABLE kind, so the sidecar keeps trying rather than parking a file
	// on a verdict the store never gave.
	store := &fakeStore{}
	h := newHarness(t, store)
	path := h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "something went wrong"
	store.writeKindless = true

	// Act.
	h.sc.pollAll()

	// Assert.
	if h.sc.parked[path] {
		t.Fatal("a kind-less failure parked the file; the store never said the batch was invalid")
	}
	if h.sc.cursors != nil {
		t.Fatal("a kind-less failure did not suspend production; it is treated as a storage failure")
	}
	rec := h.requireOnce(t, "store-write", "error")
	if _, ok := rec.Context["refusal_kind"]; ok {
		t.Fatalf("a kind-less refusal named a refusal_kind: %v", rec.Context)
	}
}

// ---- ruling R-S4: an a* spool's book is its SPAWNING CALL ----

func TestAnAgentSpoolsBookIsItsSpawningCall(t *testing.T) {
	// Arrange. An a* spool is a backgrounded subagent's transcript delivered
	// through the task spool, so its PATH names a task and nothing else. The
	// agent-transcript attribution has no filename fallback on purpose — naming
	// a book by `agent-<id>` would give one agent two books, one per plane, that
	// no consumer could reconcile — so the reader must supply the identity the
	// subagent is announced under on BOTH planes: the spawning call's
	// tool_use_id.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "a1", promptLine+"\n")
	h.sc.TaskSpawned("a1", "toolu_spawn_0001", "owner-agent", spool, true)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	w, watched := h.sc.watchers[spool]
	if !watched {
		t.Fatalf("the agent spool is not being watched; watchers=%v", h.sc.watchers)
	}

	// Assert.
	if got := w.tailer.Context().AgentID; got != "toolu_spawn_0001" {
		t.Fatalf("the agent spool's book is %q, want the spawning call's tool_use_id", got)
	}
}

func TestAnAgentSpoolWithNoResolvedSpawnStatesTheReaderDefect(t *testing.T) {
	// Arrange. A spool whose owner is unresolved is HELD rather than tailed, so
	// reaching the book resolution without one is a reader defect. It is stated
	// rather than silently producing records for a book nobody can open.
	h := newHarness(t, &fakeStore{})
	target := discover.Target{
		Path: "/private/tmp/a9.output", TaskID: "a9", Kind: tail.KindAgentTranscript,
	}

	// Act.
	got := h.sc.bookFor(target)

	// Assert.
	if got != "" {
		t.Fatalf("bookFor = %q, want no book for an unresolved spawn", got)
	}
	rec := h.requireOnce(t, "spool-book", "error")
	if got := ctxString(t, rec, "task_id"); got != "a9" {
		t.Fatalf("task_id = %q, want the spool with no resolved spawn", got)
	}
}

// ---- the outage ladder's levels ------------------------------------------

// TestTheFirstFailedAttemptOfAnOutageIsAnError asserts the loudest record of an
// outage is its first refusal: the moment the file plane stopped.
func TestTheFirstFailedAttemptOfAnOutageIsAnError(t *testing.T) {
	// Arrange: no store is listening at all.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()

	// Assert.
	rec := h.requireOnce(t, "recover-cursors", "error")
	if got, ok := rec.Context["attempt"].(float64); !ok || int(got) != 1 {
		t.Fatalf("attempt = %v, want the outage's first attempt", rec.Context["attempt"])
	}
}

// TestLaterAttemptsOfTheSameOutageAreWarnings asserts the ladder descends: the
// attempts after the first are the same known outage still running.
func TestLaterAttemptsOfTheSameOutageAreWarnings(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)

	// Act: three attempts down the ladder.
	h.sc.attempt()
	h.advance(time.Minute)
	h.sc.attemptDue()
	h.advance(time.Minute)
	h.sc.attemptDue()

	// Assert.
	if got := h.opsAt(t, "recover-cursors", "error"); len(got) != 1 {
		t.Errorf("the ladder wrote %d error records, want only the outage's first refusal", len(got))
	}
	if got := h.opsAt(t, "recover-cursors", "warn"); len(got) != 2 {
		t.Errorf("the ladder wrote %d warning records for its later attempts, want two", len(got))
	}
}

// TestEveryLadderRecordNamesItsAttemptAndBackoff asserts the ladder's progress
// is filterable: the ordinal and the armed delay ride dedicated keys.
func TestEveryLadderRecordNamesItsAttemptAndBackoff(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()
	h.advance(time.Minute)
	h.sc.attemptDue()

	// Assert.
	for _, r := range h.ops(t, "recover-cursors") {
		for _, key := range []string{"attempt", "backoff_ms"} {
			if _, ok := r.Context[key]; !ok {
				t.Errorf("ladder record at %s carries no %q; its context was %v", r.Level, key, r.Context)
			}
		}
	}
}

// TestNoLadderRecordIsVerbose asserts an outage is visible without turning
// verbose emission on. An outage nobody sees is an outage nobody fixes.
func TestNoLadderRecordIsVerbose(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)

	// Act.
	h.sc.attempt()
	h.advance(time.Minute)
	h.sc.attemptDue()

	// Assert.
	for _, r := range h.ops(t, "recover-cursors") {
		if r.Verbosity != "normal" {
			t.Errorf("a ladder record was written at verbosity %q: %q", r.Verbosity, r.Message)
		}
	}
}

// TestRecoveryStatesExactlyOneRecord asserts the outage closes with ONE info
// record, however many attempts it took to get there.
func TestRecoveryStatesExactlyOneRecord(t *testing.T) {
	// Arrange: a store that is unreachable, then reachable.
	h := newHarness(t, nil)
	h.sc.attempt()
	h.advance(time.Minute)
	h.sc.attemptDue()
	h.serve(t, &fakeStore{})

	// Act.
	h.advance(time.Minute)
	h.sc.attemptDue()

	// Assert.
	h.requireOnce(t, "production-resumed", "info")
}

// TestAParkedFilesDefectNamesTheRefusalSite asserts the refusal's SITE rides
// beside its KIND. The kind says whether a retry can help; the site says which
// call was refused, which is what joins this record to the store's own.
func TestAParkedFilesDefectNamesTheRefusalSite(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "entry 0 carries no upsert_key"
	store.writeInvalidField = "batch.entries[0].upsert_key"

	// Act.
	h.sc.pollAll()

	// Assert.
	rec := h.requireOnce(t, "producer-defect", "error")
	if got := ctxString(t, rec, "refusal_kind"); got != "invalid_request" {
		t.Errorf("refusal_kind = %q, want the arm that says a retry cannot help", got)
	}
	if got := ctxString(t, rec, "refusal_site"); got != storeclient.WriteBatchSite {
		t.Errorf("refusal_site = %q, want the write call the store refused", got)
	}
}

// TestAFileDiscoveredAfterTheCycleBeganAsksTheStoreForItsCursor pins the
// in-cycle half of the store-unreachable invariant. The cycle's snapshot is
// taken once, when the cycle begins, so a file appearing afterwards is absent
// from it — and absent from a SNAPSHOT is not the fact "the store holds no
// cursor". A tailer built on that confusion starts at zero and re-converts a
// whole conversation.
func TestAFileDiscoveredAfterTheCycleBeganAsksTheStoreForItsCursor(t *testing.T) {
	// Arrange: a cycle begun with no files at all, so its snapshot is empty.
	store := &fakeStore{}
	h := newHarness(t, store)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	turn := promptLine + "\n" + assistantLine + "\n"
	path := h.transcript(t, "sess-1", promptLine, assistantLine, promptLine, assistantLine)
	store.cursors = []*storev1.CursorState{{
		FileId: identityOf(t, path), Path: path, Offset: int64(2 * len(turn)),
	}}

	// Act: the file appears mid-cycle.
	h.sc.rescan()

	// Assert: it resumed from the store's position, not from zero.
	if got := h.sc.watchers[path].tailer.Offset(); got != int64(len(turn)) {
		t.Fatalf("resumed offset = %d, want %d; a file the cycle's snapshot did not name must be ASKED about, never assumed cold",
			got, int64(len(turn)))
	}
}

// TestAFileIsNotWatchedWhenTheStoreCannotSayWhereItIs asserts the sad path of
// the same lookup: a store that cannot answer yields no position, so no tailer
// is built — never a cold start standing in for an answer.
func TestAFileIsNotWatchedWhenTheStoreCannotSayWhereItIs(t *testing.T) {
	// Arrange: a cycle that began cleanly, and a store that then stops
	// answering cursor reads.
	store := &fakeStore{}
	h := newHarness(t, store)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	path := h.transcript(t, "sess-1", promptLine, assistantLine)
	store.cursorsFail = "database is locked"

	// Act.
	h.sc.rescan()

	// Assert.
	if _, watched := h.sc.watchers[path]; watched {
		t.Fatal("a tailer was built for a file the store could not state a position for")
	}
}

// TestTheBootRewindIsOncePerIdentityNotPerPath asserts the rewind's bound is a
// statement about a FILE: a renamed file is the same file, and re-reading its
// in-progress turn a second time is the re-conversion the bound exists to stop.
func TestTheBootRewindIsOncePerIdentityNotPerPath(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	turn := promptLine + "\n" + assistantLine + "\n"
	path := h.transcript(t, "sess-1", promptLine, assistantLine, promptLine, assistantLine)
	store.cursors = []*storev1.CursorState{{
		FileId: identityOf(t, path), Path: path, Offset: int64(2 * len(turn)),
	}}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the vendor renames the file, and it is discovered at its new path.
	renamed := filepath.Join(filepath.Dir(path), "sess-2.jsonl")
	if err := os.Rename(path, renamed); err != nil {
		t.Fatalf("rename %s: %v", path, err)
	}
	delete(h.sc.watchers, path)
	h.sc.rescan()

	// Assert.
	if got := strings.Count(h.logText(), "rewound the restored cursor"); got != 1 {
		t.Fatalf("rewind ran %d times across a rename, want once per file per boot", got)
	}
}

func TestJitterBackoffKeepsAZeroDelayImmediate(t *testing.T) {
	// Arrange, Act, Assert: a fresh suspension arms a zero delay, which must
	// stay an immediate retry rather than being spread into a small wait.
	if got := jitterBackoff(0); got != 0 {
		t.Fatalf("jitterBackoff(0) = %v, want 0", got)
	}
}

func TestJitterBackoffSpreadsAPositiveDelayWithinItsFraction(t *testing.T) {
	// Arrange
	d := 10 * time.Second
	spread := time.Duration(float64(d) * recoverJitterFraction)
	lo, hi := d-spread, d+spread

	// Act, Assert: run several draws since the spread is randomized.
	for i := 0; i < 20; i++ {
		got := jitterBackoff(d)
		if got < lo || got > hi {
			t.Fatalf("jitterBackoff(%v) = %v, want within [%v, %v]", d, got, lo, hi)
		}
	}
}

func TestBootTimeMillisNeverReturnsNegative(t *testing.T) {
	// Arrange, Act: the real syscall path, exercised directly since the
	// harness always overrides sc.bootTimeMs to keep tests deterministic.
	got := bootTimeMillis()

	// Assert: 0 is the documented "unavailable" answer; anything real is a
	// millisecond timestamp, which is never negative.
	if got < 0 {
		t.Fatalf("bootTimeMillis() = %d, want >= 0", got)
	}
}

func TestALaunchObservedAfterAFileIsWatchedStillMarksItBackgrounded(t *testing.T) {
	// Arrange. DISCOVERY ORDER IS NOT CAUSAL ORDER: a subagent's sidechain
	// transcript is routinely discovered before the parent transcript's launch
	// result has been read — on a restart it usually is. The flag was frozen at
	// watch time, so that file named the wrong top_level for the life of the
	// process while the same agent's task spool named the right one.
	h := newHarness(t, &fakeStore{})
	ctx := &tail.Context{Path: "/p/s/subagents/agent-a15.jsonl", AgentID: "toolu_spawn"}
	h.sc.watchers[ctx.Path] = &watched{
		target: discover.Target{Path: ctx.Path, AgentID: "toolu_spawn", SessionID: "s", TaskID: "a15locator"},
		ctx:    ctx,
	}

	// Act: the launch is read only now.
	h.sc.owners.observe(observation{taskID: "a15", activityID: "toolu_spawn", backgrounded: true})
	h.sc.refreshSpawnFacts()

	// Assert.
	if !ctx.SpawnBackgrounded {
		t.Fatal("a file watched before its launch was read never learns the spawn was backgrounded")
	}
}

func TestAFileWatchedWithNoBackgroundedLaunchStaysForeground(t *testing.T) {
	// Arrange. The launch is the sole evidence either way; a refresh that turned
	// the flag on without one would move every synchronous subagent's frames
	// into a top_level of its own.
	h := newHarness(t, &fakeStore{})
	ctx := &tail.Context{Path: "/p/s/subagents/agent-a16.jsonl", AgentID: "toolu_sync"}
	h.sc.watchers[ctx.Path] = &watched{
		target: discover.Target{Path: ctx.Path, AgentID: "toolu_sync", SessionID: "s", TaskID: "a16locator"},
		ctx:    ctx,
	}

	// Act.
	h.sc.owners.observe(observation{taskID: "a16", activityID: "toolu_sync", backgrounded: false})
	h.sc.refreshSpawnFacts()

	// Assert.
	if ctx.SpawnBackgrounded {
		t.Fatal("a foreground spawn was refreshed into a backgrounded one")
	}
}

// TestSignalUnwedgesTheCycleFromAnInFlightWrite asserts the shutdown promptness
// this cycle owes: a sidecar blocked inside WriteBatch used to see the signal
// only once rpcTimeout (30s) expired, because the cycle awaited the rpc on a
// context the signal did not reach. The signal now cancels that context.
func TestSignalUnwedgesTheCycleFromAnInFlightWrite(t *testing.T) {
	tests := []struct {
		name       string
		wedged     bool
		wantRecord bool
	}{
		{name: "a wedged write is cancelled and its replay stated", wedged: true, wantRecord: true},
		{name: "an idle cycle shuts down with no replay record", wedged: false, wantRecord: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := &fakeStore{writeWedged: tc.wedged, entered: make(chan struct{})}
			entered := store.entered
			h := newHarness(t, store)
			h.transcript(t, "sess-1", promptLine)
			h.sc.options.PollInterval = time.Millisecond
			if err := h.sc.beginCycle(); err != nil {
				t.Fatalf("beginCycle: %v", err)
			}
			stop := make(chan os.Signal, 1)
			returned := make(chan error, 1)

			// Act.
			go func() { returned <- h.sc.Run(stop) }()
			if tc.wedged {
				select {
				case <-entered:
				case <-time.After(2 * time.Second):
					t.Fatal("the store was never asked to write, so nothing is wedged to interrupt")
				}
			}
			start := time.Now()
			stop <- syscall.SIGTERM

			// Assert: the signal must not wait out rpcTimeout.
			select {
			case err := <-returned:
				if err != nil {
					t.Fatalf("Run returned %v, want a clean shutdown", err)
				}
			case <-time.After(2 * time.Second):
				t.Fatalf("Run did not return within 2s of the signal (rpcTimeout is %s)", rpcTimeout)
			}
			if elapsed := time.Since(start); elapsed >= rpcTimeout {
				t.Fatalf("shutdown took %s, want well under rpcTimeout %s", elapsed, rpcTimeout)
			}
			const record = "shutdown interrupted a write; it will replay"
			if got := strings.Contains(h.logText(), record); got != tc.wantRecord {
				t.Fatalf("log contains %q = %v, want %v; log: %s", record, got, tc.wantRecord, h.logText())
			}
		})
	}
}

// TestTheFirstProducerDefectRecordCountsItself covers the running count on the
// record a reader actually sees: "seen once" must be distinguishable from
// "seen ten thousand times" without counting records that were deliberately
// not written.
func TestTheFirstProducerDefectRecordCountsItself(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.writeFail = "entry 0 carries no upsert_key"
	store.writeInvalidField = "batch.entries[0].upsert_key"

	// Act.
	h.sc.pollAll()

	// Assert.
	rec := h.requireOnce(t, "producer-defect", "error")
	if got, ok := rec.Context["repeat_count"].(float64); !ok || int(got) != 1 {
		t.Errorf("repeat_count = %v, want the first occurrence counted as 1", rec.Context["repeat_count"])
	}
}

// TestARepeatedDefectForOneFileIsNotRestated covers the un-park loop: a
// condition that never changes must not become a record per poll.
func TestARepeatedDefectForOneFileIsNotRestated(t *testing.T) {
	// Arrange: one file has already defected twice on the same field.
	h := newHarness(t, &fakeStore{})
	for i := 0; i < 2; i++ {
		h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[1].page_agent_id")
	}

	// Act: the third occurrence of the identical defect.
	count, stated := h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[1].page_agent_id")

	// Assert.
	if stated {
		t.Error("the third identical defect for one file was restated; a condition that never changes drowns the log")
	}
	if count != 3 {
		t.Errorf("count = %d, want every occurrence tallied even when it is not stated", count)
	}
}

// TestARepeatedDefectIsRestatedOnAPowerOfTwo covers the other half of the
// ladder: a defect stuck in a loop stays VISIBLE, logarithmically.
func TestARepeatedDefectIsRestatedOnAPowerOfTwo(t *testing.T) {
	// Arrange: three occurrences already tallied.
	h := newHarness(t, &fakeStore{})
	for i := 0; i < 3; i++ {
		h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[1].page_agent_id")
	}

	// Act: the fourth.
	count, stated := h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[1].page_agent_id")

	// Assert.
	if !stated || count != 4 {
		t.Errorf("occurrence %d stated=%v, want the 4th restated so a stuck defect stays visible", count, stated)
	}
}

// TestADifferentFieldForOneFileIsAlwaysStated covers the carve-out: the store
// named a different part of the batch, so it is a different bug and must not
// hide behind the first one's tally.
func TestADifferentFieldForOneFileIsAlwaysStated(t *testing.T) {
	// Arrange: a file well past its restatement ladder on one field.
	h := newHarness(t, &fakeStore{})
	for i := 0; i < 5; i++ {
		h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[1].page_agent_id")
	}

	// Act: the store now names a different field.
	count, stated := h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[0].upsert_key")

	// Assert.
	if !stated {
		t.Error("a defect on a NEW field was suppressed by the previous field's tally")
	}
	if count != 1 {
		t.Errorf("count = %d, want a new field to start its own tally", count)
	}
}

// TestTwoFilesTallyTheirDefectsSeparately covers the key: one file's noisy
// defect must not suppress another file's first one.
func TestTwoFilesTallyTheirDefectsSeparately(t *testing.T) {
	// Arrange: one file well past its ladder.
	h := newHarness(t, &fakeStore{})
	for i := 0; i < 5; i++ {
		h.sc.countDefect("16777233:1", "/p/a.jsonl", "entries[1].page_agent_id")
	}

	// Act: a DIFFERENT file's first defect, on the same field.
	count, stated := h.sc.countDefect("16777233:2", "/p/b.jsonl", "entries[1].page_agent_id")

	// Assert.
	if !stated || count != 1 {
		t.Errorf("the second file's first defect counted %d stated=%v, want its own tally starting at 1", count, stated)
	}
}
