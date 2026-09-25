package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/stale"
	"agentrepl/shim-claude-sidecar/internal/storeclient"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"google.golang.org/protobuf/types/known/structpb"
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
		LockDir:           t.TempDir(),
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
		LockDir:     t.TempDir(),
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
		LockDir:            t.TempDir(),
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
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
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
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
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
	h.sc.TaskSpawned("b1settled", "toolu_settled_run", "", spool, false, "/workspace", "workspace-id", "session-1")

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
	h.sc.TaskSpawned("b1refused", "toolu_refused_run", "", spool, false, "/workspace", "workspace-id", "session-1")

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
	h.sc.TaskSpawned("b1swept", "toolu_swept_run", "", spool, false, "/workspace", "workspace-id", "session-1")
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
	h.sc.TaskSpawned("a1", "toolu_spawn_0001", "owner-agent", spool, true, "/workspace", "workspace-id", "session-1")
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
			const record = "shutdown interrupted a write that had not answered within"
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

// TestARescanStatesTheCountItWatchedRatherThanARecordPerFile covers the boot
// volume: discovery has no age bound, so a per-file record at normal verbosity
// costs megabytes of log per boot that say nothing but "still here".
func TestARescanStatesTheCountItWatchedRatherThanARecordPerFile(t *testing.T) {
	// Arrange: three transcripts to discover.
	h := newHarness(t, &fakeStore{})
	for _, session := range []string{"sess-1", "sess-2", "sess-3"} {
		h.transcript(t, session, promptLine)
	}

	// Act.
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Assert: one summary naming the count, and no per-file record beside it.
	rec := h.requireOnce(t, "rescan", "info")
	if got, ok := rec.Context["repeat_count"].(float64); !ok || int(got) != 3 {
		t.Errorf("repeat_count = %v, want the 3 files this pass started watching", rec.Context["repeat_count"])
	}
	h.requireNone(t, "watch", "info")
}

// TestARescanThatWatchedNothingStatesNothing covers the quiet pass: the rescan
// tick runs every 30s for the life of the process, and a pass that changed the
// watched set not at all has no lifecycle fact to report.
func TestARescanThatWatchedNothingStatesNothing(t *testing.T) {
	// Arrange: a cycle that has already watched everything there is.
	h := newHarness(t, &fakeStore{})
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	before := len(h.opsAt(t, "rescan", "info"))

	// Act: a second pass over the same, unchanged set.
	h.sc.rescan()

	// Assert.
	if got := len(h.opsAt(t, "rescan", "info")); got != before {
		t.Errorf("a rescan that watched nothing wrote %d more record(s), want none", got-before)
	}
}

func TestASteadyStateBookConflictSkipWarns(t *testing.T) {
	// Arrange: production is already live, so a book-conflict skip is NOT catch-up
	// backlog — nothing should be re-booking a live row.
	h := newHarness(t, &fakeStore{})
	nowMs := h.clock.UnixMilli()
	h.sc.processStartMs = nowMs - 1

	// Act: the store reported a skip for a batch read from a file that grew now.
	h.sc.noteSkips("/nonexistent/session.jsonl", []storeclient.SkippedEntry{
		{UpsertKey: "activity:msg_1:0", FromBook: "toolu_A", ToBook: "toolu_B"},
	}, nowMs)

	// Assert: one WARN per skip, and it is NOT folded into a catch-up summary.
	rec := h.requireOnce(t, "book-conflict-skip", "warn")
	if got := ctxString(t, rec, "reason"); got != "legacy_book_conflict" {
		t.Fatalf("reason = %q, want legacy_book_conflict", got)
	}
	if got := len(h.opsAt(t, "catchup-summary", "info")); got != 0 {
		t.Fatalf("a steady-state skip produced %d catch-up summaries, want none", got)
	}
}

// ---------------------------------------------------------------------------
// The startup catch-up window's boundary.
// ---------------------------------------------------------------------------

// A poll pass that walked every watcher to completion IS the boot walk: the
// corpus discovered by the cycle's first rescan has been picked up and
// converted, so the window may close.
func TestACompletedPollPassLatchesTheDrainedPass(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.pollAll()

	// Assert.
	if !h.sc.drainedPass {
		t.Fatal("a completed poll pass did not latch the drained pass")
	}
}

// AN ABANDONED PASS HAS NOT DRAINED THE CORPUS. A store outage cuts the pass
// short, so the backlog it still owes must keep being leveled.
func TestAnAbandonedPollPassDoesNotLatchTheDrainedPass(t *testing.T) {
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
	if h.sc.drainedPass {
		t.Fatal("a pass abandoned by a store outage latched the drained pass")
	}
}

// The window closes off that one fact, and a corpus-walk operation is news
// again afterwards.
func TestTheCatchupWindowClosesAfterTheFirstDrainedPass(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.sc.log.BeginCatchup(catchupOperations...)
	h.sc.drainedPass = true

	// Act.
	h.sc.endCatchupOnFirstDrainedPass()
	h.sc.log.With(logging.Context{Operation: "boot-rewind"}).Log("a live rewind")

	// Assert.
	if !strings.Contains(h.logText(), `"level":"info","verbosity":"normal","operation":"boot-rewind"`) {
		t.Fatalf("boot-rewind after the window closed is not INFO: %s", h.logText())
	}
}

// A pass that never completed leaves the window open, so the backlog it still
// owes is still leveled.
func TestTheCatchupWindowStaysOpenWithoutADrainedPass(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.sc.log.BeginCatchup(catchupOperations...)

	// Act.
	h.sc.endCatchupOnFirstDrainedPass()
	h.sc.log.With(logging.Context{Operation: "boot-rewind"}).Log("still backlog")

	// Assert.
	if !strings.Contains(h.logText(), `"level":"debug"`) {
		t.Fatalf("boot-rewind with the window still open is not DEBUG: %s", h.logText())
	}
}

// THE LATCH IS FOR THE PROCESS'S LIFETIME. A later pass must not restate the
// summaries a closed window already stated.
func TestASecondDrainedPassDoesNotRestateTheSummaries(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.sc.log.BeginCatchup(catchupOperations...)
	h.sc.log.With(logging.Context{Operation: "boot-rewind"}).Log("backlog")
	h.sc.drainedPass = true
	h.sc.endCatchupOnFirstDrainedPass()
	before := strings.Count(h.logText(), `"operation":"catchup-summary"`)

	// Act.
	h.sc.endCatchupOnFirstDrainedPass()

	// Assert.
	if got := strings.Count(h.logText(), `"operation":"catchup-summary"`); got != before {
		t.Fatalf("a second drained pass restated the summaries: %d, want %d", got, before)
	}
}

// The end of catch-up is STATED, so an operator (and a subject) has one edge
// that says "everything from here is news, stated per item".
func TestTheEndOfCatchupIsStated(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	h.sc.log.BeginCatchup(catchupOperations...)
	h.sc.drainedPass = true

	// Act.
	h.sc.endCatchupOnFirstDrainedPass()

	// Assert.
	if !strings.Contains(h.logText(), `"operation":"catchup-end"`) {
		t.Fatalf("the end of catch-up was not stated: %s", h.logText())
	}
}

// hookSuccessTranscriptLine is a transcript copy of a clean hook firing: the
// residue kind the owner ruled is never persisted (convert/neverpersist.go).
func hookSuccessTranscriptLine(uuid string) string {
	return `{"type":"attachment","uuid":"` + uuid + `","isSidechain":false,` +
		`"timestamp":"2026-08-29T12:00:00.000Z","attachment":{"type":"hook_success",` +
		`"hookName":"PreToolUse:Read","toolUseID":"toolu_gated","hookEvent":"PreToolUse","command":"/h.sh"}}`
}

// tokensReminderTranscriptLine is the vendor's per-turn budget line, the other
// other residue kind the boot-walk fixtures use.
func tokensReminderTranscriptLine(uuid string) string {
	return `{"type":"attachment","uuid":"` + uuid + `","isSidechain":false,` +
		`"timestamp":"2026-08-29T12:00:00.000Z","attachment":{"type":"total_tokens_reminder",` +
		`"text":"<total_tokens>18000 tokens left</total_tokens>"}}`
}

// readAndDropACatchupCorpus walks one transcript of withheld residue to
// completion and closes the catch-up window off that drained pass.
func readAndDropACatchupCorpus(t *testing.T, lines ...string) *harness {
	t.Helper()
	h := newHarness(t, &fakeStore{})
	h.transcript(t, "sess-1", lines...)
	h.sc.log.BeginCatchup(catchupOperations...)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	h.sc.drainedPass = true
	h.sc.endCatchupOnFirstDrainedPass()
	return h
}

// THE BOOT WALK'S DROPS ARE STATED ONCE PER FILE, with the counts by kind. The
// per-line records are DEBUG and always will be, so without this nothing at INFO
// would say the walk read a quarter-million hook attachments and stored none.
func TestTheCatchupSummaryCarriesTheDroppedResidueCountsByKind(t *testing.T) {
	// Arrange, Act.
	h := readAndDropACatchupCorpus(t,
		promptLine,
		hookSuccessTranscriptLine("h1"),
		hookSuccessTranscriptLine("h2"),
		tokensReminderTranscriptLine("r1"),
	)

	// Assert.
	text := h.logText()
	if !strings.Contains(text, `"operation":"residue-drop-summary"`) {
		t.Fatalf("no per-file residue-drop summary was stated: %s", text)
	}
	if !strings.Contains(text, "vendor_specific/attachment/hook_success=2, vendor_specific/attachment/total_tokens_reminder=1") {
		t.Fatalf("the summary does not carry the counts by residue label: %s", text)
	}
}

// A file that withheld nothing states nothing, exactly as EndCatchup states
// nothing for an operation that demoted nothing. `assistantLine` is the fixture
// that converts to a TYPED page line and nothing else; `promptLine` is not,
// because the file plane's user prompt is itself residue.
func TestAFileThatDroppedNoResidueStatesNoSummary(t *testing.T) {
	// Arrange, Act.
	h := readAndDropACatchupCorpus(t, assistantLine)

	// Assert.
	if strings.Contains(h.logText(), `"operation":"residue-drop-summary"`) {
		t.Fatalf("a file that dropped nothing stated a summary: %s", h.logText())
	}
}

// The summary is INFO: it is the one record at normal verbosity that reports the
// volume the residue rule withheld.
func TestTheResidueDropSummaryIsStatedAtInfo(t *testing.T) {
	// Arrange, Act.
	h := readAndDropACatchupCorpus(t, promptLine, hookSuccessTranscriptLine("h1"))

	// Assert.
	if got := len(h.opsAt(t, "residue-drop-summary", "info")); got != 1 {
		t.Fatalf("residue-drop-summary INFO records = %d, want exactly 1: %s", got, h.logText())
	}
}

// --- a vanished file whose whole directory went with it ---------------------

func TestTreeRemovedAnswersOnlyForADefinitelyAbsentDirectory(t *testing.T) {
	// Arrange: one case per way the parent can answer.
	root := t.TempDir()
	standing := filepath.Join(root, "standing")
	if err := os.MkdirAll(standing, 0o755); err != nil {
		t.Fatalf("creating %s: %v", standing, err)
	}
	cases := []struct {
		name string
		path string
		want bool
	}{
		{name: "the directory is gone too", path: filepath.Join(root, "removed", "b1.output"), want: true},
		{name: "the directory stands", path: filepath.Join(standing, "b1.output"), want: false},
		{name: "the directory is a file, so it is present and unusable", path: filepath.Join(root, "notadir"), want: false},
	}
	if err := os.WriteFile(filepath.Join(root, "notadir"), nil, 0o644); err != nil {
		t.Fatalf("writing the not-a-directory fixture: %v", err)
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := treeRemoved(tc.path)

			// Assert.
			if got != tc.want {
				t.Fatalf("treeRemoved(%q) = %v, want %v", tc.path, got, tc.want)
			}
		})
	}
}

func TestAFileThatWentWithItsWholeTreeIsStatedWithoutWarning(t *testing.T) {
	// Arrange: a claimed spool whose entire task directory is then removed, which
	// is what a harness deleting its own run directory does.
	h := newHarness(t, &fakeStore{})
	spool := h.spoolFile(t, "b1", "hello\n")
	h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	if err := os.RemoveAll(filepath.Dir(spool)); err != nil {
		t.Fatalf("removing %s: %v", filepath.Dir(spool), err)
	}

	// Act.
	h.sc.pollAll()

	// Assert.
	h.requireNone(t, "file-vanished", "warn")
	rec := h.requireOnce(t, "file-vanished", "info")
	if got := ctxString(t, rec, "reason"); got != reasonTreeRemoved {
		t.Fatalf("the record's reason = %q, want %q", got, reasonTreeRemoved)
	}
}

// --- a cursor recovery the shutdown withdrew --------------------------------

// TestAShutdownWithdrawingACursorRecoveryIsNotAStoreFailure applies the rule
// commit fd8105ee0 settled on the write path to the cursor path. A recovery this
// process cancelled on the way out says nothing about the store: the file is
// left unwatched exactly as the exit one instant later would leave it, and the
// next boot asks for its position again.
func TestAShutdownWithdrawingACursorRecoveryIsNotAStoreFailure(t *testing.T) {
	// Arrange: a running cycle, then a per-file recovery that wedges.
	store := &fakeStore{}
	h := newHarness(t, store)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	shutdown, cancel := context.WithCancel(context.Background())
	h.sc.shutdown = shutdown
	entered := make(chan struct{})
	store.cursorsEntered = entered
	store.cursorsWedged = true
	answered := make(chan struct{})

	// Act: the recovery wedges, then the shutdown withdraws it.
	go func() {
		defer close(answered)
		h.sc.cursorFor(discover.Target{Path: "/private/tmp/b1.output", TaskID: "b1"}, "1:1")
	}()
	select {
	case <-entered:
	case <-time.After(2 * time.Second):
		t.Fatal("the store was never asked for a cursor, so nothing is wedged to withdraw")
	}
	cancel()
	select {
	case <-answered:
	case <-time.After(2 * time.Second):
		t.Fatal("the withdrawn recovery never returned")
	}

	// Assert.
	h.requireNone(t, "recover-cursors", "warn")
	h.requireOnce(t, "shutdown", "info")
}

func TestAStoreThatCannotAnswerACursorStillWarns(t *testing.T) {
	// Arrange: no shutdown; the store refuses.
	store := &fakeStore{}
	h := newHarness(t, store)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	store.cursorsFail = "the store cannot read its cursors"

	// Act.
	h.sc.cursorFor(discover.Target{Path: "/private/tmp/b1.output", TaskID: "b1"}, "1:1")

	// Assert.
	h.requireOnce(t, "recover-cursors", "warn")
}

// --- only typed entries are persisted ---------------------------------------

// residueAttribution is the coordinates the withholding fixtures mint their
// entries at; the label, not the position, is what these subjects are about.
func residueAttribution() convert.Attribution {
	return convert.Attribution{
		VendorSessionID: "sess-1", MainAgentID: "sess-1", AgentID: "sess-1",
		Path: "/p/projects/proj/sess-1.jsonl", FileID: "1:1",
	}
}

func TestOnlyTypedEntriesReachTheStore(t *testing.T) {
	at := residueAttribution()
	cases := []struct {
		name   string
		entry  *storev1.StoreEntry
		stored bool
	}{
		{
			name:   "vendor_specific is understood and not carried, so it is not stored",
			entry:  convert.VendorSpecificEntry(at, "attachment/hook_success", map[string]any{}),
			stored: false,
		},
		{
			name:   "unknown is parsed and not modelled, so it is not stored either",
			entry:  convert.UnknownEntry(at, "a_new_line_type", "type", map[string]any{}),
			stored: false,
		},
		{
			name:   "unparsed bytes from an unowned spool are not stored",
			entry:  convert.UnparsedEntry(at, []byte("{"), errors.New("truncated object")),
			stored: false,
		},
		{
			name:   "a page line is a typed entry, so it is stored",
			entry:  convert.PageLine(at, "block:0", "unit:typed", at.AgentID, &conversationv1.AgentFrame{}),
			stored: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			store := &fakeStore{}
			h := newHarness(t, store)

			// Act.
			h.sc.emit("subject", []*storev1.StoreEntry{tc.entry})

			// Assert.
			written := 0
			for _, batch := range store.writes {
				written += len(batch.GetEntries())
			}
			if (written == 1) != tc.stored {
				t.Fatalf("entries written = %d, want stored=%v", written, tc.stored)
			}
		})
	}
}

// A BATCH OF ONLY RESIDUE STILL GOES, AND CARRIES ONLY ITS SHAPES. The records
// are withheld, but the shape catalog is the one durable thing a withheld line
// leaves behind (owner ruling 2026-09-13), and an observation dropped because
// its batch looked empty is a shape no re-read ever observes again.
func TestABatchOfOnlyResidueIsSentCarryingOnlyItsShapes(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)

	// Act.
	h.sc.emit("subject", []*storev1.StoreEntry{
		convert.VendorSpecificEntry(residueAttribution(), "attachment/hook_success", map[string]any{}),
	})

	// Assert.
	if store.writeCalls != 1 {
		t.Fatalf("write calls = %d, want the one write the shape observation needs", store.writeCalls)
	}
	if len(store.writes[0].GetEntries()) != 0 {
		t.Fatalf("entries = %d, want the residue withheld", len(store.writes[0].GetEntries()))
	}
	if len(store.shapes[0]) != 1 {
		t.Fatalf("shapes = %v, want the withheld line's one observation", store.shapes[0])
	}
}

// A BATCH OF NOTHING AT ALL IS STILL NOT SENT: no records, no position, no
// shape, so the store has nothing to do with it.
func TestABatchOfNothingIsNotSentToTheStore(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)

	// Act.
	h.sc.emit("subject", nil)

	// Assert.
	if store.writeCalls != 0 {
		t.Fatalf("write calls = %d, want none for a batch with nothing in it", store.writeCalls)
	}
}

func TestABatchWhoseEveryRecordWasResidueStillAdvancesTheCursor(t *testing.T) {
	// Arrange: the bytes WERE read, and re-reading them would produce the same
	// nothing, so the reader's position must still become durable.
	store := &fakeStore{}
	h := newHarness(t, store)
	advance := &storev1.CursorState{FileId: "1:1", Offset: 512}

	// Act.
	if _, err := h.sc.storeWrite("subject", &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{convert.VendorSpecificEntry(residueAttribution(), "attachment/hook_success", map[string]any{})},
		CursorAdvance: advance,
	}); err != nil {
		t.Fatalf("storeWrite: %v", err)
	}

	// Assert.
	if len(store.writes) != 1 {
		t.Fatalf("batches written = %d, want the cursor advance still sent", len(store.writes))
	}
	if got := store.writes[0].GetCursorAdvance().GetOffset(); got != 512 {
		t.Fatalf("cursor advance = %d, want 512", got)
	}
	if got := len(store.writes[0].GetEntries()); got != 0 {
		t.Fatalf("entries written = %d, want none", got)
	}
}

func TestTheWithheldRecordAnnouncesNoRow(t *testing.T) {
	// Arrange: the whole point is that nothing was stored, and a record naming an
	// upsert_key nobody can look up is the untraceable announcement the field-set
	// contract forbids.
	store := &fakeStore{}
	h := newHarness(t, store)

	// Act.
	h.sc.emit("subject", []*storev1.StoreEntry{
		convert.UnknownEntry(residueAttribution(), "a_new_line_type", "type", map[string]any{}),
	})

	// Assert.
	rec := h.requireOnce(t, "residue-drop", "debug")
	if got := ctxString(t, rec, "reason"); got != "unknown/type:a_new_line_type" {
		t.Fatalf("the record's reason = %q, want the residue label", got)
	}
	if _, ok := rec.Context["upsert_key"]; ok {
		t.Fatalf("the withholding record announced a row: %v", rec.Context)
	}
}

// ---- the residue shape catalog at the write door ----

// residueEntryFor builds one vendor_specific residue entry over a JSON literal,
// which is what the withholding door sees for every unmodelled line.
func residueEntryFor(t *testing.T, kind, literal string) *storev1.StoreEntry {
	t.Helper()
	var raw map[string]any
	if err := json.Unmarshal([]byte(literal), &raw); err != nil {
		t.Fatalf("decoding %s: %v", literal, err)
	}
	s, err := structpb.NewStruct(raw)
	if err != nil {
		t.Fatalf("structpb.NewStruct: %v", err)
	}
	return &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{
		AgentUpdate: &storev1.StoreAgentUpdate{AgentInfo: &storev1.StoreAgentUpdate_UnservedItem{
			UnservedItem: &storev1.StoreUnservedItem{
				UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{
					VendorSpecific: &storev1.StoreVendorSpecific{Kind: kind, Raw: s},
				},
			},
		}},
	}}
}

// typedEntryFor builds one servable page line, the entry class the door keeps.
func typedEntryFor() *storev1.StoreEntry {
	return &storev1.StoreEntry{Entry: &storev1.StoreEntry_AgentUpdate{
		AgentUpdate: &storev1.StoreAgentUpdate{AgentInfo: &storev1.StoreAgentUpdate_ServeableFrame{
			ServeableFrame: &storev1.StorePageLine{},
		}},
	}}
}

func TestAWithheldResidueLineContributesAShapeObservation(t *testing.T) {
	// Arrange. The bytes are not stored, so the shape is the only thing left
	// that says the vendor emits this line at all.
	h := newHarness(t, nil)
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		residueEntryFor(t, "hook_success", `{"a":"x"}`),
	}}

	// Act.
	kept, shapes := h.sc.withholdResidue(batch)

	// Assert.
	if len(kept) != 0 {
		t.Fatalf("kept = %d entries, want the residue withheld", len(kept))
	}
	if len(shapes) != 1 || shapes[0].GetKeyStructure() != `{a:string}` {
		t.Fatalf("shapes = %v, want one observation of the line's key structure", shapes)
	}
}

// ONE OBSERVATION PER SHAPE PER BATCH. A boot walk reads thousands of lines of
// one shape, and one observation per line would send the catalog the very
// volume the catalog exists to avoid storing.
func TestTwoWithheldLinesOfOneShapeContributeOneObservation(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		residueEntryFor(t, "hook_success", `{"a":"first"}`),
		residueEntryFor(t, "hook_success", `{"a":"second"}`),
	}}

	// Act.
	_, shapes := h.sc.withholdResidue(batch)

	// Assert.
	if len(shapes) != 1 {
		t.Fatalf("shapes = %d, want the batch deduped by hash", len(shapes))
	}
}

func TestTwoWithheldLinesOfDifferentShapesContributeTwoObservations(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		residueEntryFor(t, "hook_success", `{"a":"x"}`),
		residueEntryFor(t, "hook_success", `{"b":1}`),
	}}

	// Act.
	_, shapes := h.sc.withholdResidue(batch)

	// Assert.
	if len(shapes) != 2 {
		t.Fatalf("shapes = %d, want one per distinct key structure", len(shapes))
	}
}

func TestTheFirstExampleIsTheFirstLineOfItsShape(t *testing.T) {
	// Arrange. One readable line per shape is what makes the catalog actionable.
	h := newHarness(t, nil)
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{
		residueEntryFor(t, "hook_success", `{"a":"first"}`),
		residueEntryFor(t, "hook_success", `{"a":"second"}`),
	}}

	// Act.
	_, shapes := h.sc.withholdResidue(batch)

	// Assert.
	if string(shapes[0].GetFirstExample()) != `{"a":"first"}` {
		t.Fatalf("first_example = %q, want the first line of the shape", shapes[0].GetFirstExample())
	}
}

func TestATypedEntryContributesNoShapeObservation(t *testing.T) {
	// Arrange. The catalog is about what was NOT stored.
	h := newHarness(t, nil)
	batch := &storev1.EntryBatch{Entries: []*storev1.StoreEntry{typedEntryFor()}}

	// Act.
	kept, shapes := h.sc.withholdResidue(batch)

	// Assert.
	if len(kept) != 1 {
		t.Fatalf("kept = %d entries, want the typed entry persisted", len(kept))
	}
	if len(shapes) != 0 {
		t.Fatalf("shapes = %v, want none for a typed entry", shapes)
	}
}

// `new_shapes` MEANS FIRST SEEN BY THIS PROCESS. A shape already contributed in
// an earlier batch still rides the wire — the store's count must rise — but it
// is not news for the summary.
func TestAShapeSeenInAnEarlierBatchIsNotCountedAsNew(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	cursor := &storev1.CursorState{FileId: "16777232:1", Path: "/t/a.jsonl", Offset: 1}
	first := &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{residueEntryFor(t, "hook_success", `{"a":"x"}`)},
		CursorAdvance: cursor,
	}
	second := &storev1.EntryBatch{
		Entries:       []*storev1.StoreEntry{residueEntryFor(t, "hook_success", `{"a":"y"}`)},
		CursorAdvance: cursor,
	}
	h.sc.withholdResidue(first)

	// Act.
	_, shapes := h.sc.withholdResidue(second)

	// Assert.
	if len(shapes) != 1 {
		t.Fatalf("shapes = %d, want the observation still sent so the store's count rises", len(shapes))
	}
	if got := h.sc.newShapes["16777232:1"]; got != 1 {
		t.Fatalf("new_shapes = %d, want only the first sighting counted", got)
	}
}

func TestEachDistinctShapeCountsOnceAsNew(t *testing.T) {
	// Arrange.
	h := newHarness(t, nil)
	batch := &storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{
			residueEntryFor(t, "hook_success", `{"a":"x"}`),
			residueEntryFor(t, "hook_success", `{"b":1}`),
		},
		CursorAdvance: &storev1.CursorState{FileId: "16777232:1", Path: "/t/a.jsonl", Offset: 1},
	}

	// Act.
	h.sc.withholdResidue(batch)

	// Assert.
	if got := h.sc.newShapes["16777232:1"]; got != 2 {
		t.Fatalf("new_shapes = %d, want one per distinct key structure", got)
	}
}

func TestTheCatchupSummaryStatesTheNewShapeCount(t *testing.T) {
	// Arrange. The per-line withholding is DEBUG, so the summary is the only
	// place at INFO that says what the boot walk discovered.
	h := newHarness(t, nil)
	h.sc.withholdResidue(&storev1.EntryBatch{
		Entries: []*storev1.StoreEntry{residueEntryFor(t, "hook_success", `{"a":"x"}`)},
	})

	// Act.
	h.sc.summarizeWithheldResidue()

	// Assert.
	if !strings.Contains(h.logText(), "cataloguing 1 shape(s) this process had not seen before") {
		t.Fatalf("logs = %v, want the summary to state the new shape count", *h.logs)
	}
}

// --- a vanished file that had already given up everything it held -----------

// TestAVanishedFileIsWarnedAboutOnlyWhenSomethingWasOutstanding is the whole
// rule in one table: the WARNING exists for bytes that could have been lost, so
// it is spent only when bytes past the committed offset could have existed. A
// spool the vendor reaped after writing its terminator, and a file read to the
// last byte any poll ever saw, each gave up everything they held before they
// went.
func TestAVanishedFileIsWarnedAboutOnlyWhenSomethingWasOutstanding(t *testing.T) {
	cases := []struct {
		name       string
		content    string
		removeTree bool
		// spent makes every poll pass yield after one batch, as a pass whose
		// slice ran out does, so a file longer than one batch is left mid-tail.
		spent      bool
		wantReason string // "" = the record keeps its warning
	}{
		{
			name:       "the spool had already written its terminator",
			content:    "work\n[exited with code 0]\n",
			wantReason: reasonEnded,
		},
		{
			name:       "the committed offset is the size the last poll saw",
			content:    "hello\n",
			wantReason: reasonFullyRead,
		},
		{
			// One poll reads at most tail.MaxBatchBytes, and a pass whose slice
			// is spent yields before re-reading the rest, so a longer spool
			// leaves the tailer mid-tail: the last size it saw is past
			// everything it committed, and those bytes went with the file.
			name:    "bytes past the committed offset could have existed",
			content: strings.Repeat("a", 2*tail.MaxBatchBytes) + "\n",
			spent:   true,
		},
		{
			name:       "the whole directory went with it",
			content:    "hello\n",
			removeTree: true,
			wantReason: reasonTreeRemoved,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a claimed spool, read once, and then removed.
			h := newHarness(t, &fakeStore{})
			if tc.spent {
				h.sc.options.PollInterval = time.Nanosecond
			}
			spool := h.spoolFile(t, "b1", tc.content)
			h.sc.TaskSpawned("b1", "call-1", "", "", false, "/workspace", "workspace-id", "session-1")
			if err := h.sc.beginCycle(); err != nil {
				t.Fatalf("beginCycle: %v", err)
			}
			h.sc.pollAll()
			target := spool
			if tc.removeTree {
				target = filepath.Dir(spool)
			}
			if err := os.RemoveAll(target); err != nil {
				t.Fatalf("removing %s: %v", target, err)
			}

			// Act.
			h.sc.pollAll()

			// Assert.
			if tc.wantReason == "" {
				h.requireNone(t, "file-vanished", "info")
				h.requireOnce(t, "file-vanished", "warn")
				return
			}
			h.requireNone(t, "file-vanished", "warn")
			rec := h.requireOnce(t, "file-vanished", "info")
			if got := ctxString(t, rec, "reason"); got != tc.wantReason {
				t.Fatalf("the record's reason = %q, want %q", got, tc.wantReason)
			}
		})
	}
}

// projectsRoot creates the config root's projects directory, which exists on
// every real machine before the sidecar starts and is where the vendor puts a
// FRESH workspace's own directory. A probe candidate has to exist to be stat'd,
// so a subject about a directory appearing UNDER it stages it first.
func projectsRoot(t *testing.T, h *harness) string {
	t.Helper()
	dir := filepath.Join(h.rootA, "projects")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("creating %s: %v", dir, err)
	}
	return dir
}

// bumpProjectDir moves a directory's mtime by hand.
//
// THE PROBE'S WHOLE DECISION IS THAT MTIME, so a subject about the changed arm
// states the change explicitly rather than trusting the host filesystem's
// timestamp granularity to have noticed the write it just made.
func bumpProjectDir(t *testing.T, dir string) {
	t.Helper()
	info, err := os.Stat(dir)
	if err != nil {
		t.Fatalf("stat %s: %v", dir, err)
	}
	stamp := info.ModTime().Add(time.Second)
	if err := os.Chtimes(dir, stamp, stamp); err != nil {
		t.Fatalf("stamping %s: %v", dir, err)
	}
}

// TestTheChangeProbeWatchesANewTranscriptBeforeAnyRescan pins the whole point of
// the per-poll probe: the file the vendor wrote one instant after the last scan
// is read on the NEXT POLL, not thirty seconds later at the next rescan. That
// thirty seconds is what realtest 9 measured — the answer text of a fresh
// workspace's first turn reaching the store a full rescan after the turn ended.
func TestTheChangeProbeWatchesANewTranscriptBeforeAnyRescan(t *testing.T) {
	// Arrange: a cycle that has already scanned, and a transcript that appears
	// afterwards.
	store := &fakeStore{}
	h := newHarness(t, store)
	projects := projectsRoot(t, h)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	path := h.transcript(t, "sess-1", promptLine, assistantLine)
	bumpProjectDir(t, projects)

	// Act: one poll tick — the probe, then the read. No rescan runs.
	h.sc.discoverChanged()
	h.sc.pollAll()

	// Assert: the file is watched and its first records are already durable.
	if _, watched := h.sc.watchers[path]; !watched {
		t.Fatalf("the new transcript is not watched; watchers=%v", h.sc.watchers)
	}
	if len(store.writes) == 0 {
		t.Fatal("the newly discovered transcript's first records did not reach the store on the tick that found it")
	}
}

// TestTheChangeProbeStatesWhatItStartedWatching pins the one normal-verbosity
// record this path is allowed: a directory change that LED SOMEWHERE, naming the
// directory and how many files it put under a reader.
func TestTheChangeProbeStatesWhatItStartedWatching(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	projects := projectsRoot(t, h)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.transcript(t, "sess-1", promptLine)
	bumpProjectDir(t, projects)

	// Act.
	h.sc.discoverChanged()

	// Assert: the record names the directory the new file turned up IN, which is
	// the project directory the probe enumerated on the spot.
	rec := h.requireOnce(t, "discover-change", "info")
	if got, want := rec.Context["path"], normalized(filepath.Join(projects, "proj")); got != want {
		t.Fatalf("the record names path %v, want the changed directory %q", got, want)
	}
	if got, want := rec.Context["repeat_count"], float64(1); got != want {
		t.Fatalf("the record counts %v newly watched file(s), want %v", got, want)
	}
}

// TestAnIdleChangeProbeStatesNothing pins the other half: the probe runs once
// per second for the life of the process, so a tick that found nothing may not
// contribute a line to a normal-verbosity log.
func TestAnIdleChangeProbeStatesNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	h.transcript(t, "sess-1", promptLine)
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	*h.logs = nil

	// Act.
	h.sc.discoverChanged()

	// Assert.
	for _, rec := range h.records(t) {
		if rec.Verbosity != "verbose" {
			t.Fatalf("an idle probe wrote a non-verbose %s/%s record: %s", rec.Operation, rec.Level, rec.Message)
		}
	}
}

// TestTheChangeProbeRefusesToBuildATailerWithoutCursors pins that the probe is
// bound by the store-unreachable invariant exactly as the rescan is: it is the
// second caller of the only tailer-building path, and a second caller that
// forgot the assertion is the silent cold start the whole cycle exists to
// prevent.
func TestTheChangeProbeRefusesToBuildATailerWithoutCursors(t *testing.T) {
	// Arrange: production suspended, so there are no cursors.
	h := newHarness(t, nil)
	h.transcript(t, "sess-1", promptLine)

	// Act + Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("the change probe built tailers with no recovered cursors")
		}
	}()
	h.sc.discoverChanged()
}

// TestTheBootRewindIsForFilesThatCanCarryATurnInFlight pins the rewind's SCOPE.
// The joins it re-warms exist only for a turn this reader was half-way through,
// so a transcript that stopped growing long ago with its cursor already at its
// end has nothing to re-warm — and re-reading the whole historical corpus is
// what made a restart's catch-up take minutes while a live workspace waited.
func TestTheBootRewindIsForFilesThatCanCarryATurnInFlight(t *testing.T) {
	turn := int64(len(promptLine + "\n" + assistantLine + "\n"))
	tests := []struct {
		name       string
		age        time.Duration
		offset     int64
		wantOffset int64
	}{
		{
			// At rest: beyond the agent-silence window AND read to the end.
			name: "a transcript at rest is watched from its cursor with no re-read",
			age:  4 * time.Hour, offset: 2 * turn, wantOffset: 2 * turn,
		},
		{
			// Inside the silence window, so a turn of it may be in flight.
			name: "a transcript that grew recently is rewound to its turn start",
			age:  time.Minute, offset: 2 * turn, wantOffset: turn,
		},
		{
			// Durable bytes this reader never converted ARE the half-converted
			// turn, however long ago the file stopped growing.
			name: "a cursor behind the file's end is rewound however old the file is",
			age:  4 * time.Hour, offset: turn, wantOffset: 0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t, &fakeStore{})
			path := h.transcript(t, "sess-1", promptLine, assistantLine, promptLine, assistantLine)
			aged := h.clock.Add(-tc.age)
			if err := os.Chtimes(path, aged, aged); err != nil {
				t.Fatalf("stamping %s: %v", path, err)
			}
			h.store.cursors = []*storev1.CursorState{{
				FileId: identityOf(t, path), Path: path, Offset: tc.offset,
			}}

			// Act.
			if err := h.sc.beginCycle(); err != nil {
				t.Fatalf("beginCycle: %v", err)
			}

			// Assert.
			if got := h.sc.watchers[path].tailer.Offset(); got != tc.wantOffset {
				t.Fatalf("offset after the boot rewind = %d, want %d", got, tc.wantOffset)
			}
		})
	}
}

// TestTheBootRewindStatesWhatTheWalkReRead asserts the one INFO record that
// says how big the boot walk actually was. Both per-file decisions are verbose,
// so without this summary nothing at normal verbosity distinguishes a restart
// that re-read two transcripts from one that re-read two thousand.
func TestTheBootRewindStatesWhatTheWalkReRead(t *testing.T) {
	// Arrange: one transcript at rest and one still inside the silence window.
	h := newHarness(t, &fakeStore{})
	turn := int64(len(promptLine + "\n" + assistantLine + "\n"))
	cold := h.transcript(t, "sess-cold", promptLine, assistantLine)
	warm := h.transcript(t, "sess-warm", promptLine, assistantLine)
	aged := h.clock.Add(-4 * time.Hour)
	if err := os.Chtimes(cold, aged, aged); err != nil {
		t.Fatalf("stamping %s: %v", cold, err)
	}
	h.store.cursors = []*storev1.CursorState{
		{FileId: identityOf(t, cold), Path: cold, Offset: turn},
		{FileId: identityOf(t, warm), Path: warm, Offset: turn},
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act: the boot walk drains, which is the edge the summary is stated at.
	h.sc.pollAll()
	h.sc.endCatchupOnFirstDrainedPass()

	// Assert.
	record := h.requireOnce(t, "boot-rewind-summary", "info")
	if got := ctxInt(t, record, "repeat_count"); got != 2 {
		t.Fatalf("boot-rewind-summary repeat_count = %d, want both transcripts counted", got)
	}
}

// tickingClock is a fake clock that advances by a fixed step on every reading,
// which is how a subject exercises a time BOUND without waiting for one. Nothing
// here sleeps: the step is the only time that passes.
func tickingClock(start time.Time, step time.Duration) func() time.Time {
	now := start
	return func() time.Time {
		out := now
		now = now.Add(step)
		return out
	}
}

// TestAPollPassYieldsTheTickAtItsSliceBound pins the slice. A pass over the
// whole corpus is minutes of reading on a restart, and while it ran nothing else
// on the poll timer did — not the change probe, so no new transcript was found,
// and not a newly discovered file's first read. A pass that yields keeps both on
// their own one-second clock however large the corpus is.
func TestAPollPassYieldsTheTickAtItsSliceBound(t *testing.T) {
	// Arrange: more watchers than one slice can walk, and a clock that spends
	// 300ms of the 500ms slice on every reading.
	store := &fakeStore{}
	h := newHarness(t, store)
	for _, session := range []string{"s1", "s2", "s3", "s4", "s5", "s6"} {
		h.transcript(t, session, promptLine, assistantLine)
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.now = tickingClock(h.clock, 300*time.Millisecond)

	// Act: one tick.
	h.sc.pollAll()

	// Assert: the tick stopped short and the pass kept its place.
	if h.sc.pass == nil || len(h.sc.pass.pending) == 0 {
		t.Fatalf("one tick walked all %d watchers; the slice did not bound the pass", len(h.sc.watchers))
	}
	if h.sc.drainedPass {
		t.Fatal("a pass that stopped at its slice bound was latched as drained")
	}
}

// TestASlicedPassResumesUntilEveryWatcherIsWalked pins what the slice must NOT
// cost: `drainedPass` is a statement about a whole pass, and the startup
// catch-up window closes off it. Across slices it must fire once, when the last
// watcher has been polled, and each watcher must be walked exactly once.
func TestASlicedPassResumesUntilEveryWatcherIsWalked(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	h := newHarness(t, store)
	sessions := []string{"s1", "s2", "s3", "s4", "s5", "s6"}
	for _, session := range sessions {
		h.transcript(t, session, promptLine, assistantLine)
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.now = tickingClock(h.clock, 300*time.Millisecond)

	// Act: tick until the pass drains, bounded well above the slices it needs.
	ticks := 0
	for ; ticks < 20 && !h.sc.drainedPass; ticks++ {
		h.sc.pollAll()
		h.sc.endCatchupOnFirstDrainedPass()
	}

	// Assert.
	if !h.sc.drainedPass {
		t.Fatalf("the pass never drained across %d ticks", ticks)
	}
	if ticks < 2 {
		t.Fatalf("the pass drained in %d tick(s); the slice bound was never reached, so nothing about resuming was exercised", ticks)
	}
	walked := map[string]int{}
	for _, rec := range h.ops(t, "tail-pickup") {
		walked[ctxString(t, rec, "path")]++
	}
	if len(walked) != len(sessions) {
		t.Fatalf("%d of %d watchers were walked across the pass: %v", len(walked), len(sessions), walked)
	}
	for path, count := range walked {
		if count != 1 {
			t.Fatalf("%s was polled %d times in one pass, want exactly once", path, count)
		}
	}
	h.requireOnce(t, "catchup-end", "info")
}

// TestANewTranscriptIsReadWhileTheCorpusIsStillBeingWalked is the whole defect,
// end to end. Realtest 9, sweep rt-run37: a restart's boot walk ran from
// 23:44:21 to 23:46:45; a fresh workspace's turn concluded at 23:46:41 inside it
// and its answer rows reached the store at ~23:47:44, because the poll tick that
// would have found the file was inside the walk.
func TestANewTranscriptIsReadWhileTheCorpusIsStillBeingWalked(t *testing.T) {
	// Arrange: a corpus too large for one slice, mid-walk.
	store := &fakeStore{}
	h := newHarness(t, store)
	projects := projectsRoot(t, h)
	for _, session := range []string{"s1", "s2", "s3", "s4", "s5", "s6"} {
		h.transcript(t, session, promptLine, assistantLine)
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.now = tickingClock(h.clock, 300*time.Millisecond)
	h.sc.pollAll()
	if h.sc.drainedPass {
		t.Fatal("the corpus drained in one tick; the subject needs a walk still in progress")
	}

	// Act: the vendor writes a new transcript into a known project directory,
	// and two ordinary poll ticks run — probe, then read.
	fresh := h.transcript(t, "sess-fresh", promptLine, assistantLine)
	bumpProjectDir(t, projects)
	for tick := 0; tick < 2; tick++ {
		h.sc.discoverChanged()
		h.sc.pollAll()
	}

	// Assert: it was discovered and its records are durable, with the boot walk
	// still unfinished behind it.
	if _, watched := h.sc.watchers[fresh]; !watched {
		t.Fatal("the new transcript was not discovered while the corpus was being walked")
	}
	picked := false
	for _, rec := range h.ops(t, "tail-pickup") {
		if ctxString(t, rec, "path") == fresh {
			picked = true
		}
	}
	if !picked {
		t.Fatal("the new transcript's records did not reach the store within two poll ticks of it appearing")
	}
}

// --- bounded writes -----------------------------------------------------------

// boundedSpool arranges a claimed shell spool longer than two batches and
// drives one poll tick over it, answering the store and the spool's content.
func boundedSpool(t *testing.T) (*fakeStore, string) {
	t.Helper()
	store := &fakeStore{}
	h := newHarness(t, store)
	content := strings.Repeat("line of test output\n", (2*tail.MaxBatchBytes)/20+7)
	h.spoolFile(t, "b1", content)
	h.sc.TaskSpawned("b1", "call-1", "agent-1", "", false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.sc.pollAll()
	return store, content
}

// tailsOf answers the bash tails a store was handed, in write order.
func tailsOf(store *fakeStore) []*conversationv1.AgentBashTail {
	var out []*conversationv1.AgentBashTail
	for _, batch := range store.writes {
		for _, e := range batch.GetEntries() {
			if tail := e.GetAgentUpdate().GetBash().GetFrame().GetTail(); tail != nil {
				out = append(out, tail)
			}
		}
	}
	return out
}

func TestAClaimedSpoolLongerThanOneBatchIsWrittenInBoundedBatches(t *testing.T) {
	// Arrange, Act.
	store, _ := boundedSpool(t)

	// Assert: the file reached the store across several bounded batches.
	if len(store.writes) < 3 {
		t.Fatalf("the spool reached the store in %d write(s), want it split across at least 3 bounded batches", len(store.writes))
	}
}

func TestNoWriteOfALongSpoolStoresMoreThanTheRenderedTail(t *testing.T) {
	// Arrange, Act: a spool of more than two batches' bytes.
	store, _ := boundedSpool(t)

	// Assert: output beyond what is rendered is never stored, so no bash row
	// in any write carries more than the renderer's cap.
	limit := int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES)
	for i, batch := range store.writes {
		for _, e := range batch.GetEntries() {
			frame := e.GetAgentUpdate().GetBash().GetFrame()
			if n := len(frame.GetTail().GetText()); n > limit {
				t.Fatalf("write %d stores a %d-byte tail, past the %d-byte cap", i, n, limit)
			}
			if n := len(frame.GetSuccess().GetCompleted().GetOutput().GetText().GetStdout()); n > limit {
				t.Fatalf("write %d stores a %d-byte terminal output, past the %d-byte cap", i, n, limit)
			}
		}
	}
}

func TestAClaimedSpoolsLastTailIsTheFilesRenderedTail(t *testing.T) {
	// Arrange, Act.
	store, content := boundedSpool(t)

	// Assert: the newest tail accounts for every byte of the file and holds
	// its end, cut on a line start.
	tails := tailsOf(store)
	if len(tails) == 0 {
		t.Fatal("the spool produced no tail")
	}
	last := tails[len(tails)-1]
	if last.GetBytesOmitted()+uint64(len(last.GetText())) != uint64(len(content)) {
		t.Fatalf("the last tail accounts for %d+%d bytes, want the spool's %d", last.GetBytesOmitted(), len(last.GetText()), len(content))
	}
	if !strings.HasSuffix(content, last.GetText()) || !strings.HasPrefix(last.GetText(), "line of test output\n") {
		t.Fatalf("the last tail is not the file's end cut on a line start: %q", last.GetText()[:40])
	}
	if want := uint64(strings.Count(content[:last.GetBytesOmitted()], "\n")); last.GetLinesOmitted() != want {
		t.Fatalf("lines_omitted = %d, want %d", last.GetLinesOmitted(), want)
	}
}

func TestABoundedBatchIsReadAgainOnTheSameTick(t *testing.T) {
	// Arrange, Act: one pollAll.
	store, content := boundedSpool(t)

	// Assert: the pass re-read the file until it was drained, rather than
	// leaving the rest for later ticks.
	last := store.writes[len(store.writes)-1].GetCursorAdvance().GetOffset()
	if last != int64(len(content)) {
		t.Fatalf("one tick committed the cursor to %d, want the file's end %d", last, len(content))
	}
}

func TestASpentSliceYieldsABoundedFileToTheNextTick(t *testing.T) {
	// Arrange: every pass yields after one batch.
	store := &fakeStore{}
	h := newHarness(t, store)
	h.sc.options.PollInterval = time.Nanosecond
	h.spoolFile(t, "b1", strings.Repeat("x", 2*tail.MaxBatchBytes))
	h.sc.TaskSpawned("b1", "call-1", "agent-1", "", false, "/workspace", "workspace-id", "session-1")
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}

	// Act.
	h.sc.pollAll()

	// Assert: one bounded write, and the rest waits for the next tick.
	if len(store.writes) != 1 {
		t.Fatalf("a spent slice still made %d write(s), want 1", len(store.writes))
	}
	if got := store.writes[0].GetCursorAdvance().GetOffset(); got != tail.MaxBatchBytes {
		t.Fatalf("cursor = %d, want the first batch's bound %d", got, tail.MaxBatchBytes)
	}
}

// TestARescanInsideTheClearIntervalKeepsTheRememberedMisses pins the rescan's
// cost: an unconditional refresh dropped every miss, and the rekey after it
// re-globbed every shim directory once per watcher (~5 s of CPU per 30 s rescan
// on the owner's machine, 2026-09-24).
func TestARescanInsideTheClearIntervalKeepsTheRememberedMisses(t *testing.T) {
	// Arrange: a first pass over watched transcripts nothing links, so each id
	// is a remembered miss.
	h := newHarness(t, &fakeStore{})
	// Every watched MAIN transcript belongs to a live workspace and so is named
	// by an identity record; the watchers whose ids nothing links are claimed
	// spools whose spawner is an unrecorded agent.
	for i, spawner := range []string{"agent-1", "agent-2", "agent-3"} {
		task := fmt.Sprintf("b%dmiss", i)
		h.spoolFile(t, task, "work\n")
		h.sc.TaskSpawned(task, fmt.Sprintf("call-%d", i), spawner, "", false, "/workspace", "workspace-id", "session-1")
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(DefaultRescanInterval)
	before := h.sc.identity.Globs()

	// Act.
	h.sc.rescan()

	// Assert: a constant — the refresh's three record reads plus the rekey's
	// one fingerprint — and no per-watcher glob.
	if got := h.sc.identity.Globs() - before; got > 4 {
		t.Errorf("a rescan inside the clear interval ran %d glob(s), want at most the constant 4", got)
	}
}

// TestARescanPastTheClearIntervalDropsTheRememberedMisses keeps the net: at
// most every missClearInterval the rescan clears the misses outright, so a
// link write the directory fingerprint missed is found.
func TestARescanPastTheClearIntervalDropsTheRememberedMisses(t *testing.T) {
	// Arrange.
	h := newHarness(t, &fakeStore{})
	// Every watched MAIN transcript belongs to a live workspace and so is named
	// by an identity record; the watchers whose ids nothing links are claimed
	// spools whose spawner is an unrecorded agent.
	for i, spawner := range []string{"agent-1", "agent-2", "agent-3"} {
		task := fmt.Sprintf("b%dmiss", i)
		h.spoolFile(t, task, "work\n")
		h.sc.TaskSpawned(task, fmt.Sprintf("call-%d", i), spawner, "", false, "/workspace", "workspace-id", "session-1")
	}
	if err := h.sc.beginCycle(); err != nil {
		t.Fatalf("beginCycle: %v", err)
	}
	h.advance(missClearInterval)
	before := h.sc.identity.Globs()

	// Act.
	h.sc.rescan()

	// Assert: each of the three watchers' ids was asked of the disk again.
	if got := h.sc.identity.Globs() - before; got < 3+3 {
		t.Errorf("a rescan past the clear interval ran %d glob(s), want the refresh's 3 plus one per watcher", got)
	}
}
