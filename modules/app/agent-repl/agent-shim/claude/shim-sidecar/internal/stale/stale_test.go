package stale

import (
	"bytes"
	"io"
	"strings"
	"testing"
	"time"

	agentshimv1 "agentrepl/proto/agentshim/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

func testLog() *logging.Bound {
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	return log
}

const min = int64(60_000) // one minute in ms

// onlyLost asserts the sweep produced exactly one record and that it is an
// end-of-work carrying the LOST outcome, returning that outcome.
//
// The shape moved with the schema: a sweep now emits a conversation
// MessageEntry whose DetachedWorkEnded names the `lost` arm, in place of the
// retired Event/TaskEnded pair with its TERMINAL_STATUS_LOST enum.
func onlyLost(t *testing.T, entries []*agentshimv1.Entry) *conversationv1.DetachedLost {
	t.Helper()
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	msg := entries[0].GetExternal().GetMessage()
	if msg == nil {
		t.Fatalf("entry carries no conversation message: %+v", entries[0])
	}
	lost := msg.GetDetachedWorkEnded().GetLost()
	if lost == nil {
		t.Fatalf("entry is not a LOST end-of-work: %+v", msg)
	}
	return lost
}

// lostTaskID recovers the task a LOST record is about from the message id the
// card is derived from ("dw:"+task id).
func lostTaskID(t *testing.T, entry *agentshimv1.Entry) string {
	t.Helper()
	id := entry.GetExternal().GetMessage().GetMessageId()
	if !strings.HasPrefix(id, "dw:") {
		t.Fatalf("message_id = %q, want a dw: detached-work card id", id)
	}
	return strings.TrimPrefix(id, "dw:")
}

func TestInferredLostIsWarnBecauseTheUserReadsTheVerdict(t *testing.T) {
	// Arrange — a synthetic LOST draws the task as lost, never DONE.
	var seen []logging.Diagnostic
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(d logging.Diagnostic) { seen = append(seen, d) })
	tr := New(Options{Grace: 30 * time.Second}, log)
	tr.Open("b1", tail.KindShellSpool, "s1", "/p/b1.output", 1000, 1000)
	tr.MarkVanished("s1", "b1", 10_000)
	// Act
	tr.Sweep(10_000 + 30_000)
	// Assert
	var levels []string
	for _, d := range seen {
		if d.Operation == "infer-lost" {
			levels = append(levels, d.Level)
		}
	}
	if len(levels) != 1 || levels[0] != "warn" {
		t.Fatalf("infer-lost levels = %v, want exactly one warn", levels)
	}
}

func TestLostIsReadFromTheFilePlane(t *testing.T) {
	// Arrange — the sidecar only ever observes the file plane, and an inference
	// it draws from that observation is still a file-plane record. The old
	// PLANE_SYNTHETIC marking has no successor in the new Plane oneof; what
	// separates an inference from a reading now is its synthetic write id.
	tr := New(Options{Grace: 30 * time.Second}, testLog())
	tr.Open("b1", tail.KindShellSpool, "s1", "/p/b1.output", 1000, 1000)
	tr.MarkVanished("s1", "b1", 10_000)
	// Act
	entries := tr.Sweep(10_000 + 30_000)
	// Assert
	onlyLost(t, entries)
	if entries[0].GetInternal().GetPlane().GetFile() == nil {
		t.Fatalf("plane = %+v, want the file plane", entries[0].GetInternal().GetPlane())
	}
}

func TestVanishGraceEmitsLostAfterWindow(t *testing.T) {
	// Arrange
	tr := New(Options{Grace: 30 * time.Second}, testLog())
	tr.Open("b1", tail.KindShellSpool, "s1", "/p/b1.output", 1000, 1000)
	tr.MarkVanished("s1", "b1", 10_000)
	// Act: sweep before grace elapses.
	if entries := tr.Sweep(20_000); len(entries) != 0 {
		t.Fatalf("premature LOST: %+v", entries)
	}
	// Act: sweep after grace (10_000 + 30s).
	entries := tr.Sweep(10_000 + 30_000)
	// Assert
	lost := onlyLost(t, entries)
	if lost.GetInference() != "vanished-file" {
		t.Fatalf("inference = %q, want vanished-file", lost.GetInference())
	}
	if tr.IsOpen("s1", "b1") {
		t.Fatalf("task should be closed after LOST")
	}
}

func TestOpenRejectsIncompleteTaskIdentityLoudly(t *testing.T) {
	var logs bytes.Buffer
	log := logging.New(&logs, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	tr := New(Options{}, log)
	defer func() {
		recovered := recover()
		message, ok := recovered.(string)
		if !ok || !strings.Contains(message, "task identity is required") {
			t.Fatalf("Open panic = %v, want task identity invariant", recovered)
		}
		if !strings.Contains(logs.String(), `"operation":"stale-open"`) ||
			!strings.Contains(logs.String(), `"level":"error"`) ||
			!strings.Contains(logs.String(), "task identity is required") {
			t.Fatalf("canonical invariant log = %q", logs.String())
		}
	}()
	tr.Open("", tail.KindAgentTranscript, "s1", "/tmp/agent.jsonl", 1, 1)
}

func TestVanishThenPresentDoesNotLose(t *testing.T) {
	// Arrange
	tr := New(Options{Grace: 30 * time.Second}, testLog())
	tr.Open("a1", tail.KindAgentTranscript, "s1", "", 1000, 1000)
	tr.MarkVanished("s1", "a1", 10_000)
	// Act: the file reappears, then a sweep well past the grace window.
	tr.MarkPresent("s1", "a1")
	tr.Activity("s1", "a1", 15_000)
	entries := tr.Sweep(60_000)
	// Assert: no LOST (activity is recent, vanish cleared).
	if len(entries) != 0 {
		t.Fatalf("unexpected LOST after file reappeared: %+v", entries)
	}
}

func TestSilenceTimeoutPerKind(t *testing.T) {
	// Arrange: shell (30m) and agent (60m) tasks both silent for 40 minutes.
	tr := New(Options{}, testLog())
	tr.Open("b1", tail.KindShellSpool, "s1", "", 0, 0)
	tr.Open("a1", tail.KindAgentTranscript, "s1", "", 0, 0)
	now := 40 * min
	// Act
	entries := tr.Sweep(now)
	// Assert: only the shell task is LOST (past its 30m window); agent's 60m
	// window has not elapsed.
	lost := onlyLost(t, entries)
	if got := lostTaskID(t, entries[0]); got != "b1" || lost.GetInference() != "silence-timeout" {
		t.Fatalf("LOST task=%q inference=%q, want shell b1 silence-timeout", got, lost.GetInference())
	}
	if !tr.IsOpen("s1", "a1") {
		t.Fatalf("agent task should still be open at 40m")
	}
}

func TestBootSweepLosesPreBootTasks(t *testing.T) {
	// Arrange: one task started before boot, one after.
	tr := New(Options{}, testLog())
	boot := int64(100_000)
	tr.Open("old", tail.KindAgentTranscript, "s1", "", 50_000, 50_000)   // pre-boot
	tr.Open("new", tail.KindAgentTranscript, "s1", "", 150_000, 150_000) // post-boot
	// Act
	entries := tr.BootSweep(boot, 200_000)
	// Assert
	lost := onlyLost(t, entries)
	if got := lostTaskID(t, entries[0]); got != "old" || lost.GetInference() != "boot-sweep" {
		t.Fatalf("LOST task=%q inference=%q, want old boot-sweep", got, lost.GetInference())
	}
	if !tr.IsOpen("s1", "new") {
		t.Fatalf("post-boot task should survive the boot sweep")
	}
}

// openState builds a recovered open task carrying the ONLY field the schema
// still gives one: when it was last active.
func openState(lastActivityAtMs int64) *agentshimv1.OpenTaskState {
	return &agentshimv1.OpenTaskState{LastActivityAtMs: lastActivityAtMs}
}

func TestRestoreResetsTheTrackerToEmpty(t *testing.T) {
	// Arrange — `OpenTaskState.started` is gone with no successor, so a recovered
	// state can no longer name the task it is about. Restore therefore restores
	// NOTHING and resets, rather than inventing an identity the schema dropped.
	tr := New(Options{}, testLog())
	tr.Open("prior", tail.KindAgentTranscript, "s1", "/old", 10, 10)
	// Act
	if err := tr.Restore([]*agentshimv1.OpenTaskState{openState(55_000)}); err != nil {
		t.Fatalf("Restore: %v", err)
	}
	// Assert
	if tr.IsOpen("s1", "prior") {
		t.Fatal("Restore retained a pre-existing task; the store snapshot replaces tracker state")
	}
}

func TestRestoreLoudlyReportsTheUnrestorableOpenTasks(t *testing.T) {
	// Arrange — untracked work is silent data loss in the user's feed, so the
	// count that could not be restored is reported at error level.
	var logs bytes.Buffer
	log := logging.New(io.Discard, &logs).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	tr := New(Options{}, log)
	// Act
	if err := tr.Restore([]*agentshimv1.OpenTaskState{openState(1), openState(2)}); err != nil {
		t.Fatalf("Restore: %v", err)
	}
	// Assert
	if !strings.Contains(logs.String(), `"operation":"restore-open-tasks"`) ||
		!strings.Contains(logs.String(), `"level":"error"`) ||
		!strings.Contains(logs.String(), "no task identity to restore them by") {
		t.Fatalf("canonical unrestorable-open-tasks log = %q", logs.String())
	}
}

func TestRestoreOfAnEmptySnapshotReportsNothingUnrestorable(t *testing.T) {
	// Arrange — a store with no open tasks lost nothing, so the error-level
	// report must not fire.
	var logs bytes.Buffer
	log := logging.New(io.Discard, &logs).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	tr := New(Options{}, log)
	// Act
	if err := tr.Restore(nil); err != nil {
		t.Fatalf("Restore: %v", err)
	}
	// Assert
	if strings.Contains(logs.String(), "no task identity to restore them by") {
		t.Fatalf("empty snapshot reported unrestorable tasks: %q", logs.String())
	}
}

func TestRestoreValidationErrorsAreLoggedWithoutMutatingTracker(t *testing.T) {
	cases := []struct {
		name   string
		states []*agentshimv1.OpenTaskState
		cause  string
	}{
		{
			name:   "nil open task",
			states: []*agentshimv1.OpenTaskState{nil},
			cause:  "recovery contains a nil open task",
		},
		{
			name:   "unset last activity",
			states: []*agentshimv1.OpenTaskState{openState(0)},
			cause:  "invalid recovered open task last_activity_at_ms=0",
		},
		{
			name:   "negative last activity",
			states: []*agentshimv1.OpenTaskState{openState(-1)},
			cause:  "invalid recovered open task last_activity_at_ms=-1",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var global bytes.Buffer
			log := logging.New(io.Discard, &global).With(logging.Context{Component: "test"})
			log.SetDiagnosticSink(func(logging.Diagnostic) {})
			tr := New(Options{}, log)
			tr.Open("prior", tail.KindShellSpool, "s1", "", 10, 10)
			global.Reset()

			// Act
			err := tr.Restore(tc.states)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.cause) {
				t.Fatalf("Restore err = %v, want cause %q", err, tc.cause)
			}
			if !tr.IsOpen("s1", "prior") {
				t.Fatal("failed Restore mutated prior tracker state")
			}
			if !strings.Contains(global.String(), `"operation":"restore-open-tasks"`) ||
				!strings.Contains(global.String(), `"level":"error"`) ||
				!strings.Contains(global.String(), tc.cause) {
				t.Fatalf("canonical global validation log = %q", global.String())
			}
		})
	}
}

func TestRestoreRepeatedIdenticalFailureIsLoggedOnceUntilSuccess(t *testing.T) {
	// Arrange — establishment retries the same authoritative snapshot until the
	// store changes, so one canonical record is kept rather than one per retry.
	var global bytes.Buffer
	log := logging.New(io.Discard, &global).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	tr := New(Options{}, log)
	invalid := []*agentshimv1.OpenTaskState{openState(0)}

	// Act
	for attempt := 0; attempt < 4; attempt++ {
		if err := tr.Restore(invalid); err == nil {
			t.Fatalf("Restore attempt %d accepted invalid snapshot", attempt)
		}
	}

	// Assert
	if got := strings.Count(global.String(), "recovery validation failed"); got != 1 {
		t.Fatalf("repeated invalid snapshot logged %d validation failures, want 1", got)
	}
}

func TestRestoreLogsANewFailureAfterASuccessfulRecovery(t *testing.T) {
	// Arrange — a success clears the retained fingerprint, so the next failure is
	// a distinct occurrence rather than a suppressed repeat.
	var global bytes.Buffer
	log := logging.New(io.Discard, &global).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	tr := New(Options{}, log)
	invalid := []*agentshimv1.OpenTaskState{openState(0)}
	if err := tr.Restore(invalid); err == nil {
		t.Fatal("Restore accepted invalid snapshot")
	}
	if err := tr.Restore([]*agentshimv1.OpenTaskState{openState(50_000)}); err != nil {
		t.Fatalf("valid Restore: %v", err)
	}

	// Act
	if err := tr.Restore(invalid); err == nil {
		t.Fatal("Restore accepted invalid snapshot after successful recovery")
	}

	// Assert
	if got := strings.Count(global.String(), "recovery validation failed"); got != 2 {
		t.Fatalf("validation failures logged = %d, want 2 distinct occurrences", got)
	}
}

func TestLostCarriesStableSyntheticWriteIdentity(t *testing.T) {
	// Arrange — a task's LOST verdict is ONE fact however many processes infer
	// it, so two independent trackers must mint the same write id for it.
	sweep := func() *agentshimv1.Entry {
		tr := New(Options{Grace: time.Second}, testLog())
		tr.Open("b1", tail.KindShellSpool, "s1", "", 10, 10)
		tr.MarkVanished("s1", "b1", 20)
		entries := tr.Sweep(2_000)
		onlyLost(t, entries)
		return entries[0]
	}
	// Act
	first, second := sweep(), sweep()
	// Assert
	if got := first.GetInternal().GetWriteId(); got == "" || got != second.GetInternal().GetWriteId() {
		t.Fatalf("write_id = %q vs %q, want one stable non-empty identity", got, second.GetInternal().GetWriteId())
	}
}

func TestLostForTheSameTaskIdInSeparateSessionsIsADistinctRecord(t *testing.T) {
	// Arrange — task ids are only unique within a conversation, so the LOST
	// write identity is session-scoped or two sessions' verdicts collide.
	lostIn := func(session string) *agentshimv1.Entry {
		tr := New(Options{Grace: time.Second}, testLog())
		tr.Open("shared-id", tail.KindShellSpool, session, "", 10, 10)
		tr.MarkVanished(session, "shared-id", 20)
		entries := tr.Sweep(2_000)
		onlyLost(t, entries)
		return entries[0]
	}
	// Act
	a, b := lostIn("session-a"), lostIn("session-b")
	// Assert
	if a.GetInternal().GetWriteId() == b.GetInternal().GetWriteId() {
		t.Fatal("two sessions' LOST verdicts for the same task id share one write identity")
	}
}

func TestSweepOnlyClosesTheSessionItSwept(t *testing.T) {
	// Arrange — the tracker keys tasks by session AND id, so the same task id in
	// two sessions is two tasks.
	tr := New(Options{Grace: time.Second}, testLog())
	tr.Open("shared-id", tail.KindShellSpool, "session-a", "", 1, 1)
	tr.Open("shared-id", tail.KindAgentTranscript, "session-b", "", 1, 1)
	tr.MarkVanished("session-a", "shared-id", 1)
	// Act
	entries := tr.Sweep(2_000)
	// Assert
	onlyLost(t, entries)
	if got := entries[0].GetExternal().GetSessionId(); got != "session-a" {
		t.Fatalf("LOST session = %q, want session-a", got)
	}
	if !tr.IsOpen("session-b", "shared-id") {
		t.Fatal("sweeping session-a's task closed session-b's same-id task")
	}
}

func TestCloseRemovesTaskFromSweep(t *testing.T) {
	// Arrange: a task that reached a real terminal elsewhere.
	tr := New(Options{Grace: time.Second}, testLog())
	tr.Open("b1", tail.KindShellSpool, "s1", "", 0, 0)
	tr.MarkVanished("s1", "b1", 0)
	tr.Close("s1", "b1")
	// Act: a sweep long after the grace window.
	entries := tr.Sweep(10 * min)
	// Assert: no LOST (a closed task is never swept).
	if len(entries) != 0 {
		t.Fatalf("closed task was LOST: %+v", entries)
	}
}

func TestActivityResetsSilence(t *testing.T) {
	// Arrange: a shell task kept alive by activity just under the window.
	tr := New(Options{}, testLog())
	tr.Open("b1", tail.KindShellSpool, "s1", "", 0, 0)
	tr.Activity("s1", "b1", 20*min)
	// Act: sweep at 40m — only 20m since the last activity (< 30m window).
	entries := tr.Sweep(40 * min)
	// Assert
	if len(entries) != 0 {
		t.Fatalf("LOST despite recent activity: %+v", entries)
	}
}
