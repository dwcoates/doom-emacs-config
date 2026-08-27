package stale

import (
	"bytes"
	"io"
	"strings"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-claude-sidecar/internal/convert"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
	"google.golang.org/protobuf/types/known/structpb"
)

func testLog() *logging.Bound {
	log := logging.New(io.Discard, io.Discard).With(logging.Context{Component: "test"})
	log.SetDiagnosticSink(func(logging.Diagnostic) {})
	return log
}

const min = int64(60_000) // one minute in ms

// onlyLost asserts the sweep produced exactly one record and that it is a LOST
// verdict, returning the body that carries the inference.
//
// THE SHAPE MOVED WITH THE SCHEMA, TWICE. It was a retired Event/TaskEnded pair
// with a TERMINAL_STATUS_LOST enum; then a conversation MessageEntry whose
// DetachedWorkEnded named the `lost` arm; and now — with that whole record model
// deleted and no successor a file reader can mint — an UNPORTED record filed
// under the `detached_lost` discriminator, carrying the task and the inference
// verbatim. WHAT IS ASSERTED IS UNCHANGED: exactly one record, and it says LOST.
func onlyLost(t *testing.T, entries []*storev1.StoreEntry) *structpb.Struct {
	t.Helper()
	if len(entries) != 1 {
		t.Fatalf("entries = %d, want 1", len(entries))
	}
	unknown := entries[0].GetAgentUpdate().GetUnservedItem().GetUnknown()
	if unknown.GetDiscriminatorField() != convert.UnportedField || unknown.GetDiscriminator() != "detached_lost" {
		t.Fatalf("entry is not a LOST verdict: %+v", entries[0])
	}
	return unknown.GetRaw()
}

// lostTaskID recovers the task a LOST record is about.
func lostTaskID(t *testing.T, entry *storev1.StoreEntry) string {
	t.Helper()
	raw := entry.GetAgentUpdate().GetUnservedItem().GetUnknown().GetRaw()
	id := raw.GetFields()["task_id"].GetStringValue()
	if id == "" {
		t.Fatalf("LOST record names no task: %+v", entry)
	}
	return id
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
	if entries[0].GetPlane().GetFile() == nil {
		t.Fatalf("plane = %+v, want the file plane", entries[0].GetPlane())
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
	if lost.GetFields()["inference"].GetStringValue() != "vanished-file" {
		t.Fatalf("inference = %q, want vanished-file", lost.GetFields()["inference"].GetStringValue())
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
	if got := lostTaskID(t, entries[0]); got != "b1" || lost.GetFields()["inference"].GetStringValue() != "silence-timeout" {
		t.Fatalf("LOST task=%q inference=%q, want shell b1 silence-timeout", got, lost.GetFields()["inference"].GetStringValue())
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
	if got := lostTaskID(t, entries[0]); got != "old" || lost.GetFields()["inference"].GetStringValue() != "boot-sweep" {
		t.Fatalf("LOST task=%q inference=%q, want old boot-sweep", got, lost.GetFields()["inference"].GetStringValue())
	}
	if !tr.IsOpen("s1", "new") {
		t.Fatalf("post-boot task should survive the boot sweep")
	}
}

func TestRestoreResetsTheTrackerToEmpty(t *testing.T) {
	// Arrange — the store reports no open tasks at all now, so a recovered
	// state can no longer name the task it is about. Restore therefore restores
	// NOTHING and resets, rather than inventing an identity the schema dropped.
	tr := New(Options{}, testLog())
	tr.Open("prior", tail.KindAgentTranscript, "s1", "/old", 10, 10)
	// Act
	if err := tr.Restore(); err != nil {
		t.Fatalf("Restore: %v", err)
	}
	// Assert
	if tr.IsOpen("s1", "prior") {
		t.Fatal("Restore retained a pre-existing task; the store snapshot replaces tracker state")
	}
}

func TestLostCarriesStableSyntheticWriteIdentity(t *testing.T) {
	// Arrange — a task's LOST verdict is ONE fact however many processes infer
	// it, so two independent trackers must mint the same write id for it.
	sweep := func() *storev1.StoreEntry {
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
	if got := first.GetWriteId(); got == "" || got != second.GetWriteId() {
		t.Fatalf("write_id = %q vs %q, want one stable non-empty identity", got, second.GetWriteId())
	}
}

func TestLostForTheSameTaskIdInSeparateSessionsIsADistinctRecord(t *testing.T) {
	// Arrange — task ids are only unique within a conversation, so the LOST
	// write identity is session-scoped or two sessions' verdicts collide.
	lostIn := func(session string) *storev1.StoreEntry {
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
	if a.GetWriteId() == b.GetWriteId() {
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
	// THE SESSION IS NO LONGER ASSERTABLE ON THE RECORD. It sat on protocol.v1
	// ExternalEntry.session_id, which store.v1 StoreEntry does not carry in any
	// form, so the only remaining evidence that the right session was swept is
	// that the other session's same-id task survived — which is what this checks.
	if got := lostTaskID(t, entries[0]); got != "shared-id" {
		t.Fatalf("LOST task = %q, want shared-id", got)
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
