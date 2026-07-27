package stale

import (
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

const min = int64(60_000) // one minute in ms

func onlyLost(t *testing.T, evs []*corev1.Event) *corev1.TaskEnded {
	t.Helper()
	if len(evs) != 1 {
		t.Fatalf("events = %d, want 1", len(evs))
	}
	te := evs[0].GetTaskEnded()
	if te == nil {
		t.Fatalf("event is not a TaskEnded: %+v", evs[0])
	}
	if te.GetStatus() != corev1.TerminalStatus_TERMINAL_STATUS_LOST {
		t.Fatalf("status = %v, want LOST", te.GetStatus())
	}
	if evs[0].GetPlane() != corev1.Plane_PLANE_SYNTHETIC {
		t.Fatalf("plane = %v, want SYNTHETIC", evs[0].GetPlane())
	}
	return te
}

func TestVanishGraceEmitsLostAfterWindow(t *testing.T) {
	// Arrange
	tr := New(Options{Grace: 30 * time.Second}, nil)
	tr.Open("b1", tail.KindShellSpool, "s1", "/p/b1.output", 1000, 1000)
	tr.MarkVanished("b1", 10_000)
	// Act: sweep before grace elapses.
	if evs := tr.Sweep(20_000); len(evs) != 0 {
		t.Fatalf("premature LOST: %+v", evs)
	}
	// Act: sweep after grace (10_000 + 30s).
	evs := tr.Sweep(10_000 + 30_000)
	// Assert
	te := onlyLost(t, evs)
	if te.GetInference() != "vanished-file" {
		t.Fatalf("inference = %q, want vanished-file", te.GetInference())
	}
	if tr.IsOpen("b1") {
		t.Fatalf("task should be closed after LOST")
	}
}

func TestVanishThenPresentDoesNotLose(t *testing.T) {
	// Arrange
	tr := New(Options{Grace: 30 * time.Second}, nil)
	tr.Open("a1", tail.KindAgentTranscript, "s1", "", 1000, 1000)
	tr.MarkVanished("a1", 10_000)
	// Act: the file reappears, then a sweep well past the grace window.
	tr.MarkPresent("a1")
	tr.Activity("a1", 15_000)
	evs := tr.Sweep(60_000)
	// Assert: no LOST (activity is recent, vanish cleared).
	if len(evs) != 0 {
		t.Fatalf("unexpected LOST after file reappeared: %+v", evs)
	}
}

func TestSilenceTimeoutPerKind(t *testing.T) {
	// Arrange: shell (30m) and agent (60m) tasks both silent for 40 minutes.
	tr := New(Options{}, nil)
	tr.Open("b1", tail.KindShellSpool, "s1", "", 0, 0)
	tr.Open("a1", tail.KindAgentTranscript, "s1", "", 0, 0)
	now := 40 * min
	// Act
	evs := tr.Sweep(now)
	// Assert: only the shell task is LOST (past its 30m window); agent's 60m
	// window has not elapsed.
	te := onlyLost(t, evs)
	if te.GetTaskId() != "b1" || te.GetInference() != "silence-timeout" {
		t.Fatalf("LOST = %+v, want shell b1 silence-timeout", te)
	}
	if !tr.IsOpen("a1") {
		t.Fatalf("agent task should still be open at 40m")
	}
}

func TestBootSweepLosesPreBootTasks(t *testing.T) {
	// Arrange: one task started before boot, one after.
	tr := New(Options{}, nil)
	boot := int64(100_000)
	tr.Open("old", tail.KindAgentTranscript, "s1", "", 50_000, 50_000)   // pre-boot
	tr.Open("new", tail.KindAgentTranscript, "s1", "", 150_000, 150_000) // post-boot
	// Act
	evs := tr.BootSweep(boot, 200_000)
	// Assert
	te := onlyLost(t, evs)
	if te.GetTaskId() != "old" || te.GetInference() != "boot-sweep" {
		t.Fatalf("LOST = %+v, want old boot-sweep", te)
	}
	if !tr.IsOpen("new") {
		t.Fatalf("post-boot task should survive the boot sweep")
	}
}

func TestRestoreReplacesArtifactDerivedStateWithPersistedOpenTasks(t *testing.T) {
	tr := New(Options{}, nil)
	tr.Open("artifact-only", tail.KindAgentTranscript, "s1", "/old", 10, 10)
	start := recoveredStart("s2", "persisted-open", corev1.TaskKind_TASK_KIND_WORKFLOW, 50_000, 55_000)

	if err := tr.Restore([]*corev1.OpenTaskState{start}); err != nil {
		t.Fatalf("Restore: %v", err)
	}
	if tr.IsOpen("artifact-only") {
		t.Fatal("Restore retained artifact-only task; store snapshot must replace tracker state")
	}
	if !tr.IsOpen("persisted-open") {
		t.Fatal("Restore did not open persisted task")
	}
}

func TestRestoreRejectsMalformedSnapshotWithoutMutatingTracker(t *testing.T) {
	tr := New(Options{}, nil)
	tr.Open("prior", tail.KindShellSpool, "s1", "", 10, 10)

	err := tr.Restore([]*corev1.OpenTaskState{
		recoveredStart("", "bad", corev1.TaskKind_TASK_KIND_SHELL, 50_000, 50_000),
	})
	if err == nil {
		t.Fatal("Restore accepted TaskStarted with no session")
	}
	if !tr.IsOpen("prior") {
		t.Fatal("failed Restore mutated prior tracker state")
	}
}

func TestRestoreUsesPersistedLastActivityForSilence(t *testing.T) {
	tr := New(Options{}, nil)
	state := recoveredStart("s1", "b1", corev1.TaskKind_TASK_KIND_SHELL, 1, 25*min)
	if err := tr.Restore([]*corev1.OpenTaskState{state}); err != nil {
		t.Fatalf("Restore: %v", err)
	}
	if evs := tr.Sweep(40 * min); len(evs) != 0 {
		t.Fatalf("task LOST 15m after persisted activity: %+v", evs)
	}
}

func TestLostCarriesStableSyntheticDedupIdentity(t *testing.T) {
	tr := New(Options{Grace: time.Second}, nil)
	tr.Open("b1", tail.KindShellSpool, "s1", "", 10, 10)
	tr.MarkVanished("b1", 20)
	evs := tr.Sweep(2_000)
	onlyLost(t, evs)
	if got := evs[0].GetDedupKey(); got != "task-lost:b1" {
		t.Fatalf("LOST dedup_key = %q, want task-lost:b1", got)
	}
}

func recoveredStart(session, taskID string, kind corev1.TaskKind, startedAt, lastActivityAt int64) *corev1.OpenTaskState {
	return &corev1.OpenTaskState{
		LastActivityAtMs: lastActivityAt,
		Started: &corev1.Event{
			SessionId: session, Seq: 1, ProducedAtMs: startedAt,
			Plane: corev1.Plane_PLANE_FILE, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT,
			Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{
				TaskId: taskID, Kind: kind, OutputPath: "/tmp/" + taskID,
			}},
		},
	}
}

func TestCloseRemovesTaskFromSweep(t *testing.T) {
	// Arrange: a task that reached a real terminal elsewhere.
	tr := New(Options{Grace: time.Second}, nil)
	tr.Open("b1", tail.KindShellSpool, "s1", "", 0, 0)
	tr.MarkVanished("b1", 0)
	tr.Close("b1")
	// Act: a sweep long after the grace window.
	evs := tr.Sweep(10 * min)
	// Assert: no LOST (a closed task is never swept).
	if len(evs) != 0 {
		t.Fatalf("closed task was LOST: %+v", evs)
	}
}

func TestActivityResetsSilence(t *testing.T) {
	// Arrange: a shell task kept alive by activity just under the window.
	tr := New(Options{}, nil)
	tr.Open("b1", tail.KindShellSpool, "s1", "", 0, 0)
	tr.Activity("b1", 20*min)
	// Act: sweep at 40m — only 20m since the last activity (< 30m window).
	evs := tr.Sweep(40 * min)
	// Assert
	if len(evs) != 0 {
		t.Fatalf("LOST despite recent activity: %+v", evs)
	}
}
