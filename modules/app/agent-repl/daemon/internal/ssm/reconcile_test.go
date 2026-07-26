package ssm

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func TestReconcileSweepsGhostStarts(t *testing.T) {
	// Arrange: two tasks started, neither ended; the session says only one runs.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	mustApply(t, m, evTaskStarted("s1", 3, "ghost"))

	// Act.
	if err := m.ReconcileTasks("s1", []string{"a1"}); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetLiveTaskCount(); got != 1 {
		t.Fatalf("live_task_count = %d, want 1", got)
	}
}

func TestReconcileSettlesOrphanEndsFromReplayedHistory(t *testing.T) {
	// Arrange: the 2026-07-26 shape — a cursor-recovery failure re-read every
	// transcript from offset 0, so historical task_ended events landed with no
	// logged task_started and the count went IMPOSSIBLY negative.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	for i, id := range []string{"h1", "h2", "h3"} {
		mustApply(t, m, evTaskEnded("s1", uint64(2+i), id, corev1.TerminalStatus_TERMINAL_STATUS_DONE))
	}

	// Act: the session reports nothing running.
	if err := m.ReconcileTasks("s1", nil); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetLiveTaskCount(); got != 0 {
		t.Fatalf("live_task_count = %d, want 0", got)
	}
}

func TestReconcileStopsTheImpossibleCountFromRecurring(t *testing.T) {
	// Arrange: orphan ends, reconciled once.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskEnded("s1", 2, "h1", corev1.TerminalStatus_TERMINAL_STATUS_DONE))
	if err := m.ReconcileTasks("s1", nil); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}
	before := cl.count("IMPOSSIBLE live_task_count")

	// Act: resolve again — the clamp's cause is gone, so it must not fire.
	mustCurrent(t, m, "ws1")

	// Assert.
	if got := cl.count("IMPOSSIBLE live_task_count"); got != before {
		t.Fatalf("IMPOSSIBLE clamp fired %d more time(s) after reconciliation, want 0", got-before)
	}
}

func TestReconcileAdoptsTasksItNeverSawStart(t *testing.T) {
	// Arrange: a task began before this daemon was watching.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))

	// Act.
	if err := m.ReconcileTasks("s1", []string{"unseen-1", "unseen-2"}); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetLiveTaskCount(); got != 2 {
		t.Fatalf("live_task_count = %d, want 2", got)
	}
}

func TestReconcileMakesTheCountMatchTheAuthoritativeSetExactly(t *testing.T) {
	// Arrange: every failure direction at once — a ghost, an orphan end, an
	// honest live task, and one the daemon never saw start.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "live"))
	mustApply(t, m, evTaskStarted("s1", 3, "ghost"))
	mustApply(t, m, evTaskEnded("s1", 4, "orphan", corev1.TerminalStatus_TERMINAL_STATUS_DONE))

	// Act.
	if err := m.ReconcileTasks("s1", []string{"live", "unseen"}); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetLiveTaskCount(); got != 2 {
		t.Fatalf("live_task_count = %d, want 2 (the authoritative set's size)", got)
	}
}

func TestReconcileIsIdempotent(t *testing.T) {
	// Arrange: a set already reconciled once.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	if err := m.ReconcileTasks("s1", []string{"a1"}); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Act.
	if err := m.ReconcileTasks("s1", []string{"a1"}); err != nil {
		t.Fatalf("ReconcileTasks (again): %v", err)
	}

	// Assert.
	if got := mustCurrent(t, m, "ws1").GetLiveTaskCount(); got != 1 {
		t.Fatalf("live_task_count = %d, want 1", got)
	}
}

func TestReconcileSurfacesAnUnrepresentableRelive(t *testing.T) {
	// Arrange: the set claims a task is live whose task_ended is already logged.
	// An append-only DISTINCT count cannot re-open it, so it must be said out
	// loud rather than silently ignored.
	m, cl, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	mustApply(t, m, evTaskEnded("s1", 3, "a1", corev1.TerminalStatus_TERMINAL_STATUS_DONE))

	// Act.
	if err := m.ReconcileTasks("s1", []string{"a1"}); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert.
	if !cl.contains("task_ended is already logged") {
		t.Fatal("an unrepresentable re-live must be loud-logged")
	}
}

func TestReconcilePushesTheSweptCount(t *testing.T) {
	// Arrange: a ghost is inflating the workspace's live-task figure.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "ghost"))
	ch, _ := m.Subscribe()

	// Act.
	if err := m.ReconcileTasks("s1", nil); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert: the frontend is TOLD the ghost is gone.
	select {
	case msg := <-ch:
		if msg.GetLiveTaskCount() != 0 {
			t.Fatalf("pushed live_task_count = %d, want 0", msg.GetLiveTaskCount())
		}
	default:
		t.Fatal("a swept ghost must be pushed, or the stale count stays on screen")
	}
}

func TestReconcileWithNothingToDoStaysQuiet(t *testing.T) {
	// Arrange: the log already agrees with the session.
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	mustApply(t, m, evSessionStarted("s1", 1))
	mustApply(t, m, evTaskStarted("s1", 2, "a1"))
	ch, _ := m.Subscribe()

	// Act.
	if err := m.ReconcileTasks("s1", []string{"a1"}); err != nil {
		t.Fatalf("ReconcileTasks: %v", err)
	}

	// Assert.
	select {
	case msg := <-ch:
		t.Fatalf("unexpected push for an agreeing reconciliation: live_tasks=%d", msg.GetLiveTaskCount())
	default:
	}
}

func TestReconcileRejectsAnEmptySessionID(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act
	err := m.ReconcileTasks("", []string{"a1"})
	// Assert
	if err == nil {
		t.Fatal("ReconcileTasks with no session id must error")
	}
}

func TestReconcileRejectsAnUnboundSession(t *testing.T) {
	// Arrange
	m, _, _ := openTest(t, fakeResolver{"s1": "ws1"})
	// Act
	err := m.ReconcileTasks("unknown", []string{"a1"})
	// Assert
	if err == nil {
		t.Fatal("ReconcileTasks for an unbound session must error")
	}
}
