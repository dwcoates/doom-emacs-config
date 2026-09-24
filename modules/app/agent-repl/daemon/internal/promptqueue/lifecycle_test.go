package promptqueue

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/classifier"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestOnTurnEndedStampsTheTurnsClose(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseFailed)
	// Assert
	if got := h.db.closedTurns["running-turn"]; got != wsm.CloseFailed {
		t.Fatalf("close = %s, want failed", closeName(got))
	}
}

// TestOnTurnEndedWithNothingHeldIsRecordedAtInfo covers the turn end that
// delivers nothing: it still leaves an INFO record, so a turn end reaching the
// queue is always visible on disk and "the queue was never told" cannot be
// confused with "the queue was told and had nothing to do".
func TestOnTurnEndedWithNothingHeldIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	beforeRecords := len(h.log.Records())
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	for _, record := range h.log.Records()[beforeRecords:] {
		if record.Level == "info" && record.Operation == "daemon.promptqueue.turn_ended" {
			return
		}
	}
	t.Fatalf("records = %+v, want an info daemon.promptqueue.turn_ended", h.log.Records()[beforeRecords:])
}

func TestOnTurnEndedClearsTheInterruptingStatus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseKilled)
	// Assert
	got := h.footer.interruptions()
	if len(got) == 0 || got[len(got)-1] {
		t.Fatalf("footer = %v, want the interrupting status cleared", got)
	}
}

func TestOnTurnEndedDeliversTheNextHeldPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the held prompt", started)
	}
}

func TestOnTurnEndedDeliversTheOldestHoldFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	h.q.deps.Now = func() time.Time { return instant }
	if _, err := h.q.Submit(context.Background(), submission("older", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.deps.Now = func() time.Time { return instant.Add(time.Minute) }
	if _, err := h.q.Submit(context.Background(), submission("newer", "second")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "older" {
		t.Fatalf("started = %v, want the oldest hold first", started)
	}
}

func TestOnTurnEndedLeavesAHoldADaemonConditionStillHolds(t *testing.T) {
	// Arrange: a drain hold is not released by a turn ending.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)
	// Assert
	if len(h.sender.started()) != 0 {
		t.Fatal("a prompt the drain still holds must not be delivered")
	}
}

func TestOnLeaseChangedStampsANewHoldOnEveryStandingPrompt(t *testing.T) {
	// Arrange: a prompt held for the running turn, then a drain lease arrives.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	held := h.db.hold("t1")
	if held.Hold == nil || *held.Hold != wsm.HoldShutdown {
		t.Fatalf("hold = %+v, want a shutdown hold", held.Hold)
	}
}

func TestOnLeaseChangedReleasesAHoldTheLeaseNoLongerProjects(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if h.db.hold("t1").Hold != nil {
		t.Fatal("a hold the lease no longer projects must be released")
	}
}

func TestOnLeaseChangedDeliversAReleasedHoldWhenNothingIsRunning(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the released hold delivered", started)
	}
}

func TestOnLeaseChangedHoldsBackWhileATurnStillRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	h.watcher.running("running-turn")
	// Act
	h.q.OnLeaseChanged(theWorkspace)
	// Assert
	if len(h.sender.started()) != 0 {
		t.Fatal("a released hold waits for the running turn to end")
	}
}

func TestRestoreHoldsPublishesEveryStandingHoldPerWorkspace(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.holds.pushes = nil
	// Act
	if err := h.q.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}
	// Assert
	if len(h.holds.last()) != 1 {
		t.Fatalf("tray = %v, want the restored hold", h.holds.last())
	}
}

func TestRestoreHoldsIsAllOrNothing(t *testing.T) {
	// Arrange: a corrupt row fails the read, and nothing is loaded.
	h := newHarness(t)
	h.db.allHeldErr = errors.New("a held-prompt row does not decode")
	// Act
	err := h.q.RestoreHolds(context.Background())
	// Assert
	if err == nil {
		t.Fatal("a failed restore must be surfaced")
	}
	if h.holds.pushCount() != 0 {
		t.Fatal("nothing may be published when the restore failed")
	}
}

func TestRestoreHoldsClosesTheOrphanedTurnsOfADeadSession(t *testing.T) {
	// Arrange: a turn was in flight and the workspace has no session left.
	h := newHarness(t)
	if err := h.db.PutTurn(context.Background(), wsm.Turn{
		ID: "running-turn", Workspace: theWorkspace, Text: "work", StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	h.noSession = true
	// Act
	if err := h.q.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}
	// Assert
	if len(h.db.orphaned) != 1 || h.db.orphaned[0] != theWorkspace {
		t.Fatalf("orphaned = %v, want the dead session's workspace", h.db.orphaned)
	}
}

func TestRestoreHoldsLeavesALiveSessionsTurnsToTheWatcher(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.db.PutTurn(context.Background(), wsm.Turn{
		ID: "running-turn", Workspace: theWorkspace, Text: "work", StartedAt: instant,
	}); err != nil {
		t.Fatalf("PutTurn: %v", err)
	}
	// Act
	if err := h.q.RestoreHolds(context.Background()); err != nil {
		t.Fatalf("RestoreHolds: %v", err)
	}
	// Assert
	if len(h.db.orphaned) != 0 {
		t.Fatal("a live session's in-flight turn is the watcher's, not an orphan")
	}
}

func TestNextDeliverablePrefersTheSemanticHead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.verdict = classifier.Verdict{Interject: false, Reason: "independent"}
	if _, err := h.q.Submit(context.Background(), submission("older", "first")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	head := ids.TurnID("older")
	if _, err := h.q.Submit(context.Background(), submission("newer", "second")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
	h.q.state(theWorkspace).head = &head
	// Act
	got, ok, _, err := h.q.nextDeliverable(context.Background(), theWorkspace)
	// Assert
	if err != nil || !ok {
		t.Fatalf("nextDeliverable: %v, ok=%v", err, ok)
	}
	if got.Turn != head {
		t.Fatalf("next = %q, want the semantic head", got.Turn)
	}
}

// TestTheTurnEndsDrainExcludesALeaseChangeForTheWholeDelivery covers the
// serialization the handover's in-order intake drain rests on: a turn's end
// and a lease change both deliver from the same standing holds, so a quiesce
// that arrived while the turn end was still delivering would read the holds
// the delivery has not yet retired and let the intake out of order.
func TestTheTurnEndsDrainExcludesALeaseChangeForTheWholeDelivery(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	free := true
	h.sender.startHook = func() { free = h.q.state(theWorkspace).drain.TryLock() }

	// Act
	h.watcher.idle()
	h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseCompleted)

	// Assert
	if free {
		t.Fatal("the workspace's drain was free while the turn end's own delivery was in flight")
	}
}
