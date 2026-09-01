package promptqueue

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/classifier"
	"claude-repld/internal/wsm"
)

// heldPrompt parks one prompt behind a running turn and returns the harness.
func heldPrompt(t *testing.T, h *harness, turn string, verdict classifier.Verdict) {
	t.Helper()
	h.judge.verdict = verdict
	if _, err := h.q.Submit(context.Background(), submission(idsTurn(turn), "the held prompt")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.q.waitForClassifications()
}

func TestReleaseDeliversAtOnceWhenNothingIsRunning(t *testing.T) {
	// Arrange: the prompt was held by the drain lease, which has since lifted.
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if started := h.sender.started(); len(started) != 1 || started[0] != "t1" {
		t.Fatalf("started = %v, want the released prompt", started)
	}
}

func TestReleaseRetiresTheHoldItDelivered(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.schedule = &wsm.DrainSchedule{SetAt: instant}
	h.lease(wsm.HolderDrain, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	h.clearLease()
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if h.db.hold("t1").Tombstone == nil {
		t.Fatal("a delivered hold must be retired")
	}
}

func TestReleaseInterruptsWhenDeliveryTakesOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	// Act
	if err := h.q.Release(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Release: %v", err)
	}
	// Assert
	if killed := h.sender.killed(); len(killed) != 1 || killed[0] != "running-turn" {
		t.Fatalf("killed = %v, want the running turn", killed)
	}
}

func TestReleaseRefusesAnUninterruptibleHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := h.q.SubmitSessionAct(context.Background(), theWorkspace, Act{Kind: ActClear}); err != nil {
		t.Fatalf("SubmitSessionAct: %v", err)
	}
	running(t, h, "running-turn", "/clear")
	heldPrompt(t, h, "t1", classifier.Verdict{})
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("err = %v, want ErrReleaseRefused", err)
	}
}

func TestReleaseRefusesAHoldWaitingOnASessionComingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.lease(wsm.HolderHibernate, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrReleaseRefused) {
		t.Fatalf("err = %v, want ErrReleaseRefused", err)
	}
}

func TestReleaseReportsAnUnknownHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	err := h.q.Release(context.Background(), theWorkspace, "never-held")
	// Assert
	if !errors.Is(err, ErrNoSuchHold) {
		t.Fatalf("err = %v, want ErrNoSuchHold", err)
	}
}

func TestDropRetiresTheHoldDurablyFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
	// Act
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}
	// Assert
	tomb := h.db.hold("t1").Tombstone
	if tomb == nil || tomb.Kind != tombstoneDropped {
		t.Fatalf("tombstone = %+v, want a dropped tombstone", tomb)
	}
}

func TestDropRefusesWhenTheDurableDropFails(t *testing.T) {
	// Arrange: a failed drop refuses the cancel rather than reporting a prompt
	// gone that is still standing.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
	h.db.tombstoneErr = errors.New("the database is read-only")
	// Act
	err := h.q.Drop(context.Background(), theWorkspace, "t1")
	// Assert
	if err == nil {
		t.Fatal("a failed durable drop must refuse the cancel")
	}
}

func TestDropRepublishesTheTray(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Reason: "independent"})
	before := h.holds.pushCount()
	// Act
	if err := h.q.Drop(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Drop: %v", err)
	}
	// Assert
	if h.holds.pushCount() <= before {
		t.Fatal("the tray must be republished after a drop")
	}
	if len(h.holds.last()) != 0 {
		t.Fatalf("tray = %v, want the dropped prompt gone", h.holds.last())
	}
}

func TestAcceptFlipsAHoldForTurnEndVerdict(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	// Act
	if err := h.q.Accept(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Accept: %v", err)
	}
	// Assert
	if !h.db.hold("t1").Accepted {
		t.Fatal("the acceptance must be recorded")
	}
}

func TestAcceptRefusesAnyOtherVerdict(t *testing.T) {
	// Arrange: a classification_error verdict is not a hold_for_turn_end.
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	h.judge.err = errors.New("the vendor run failed")
	heldPrompt(t, h, "t1", classifier.Verdict{})
	// Act
	err := h.q.Accept(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrAcceptNotApplicable) {
		t.Fatalf("err = %v, want ErrAcceptNotApplicable", err)
	}
}

func TestAcceptRefusesAHoldWithNoVerdictAtAll(t *testing.T) {
	// Arrange: a lease hold carries no classification.
	h := newHarness(t)
	h.lease(wsm.HolderRestart, wsm.PolicyHold)
	if _, err := h.q.Submit(context.Background(), submission("t1", "hello")); err != nil {
		t.Fatalf("Submit: %v", err)
	}
	// Act
	err := h.q.Accept(context.Background(), theWorkspace, "t1")
	// Assert
	if !errors.Is(err, ErrAcceptNotApplicable) {
		t.Fatalf("err = %v, want ErrAcceptNotApplicable", err)
	}
}

func TestAcceptRepublishesTheTray(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	before := h.holds.pushCount()
	// Act
	if err := h.q.Accept(context.Background(), theWorkspace, "t1"); err != nil {
		t.Fatalf("Accept: %v", err)
	}
	// Assert
	if h.holds.pushCount() <= before {
		t.Fatal("the tray must be republished after an accept")
	}
}

func TestAcceptSurfacesAFailedWrite(t *testing.T) {
	// Arrange
	h := newHarness(t)
	running(t, h, "running-turn", "the running work")
	heldPrompt(t, h, "t1", classifier.Verdict{Interject: false, Reason: "independent"})
	h.db.acceptErr = errors.New("the database is read-only")
	// Act
	err := h.q.Accept(context.Background(), theWorkspace, "t1")
	// Assert
	if err == nil {
		t.Fatal("a failed acceptance write must be surfaced")
	}
}
