package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"testing"

	"claude-repld/internal/shimclient"
)

// A user's stop that reached the shim and lost only its answer. The shim ran the
// interrupt; the daemon has its fate and not its outcome.

// unackedHarness is a queue harness whose fake shim fails the interrupt the way
// a connection dying mid-exchange does.
func unackedHarness(t *testing.T, logf func(string, ...any)) *queueHarness {
	t.Helper()
	h := newQueueHarnessWithPusher(t, nil, nil, logf)
	h.client.mu.Lock()
	h.client.interruptErr = fmt.Errorf("%w: request_id=daemon-interrupt-1: shim connection closed", shimclient.ErrDeliveredUnacked)
	h.client.mu.Unlock()
	return h
}

func TestDeliveredUnackedInterruptPausesTheQueue(t *testing.T) {
	// Arrange — the user asked the work to stop and the request reached the
	// agent; the next held prompt must not start it again.
	h := unackedHarness(t, nil)

	// Act.
	_ = h.interrupt()

	// Assert.
	if !h.paused() {
		t.Fatal("queue paused = false, want the pause that follows from the stop having been delivered")
	}
}

func TestDeliveredUnackedInterruptStillReportsTheFailure(t *testing.T) {
	// Arrange — the frontend must not be told a stop was confirmed when its
	// outcome was never received.
	h := unackedHarness(t, nil)

	// Act.
	err := h.interrupt()

	// Assert.
	if !errors.Is(err, shimclient.ErrDeliveredUnacked) {
		t.Fatalf("Interrupt err = %v, want the unanswered-stop failure surfaced to the caller", err)
	}
}

func TestDeliveredUnackedInterruptPaintsNoTurnOutcome(t *testing.T) {
	// Arrange — `interrupted` names a verdict this exchange never produced.
	h := unackedHarness(t, nil)

	// Act.
	_ = h.interrupt()

	// Assert.
	if got := h.applier.interruptMarked(); len(got) != 0 {
		t.Fatalf("interrupted turn marks = %v, want none — the outcome was never received", got)
	}
}

func TestDeliveredUnackedInterruptOpensNoFooterWindow(t *testing.T) {
	// Arrange — the window's whole content is the outcome.
	h := unackedHarness(t, nil)

	// Act.
	_ = h.interrupt()

	// Assert.
	if got := h.prog.interruptNotes(); len(got) != 0 {
		t.Fatalf("interrupt notes = %+v, want none without an outcome to report", got)
	}
}

func TestAnOrdinaryInterruptFailureArmsNoReattachResolution(t *testing.T) {
	// Arrange — a stop that never left the daemon has nothing to reconcile.
	h := newQueueHarness(t, nil)
	h.client.mu.Lock()
	h.client.interruptErr = shimclient.ErrNotConnected
	h.client.mu.Unlock()

	// Act.
	_ = h.interrupt()

	// Assert.
	d := h.controller()
	h.m.mu.Lock()
	armed := d.unackedInterrupt
	h.m.mu.Unlock()
	if armed != nil {
		t.Fatalf("unacked interrupt = %+v, want none — this stop never reached the shim", armed)
	}
}

func TestReattachResolvesTheUnansweredStopWithoutASecondInterrupt(t *testing.T) {
	// Arrange — the reattaching shim names none of the stopped turns.
	var logged []string
	h := unackedHarness(t, func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })
	d := h.controller()
	h.m.mu.Lock()
	d.unackedInterrupt = &unackedInterrupt{requestID: "fe-1", turnIDs: []string{"turn-stopped"}}
	h.m.mu.Unlock()
	before := h.client.interruptCount()

	// Act.
	h.m.resolveUnackedInterrupt(d, nil)

	// Assert.
	if got := h.client.interruptCount(); got != before {
		t.Fatalf("interrupts sent = %d, want %d — a stop is never replayed onto a reattach", got, before)
	}
	if !strings.Contains(strings.Join(logged, "\n"), "user stop RESOLVED BY REATTACH") {
		t.Fatalf("missing the resolution record; log:\n%s", strings.Join(logged, "\n"))
	}
}

func TestReattachReportsAStopTheTurnOutlived(t *testing.T) {
	// Arrange — the returning shim STILL names the turn the stop targeted.
	var logged []string
	h := unackedHarness(t, func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })
	d := h.controller()
	h.m.mu.Lock()
	d.unackedInterrupt = &unackedInterrupt{requestID: "fe-1", turnIDs: []string{"turn-stopped"}}
	h.m.mu.Unlock()
	before := h.client.interruptCount()

	// Act.
	h.m.resolveUnackedInterrupt(d, []string{"turn-stopped"})

	// Assert.
	if got := h.client.interruptCount(); got != before {
		t.Fatalf("interrupts sent = %d, want %d — an ineffective stop is reported, never retried", got, before)
	}
	if !strings.Contains(strings.Join(logged, "\n"), "user stop APPARENTLY INEFFECTIVE") {
		t.Fatalf("missing the ineffective-stop record; log:\n%s", strings.Join(logged, "\n"))
	}
}

func TestResolvingConsumesTheUnansweredStop(t *testing.T) {
	// Arrange — a second reattach must not re-report a stop already answered.
	h := unackedHarness(t, func(string, ...any) {})
	d := h.controller()
	h.m.mu.Lock()
	d.unackedInterrupt = &unackedInterrupt{requestID: "fe-1", turnIDs: []string{"turn-stopped"}}
	h.m.mu.Unlock()

	// Act.
	h.m.resolveUnackedInterrupt(d, nil)

	// Assert.
	h.m.mu.Lock()
	armed := d.unackedInterrupt
	h.m.mu.Unlock()
	if armed != nil {
		t.Fatalf("unacked interrupt = %+v, want it consumed by its resolution", armed)
	}
}

func TestResolvingWithNothingArmedIsANoOp(t *testing.T) {
	// Arrange — the ordinary reattach, with no stop outstanding.
	var logged []string
	h := newQueueHarnessWithPusher(t, nil, nil, func(f string, a ...any) { logged = append(logged, fmt.Sprintf(f, a...)) })

	// Act.
	h.m.resolveUnackedInterrupt(h.controller(), []string{"turn-live"})

	// Assert.
	if strings.Contains(strings.Join(logged, "\n"), "user stop") {
		t.Fatalf("a reattach with no outstanding stop wrote a stop record; log:\n%s", strings.Join(logged, "\n"))
	}
}

func TestIntersectTurnIDsNamesOnlyTheSurvivors(t *testing.T) {
	// Arrange.
	want := []string{"turn-a", "turn-b", "turn-c"}
	have := []string{"turn-c", "turn-a"}

	// Act.
	got := intersectTurnIDs(want, have)

	// Assert.
	if len(got) != 2 || got[0] != "turn-a" || got[1] != "turn-c" {
		t.Fatalf("intersectTurnIDs = %v, want the survivors in the stop's own order", got)
	}
}
