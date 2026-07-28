package sessiondrv

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// --- helpers ----------------------------------------------------------------

// interrupt runs the USER-COMMANDED stop: the same call the frontend
// interrupt command handler makes, and the only path that opens a window,
// marks the turn, or pauses the queue.
func (h *queueHarness) interrupt() error {
	h.t.Helper()
	return h.m.Interrupt(context.Background(), "ws")
}

// ackWith arms the fake shim's interrupt ack with a specific outcome.
func (h *queueHarness) ackWith(outcome corev1.InterruptOutcome) {
	h.client.mu.Lock()
	h.client.interruptOutcome = outcome
	h.client.mu.Unlock()
}

// paused reads the queue's pause posture under the manager mutex.
func (h *queueHarness) paused() bool {
	d := h.driver()
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	return d.paused
}

// texts returns the queued entries' prompt texts, front to back.
func (h *queueHarness) texts() []string {
	out := []string{}
	for _, e := range h.entries() {
		out = append(out, e.text)
	}
	return out
}

// --- the window and the turn outcome ---------------------------------------

// The window opens on the ack, carrying the shim's verdict, whatever that
// verdict is.
func TestUserInterruptOpensTheWindowForEveryOutcome(t *testing.T) {
	tests := []struct {
		name    string
		outcome corev1.InterruptOutcome
	}{
		{"a live turn was stopped", corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED},
		{"the turn had already ended", corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE},
		{"the stop could not be delivered", corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newQueueHarness(t, nil)
			h.ackWith(tc.outcome)
			// Act — a FAILED stop returns an error; the window still opens.
			_ = h.interrupt()
			// Assert.
			got := h.prog.interruptNotes()
			if len(got) != 1 || got[0].workspace != "ws" || got[0].outcome != tc.outcome {
				t.Fatalf("interrupt notes = %+v, want one for ws with outcome %s", got, tc.outcome)
			}
		})
	}
}

// The turn outcome is marked ONLY when a turn was really stopped: the other
// two outcomes stopped nothing and have no turn to name.
func TestOnlyAnInterruptedAckMarksTheTurn(t *testing.T) {
	tests := []struct {
		name    string
		outcome corev1.InterruptOutcome
		want    int
	}{
		{"a live turn was stopped", corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED, 1},
		{"the turn had already ended", corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE, 0},
		{"the stop could not be delivered", corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED, 0},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newQueueHarness(t, nil)
			h.ackWith(tc.outcome)
			// Act.
			_ = h.interrupt()
			// Assert.
			if got := h.applier.interruptMarked(); len(got) != tc.want {
				t.Fatalf("marks = %v, want %d", got, tc.want)
			}
		})
	}
}

// THE ROUTING BOUNDARY. An interject sends the SAME Interrupt to the same
// shim, and none of the user-stop consequences may follow it: it is machinery
// acting on a held prompt's behalf, not the user asking for the turn to stop.
func TestAnInterjectOpensNoWindowAndMarksNoTurn(t *testing.T) {
	// Arrange — a turn is running with a held prompt behind it.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("later"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	id := h.entries()[0].id

	// Act — the user forces it, which runs the interject sequence.
	if err := h.m.ForceQueueEntry("ws", id); err != nil {
		t.Fatalf("force: %v", err)
	}
	waitFor(t, "the interject's stop to reach the shim", func() bool { return h.client.interruptCount() == 1 })

	// Assert.
	if got := h.prog.interruptNotes(); len(got) != 0 {
		t.Fatalf("interrupt windows = %+v, want none from an interject", got)
	}
	if got := h.applier.interruptMarked(); len(got) != 0 {
		t.Fatalf("turn marks = %v, want none from an interject", got)
	}
	if h.paused() {
		t.Fatal("an interject must not pause the queue")
	}
}

// --- the queue pause --------------------------------------------------------

// A user's stop pauses the drain and RETAINS everything: delivering the next
// held prompt the moment the stopped turn ends would start exactly the work
// they just stopped.
func TestUserInterruptPausesTheQueueRetainingEveryEntry(t *testing.T) {
	// Arrange — a turn is running with two prompts held behind it.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("first"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	if err := h.submit("second"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "both prompts queued", func() bool { return len(h.entries()) == 2 })

	// Act — the user stops the turn, and it ends.
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	h.turn(false)

	// Assert — nothing was delivered and nothing was dropped.
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none delivered while paused", got)
	}
	if got := h.texts(); len(got) != 2 || got[0] != "first" || got[1] != "second" {
		t.Fatalf("entries = %v, want both retained in order", got)
	}
}

// A stop that could not be DELIVERED changes nothing about the session, so it
// must not pause a queue that is still draining normally.
func TestAFailedStopDoesNotPauseTheQueue(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_FAILED)
	// Act.
	if err := h.interrupt(); err == nil {
		t.Fatal("an undeliverable stop must surface as an error")
	}
	// Assert.
	if h.paused() {
		t.Fatal("a FAILED stop delivered nothing; the queue must keep draining")
	}
}

// ALREADY_COMPLETE pauses too: the turn being over already does not make the
// user's stop mean less, and the queue would otherwise deliver into the
// silence they just asked for.
func TestAnAlreadyCompleteStopStillPausesTheQueue(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	// Act.
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	// Assert.
	if !h.paused() {
		t.Fatal("the user commanded a stop; the queue must pause")
	}
}

// A prompt submitted into a PAUSED, idle session runs ALONE: it goes straight
// to the shim and the retained entries stay retained.
func TestAPromptSubmittedWhilePausedRunsAlone(t *testing.T) {
	// Arrange — one held prompt, then a user stop, then the turn ends.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("held"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	h.turn(false)

	// Act.
	if err := h.submit("urgent"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert — only the new prompt ran; the held one is untouched.
	if got := h.client.promptTexts(); len(got) != 1 || got[0] != "urgent" {
		t.Fatalf("prompts = %v, want only [urgent]", got)
	}
	if got := h.texts(); len(got) != 1 || got[0] != "held" {
		t.Fatalf("entries = %v, want [held] still retained", got)
	}
}

// A prompt submitted while the paused session is MID-TURN jumps the head of
// the retained queue rather than joining the back of it.
func TestAPromptSubmittedWhilePausedAndBusyJumpsTheHead(t *testing.T) {
	// Arrange — a held prompt, a user stop, and a turn still in flight.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("held"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}

	// Act.
	if err := h.submit("urgent"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "both entries queued", func() bool { return len(h.entries()) == 2 })

	// Assert.
	if got := h.texts(); got[0] != "urgent" || got[1] != "held" {
		t.Fatalf("entries = %v, want the new prompt at the head", got)
	}
}

// The paused queue delivers exactly ONE thing at a boundary: the head jump.
func TestAPausedBoundaryDeliversOnlyTheHeadJump(t *testing.T) {
	// Arrange — a head jump queued behind a running turn on a paused queue.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("held"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	if err := h.submit("urgent"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "both entries queued", func() bool { return len(h.entries()) == 2 })

	// Act.
	h.turn(false)

	// Assert.
	waitFor(t, "the head jump to be delivered", func() bool {
		got := h.client.promptTexts()
		return len(got) == 1 && got[0] == "urgent"
	})
	if got := h.texts(); len(got) != 1 || got[0] != "held" {
		t.Fatalf("entries = %v, want [held] still retained", got)
	}
}

// The lone runner's CLEAN end resumes the drain, and the retained entries go
// out in their original order.
func TestACleanLoneRunResumesTheDrainInOrder(t *testing.T) {
	// Arrange — two held prompts, a user stop, and a prompt that ran alone.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("first"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	if err := h.submit("second"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "both prompts queued", func() bool { return len(h.entries()) == 2 })
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	h.turn(false)
	if err := h.submit("urgent"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.turn(true)

	// Act — the lone prompt finishes on its own.
	h.turn(false)

	// Assert — the drain is running again, front entry first.
	waitFor(t, "the first retained prompt to be delivered", func() bool {
		got := h.client.promptTexts()
		return len(got) == 2 && got[1] == "first"
	})
	if h.paused() {
		t.Fatal("a clean lone run must resume the drain")
	}
	if got := h.texts(); len(got) != 1 || got[0] != "second" {
		t.Fatalf("entries = %v, want [second] still queued behind it", got)
	}
}

// An INTERRUPTED lone runner is the opposite signal: the user stopped it too,
// so the pause stands and nothing drains.
func TestAnInterruptedLoneRunLeavesTheQueuePaused(t *testing.T) {
	// Arrange — a held prompt, a user stop, and a prompt running alone.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("held"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	h.turn(false)
	if err := h.submit("urgent"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.turn(true)

	// Act — the user stops the lone runner as well.
	if err := h.interrupt(); err != nil {
		t.Fatalf("second interrupt: %v", err)
	}
	h.turn(false)

	// Assert.
	if !h.paused() {
		t.Fatal("an interrupted lone run must leave the queue paused")
	}
	if got := h.client.promptTexts(); len(got) != 1 || got[0] != "urgent" {
		t.Fatalf("prompts = %v, want the retained entry left undelivered", got)
	}
	if got := h.texts(); len(got) != 1 || got[0] != "held" {
		t.Fatalf("entries = %v, want [held] retained", got)
	}
}

// An interject arriving into a PAUSED queue must not send a stop of its own:
// the user's stop is the standing instruction, and machinery does not overrule
// it. The entry becomes a head jump instead.
func TestAnInterjectIntoAPausedQueueSendsNoStop(t *testing.T) {
	// Arrange — a paused queue with a prompt running alone and one held
	// behind it.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submit("held"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	h.turn(false)
	if err := h.submit("urgent"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.turn(true)
	before := h.client.interruptCount()
	id := h.entries()[0].id

	// Act.
	if err := h.m.ForceQueueEntry("ws", id); err != nil {
		t.Fatalf("force: %v", err)
	}

	// Assert.
	if got := h.client.interruptCount(); got != before {
		t.Fatalf("interrupts = %d, want %d — a paused queue's interject sends no stop", got, before)
	}
}

// --- the turn fact the confirm gate reads -----------------------------------

// TurnActive reports the OBSERVED boundary, which is the fact the confirm gate
// needs: is there a turn to stop right now.
func TestTurnActiveReportsTheObservedBoundary(t *testing.T) {
	tests := []struct {
		name   string
		active bool
	}{
		{"a turn is running", true},
		{"the session is idle", false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := newQueueHarness(t, nil)
			h.turn(tc.active)
			// Act.
			got, err := h.m.TurnActive("ws")
			// Assert.
			if err != nil {
				t.Fatalf("TurnActive: %v", err)
			}
			if got != tc.active {
				t.Fatalf("TurnActive = %v, want %v", got, tc.active)
			}
		})
	}
}

// A workspace with no live driver is a loud error, not a false answer that
// would let the gate decide on a session it cannot see.
func TestTurnActiveOnAnUndrivenWorkspaceIsAnError(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	// Act.
	_, err := h.m.TurnActive("/nowhere")
	// Assert.
	if err == nil {
		t.Fatal("want an error for a workspace with no live session")
	}
}
