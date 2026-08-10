package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/ssm"
)

// --- helpers ----------------------------------------------------------------

// interrupt runs the USER-COMMANDED stop: the same call the frontend
// interrupt command handler makes, and the only path that opens a window,
// marks the turn, or pauses the queue.
func (h *queueHarness) interrupt() error {
	h.t.Helper()
	return h.m.Interrupt(context.Background(), "ws", "fe-1")
}

// ackWith arms the fake shim's interrupt ack with a specific outcome.
func (h *queueHarness) ackWith(outcome corev1.InterruptOutcome) {
	h.client.mu.Lock()
	h.client.interruptOutcome = outcome
	h.client.mu.Unlock()
}

// paused reads the queue's pause posture under the manager mutex.
func (h *queueHarness) paused() bool {
	d := h.controller()
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

// THE INVARIANT: an interject's stop IS the user's stop. It goes out through
// the same stopTurn, so it carries every consequence a pressed stop carries —
// the interrupt window, the `interrupted` turn outcome, and the queue pause —
// and there is no second kind of stop that could carry fewer.
func TestAnInterjectsStopCarriesEveryUserStopConsequence(t *testing.T) {
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
	waitForSettled(t, "the interject's stop to pause the queue", func() bool { return h.paused() })

	// Assert.
	if got := h.prog.interruptNotes(); len(got) != 1 || got[0].workspace != "ws" {
		t.Fatalf("interrupt windows = %+v, want one opened by the interject's stop", got)
	}
	if got := h.applier.interruptMarked(); len(got) != 1 {
		t.Fatalf("turn marks = %v, want the stopped turn marked interrupted", got)
	}
}

// THE REPORTED DEFECT. A queue already paused by an earlier stop used to refuse
// to interrupt for an interject, so a classified interrupt did nothing at all
// and the user had to press stop by hand for the prompt they had already typed
// to go. An interject is the user's stop, so it stops the turn either way.
func TestAnInterjectStopsTheTurnEvenWhenTheQueueIsAlreadyPaused(t *testing.T) {
	// Arrange — the user stops the session, then runs one prompt alone against
	// the paused queue, and types a second one while that one is in flight.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}
	h.turn(false)
	if !h.paused() {
		t.Fatal("arrange: the user's stop must have paused the queue")
	}
	h.turn(true)
	if err := h.submit("run this now"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	waitFor(t, "the prompt to be queued", func() bool { return len(h.entries()) == 1 })
	id := h.entries()[0].id
	before := h.client.interruptCount()

	// Act.
	if err := h.m.ForceQueueEntry("ws", id); err != nil {
		t.Fatalf("force: %v", err)
	}

	// Assert — the stop went out rather than being swallowed by the pause.
	waitFor(t, "the interject's stop to reach the shim", func() bool {
		return h.client.interruptCount() == before+1
	})
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

// A workspace with no live session controller is a loud error, not a false answer that
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

// --- recovering a stop for a turn whose controller went away --------------------
//
// A state that should not exist: `hibernate()` refuses any workspace that has not
// settled, so a live turn with no session controller behind it means some writer closed the
// axis behind work in flight. The user is looking at a tab that says nothing is
// happening, pressing stop on it, and the old behavior answered with a nack about
// the interrupt that said nothing about the turn still burning tokens.

// uncontrolledHarness returns a harness whose workspace has no live session controller, plus the
// applier whose resolved state the recovery reads.
func uncontrolledHarness(t *testing.T) *queueHarness {
	t.Helper()
	h := newQueueHarness(t, nil)
	// Evict the session controller WITHOUT hibernating: the point is a workspace nothing is
	// driving, arrived at by the same shape as the writer bug this recovers from.
	h.m.mu.Lock()
	delete(h.m.byWS, "ws")
	h.m.mu.Unlock()
	return h
}

func TestInterruptBringsUpAWorkspaceWhoseTurnOutlivedItsSessionController(t *testing.T) {
	// Arrange — no session controller, but the log still shows a turn in flight.
	h := uncontrolledHarness(t)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_HIBERNATED,
		TurnActive: true,
	})

	// Act.
	err := h.interrupt()

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt = %v, want the bring-up paid and the stop delivered", err)
	}
	if _, err := h.m.existing("ws"); err != nil {
		t.Fatalf("the workspace has no session controller after the recovery: %v", err)
	}
}

// THE ORDER IS BRING UP, THEN INTERRUPT, which is the order the user asked for:
// the stream is re-established first, so the stop's consequences flow to a
// frontend that can see them the moment it lands.
func TestInterruptRecoveryStopsTheTurnAfterBringingItUp(t *testing.T) {
	// Arrange.
	h := uncontrolledHarness(t)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_HIBERNATED,
		TurnActive: true,
	})

	// Act.
	if err := h.interrupt(); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert — the freshly brought-up client actually received the stop.
	d := h.controller()
	fc, ok := d.client.(*fakeClient)
	if !ok {
		t.Fatalf("controller client is %T, want the fake", d.client)
	}
	if got := fc.interruptCount(); got != 1 {
		t.Fatalf("interrupts = %d, want 1 — the recovery brought the session up but never delivered the stop", got)
	}
}

// A RED resolved state recovers too, for a workspace without a session controller whose log never
// received the hibernation row at all. `turn_active` is the discriminator that
// catches the teal case (teal outranks the session-status lifecycle, so the violated state
// resolves `hibernated` and never reads red), and this is the other direction.
func TestInterruptRecoversAWorkspaceStillReadingRed(t *testing.T) {
	// Arrange — no session controller, and the resolved state is the turn itself.
	h := uncontrolledHarness(t)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_THINKING})

	// Act.
	err := h.interrupt()

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt = %v, want the stop delivered to the turn the log still shows", err)
	}
}

// The recovery is announced as an INVARIANT VIOLATION, never silently. A bring-up
// nobody asked for, paid on the way to a stop, is exactly the kind of thing that
// must be findable in a log afterwards.
func TestInterruptRecoveryLogsTheInvariantViolation(t *testing.T) {
	// Arrange.
	var mu sync.Mutex
	var lines []string
	h := newQueueHarnessWithPusher(t, nil, nil, func(format string, args ...any) {
		mu.Lock()
		defer mu.Unlock()
		lines = append(lines, fmt.Sprintf(format, args...))
	})
	h.m.mu.Lock()
	delete(h.m.byWS, "ws")
	h.m.mu.Unlock()
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_HIBERNATED,
		TurnActive: true,
	})

	// Act.
	if err := h.interrupt(); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	mu.Lock()
	defer mu.Unlock()
	for _, l := range lines {
		if strings.Contains(l, "INVARIANT VIOLATION RECOVERY") {
			return
		}
	}
	t.Fatalf("the recovery bring-up was not announced: %v", lines)
}

// A SETTLED workspace keeps the ORIGINAL error. Paying a ~500MB bring-up and a
// handshake to deliver a stop to a session with nothing running is a worse answer
// than the honest refusal, so an interrupt must never spuriously spawn a shim to
// stop nothing.
func TestInterruptOnASettledHibernatedWorkspaceKeepsTheError(t *testing.T) {
	// Arrange — asleep after a clean turn: no session controller, and nothing to stop.
	h := uncontrolledHarness(t)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State: frontendv1.RenderState_RENDER_STATE_HIBERNATED,
	})

	// Act.
	err := h.interrupt()

	// Assert.
	if !errors.Is(err, ErrNoLiveSessionController) {
		t.Fatalf("Interrupt on a settled hibernated workspace = %v, want ErrNoLiveSessionController", err)
	}
	if _, existsErr := h.m.existing("ws"); existsErr == nil {
		t.Fatal("the interrupt spawned a shim to stop nothing")
	}
}

// A workspace the log knows NOTHING about keeps the original error too: there is
// no positive evidence of a turn, and the recovery acts only on evidence.
func TestInterruptOnAnUnknownWorkspaceKeepsTheError(t *testing.T) {
	// Arrange — no session controller, no resolved state at all.
	h := uncontrolledHarness(t)

	// Act.
	err := h.interrupt()

	// Assert.
	if !errors.Is(err, ErrNoLiveSessionController) {
		t.Fatalf("Interrupt on an unknown workspace = %v, want ErrNoLiveSessionController", err)
	}
}

// A STATE READ FAILURE keeps the original error. The absent controller is a fact we
// already have; the recovery is a discretionary extra needing positive evidence,
// and spawning a shim on an unreadable log would be acting on a guess.
func TestInterruptKeepsTheErrorWhenTheStateReadFails(t *testing.T) {
	// Arrange.
	h := uncontrolledHarness(t)
	h.applier.reconcMutex.Lock()
	h.applier.currentErr = errors.New("the state log is unreadable")
	h.applier.reconcMutex.Unlock()

	// Act.
	err := h.interrupt()

	// Assert.
	if !errors.Is(err, ErrNoLiveSessionController) {
		t.Fatalf("Interrupt with an unreadable state = %v, want ErrNoLiveSessionController", err)
	}
}

// THE INTERJECT PATH IS STRUCTURALLY SEPARATE and must stay that way. The queue
// calls d.client.Interrupt DIRECTLY, so it reaches none of this — no recovery, no
// window, no turn outcome, no pause — by construction rather than by remembering
// to pass a flag. A queue that could trigger a bring-up on its own machinery stop
// would spawn shims behind the user's back.
func TestTheInterjectStopDoesNotRouteThroughTheRecovery(t *testing.T) {
	// Arrange — a workspace without a session controller whose log shows a turn in flight, which is
	// exactly the shape the user-commanded path recovers from.
	h := uncontrolledHarness(t)
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		State:      frontendv1.RenderState_RENDER_STATE_HIBERNATED,
		TurnActive: true,
	})

	// Act — the queue's own interject sequence, reached the way the frontend
	// reaches it, which resolves its controller through the UNRECOVERED m.existing.
	err := h.m.ForceQueueEntry("ws", "any-entry")

	// Assert — it refuses rather than bringing anything up.
	if err == nil {
		t.Fatal("the interject path was served through a recovery bring-up; it must reach the live session controller or nothing")
	}
	if _, existsErr := h.m.existing("ws"); existsErr == nil {
		t.Fatal("the interject stop spawned a shim; only the user-commanded path may")
	}
}

// --- the stop that landed on a turn boundary --------------------------------
//
// The shim answers ALREADY_COMPLETE for the turn the user aimed at, and a newer
// turn is running by the time that answer is reconciled. The stop used to die
// there, so a user who pressed stop saw nothing happen at all.

// armSupersededStop arms an ALREADY_COMPLETE ack whose reconciliation reports a
// newer turn already active, and answers the NEXT stop with `then`.
func armSupersededStop(h *queueHarness, then corev1.InterruptOutcome) {
	h.client.mu.Lock()
	h.client.interruptOutcomeQueue = []corev1.InterruptOutcome{
		corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE, then,
	}
	h.client.mu.Unlock()
	h.applier.reconcMutex.Lock()
	h.applier.alreadyCompleteErr = fmt.Errorf("%w: a newer turn is active", ssm.ErrSettledTurnSuperseded)
	h.applier.reconcMutex.Unlock()
}

func TestAStopSupersededAtATurnBoundaryIsReAimed(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	armSupersededStop(h, corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)

	// Act.
	err := h.interrupt()

	// Assert: the user's press produced a real stop instead of silence.
	if err != nil {
		t.Fatalf("interrupt err = %v, want the re-aimed stop to succeed", err)
	}
	if got := h.client.interruptCount(); got != 2 {
		t.Fatalf("stops delivered = %d, want 2 (the aimed-at turn and the re-aim)", got)
	}
	notes := h.prog.interruptNotes()
	if len(notes) != 1 || notes[0].outcome != corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED {
		t.Fatalf("interrupt windows = %+v, want one INTERRUPTED window", notes)
	}
}

func TestAReAimedStopIsNotRetriedForever(t *testing.T) {
	// Arrange: every stop lands on a boundary.
	h := newQueueHarness(t, nil)
	armSupersededStop(h, corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)

	// Act.
	err := h.interrupt()

	// Assert: the second failure is reported rather than chased.
	if !errors.Is(err, ssm.ErrSettledTurnSuperseded) {
		t.Fatalf("interrupt err = %v, want the superseded verdict surfaced", err)
	}
	if got := h.client.interruptCount(); got != 2 {
		t.Fatalf("stops delivered = %d, want exactly one re-aim", got)
	}
}

func TestAnOrdinaryAlreadyCompleteFailureIsNotReAimed(t *testing.T) {
	// Arrange: the reconciliation failed for a reason that is not a boundary
	// race, so delivering a second stop would be delivering it into a fault.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.applier.reconcMutex.Lock()
	h.applier.alreadyCompleteErr = errors.New("state database unreadable")
	h.applier.reconcMutex.Unlock()

	// Act.
	err := h.interrupt()

	// Assert.
	if err == nil {
		t.Fatal("interrupt err = nil, want the reconciliation failure surfaced")
	}
	if got := h.client.interruptCount(); got != 1 {
		t.Fatalf("stops delivered = %d, want 1", got)
	}
}
