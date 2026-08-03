package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/ssm"
)

// --- helpers ----------------------------------------------------------------

// handshake drives the shim's pre-subscription hello through the real hook.
func (h *queueHarness) handshake(inFlight bool, ids ...string) error {
	h.t.Helper()
	return h.m.onHandshake("ws", "s1", &corev1.ShimHello{
		Pid:           4242,
		TurnInFlight:  inFlight,
		ActiveTurnIds: ids,
	})
}

// shimReady closes the bring-up gate, the frame the phantom release rides.
func (h *queueHarness) shimReady() {
	h.t.Helper()
	h.m.onConnected("ws", "s1", &corev1.ShimHello{Pid: 4242})
}

// turnActive reads the queue's process-local latch under the manager mutex.
func (h *queueHarness) turnActive() bool {
	d := h.controller()
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	return d.turn.active()
}

// capturedLog is a concurrency-safe daemon log sink for the tests that assert
// the interruption was SURFACED rather than merely handled.
type capturedLog struct {
	mu    sync.Mutex
	lines []string
}

func (c *capturedLog) logf(format string, args ...any) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.lines = append(c.lines, fmt.Sprintf(format, args...))
}

func (c *capturedLog) contains(want string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	for _, line := range c.lines {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
}

func (c *capturedLog) dump() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	return strings.Join(c.lines, "\n")
}

// --- the handshake edge -----------------------------------------------------

// The wedge, end to end: a claim survives a restart, the returning shim says no
// turn is in flight, and the prompt held behind the phantom is delivered.
func TestHandshakeReportingNoTurnReleasesTheQueue(t *testing.T) {
	// Arrange — a turn is claimed, and a prompt is queued behind it.
	h := newQueueHarness(t, nil)
	if err := h.handshake(true, "turn-old"); err != nil {
		t.Fatalf("first handshake: %v", err)
	}
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	if got := h.texts(); len(got) != 1 {
		t.Fatalf("queued = %v, want the prompt held behind the claimed turn", got)
	}

	// Act — the shim comes back reporting nothing, then becomes driveable.
	if err := h.handshake(false); err != nil {
		t.Fatalf("second handshake: %v", err)
	}
	h.shimReady()

	// Assert.
	waitFor(t, "the held prompt to be delivered", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if got := h.client.promptTexts()[0]; got != "run me" {
		t.Fatalf("delivered %q, want the held prompt", got)
	}
}

// The latch is the queue's authority, and the shim's statement moves it.
func TestHandshakeReportingNoTurnClearsTheTurnLatch(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	if err := h.handshake(true, "turn-old"); err != nil {
		t.Fatalf("first handshake: %v", err)
	}

	// Act.
	if err := h.handshake(false); err != nil {
		t.Fatalf("second handshake: %v", err)
	}

	// Assert.
	if h.turnActive() {
		t.Fatal("turn latch still set after the shim reported no turn in flight")
	}
}

// The interruption is SURFACED: a user must be able to see the turn was cut by
// a restart rather than merely notice their prompt eventually ran.
func TestHandshakePhantomCloseIsLoudlyLogged(t *testing.T) {
	// Arrange.
	log := &capturedLog{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)
	if err := h.handshake(true, "turn-old"); err != nil {
		t.Fatalf("first handshake: %v", err)
	}

	// Act.
	if err := h.handshake(false); err != nil {
		t.Fatalf("second handshake: %v", err)
	}

	// Assert.
	if !log.contains("turn INTERRUPTED BY RESTART ws=\"ws\" session=s1 closed=[turn-old] cause=" + ssm.TurnCloseRestartInterrupted) {
		t.Fatalf("missing the loud restart-interrupt record; log:\n%s", log.dump())
	}
}

// A shim CONFIRMING the claim changes nothing: the turn really is running, and
// the queue must go on holding what is behind it.
func TestHandshakeConfirmingTheTurnHoldsTheQueue(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	if err := h.handshake(true, "turn-old"); err != nil {
		t.Fatalf("first handshake: %v", err)
	}
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act — the same live turn, reported again on a reattach.
	if err := h.handshake(true, "turn-old"); err != nil {
		t.Fatalf("second handshake: %v", err)
	}
	h.shimReady()

	// Assert.
	if !h.turnActive() {
		t.Fatal("turn latch cleared under a shim reporting a live turn")
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want the queue still holding — the turn is genuinely running", got)
	}
	if got := h.texts(); len(got) != 1 || got[0] != "run me" {
		t.Fatalf("queued = %v, want the prompt retained", got)
	}
}

// An ordinary reattach owes the queue nothing, so ShimReady must not manufacture
// a boundary that delivers a prompt into a turn nobody ended.
func TestShimReadyWithoutAPhantomCloseDeliversNothing(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	if err := h.handshake(true, "turn-old"); err != nil {
		t.Fatalf("handshake: %v", err)
	}
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	h.shimReady()

	// Assert.
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none — no boundary was owed", got)
	}
}

// --- the ALREADY_COMPLETE edge ---------------------------------------------

// The interject's stop is acked ALREADY_COMPLETE, which is the shim saying the
// TurnEnded this entry is waiting for is never coming. The entry must run.
func TestInterjectAlreadyCompleteDeliversTheHeldPrompt(t *testing.T) {
	// Arrange — a turn is claimed and a prompt is held behind it.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.turn(true)
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	entry := h.entries()[0]

	// Act — the user forces it, which interrupts and then waits for TurnEnded.
	if err := h.m.ForceQueueEntry("ws", entry.id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert.
	waitFor(t, "the held prompt to be delivered", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if got := h.client.promptTexts()[0]; got != "run me" {
		t.Fatalf("delivered %q, want the held prompt", got)
	}
}

// The durable claim is the other half of the same statement, and it is closed
// with the cause the Ack authorized.
func TestInterjectAlreadyCompleteClosesTheDurableClaim(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.turn(true)
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	entry := h.entries()[0]

	// Act.
	if err := h.m.ForceQueueEntry("ws", entry.id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert.
	waitFor(t, "the synthesized turn close", func() bool {
		got := h.applier.synthesizedTurnCloses()
		return len(got) == 1 && got[0] == ssm.TurnCloseAlreadyComplete
	})
}

// The status axis is reconciled by the same settlement, so the footer and the
// workspace cannot report a turn the shim has just denied.
func TestInterjectAlreadyCompleteReconcilesTheStatusAxis(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.turn(true)
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	entry := h.entries()[0]

	// Act.
	if err := h.m.ForceQueueEntry("ws", entry.id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert.
	waitFor(t, "the already-complete status reconciliation", func() bool {
		return len(h.applier.alreadyCompleteCalls()) == 1
	})
}

// An interject acked INTERRUPTED is untouched: a real turn was stopped, and its
// own TurnEnded is what delivers the entry.
func TestInterjectInterruptedSynthesizesNoClose(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_INTERRUPTED)
	h.turn(true)
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	entry := h.entries()[0]

	// Act.
	if err := h.m.ForceQueueEntry("ws", entry.id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}
	waitFor(t, "the interject to be marked", func() bool {
		es := h.entries()
		return len(es) == 1 && es[0].interjecting
	})

	// Assert.
	if got := h.applier.synthesizedTurnCloses(); len(got) != 0 {
		t.Fatalf("synthesized closes = %v, want none — a real turn was stopped", got)
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none until the stopped turn reports its end", got)
	}
}

// A USER-commanded stop acked ALREADY_COMPLETE closes the claim and leaves the
// queue paused: the user asked for work to stop, so nothing is delivered.
func TestUserStopAlreadyCompleteClosesTheClaimWithoutDelivering(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.turn(true)
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act.
	if err := h.interrupt(); err != nil {
		t.Fatalf("interrupt: %v", err)
	}

	// Assert.
	if got := h.applier.synthesizedTurnCloses(); len(got) != 1 ||
		got[0] != ssm.TurnCloseAlreadyComplete {
		t.Fatalf("synthesized closes = %v, want exactly the already-complete close", got)
	}
	if !h.paused() {
		t.Fatal("queue resumed under a user-commanded stop")
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none — the user asked the work to stop", got)
	}
}

// ALREADY_COMPLETE over a session holding NO claim is benign: nothing is closed,
// nothing fails, and the stop still reports success.
func TestAlreadyCompleteWithNoClaimStaysBenign(t *testing.T) {
	// Arrange — no turn was ever claimed.
	h := newQueueHarness(t, nil)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)

	// Act.
	err := h.interrupt()

	// Assert.
	if err != nil {
		t.Fatalf("interrupt = %v, want the ordinary success", err)
	}
	if got := h.applier.synthesizedTurnCloses(); len(got) != 1 {
		t.Fatalf("synthesized close calls = %v, want exactly one asking about a claim that is not there", got)
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("prompts = %v, want none", got)
	}
}

// --- the error paths --------------------------------------------------------

// failSynthesizedTurnClose makes the applier's SynthesizeTurnClose fail.
func (h *queueHarness) failSynthesizedTurnClose(err error) {
	h.applier.reconcMutex.Lock()
	h.applier.synthesizeErr = err
	h.applier.reconcMutex.Unlock()
}

// A ledger write the daemon could not make does not strand the user's prompt:
// the shim's statement stands either way, and the failure is loud.
func TestInterjectAlreadyCompleteReleasesTheQueueDespiteALedgerFailure(t *testing.T) {
	// Arrange.
	log := &capturedLog{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.failSynthesizedTurnClose(errors.New("state store is gone"))
	h.turn(true)
	if err := h.submit("run me"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	entry := h.entries()[0]

	// Act.
	if err := h.m.ForceQueueEntry("ws", entry.id); err != nil {
		t.Fatalf("ForceQueueEntry: %v", err)
	}

	// Assert.
	waitFor(t, "the held prompt to be delivered anyway", func() bool {
		return len(h.client.promptTexts()) == 1
	})
	if !log.contains("interject already-complete turn-claim close FAILED ws=ws session=s1") {
		t.Fatalf("missing the canonical ledger-failure record; log:\n%s", log.dump())
	}
}

// The same failure under a USER-commanded stop is loud too, and never replaces
// the interrupt's own answer.
func TestUserStopAlreadyCompleteReportsALedgerFailureWithoutFailingTheStop(t *testing.T) {
	// Arrange.
	log := &capturedLog{}
	h := newQueueHarnessWithPusher(t, nil, nil, log.logf)
	h.ackWith(corev1.InterruptOutcome_INTERRUPT_OUTCOME_ALREADY_COMPLETE)
	h.failSynthesizedTurnClose(errors.New("state store is gone"))
	h.turn(true)

	// Act.
	err := h.interrupt()

	// Assert.
	if err != nil {
		t.Fatalf("interrupt = %v, want the stop's own answer preserved", err)
	}
	if !log.contains("user-stop already-complete turn-claim close FAILED ws=ws session=s1") {
		t.Fatalf("missing the canonical ledger-failure record; log:\n%s", log.dump())
	}
}
