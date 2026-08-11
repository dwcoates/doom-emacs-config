package sessioncontroller

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// A PROMPT RENDERS WHEN IT ROUND-TRIPS THROUGH THE SDK, AND NEVER BEFORE.
//
// The daemon used to push a RECEIPT for an accepted submit — the user's own
// words, keyed on the frontend request id — so the prompt appeared without
// waiting for the vendor's transcript. That receipt then had to be reconciled
// against the durable line the CLI later wrote, which gave one prompt two
// identities and, when the correlation missed, two bubbles. The receipt is
// gone: the only account of a prompt is the durable line, and these tests pin
// that nothing daemon-local stands in for it at any point of the submit path.

// --- helpers ----------------------------------------------------------------

// submitAs submits a prompt for "ws" under an explicit frontend request id —
// the id the submitted turn carries on the wire.
func (h *queueHarness) submitAs(requestID, text string) error {
	return h.m.SubmitPrompt(context.Background(), "ws", requestID, text, "", testPromptOrigin)
}

// pushedTurn is one pushed conversation item carrying a user message, with the
// delta's through_seq beside it.
type pushedTurn struct {
	item       *frontendv1.Message
	throughSeq uint64
}

// userTurns returns every pushed user message, in push order.
func (h *queueHarness) userTurns() []pushedTurn {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []pushedTurn
	for _, cd := range h.push.convo {
		for _, it := range cd.GetMessages() {
			if it.GetUserMessage() != nil {
				out = append(out, pushedTurn{item: it, throughSeq: cd.GetThroughSeq()})
			}
		}
	}
	return out
}

// transcriptUserEvent is the DURABLE account of a prompt as the real pipeline
// delivers it: a file-plane transcript user line, carrying NO request id of its
// own (that field is empty on every line the file plane produces).
func transcriptUserEvent(t *testing.T, seq uint64, uuid, text string) *corev1.Event {
	t.Helper()
	return userLineEvent(t, seq, uuid, text, datav1.OriginKind_ORIGIN_KIND_UNSPECIFIED)
}

// --- nothing is drawn at submit ---------------------------------------------

func TestADirectSubmitDrawsNothingForThePrompt(t *testing.T) {
	// Arrange: an idle session, so the prompt goes straight to the shim.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: the submit itself puts no user message on any frontend. A receipt
	// here is the second identity for this prompt, and reconciling it against
	// the durable line below is what drew the prompt twice.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) at submit, want none until the durable line arrives", len(turns))
	}
}

func TestThePromptRendersFromItsDurableLine(t *testing.T) {
	// Arrange: a submitted prompt with nothing on screen for it yet.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: the CLI writes the transcript line for it.
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u-real", "hello there"))

	// Assert: exactly one bubble, and it is the store's own.
	turns := h.userTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s), want the one durable line", len(turns))
	}
	if got := turns[0].item.GetUuid(); got != "u-real" {
		t.Errorf("rendered uuid = %q, want the durable record's own", got)
	}
	if got := turns[0].throughSeq; got != 12 {
		t.Errorf("through_seq = %d, want the store seq the line was written at", got)
	}
}

func TestASubmitPushesNoStateDependentFrameBeforeItsPremise(t *testing.T) {
	// Arrange: clear any bring-up pushes so this trace names only the prompt's
	// critical path.
	h := newQueueHarness(t, nil)
	h.push.mu.Lock()
	h.push.trace = nil
	h.push.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: the synchronous accepted-prompt publication, and nothing else.
	// The conversation push that used to follow it was the receipt.
	h.push.mu.Lock()
	trace := append([]string(nil), h.push.trace...)
	h.push.mu.Unlock()
	want := []string{"progress", "workspace:RENDER_STATE_SUBMITTING"}
	if !reflect.DeepEqual(trace, want) {
		t.Fatalf("frontend push trace = %v, want %v", trace, want)
	}
}

// The state edge precedes the submit, so its failure is a failure to even START
// the prompt rather than a failure to describe one already accepted.
func TestPromptStatePublicationFailureWithholdsTheSubmit(t *testing.T) {
	// Arrange: the SSM cannot establish the state premise the prompt is about
	// to be published under.
	h := newQueueHarness(t, nil)
	h.applier.promptAcceptErr = errors.New("state database unavailable")
	h.push.mu.Lock()
	h.push.trace = nil
	h.push.mu.Unlock()

	// Act.
	err := h.submitAs("r1", "hello there")

	// Assert: fail loudly, having mutated nothing outside the daemon and
	// exposed no frame whose premise could not be established.
	if err == nil || !strings.Contains(err.Error(), "synchronous state publication failed before submitting") {
		t.Fatalf("submit error = %v, want a pre-submit synchronous-publication failure", err)
	}
	h.push.mu.Lock()
	trace := append([]string(nil), h.push.trace...)
	h.push.mu.Unlock()
	if len(trace) != 0 {
		t.Fatalf("frontend push trace = %v, want no state-dependent local frames", trace)
	}
	if got := h.client.promptTexts(); len(got) != 0 {
		t.Fatalf("shim submissions = %q, want none — the state premise failed before the prompt could be sent", got)
	}
	if active, activeErr := h.m.TurnActive("ws"); activeErr != nil || active {
		t.Fatalf("TurnActive after a prompt that was never submitted = (%v, %v), want false/nil so later prompts are not queued behind a turn that never began", active, activeErr)
	}
}

func TestASubmitWithNoRequestIDIsRejected(t *testing.T) {
	// Arrange.
	h := newQueueHarness(t, nil)

	// Act.
	err := h.submitAs("", "hello there")

	// Assert.
	if err == nil {
		t.Fatal("submit accepted an empty request id")
	}
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a rejected submit, want none", len(turns))
	}
}

func TestAQueuedPromptDrawsNothingAtSubmit(t *testing.T) {
	// Arrange: a turn is running, so the prompt is HELD as a queue chip.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	if err := h.submitAs("r1", "the held prompt"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: the chip is the honest report until the prompt is delivered.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a queued prompt, want none", len(turns))
	}
}

func TestDeliveringAQueuedPromptDrawsNothingEither(t *testing.T) {
	// Arrange: a prompt held behind a running turn.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submitAs("r1", "the held prompt"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: the turn ends and the drain delivers it.
	h.turn(false)

	// Assert: the delivery reaches the shim and still draws nothing — the
	// queue's drain is a submit like any other.
	waitFor(t, "the delivered prompt reaching the shim", func() bool {
		for _, text := range h.client.promptTexts() {
			if text == "the held prompt" {
				return true
			}
		}
		return false
	})
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) on delivery, want none until the durable line arrives", len(turns))
	}
}

func TestARefusedSubmitDrawsNothing(t *testing.T) {
	// Arrange: a shim that refuses the prompt.
	h := newQueueHarness(t, nil)
	h.client.mu.Lock()
	h.client.submitErrOnce = errors.New("the shim refused it")
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "hello there"); err == nil {
		t.Fatal("submit succeeded, want the shim's refusal")
	}

	// Assert: nothing was drawn for a prompt no session received, and nothing
	// daemon-local is left behind for a later transcript line to attach to.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a refused submit, want none", len(turns))
	}
}

// --- the submit lock ---------------------------------------------------------
//
// The ORDER prompts reach the SDK in is the order the durable lines that render
// them come back in, so two prompts submitted close together read as each
// other's if their submits can interleave.

func TestTheSubmitRunsUnderTheSessionSubmitLock(t *testing.T) {
	// Arrange: a hook that tries to take the session's submit lock from inside
	// SubmitPrompt. This is the mutual exclusion itself, asserted directly.
	h := newQueueHarness(t, nil)
	free := true
	h.client.mu.Lock()
	h.client.onSubmit = func() {
		d := h.controller()
		if d.submitMu.TryLock() {
			d.submitMu.Unlock()
			return
		}
		free = false
	}
	h.client.mu.Unlock()

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	if free {
		t.Fatal("the submit lock was free while a prompt was being submitted, so two submits are not ordered by construction")
	}
}
