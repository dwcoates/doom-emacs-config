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

	"google.golang.org/protobuf/types/known/anypb"
)

// --- helpers ----------------------------------------------------------------

// submitAs submits a prompt for "ws" under an explicit frontend request id —
// the id the receipt is keyed on.
func (h *queueHarness) submitAs(requestID, text string) error {
	return h.m.SubmitPrompt(context.Background(), "ws", requestID, text, "")
}

// userTurnItems returns every pushed conversation item carrying a user message,
// in push order, with the delta's through_seq beside it.
type pushedTurn struct {
	item       *frontendv1.ConversationItem
	throughSeq uint64
}

func (h *queueHarness) userTurns() []pushedTurn {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []pushedTurn
	for _, cd := range h.push.convo {
		for _, it := range cd.GetItems() {
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
	a, err := anypb.New(&datav1.TranscriptLine{
		Line: &datav1.TranscriptLine_User{User: &datav1.UserLine{
			Envelope: &datav1.LineEnvelope{Uuid: uuid},
			Message: &datav1.ApiUserMessage{
				Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// --- the receipt ------------------------------------------------------------

func TestDirectSubmitPushesTheReceiptKeyedOnItsRequestID(t *testing.T) {
	// Arrange: an idle session, so the prompt goes straight to the shim.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: one bubble, carrying the submit's id and the user's own text.
	turns := h.userTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s), want the one receipt", len(turns))
	}
	if got := turns[0].item.GetRequestId(); got != "r1" {
		t.Errorf("receipt request_id = %q, want r1", got)
	}
	if got := turns[0].item.GetUserMessage().GetContentString(); got != "hello there" {
		t.Errorf("receipt text = %q, want the submitted prompt", got)
	}
}

func TestPromptThinkingPublicationPrecedesReceipt(t *testing.T) {
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

	// Assert: one synchronous state publication must precede the local prompt
	// delta. Separate per-kind slices could prove both happened but not order;
	// this shared trace makes the invariant executable.
	h.push.mu.Lock()
	trace := append([]string(nil), h.push.trace...)
	h.push.mu.Unlock()
	want := []string{"progress", "workspace:RENDER_STATE_SUBMITTING", "conversation"}
	if !reflect.DeepEqual(trace, want) {
		t.Fatalf("frontend push trace = %v, want %v", trace, want)
	}
}

// The state edge now precedes the submit, so its failure is a failure to even
// START the prompt rather than a failure to describe one already accepted. That
// inversion is the point of the reordering: the daemon can fail here having
// changed nothing outside itself, which the post-Ack ordering could never do.
func TestPromptStatePublicationFailureWithholdsTheSubmitAndTheReceipt(t *testing.T) {
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

func TestTheReceiptCarriesNoStoreSeq(t *testing.T) {
	// Arrange: nothing from the store has happened at all.
	h := newQueueHarness(t, nil)

	// Act.
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: daemon-composed, so through_seq stays 0 — it is not a store fact.
	turns := h.userTurns()
	if len(turns) != 1 || turns[0].throughSeq != 0 {
		t.Fatalf("receipt pushed with through_seq=%d, want a seq-less daemon push", turns[0].throughSeq)
	}
}

func TestASubmitWithNoRequestIDIsRejectedBeforeReceiptPush(t *testing.T) {
	h := newQueueHarness(t, nil)

	if err := h.submitAs("", "hello there"); err == nil {
		t.Fatal("submit accepted an empty request id")
	}

	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a rejected submit, want none", len(turns))
	}
}

func TestAQueuedPromptPushesNoReceiptAtSubmit(t *testing.T) {
	// Arrange: a turn is running, so the prompt is HELD as a queue chip.
	h := newQueueHarness(t, nil)
	h.turn(true)

	// Act.
	if err := h.submitAs("r1", "the held prompt"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert: a bubble now would claim an execution order the session will not
	// follow — the chip is the honest report until it is delivered.
	if turns := h.userTurns(); len(turns) != 0 {
		t.Fatalf("pushed %d user turn(s) for a queued prompt, want none until delivery", len(turns))
	}
}

func TestDeliveringAQueuedPromptPushesItsReceipt(t *testing.T) {
	// Arrange: a prompt held behind a running turn.
	h := newQueueHarness(t, nil)
	h.turn(true)
	if err := h.submitAs("r1", "the held prompt"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: the turn ends and the drain delivers it.
	h.turn(false)

	// Assert: the receipt lands at DELIVERY, under the submit's own id.
	waitFor(t, "the delivered prompt's receipt", func() bool {
		turns := h.userTurns()
		return len(turns) == 1 && turns[0].item.GetRequestId() == "r1"
	})
}

// --- attribution ------------------------------------------------------------

func TestTheDurableLineIsStampedWithTheSubmitItAnswers(t *testing.T) {
	// Arrange: a submit whose receipt is outstanding.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: the transcript's own account of that prompt arrives live.
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u1", "hello there"))

	// Assert: the durable line carries the request id, so a frontend reconciles
	// it onto the bubble already on screen instead of drawing a second one.
	turns := h.userTurns()
	if len(turns) != 2 {
		t.Fatalf("pushed %d user turn(s), want the receipt and the durable line", len(turns))
	}
	if got := turns[1].item.GetRequestId(); got != "r1" {
		t.Errorf("durable line request_id = %q, want the submit's id r1", got)
	}
	if got := turns[1].item.GetUuid(); got != "u1" {
		t.Errorf("durable line uuid = %q, want the record's own uuid", got)
	}
}

func TestAnUnmatchedDurableLineStaysUnattributed(t *testing.T) {
	// Arrange: no submit of this daemon's is outstanding (resumed history, or
	// a prompt sent through some other client).
	h := newQueueHarness(t, nil)

	// Act.
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u1", "from somewhere else"))

	// Assert: blank rather than invented — a request id here would attach the
	// prompt to a submit that has nothing to do with it.
	turns := h.userTurns()
	if len(turns) != 1 {
		t.Fatalf("pushed %d user turn(s), want the durable line alone", len(turns))
	}
	if got := turns[0].item.GetRequestId(); got != "" {
		t.Errorf("durable line request_id = %q, want it left blank", got)
	}
}

func TestTwoOutstandingSubmitsAreAttributedOldestFirst(t *testing.T) {
	// Arrange: two receipts outstanding, in submit order.
	h := newQueueHarness(t, nil)
	for _, r := range []struct{ id, text string }{{"r1", "first"}, {"r2", "second"}} {
		if err := h.submitAs(r.id, r.text); err != nil {
			t.Fatalf("submit %s: %v", r.id, err)
		}
	}

	// Act: the first prompt's durable line comes back.
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u1", "first"))

	// Assert: it claims the OLDEST outstanding submit, the only conservative
	// correlation available (the line names no request of its own).
	turns := h.userTurns()
	if got := turns[len(turns)-1].item.GetRequestId(); got != "r1" {
		t.Errorf("durable line request_id = %q, want the oldest outstanding submit r1", got)
	}
}

func TestAReplayedUserLineClaimsNoReceipt(t *testing.T) {
	// Arrange: a submit outstanding, and a resync replaying old history.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: a REPLAYED user line (the resync path, live=false).
	h.controller().consumer.pushConversation(transcriptUserEvent(t, 3, "old", "ancient prompt"), false)

	// Assert: replayed history predates the submit, so it is left unattributed
	// and the receipt stays outstanding for the line that really answers it.
	turns := h.userTurns()
	if got := turns[len(turns)-1].item.GetRequestId(); got != "" {
		t.Errorf("replayed line request_id = %q, want it left blank", got)
	}
	if got := len(h.controller().consumer.snapshotEchoes()); got != 1 {
		t.Errorf("outstanding receipts = %d, want the unclaimed one retained", got)
	}
}

// --- retention --------------------------------------------------------------

func TestAnUnclaimedReceiptIsReplayedOnResync(t *testing.T) {
	// Arrange: a submit whose durable line has not arrived (the shim died, or
	// it simply has not landed yet).
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: a frontend reconnects and asks for everything.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert: it carries no store seq, so no from_seq could cover it — a
	// reconnecting frontend would otherwise find the user's own prompt missing.
	turns := h.userTurns()
	if len(turns) != 2 || turns[1].item.GetRequestId() != "r1" {
		t.Fatalf("pushed %d user turn(s), want the receipt re-pushed on resync", len(turns))
	}
}

func TestAClaimedReceiptIsNotReplayedOnResync(t *testing.T) {
	// Arrange: a submit whose durable line has already claimed it.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}
	h.controller().consumer.Consume(transcriptUserEvent(t, 12, "u1", "hello there"))
	before := len(h.userTurns())

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert: the replayed ring carries the durable line itself, and the
	// superseded receipt is NOT pushed beside it.
	for _, turn := range h.userTurns()[before:] {
		if turn.item.GetUuid() == echoUUID("r1") {
			t.Fatal("a superseded receipt was replayed beside the durable line it became")
		}
	}
}

func TestARaisedReplayFloorDropsTheOutstandingReceipts(t *testing.T) {
	// Arrange: a submit outstanding when the conversation is cleared.
	h := newQueueHarness(t, nil)
	if err := h.submitAs("r1", "hello there"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Act: a clear, which raises this conversation's replay floor.
	h.controller().consumer.Consume(&corev1.Event{
		SessionId: "vendor-uuid", Seq: 20,
		Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}},
	})

	// Assert: the receipt went with the history the floor now hides — replaying
	// it would put pre-clear text back above the floor.
	if got := len(h.controller().consumer.snapshotEchoes()); got != 0 {
		t.Fatalf("outstanding receipts = %d after a clear, want none", got)
	}
}
