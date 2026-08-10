package sessioncontroller

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// ---------------------------------------------------------------------------
// EVERY CARD A BOUNCE MINTS GETS A CLOSING EDGE KEYED TO THE RECOVERY.
//
// The forensic baseline: session.resume_failed and its siblings were minted
// with resolved_at_ms=0 and no resolution edge at all. They lingered, and every
// reconnect re-served them, so a workspace that had been healthy for hours
// still showed the alarm from a deploy that morning.
//
// These cards are WINDOW-shaped and were recorded as if they were EVENT-shaped.
// "The conversation could not be resumed" is a true account of something that
// happened during a bounce, and it stops describing this session the moment a
// live shim wires — which is a recovery event the daemon already observes.
// ---------------------------------------------------------------------------

// bounceCard files one unresolved card of the given type on a consumer.
func bounceCard(c *consumer, uuid string, kind errclass.Type) *frontendv1.FailureCardView {
	card := errclass.Card(kind, "the bounce put this up")
	c.pushFailure(uuid, card)
	return card
}

// bounceRetainedCard reads back the card a consumer holds under uuid.
func bounceRetainedCard(c *consumer, uuid string) *frontendv1.FailureCardView {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.failItems[uuid].GetFailureCard()
}

func TestASuccessfulWireResolvesAResumeFailedCard(t *testing.T) {
	// Arrange — the case named in the baseline.
	c := receiptConsumer(t, newFakeReceiptStore())
	bounceCard(c, "f-1", errclass.TypeSessionResumeFailed)

	// Act.
	c.resolveBounceWindowCards("shim_ready")

	// Assert.
	if got := bounceRetainedCard(c, "f-1"); errclass.ResolvedAtMs(got) == 0 {
		t.Fatal("a resume_failed card must be resolved by the wire that disproves it")
	}
}

func TestASuccessfulWireResolvesAStartFailedCard(t *testing.T) {
	// Arrange — "a bring-up ended without wiring, and the fresh retry ended the
	// same way" is disproved by a wire.
	c := receiptConsumer(t, newFakeReceiptStore())
	bounceCard(c, "f-1", errclass.TypeSessionStartFailed)

	// Act.
	c.resolveBounceWindowCards("shim_ready")

	// Assert.
	if got := bounceRetainedCard(c, "f-1"); errclass.ResolvedAtMs(got) == 0 {
		t.Fatal("a start_failed card must be resolved by a later successful wire")
	}
}

func TestTheResolutionKeepsTheCardRatherThanDeletingIt(t *testing.T) {
	// Arrange — resolution, never deletion: the history stays queryable and a
	// resync replays the SETTLED card instead of re-opening the alarm.
	c := receiptConsumer(t, newFakeReceiptStore())
	original := bounceCard(c, "f-1", errclass.TypeSessionResumeFailed)

	// Act.
	c.resolveBounceWindowCards("shim_ready")

	// Assert.
	got := bounceRetainedCard(c, "f-1")
	if got == nil {
		t.Fatal("the card was deleted, not resolved")
	}
	if got.GetMessage() != original.GetMessage() {
		t.Fatalf("message = %q, want the source detail preserved", got.GetMessage())
	}
	kind, ok := errclass.TypeOf(got.GetKind())
	if !ok || kind != errclass.TypeSessionResumeFailed {
		t.Fatalf("kind = %v, want the card to keep its type", kind)
	}
}

func TestACardTheWireDoesNotDisproveIsLeftOpen(t *testing.T) {
	// Arrange — a closed list, not "resolve everything". A tool failure or an
	// API error is still true after the shim comes up, and settling it because
	// the bring-up succeeded would be the daemon claiming something it never
	// verified.
	c := receiptConsumer(t, newFakeReceiptStore())
	bounceCard(c, "f-1", errclass.TypeAPIRateLimit)

	// Act.
	c.resolveBounceWindowCards("shim_ready")

	// Assert.
	if got := bounceRetainedCard(c, "f-1"); errclass.ResolvedAtMs(got) != 0 {
		t.Fatal("a card the wire does not disprove must stay open")
	}
}

func TestResolvingTwiceKeepsTheFirstSettlementsInstant(t *testing.T) {
	// Arrange — two recovery events can legitimately race for one card, and the
	// loser must not re-stamp a later instant onto a settlement that already
	// happened.
	c := receiptConsumer(t, newFakeReceiptStore())
	bounceCard(c, "f-1", errclass.TypeSessionResumeFailed)
	c.resolveBounceWindowCards("shim_ready")
	first := errclass.ResolvedAtMs(bounceRetainedCard(c, "f-1"))

	// Act.
	c.resolveBounceWindowCards("shim_ready_again")

	// Assert.
	if got := errclass.ResolvedAtMs(bounceRetainedCard(c, "f-1")); got != first {
		t.Fatalf("resolved_at_ms = %d, want the first settlement's instant %d kept", got, first)
	}
}

func TestASessionHoldingNoBounceCardsResolvesNothing(t *testing.T) {
	// Arrange — the ordinary case, which must cost nothing and push nothing.
	c := receiptConsumer(t, newFakeReceiptStore())

	// Act.
	c.resolveBounceWindowCards("shim_ready")

	// Assert.
	if len(c.snapshotFailItems()) != 0 {
		t.Fatalf("items = %v, want nothing pushed for a session with no cards", c.snapshotFailItems())
	}
}
