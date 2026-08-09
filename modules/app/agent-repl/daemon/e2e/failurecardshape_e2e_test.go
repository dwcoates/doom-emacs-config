// The FAILURE CARD's shape, and how a window-shaped failure ends.
//
// failure-card.proto states both halves. A card carries its kind ("WHAT failed,
// and its evidence. The card takes its color from the kind's side of the
// vocabulary") and a lifecycle arm ("HOW this card ends, as arms, so that an
// open alarm and its all-clear can never be the same shape with a different
// number in it").
//
// And the resolution rule: "A window-shaped failure is re-sent under the SAME
// ConversationItem.uuid with a different arm here, and the feed reconciles it
// in place. Appending instead would leave the alarm standing beside its own
// all-clear."
//
// A NEW uuid for the all-clear is precisely the defect the same-uuid rule
// exists to prevent, and it is invisible to any assertion that only checks the
// resolved card exists — which is why this test carries the opening card's uuid
// forward and demands it back.
package e2e

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// TestE2EAWindowShapedFailureResolvesUnderItsOwnUUID covers the FAILURE CARD
// SHAPE edge.
func TestE2EAWindowShapedFailureResolvesUnderItsOwnUUID(t *testing.T) {
	// Arrange
	h := newUDSHarness(t)
	cwd := t.TempDir()
	_, conn, vendorID, store := liveSession(t, h, cwd)
	// A connectivity-impact component, which is window-shaped by construction:
	// the shim reports the degradation and later reports the SAME component
	// recovered, and those two reports are the window's two edges.
	const (
		component = "shim-store-client"
		reason    = "e2e_store_link_lost"
	)

	// Act — the window OPENS.
	store.write(degradedStateEvent(vendorID, component, reason, 7, false))

	// Assert — the opening card, whole.
	opened, _ := awaitItem(t, conn, cwd, "the opening failure card", func(it *frontendv1.ConversationItem) bool {
		return it.GetFailureCard() != nil
	})
	// FailureKind is ITSELF a oneof over the classified vocabulary
	// (errors.proto), so "a kind is set" means the INNER arm is present. Checking
	// only that the outer message is non-nil would pass for an empty
	// classification, which is the same unusable card with one more indirection.
	card := opened.GetFailureCard()
	if card.GetKind().GetKind() == nil {
		t.Errorf("the failure card arrived with NO kind arm set: the card has no classification to take its color or its evidence from")
	}
	if card.GetLifecycle() == nil {
		t.Fatalf("the failure card arrived with NO lifecycle arm set: a reader cannot tell an open alarm from one that has already ended")
	}
	if card.GetOpen() == nil {
		t.Errorf("the opening card's lifecycle arm = %T, want FailureCardOpen: a window that has only just opened must invite waiting", card.GetLifecycle())
	}
	if opened.GetUuid() == "" {
		t.Fatal("the failure card arrived with an empty ConversationItem.uuid: nothing can be re-sent under it, so the window can never close in place")
	}

	// Act — the window CLOSES.
	store.write(degradedStateEvent(vendorID, component, reason, 7, true))

	// Assert — the SAME card, resolved.
	resolved, _ := awaitItem(t, conn, cwd, "the resolved failure card", func(it *frontendv1.ConversationItem) bool {
		return it.GetFailureCard().GetResolved() != nil
	})
	if got, want := resolved.GetUuid(), opened.GetUuid(); got != want {
		t.Errorf("the all-clear arrived under uuid %q, want the opening card's %q: the feed cannot reconcile it in place, so the alarm stands beside its own resolution",
			got, want)
	}
	if got := resolved.GetFailureCard().GetResolved().GetResolvedAtMs(); got <= 0 {
		t.Errorf("the resolved card's resolved_at_ms = %d, want the instant the window closed: a settled card with no closing stamp cannot say when it ended", got)
	}
	if resolved.GetFailureCard().GetKind().GetKind() == nil {
		t.Errorf("the resolved card dropped its kind arm: a settled card keeps its place in the conversation and still has to say what it was about")
	}
}
