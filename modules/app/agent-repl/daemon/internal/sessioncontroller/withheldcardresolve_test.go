package sessioncontroller

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// ---------------------------------------------------------------------------
// A WITHHELD DEGRADATION CARD CLOSES WHEN THE LIVE QUERY IS UP, AND THE
// BRING-UP GATE IS THE ONLY THING THAT MAY SAY SO.
//
// The withhold path deliberately KEEPS the failure card for a retired query's
// death: it is a true account of something that happened, and dropping it would
// lose history the user can read. But the row is DURABLE, so it replays at
// every boot, and nothing ever gave it a closing edge — an unresolved
// degradation card for a query that died days ago sat on a session whose live
// query was healthy and driveable.
//
// The resolving edge is the same one supersederesolve.go uses, for the same
// reason: "the live query genuinely has this workspace" is not a fact until the
// bring-up gate closes, and no weaker edge may claim it.
// ---------------------------------------------------------------------------

// resolvedFailureCards returns every pushed card carrying resolved_at_ms.
func resolvedFailureCards(h *escapeHarness) []*frontendv1.SystemFailureItem {
	var out []*frontendv1.SystemFailureItem
	for _, c := range h.failureCards() {
		if c.GetResolvedAtMs() != 0 {
			out = append(out, c)
		}
	}
	return out
}

// withheldCardHarness replays a retired query's degradation at a session whose
// bring-up gate has NOT yet closed — the exact shape a resuming daemon holds
// while the store hands it durable history.
func withheldCardHarness(t *testing.T) *escapeHarness {
	t.Helper()
	h := terminationHarness(t, &fakeClient{})
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))
	return h
}

func TestAWithheldDegradationCardIsUnresolvedBeforeTheGateCloses(t *testing.T) {
	// Arrange — this is the state the boot log showed: the card is up, and
	// nothing has yet proved the live query is driveable.
	h := withheldCardHarness(t)

	// Act — nothing; no ShimReady has arrived.

	// Assert.
	if got := resolvedFailureCards(h); len(got) != 0 {
		t.Fatalf("resolved cards = %v before the gate closed, want none", got)
	}
}

func TestAWithheldDegradationCardResolvesWhenTheLiveQueryWires(t *testing.T) {
	// Arrange.
	h := withheldCardHarness(t)

	// Act — the bring-up gate closes.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := resolvedFailureCards(h); len(got) != 1 {
		t.Fatalf("resolved cards = %v, want the withheld degradation settled once", got)
	}
}

func TestAResolvedWithheldCardKeepsItsIdentityAndDetail(t *testing.T) {
	// Arrange — RESOLUTION, NOT DELETION. History stays queryable: the card is
	// re-sent under the same uuid with the same account of what happened.
	h := withheldCardHarness(t)
	opened := h.failureCards()
	if len(opened) == 0 {
		t.Fatal("the withheld degradation never produced a card to resolve")
	}
	want := opened[len(opened)-1]

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	got := resolvedFailureCards(h)
	if len(got) != 1 {
		t.Fatalf("resolved cards = %v, want exactly one", got)
	}
	if got[0].GetItemUuid() != want.GetItemUuid() || got[0].GetErrorType() != want.GetErrorType() || got[0].GetSourceDetail() != want.GetSourceDetail() {
		t.Fatalf("the settled card lost the opening card's account: got %+v, want the identity and detail of %+v", got[0], want)
	}
}

func TestAResolvedWithheldCardIsStillTheUnexpectedTerminationCard(t *testing.T) {
	// Arrange — the resolution must not swap the card's classification, or a
	// frontend reconciling on uuid would show a settled card about something
	// else.
	h := withheldCardHarness(t)

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	got := resolvedFailureCards(h)
	if len(got) != 1 || got[0].GetErrorType() != string(errclass.TypeUnexpectedQueryTermination) {
		t.Fatalf("resolved cards = %v, want the unexpected-termination card settled", got)
	}
}

func TestTheWithheldCardResolutionNamesItsReason(t *testing.T) {
	// Arrange — a card that changed state with no record would be a silent
	// mutation of something the user can see.
	h := withheldCardHarness(t)

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if !h.log.contains("withheld degradation card RESOLVED") ||
		!h.log.contains("reason=shim_ready") ||
		!h.log.contains("decision=live_successor_healthy") {
		t.Fatalf("the withheld card resolution was not named in the log: %v", h.log.lines)
	}
}

func TestTheWithheldCardResolutionRecordStaysOnTheInfoChannel(t *testing.T) {
	// Arrange — closing a card because the live successor came up is ordinary
	// progress, and it happens at every boot for as long as the row is durable.
	h := withheldCardHarness(t)

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if h.warn.contains("withheld degradation card RESOLVED") {
		t.Fatalf("a routine card resolution took the warn channel: %v", h.warn.lines)
	}
}

func TestAWithheldCardResolvesOnlyOnce(t *testing.T) {
	// Arrange — a second gate close (a reattach) must not re-settle a card that
	// is already settled, or the feed would gain a duplicate on every reconnect.
	h := withheldCardHarness(t)
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := h.log.count("withheld degradation card RESOLVED"); got != 1 {
		t.Fatalf("resolution records = %d, want exactly 1 across two gate closes: %v", got, h.log.lines)
	}
}

func TestALiveDegradationCardIsNotResolvedByTheGate(t *testing.T) {
	// Arrange — the gate settles WITHHELD cards only. A degradation the live
	// query is reporting right now must keep surfacing exactly as before.
	h := terminationHarness(t, &fakeClient{})
	live := degradationEvent(4142, "live-query")
	live.GetDegradedState().Component = "shim-store-client"
	live.GetDegradedState().Reason = "store unreachable"
	reportDegradation(t, h, live)

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := resolvedFailureCards(h); len(got) != 0 {
		t.Fatalf("resolved cards = %v, want a LIVE degradation left open", got)
	}
}

func TestALiveDegradationOnTheSameCardCancelsTheWithheldResolution(t *testing.T) {
	// Arrange — the retired query's replay put the card up; then the LIVE query
	// reported the same component degraded. The uuid is shared, so resolving it
	// at the gate would close an alarm that is currently true.
	h := terminationHarness(t, &fakeClient{})
	replayed := degradationEvent(4142, "retired-query")
	replayed.GetDegradedState().Component = "shim-store-client"
	replayed.GetDegradedState().Reason = "store unreachable"
	reportDegradation(t, h, replayed)
	live := degradationEvent(4143, "live-query")
	live.GetDegradedState().Component = "shim-store-client"
	live.GetDegradedState().Reason = "store unreachable"
	reportDegradation(t, h, live)

	// Act.
	h.m.onConnected("ws", "s1", &corev1.ShimHello{})

	// Assert.
	if got := resolvedFailureCards(h); len(got) != 0 {
		t.Fatalf("resolved cards = %v, want the live report to keep its card open", got)
	}
}

func TestAStaleShimReadyResolvesNoWithheldCard(t *testing.T) {
	// Arrange — a ShimReady from a RETIRED controller generation proves nothing
	// about this session's live query, so it may not settle a card on its
	// behalf.
	h := withheldCardHarness(t)

	// Act.
	h.m.onConnectedForGeneration("ws", "s1", "g_retired", &corev1.ShimHello{SessionId: "s1"})

	// Assert.
	if got := resolvedFailureCards(h); len(got) != 0 {
		t.Fatalf("resolved cards = %v for a retired generation, want none", got)
	}
}
