package footer

import (
	"testing"

	"claude-repld/internal/shimclient"
)

func TestABringUpOnARouteNeverSeenIsStarting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)

	// Act
	h.r.SetBringingUp(testWS, true)

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetAgentReplFault().GetStarting(); got == nil {
		t.Fatalf("status = %v, want agent_repl_fault · starting", h.view(t).GetStrip().GetStatus())
	}
}

func TestABringUpOnASeenRouteDefersToTheLink(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetBringingUp(testWS, true)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle: a connected route is proven", got)
	}
}

func TestATurnAcceptedOnARouteNeverSeenHasNoClock(t *testing.T) {
	// Arrange: a cold submit held while the session comes up.
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Assert: no clock, so no stop control for a turn no session holds.
	if h.view(t).GetStrip().GetClock().TurnStartedAtMs != nil {
		t.Fatal("the clock runs for a turn accepted on a route never seen")
	}
}

func TestTheClockStartsOnceTheRouteIsSeen(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetParticipants(testWS, true, true)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.OnLink(testWS, shimclient.LinkConnected)

	// Assert
	if h.view(t).GetStrip().GetClock().TurnStartedAtMs == nil {
		t.Fatal("the clock did not start once the route was seen")
	}
}
