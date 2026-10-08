package footer

import "testing"

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
