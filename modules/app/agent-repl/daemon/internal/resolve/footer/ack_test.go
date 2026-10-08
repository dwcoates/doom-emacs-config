package footer

import "testing"

func TestTheAckEndsTheSubmittingStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetTurn(testWS, &TurnStarted{At: instant, Act: ActPrompt})

	// Act
	h.r.AckTurn(testWS)

	// Assert
	if got := h.view(t).GetStrip().GetStatus().GetWorking().GetThinking(); got == nil {
		t.Fatalf("status = %v, want working · thinking", h.view(t).GetStrip().GetStatus())
	}
}

func TestAnAckWithNoTurnLeavesTheStripIdle(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.AckTurn(testWS)

	// Assert
	if got := h.status(t); got != "idle" {
		t.Fatalf("status = %q, want idle", got)
	}
}
