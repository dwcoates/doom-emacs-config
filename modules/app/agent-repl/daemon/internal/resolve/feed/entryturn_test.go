package feed

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
)

// A LIVE ROW TAKES THE TURN ITS ENTRY NAMES, not the turn in flight.
func TestALiveRowIsStampedWithItsEntrysTurn(t *testing.T) {
	cases := []struct {
		name  string
		stamp *conversationv1.TurnId
		want  string
	}{
		{name: "a stamped entry of an earlier turn keeps that turn", stamp: &conversationv1.TurnId{Value: "turn-1"}, want: "turn-1"},
		{name: "an unstamped entry falls back to the turn in flight", stamp: nil, want: "turn-2"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: turn-2 is in flight.
			h := newHarness(t)
			h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-1"))
			h.resolver.OnTurnOpened(testWorkspace, ids.TurnID("turn-2"))

			// Act
			h.resolver.OnActivity(testWorkspace, mainAgent(),
				responseFrame("unit-1", &conversationv1.AgentResponseStart{}, nil), tc.stamp, nil)

			// Assert
			if got := h.activityRow("unit-1").GetTurn().GetValue(); got != tc.want {
				t.Fatalf("row turn = %q, want %q", got, tc.want)
			}
		})
	}
}
