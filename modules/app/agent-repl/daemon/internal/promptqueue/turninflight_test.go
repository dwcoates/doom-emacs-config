package promptqueue

import (
	"testing"

	"agent-repld/internal/ids"
)

func TestTurnInFlightAnswersTheWatchersTurnInFlight(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		want      ids.TurnID
		wantFound bool
	}{
		{name: "a turn in flight", arrange: func(h *harness) { h.watcher.running("running-turn") }, want: "running-turn", wantFound: true},
		{name: "no turn in flight", arrange: func(h *harness) { h.watcher.idle() }, want: "", wantFound: false},
		{name: "no watcher", arrange: func(h *harness) { h.watcher.running("running-turn"); h.noWatcher = true }, want: "", wantFound: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			got, found := h.q.turnInFlight(theWorkspace)

			// Assert
			if got != tt.want || found != tt.wantFound {
				t.Fatalf("turnInFlight = (%q, %v), want (%q, %v)", got, found, tt.want, tt.wantFound)
			}
		})
	}
}
