package promptqueue

import (
	"slices"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

func TestTheTurnEdgesAreToldWhereTheyHappen(t *testing.T) {
	tests := []struct {
		name string
		act  func(t *testing.T, h *harness)
		want []string
	}{
		{name: "a vendor-started turn the shim took is a start", act: func(_ *testing.T, h *harness) {
			h.q.OnTurnAdopted(theWorkspace, vendorStartedTurn)
		}, want: []string{"started:" + string(theWorkspace)}},
		{name: "a turn closing is an end", act: func(t *testing.T, h *harness) {
			running(t, h, "running-turn", "the running work")
			h.watcher.idle()
			h.q.OnTurnEnded(theWorkspace, "running-turn", wsm.CloseFailed)
		}, want: []string{"ended:" + string(theWorkspace)}},
		{name: "turns an adoption found ended are one unobserved end", act: func(_ *testing.T, h *harness) {
			h.q.OnTurnsEndedUnobserved(theWorkspace, []ids.TurnID{"t1", "t2"})
		}, want: []string{"ended_unobserved:" + string(theWorkspace)}},
		{name: "an adoption that found no ended turn ends nothing", act: func(_ *testing.T, h *harness) {
			h.q.OnTurnsEndedUnobserved(theWorkspace, nil)
		}},
		{name: "a prompt folded into the running turn ends nothing", act: func(t *testing.T, h *harness) {
			running(t, h, "running-turn", "the running work")
			h.q.OnTurnEnded(theWorkspace, "folded-turn", wsm.CloseFolded)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			tt.act(t, h)

			// Assert
			if got := h.edges.edgesTold(); !slices.Equal(got, tt.want) {
				t.Errorf("edges = %v, want %v", got, tt.want)
			}
		})
	}
}
