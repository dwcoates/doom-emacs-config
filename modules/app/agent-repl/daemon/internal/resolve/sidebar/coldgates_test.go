package sidebar_test

import (
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
	"agent-repld/internal/wsm"
)

// gateCard is a standing cold gate's card as the fleet hands it over.
func gateCard(tokens int64) *frontendv1.FeedColdGate {
	return &frontendv1.FeedColdGate{State: &frontendv1.FeedColdGate_Standing{Standing: &frontendv1.FeedColdGateStanding{
		ContextTokens: &frontendv1.FeedColdGateContextTokens{Tokens: tokens},
	}}}
}

func TestTheRosterCarriesTheShownColdGates(t *testing.T) {
	hidden := workspace("w2", "two")
	hidden.Hidden, hidden.HiddenAt = true, at(time.Hour)
	closed := workspace("w3", "three")
	closed.Closed = true
	row := &frontendv1.FeedId{Value: "cold-gate:w1"}
	tests := []struct {
		name   string
		listed wsm.Workspace
		gate   *frontendv1.FeedColdGate
		want   []*frontendv1.RosterColdGate
	}{
		{name: "an open workspace's shown gate is carried", listed: workspace("w1", "one"), gate: gateCard(5),
			want: []*frontendv1.RosterColdGate{{Workspace: workspace("w1", "one").Ref(), Row: row, Gate: gateCard(5)}}},
		{name: "a lifted gate is no longer carried", listed: workspace("w1", "one"), gate: nil, want: nil},
		{name: "a hidden workspace's gate is not carried", listed: hidden, gate: gateCard(5), want: nil},
		{name: "a closed workspace's gate is not carried", listed: closed, gate: gateCard(5), want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)
			r.SetRegistry(registry(tt.listed))
			ws := tt.listed.ID
			r.SetColdGate(ws, &frontendv1.FeedId{Value: "cold-gate:" + string(ws)}, gateCard(1))
			var id *frontendv1.FeedId
			if tt.gate != nil {
				id = &frontendv1.FeedId{Value: "cold-gate:" + string(ws)}
			}

			// Act
			r.SetColdGate(ws, id, tt.gate)

			// Assert
			got := latest(t, r).GetColdGates()
			if len(got) != len(tt.want) {
				t.Fatalf("cold gates = %v, want %v", got, tt.want)
			}
			for i := range got {
				if !proto.Equal(got[i], tt.want[i]) {
					t.Fatalf("cold gate %d = %v, want %v", i, got[i], tt.want[i])
				}
			}
		})
	}
}

func TestARowIDWithoutACardIsRefused(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	r.SetRegistry(registry(workspace("w1", "one")))
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("SetColdGate took a row id without a card, want a panic")
		}
	}()

	// Act
	r.SetColdGate("w1", &frontendv1.FeedId{Value: "cold-gate:w1"}, nil)
}
