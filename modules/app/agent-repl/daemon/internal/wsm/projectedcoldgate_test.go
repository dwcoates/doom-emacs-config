package wsm

import (
	"context"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func projectedCold(tokens uint64) *conversationv1.SessionCold {
	return &conversationv1.SessionCold{
		ContextTokens: tokens, LastRequestAtMs: 1800,
		Reason: &conversationv1.SessionCold_Lapsed{Lapsed: &conversationv1.SessionColdLapsed{CacheTtlMs: 3_600_000}},
	}
}

func TestProjectedColdGateRecord(t *testing.T) {
	due := time.Unix(1_800_000, 0).UTC()
	tests := []struct {
		name      string
		set       []*conversationv1.SessionCold
		clear     bool
		want      *conversationv1.SessionCold
		wantFound bool
	}{
		{name: "a workspace with no projection has none"},
		{name: "a recorded projection is read back whole", set: []*conversationv1.SessionCold{projectedCold(120_000)}, want: projectedCold(120_000), wantFound: true},
		{name: "a later projection replaces it", set: []*conversationv1.SessionCold{projectedCold(120_000), projectedCold(90_000)}, want: projectedCold(90_000), wantFound: true},
		{name: "a cleared projection is gone", set: []*conversationv1.SessionCold{projectedCold(120_000)}, clear: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			for _, cold := range tt.set {
				if err := s.SetProjectedColdGate(context.Background(), ws, due, cold); err != nil {
					t.Fatalf("SetProjectedColdGate: %v", err)
				}
			}
			if tt.clear {
				if cleared, err := s.ClearProjectedColdGate(context.Background(), ws); err != nil || !cleared {
					t.Fatalf("ClearProjectedColdGate = %v, %v; want the projection cleared", cleared, err)
				}
			}

			// Act
			got, found, err := s.ProjectedColdGate(context.Background(), ws)

			// Assert
			if err != nil {
				t.Fatalf("ProjectedColdGate: %v", err)
			}
			if found != tt.wantFound || (found && (!got.DueAt.Equal(due) || !proto.Equal(got.Cold, tt.want))) {
				t.Fatalf("ProjectedColdGate = %+v, %v; want %v at %v, %v", got, found, tt.want, due, tt.wantFound)
			}
		})
	}
}

func TestSetProjectedColdGateRefusesNoFacts(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID

	// Act
	err := s.SetProjectedColdGate(context.Background(), ws, time.Now(), nil)

	// Assert
	if err == nil {
		t.Fatal("SetProjectedColdGate with no cold facts = nil, want a refusal")
	}
}

func TestClearProjectedColdGateWithNoneAnswersFalse(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID

	// Act
	cleared, err := s.ClearProjectedColdGate(context.Background(), ws)

	// Assert
	if err != nil || cleared {
		t.Fatalf("ClearProjectedColdGate = %v, %v; want false, nil", cleared, err)
	}
}

func TestProjectedColdGatesListsEveryWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	a, b := testWorkspace(t, s).ID, testWorkspace(t, s).ID
	due := time.Unix(1_800_000, 0).UTC()
	for _, ws := range []WorkspaceID{a, b} {
		if err := s.SetProjectedColdGate(context.Background(), ws, due, projectedCold(120_000)); err != nil {
			t.Fatalf("SetProjectedColdGate: %v", err)
		}
	}

	// Act
	all, err := s.ProjectedColdGates(context.Background())

	// Assert
	if err != nil || len(all) != 2 {
		t.Fatalf("ProjectedColdGates = %v, %v; want both workspaces", all, err)
	}
}
