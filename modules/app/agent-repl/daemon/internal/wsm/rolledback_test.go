package wsm

import (
	"context"
	"slices"
	"testing"
)

func TestRecordRolledBackTurnsRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	if err := s.RecordRolledBackTurns(ctx, ws.ID, []TurnID{"t2", "t3"}); err != nil {
		t.Fatalf("RecordRolledBackTurns: %v", err)
	}
	got, err := s.RolledBackTurns(ctx, ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("RolledBackTurns: %v", err)
	}
	slices.Sort(got)
	if !slices.Equal(got, []TurnID{"t2", "t3"}) {
		t.Fatalf("RolledBackTurns() = %v, want [t2 t3]", got)
	}
}

func TestRecordRolledBackTurnsAcceptsATurnRecordedTwice(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.RecordRolledBackTurns(ctx, ws.ID, []TurnID{"t2"}); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	err := s.RecordRolledBackTurns(ctx, ws.ID, []TurnID{"t2"})

	// Assert
	if err != nil {
		t.Fatalf("RecordRolledBackTurns again: %v", err)
	}
	if got, _ := s.RolledBackTurns(ctx, ws.ID); len(got) != 1 {
		t.Fatalf("RolledBackTurns() = %v, want one", got)
	}
}

func TestRolledBackTurnsAreKeptPerWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws, other := testWorkspace(t, s), testWorkspace(t, s)
	ctx := context.Background()

	// Act
	if err := s.RecordRolledBackTurns(ctx, ws.ID, []TurnID{"t2"}); err != nil {
		t.Fatalf("RecordRolledBackTurns: %v", err)
	}

	// Assert
	if got, _ := s.RolledBackTurns(ctx, other.ID); len(got) != 0 {
		t.Fatalf("the other workspace has %v, want none", got)
	}
}

func TestRecordRolledBackTurnsRefusesAnIncompleteRollback(t *testing.T) {
	cases := []struct {
		name  string
		turns []TurnID
	}{
		{name: "no turns"},
		{name: "an empty turn", turns: []TurnID{"t2", ""}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := s.RecordRolledBackTurns(context.Background(), ws.ID, tc.turns)

			// Assert
			if err == nil {
				t.Fatal("RecordRolledBackTurns accepted an incomplete rollback")
			}
			if !loggedOperation(log, "daemon.wsm.record_rolled_back_turns", "error") {
				t.Fatalf("no ERROR record: %v", log.Records())
			}
			if got, _ := s.RolledBackTurns(context.Background(), ws.ID); len(got) != 0 {
				t.Fatalf("RolledBackTurns() = %v, want nothing recorded", got)
			}
		})
	}
}
