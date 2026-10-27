package wsm

import (
	"context"
	"slices"
	"testing"
)

// everyTurnSet is every durable turn set, so each invariant the helper
// guarantees is pinned for every set at once.
var everyTurnSet = []turnSet{rolledBackTurns, signedInTurns, reconnectedTurns}

func TestTurnSetRecordRoundTripsInJoinOrder(t *testing.T) {
	for _, set := range everyTurnSet {
		t.Run(set.table, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			ctx := context.Background()

			// Act
			if err := s.recordTurnSet(ctx, set, ws.ID, []TurnID{"t2", "t3"}); err != nil {
				t.Fatalf("recordTurnSet: %v", err)
			}
			got, err := s.loadTurnSet(ctx, set, ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("loadTurnSet: %v", err)
			}
			slices.Sort(got)
			if !slices.Equal(got, []TurnID{"t2", "t3"}) {
				t.Fatalf("loadTurnSet() = %v, want [t2 t3]", got)
			}
		})
	}
}

func TestTurnSetRecordIsIdempotent(t *testing.T) {
	for _, set := range everyTurnSet {
		t.Run(set.table, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			ctx := context.Background()
			if err := s.recordTurnSet(ctx, set, ws.ID, []TurnID{"t2"}); err != nil {
				t.Fatalf("seed: %v", err)
			}

			// Act
			err := s.recordTurnSet(ctx, set, ws.ID, []TurnID{"t2"})

			// Assert
			if err != nil {
				t.Fatalf("recordTurnSet again: %v", err)
			}
			if got, _ := s.loadTurnSet(ctx, set, ws.ID); len(got) != 1 {
				t.Fatalf("loadTurnSet() = %v, want one", got)
			}
		})
	}
}

func TestTurnSetRecordRefusesAnIncompleteRecording(t *testing.T) {
	cases := []struct {
		name  string
		turns []TurnID
	}{
		{name: "no turns"},
		{name: "an empty turn", turns: []TurnID{"t2", ""}},
	}
	for _, set := range everyTurnSet {
		for _, tc := range cases {
			t.Run(set.table+"/"+tc.name, func(t *testing.T) {
				// Arrange
				s, log := testStore(t)
				ws := testWorkspace(t, s)

				// Act
				err := s.recordTurnSet(context.Background(), set, ws.ID, tc.turns)

				// Assert
				if err == nil {
					t.Fatal("recordTurnSet accepted an incomplete recording")
				}
				if !loggedOperation(log, set.recordOp, "error") {
					t.Fatalf("no ERROR record under %s: %v", set.recordOp, log.Records())
				}
				if got, _ := s.loadTurnSet(context.Background(), set, ws.ID); len(got) != 0 {
					t.Fatalf("loadTurnSet() = %v, want nothing recorded", got)
				}
			})
		}
	}
}

// TestTurnSetPublicMethodsShareTheHelper pins that each set's public pair goes
// through the helper: what the public record writes, the helper's load reads,
// and the other way round.
func TestTurnSetPublicMethodsShareTheHelper(t *testing.T) {
	type pair struct {
		set    turnSet
		record func(s *store, ctx context.Context, id WorkspaceID, turns []TurnID) error
		load   func(s *store, ctx context.Context, id WorkspaceID) ([]TurnID, error)
	}
	pairs := []pair{
		{set: rolledBackTurns, record: (*store).RecordRolledBackTurns, load: (*store).RolledBackTurns},
		{set: signedInTurns, record: (*store).RecordSignedInTurns, load: (*store).SignedInTurns},
		{set: reconnectedTurns, record: (*store).RecordReconnectedTurns, load: (*store).ReconnectedTurns},
	}
	if len(pairs) != len(everyTurnSet) {
		t.Fatalf("%d public pairs for %d turn sets: a set has no pinned public pair", len(pairs), len(everyTurnSet))
	}
	for _, p := range pairs {
		t.Run(p.set.table, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			ctx := context.Background()

			// Act
			if err := p.record(s, ctx, ws.ID, []TurnID{"public"}); err != nil {
				t.Fatalf("public record: %v", err)
			}
			if err := s.recordTurnSet(ctx, p.set, ws.ID, []TurnID{"helper"}); err != nil {
				t.Fatalf("helper record: %v", err)
			}
			viaHelper, _ := s.loadTurnSet(ctx, p.set, ws.ID)
			viaPublic, _ := p.load(s, ctx, ws.ID)

			// Assert
			slices.Sort(viaHelper)
			slices.Sort(viaPublic)
			want := []TurnID{"helper", "public"}
			if !slices.Equal(viaHelper, want) || !slices.Equal(viaPublic, want) {
				t.Fatalf("helper load %v, public load %v, want both %v", viaHelper, viaPublic, want)
			}
		})
	}
}
