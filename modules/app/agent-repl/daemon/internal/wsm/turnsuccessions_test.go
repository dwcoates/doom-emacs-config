package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestRecordTurnSuccessionRefusesAnInconsistentMove(t *testing.T) {
	tests := []struct {
		name string
		move func(ws WorkspaceID) TurnSuccession
	}{
		{"no workspace", func(WorkspaceID) TurnSuccession {
			return TurnSuccession{Turn: "a", Kind: SuccessionReheld, Successor: "b"}
		}},
		{"no turn", func(ws WorkspaceID) TurnSuccession {
			return TurnSuccession{Workspace: ws, Kind: SuccessionReheld, Successor: "b"}
		}},
		{"no successor", func(ws WorkspaceID) TurnSuccession {
			return TurnSuccession{Workspace: ws, Turn: "a", Kind: SuccessionReheld}
		}},
		{"an undeclared kind", func(ws WorkspaceID) TurnSuccession {
			return TurnSuccession{Workspace: ws, Turn: "a", Kind: "teleported", Successor: "b"}
		}},
		{"a turn succeeding itself", func(ws WorkspaceID) TurnSuccession {
			return TurnSuccession{Workspace: ws, Turn: "a", Kind: SuccessionReheld, Successor: "a"}
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testWorkspace(t, s).ID

			// Act
			err := s.RecordTurnSuccession(context.Background(), tc.move(ws))

			// Assert
			if err == nil {
				t.Fatal("RecordTurnSuccession recorded an inconsistent move")
			}
			if !loggedOperation(log, "daemon.wsm.record_turn_succession", "error") {
				t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
			}
			if n := scalar[int](t, s, `SELECT COUNT(*) FROM turn_successions`); n != 0 {
				t.Fatalf("%d rows written by a refused move", n)
			}
		})
	}
}

func TestRecordTurnSuccessionTwiceKeepsOneMove(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	move := TurnSuccession{Workspace: ws, Turn: "a", Kind: SuccessionReheld, Successor: "b", At: instant}

	// Act
	err1 := s.RecordTurnSuccession(context.Background(), move)
	err2 := s.RecordTurnSuccession(context.Background(), move)

	// Assert
	if err1 != nil || err2 != nil {
		t.Fatalf("RecordTurnSuccession = %v, %v; want both to stand", err1, err2)
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM turn_successions`); n != 1 {
		t.Fatalf("%d rows, want the one move", n)
	}
}

func TestRecordTurnSuccessionRefusesASecondSuccessorForTheSameMove(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	ctx := context.Background()
	if err := s.RecordTurnSuccession(ctx, TurnSuccession{Workspace: ws, Turn: "a", Kind: SuccessionReheld, Successor: "b", At: instant}); err != nil {
		t.Fatalf("first move: %v", err)
	}

	// Act
	err := s.RecordTurnSuccession(ctx, TurnSuccession{Workspace: ws, Turn: "a", Kind: SuccessionReheld, Successor: "c", At: instant})

	// Assert
	if err == nil {
		t.Fatal("a turn was left twice the same way, to two turns")
	}
	got, _ := s.SucceedingTurn(ctx, "a")
	if got != "b" {
		t.Fatalf("SucceedingTurn = %q, want the first successor kept", got)
	}
}

func TestSucceedingTurnFollowsTheChain(t *testing.T) {
	tests := []struct {
		name  string
		moves []TurnSuccession
		want  TurnID
	}{
		{name: "a turn that never moved", want: "a"},
		{name: "one re-hold", moves: []TurnSuccession{{Turn: "a", Kind: SuccessionReheld, Successor: "b"}}, want: "b"},
		{name: "a fold", moves: []TurnSuccession{{Turn: "a", Kind: SuccessionFolded, Successor: "r"}}, want: "r"},
		{name: "a fold into a turn a stand-down cut", moves: []TurnSuccession{
			{Turn: "a", Kind: SuccessionFolded, Successor: "r"},
			{Turn: "r", Kind: SuccessionResumed, Successor: "y"},
		}, want: "y"},
		{name: "a resubmission outranks the fold it followed", moves: []TurnSuccession{
			{Turn: "a", Kind: SuccessionFolded, Successor: "r"},
			{Turn: "a", Kind: SuccessionResubmitted, Successor: "c"},
		}, want: "c"},
		{name: "a resubmission recorded before the fold still outranks it", moves: []TurnSuccession{
			{Turn: "a", Kind: SuccessionResubmitted, Successor: "c"},
			{Turn: "a", Kind: SuccessionFolded, Successor: "r"},
		}, want: "c"},
		{name: "a re-hold then a resubmitted displacement", moves: []TurnSuccession{
			{Turn: "a", Kind: SuccessionReheld, Successor: "b"},
			{Turn: "b", Kind: SuccessionResubmitted, Successor: "c"},
		}, want: "c"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			ctx := context.Background()
			for _, m := range tc.moves {
				m.Workspace, m.At = ws, instant
				if err := s.RecordTurnSuccession(ctx, m); err != nil {
					t.Fatalf("RecordTurnSuccession(%+v): %v", m, err)
				}
			}

			// Act
			got, err := s.SucceedingTurn(ctx, "a")

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("SucceedingTurn = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestSucceedingTurnFailsHardOnACycle(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s).ID
	corrupt(t, s, `INSERT INTO turn_successions (turn_id, kind, successor_id, workspace_id, at) VALUES ('a', 'reheld', 'b', ?, 0), ('b', 'reheld', 'a', ?, 0)`, ws, ws)

	// Act
	_, err := s.SucceedingTurn(context.Background(), "a")

	// Assert
	if !errors.Is(err, errSuccessionCycle) {
		t.Fatalf("SucceedingTurn = %v, want the cycle refused", err)
	}
	if !loggedOperation(log, "daemon.wsm.succeeding_turn", "error") {
		t.Fatalf("the cycle was not recorded at ERROR: %v", log.Records())
	}
}

func TestForgettingAWorkspaceDropsItsSuccessions(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	ctx := context.Background()
	if err := s.RecordTurnSuccession(ctx, TurnSuccession{Workspace: ws, Turn: "a", Kind: SuccessionReheld, Successor: "b", At: instant}); err != nil {
		t.Fatalf("RecordTurnSuccession: %v", err)
	}

	// Act
	corrupt(t, s, `DELETE FROM workspaces WHERE id = ?`, ws)

	// Assert
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM turn_successions`); n != 0 {
		t.Fatalf("%d successions outlived their workspace", n)
	}
}
