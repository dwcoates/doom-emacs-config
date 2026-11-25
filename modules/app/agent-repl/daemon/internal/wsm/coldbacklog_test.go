package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
)

var (
	backlogGate  = time.Unix(1_700_000_000, 0).UTC()
	backlogStamp = time.Unix(1_700_200_000, 0).UTC()
)

func TestBacklogColdGateMovesAShownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	moved, err := s.BacklogColdGate(context.Background(), ws.ID, backlogGate, backlogStamp)

	// Assert
	if err != nil || !moved {
		t.Fatalf("BacklogColdGate = %v, %v; want moved", moved, err)
	}
	got := workspaceNow(t, s, ws.ID)
	if !got.Backlogged || got.BackloggedAt == nil || !got.BackloggedAt.Equal(backlogStamp) {
		t.Fatalf("backlogged = %v at %v, want backlogged at %v", got.Backlogged, got.BackloggedAt, backlogStamp)
	}
	if got.BackloggedGateAt == nil || !got.BackloggedGateAt.Equal(backlogGate) {
		t.Fatalf("backlogged gate = %v, want %v", got.BackloggedGateAt, backlogGate)
	}
}

func TestBacklogColdGateLeavesAWorkspaceAsItIs(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*testing.T, *store, Workspace)
	}{
		{name: "the user is looking at", arrange: func(t *testing.T, s *store, ws Workspace) {
			if err := s.SetCurrent(context.Background(), ws.ID, backlogGate); err != nil {
				t.Fatalf("SetCurrent: %v", err)
			}
		}},
		{name: "already backlogged", arrange: func(t *testing.T, s *store, ws Workspace) {
			mustHide(t, s, ws.ID)
		}},
		{name: "closed", arrange: func(t *testing.T, s *store, ws Workspace) {
			mustClose(t, s, ws.ID, CloseByUser)
		}},
		{name: "already backlogged for this gate and shown again", arrange: func(t *testing.T, s *store, ws Workspace) {
			if _, err := s.BacklogColdGate(context.Background(), ws.ID, backlogGate, backlogGate); err != nil {
				t.Fatalf("BacklogColdGate: %v", err)
			}
			if err := s.SetBacklogged(context.Background(), ws.ID, false, backlogGate); err != nil {
				t.Fatalf("SetBacklogged: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			tt.arrange(t, s, ws)
			before := workspaceNow(t, s, ws.ID)

			// Act
			moved, err := s.BacklogColdGate(context.Background(), ws.ID, backlogGate, backlogStamp)

			// Assert
			if err != nil || moved {
				t.Fatalf("BacklogColdGate = %v, %v; want left as it is", moved, err)
			}
			after := workspaceNow(t, s, ws.ID)
			if after.Backlogged != before.Backlogged || !clocktest.SameInstant(after.BackloggedAt, before.BackloggedAt) || !clocktest.SameInstant(after.BackloggedGateAt, before.BackloggedGateAt) {
				t.Fatalf("the row moved: before backlogged=%v at %v gate %v, after backlogged=%v at %v gate %v",
					before.Backlogged, before.BackloggedAt, before.BackloggedGateAt, after.Backlogged, after.BackloggedAt, after.BackloggedGateAt)
			}
		})
	}
}

func TestBacklogColdGateMovesAShownWorkspaceAgainForANewGate(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.BacklogColdGate(context.Background(), ws.ID, backlogGate, backlogGate); err != nil {
		t.Fatalf("BacklogColdGate: %v", err)
	}
	if err := s.SetBacklogged(context.Background(), ws.ID, false, backlogGate); err != nil {
		t.Fatalf("SetBacklogged: %v", err)
	}
	newGate := backlogGate.Add(48 * time.Hour)

	// Act
	moved, err := s.BacklogColdGate(context.Background(), ws.ID, newGate, backlogStamp)

	// Assert
	if err != nil || !moved {
		t.Fatalf("BacklogColdGate = %v, %v; want moved", moved, err)
	}
	if got := workspaceNow(t, s, ws.ID); !got.Backlogged || got.BackloggedGateAt == nil || !got.BackloggedGateAt.Equal(newGate) {
		t.Fatalf("backlogged = %v gate %v, want backlogged for %v", got.Backlogged, got.BackloggedGateAt, newGate)
	}
}

func TestBacklogColdGateRefusesAParentWithOpenChildren(t *testing.T) {
	// Arrange
	f := newFamily(t)

	// Act
	moved, err := f.s.BacklogColdGate(context.Background(), f.parent.ID, backlogGate, backlogStamp)

	// Assert
	requireHasOpenChildren(t, err, f.parent, f.child)
	if got := workspaceNow(t, f.s, f.parent.ID); moved || got.Backlogged || got.BackloggedGateAt != nil {
		t.Fatalf("a refused backlog moved the parent: moved=%v backlogged=%v gate=%v", moved, got.Backlogged, got.BackloggedGateAt)
	}
}

func TestBacklogColdGateRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	moved, err := s.BacklogColdGate(context.Background(), WorkspaceID("no-such-workspace"), backlogGate, backlogStamp)

	// Assert
	if !errors.Is(err, ErrNotFound) || moved {
		t.Fatalf("BacklogColdGate on an unknown workspace = %v, %v; want ErrNotFound", moved, err)
	}
	if !loggedOperation(log, "daemon.wsm.backlog_cold_gate", "debug") {
		t.Fatalf("the missed write was not recorded under its operation: %v", log.Records())
	}
}

func TestANonPositiveBackloggedGateIsCorrupt(t *testing.T) {
	// Arrange: a backlog instant no write path produces.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.db().Exec(`UPDATE workspaces SET cold_gate_backlogged_at = 0 WHERE id = ?`, ws.ID); err != nil {
		t.Fatalf("corrupting the row: %v", err)
	}

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "cold_gate_backlogged_at" {
		t.Fatalf("Workspace = %v, want a cold_gate_backlogged_at DecodeError", err)
	}
}

func TestANeverBackloggedWorkspaceRecordsNoGate(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := testWorkspace(t, s)

	// Assert
	if got := workspaceNow(t, s, ws.ID); got.BackloggedGateAt != nil {
		t.Fatalf("backlogged gate = %v, want none", got.BackloggedGateAt)
	}
}

func TestBacklogsColdGate(t *testing.T) {
	other := backlogGate.Add(time.Hour)
	mergeOf := MergeOrigin{Requester: "w-parent"}
	tests := []struct {
		name string
		ws   Workspace
		want bool
	}{
		{name: "a shown workspace never backlogged", ws: Workspace{}, want: true},
		{name: "a shown workspace backlogged for an earlier gate", ws: Workspace{BackloggedGateAt: &other}, want: true},
		{name: "a shown workspace already backlogged for this gate", ws: Workspace{BackloggedGateAt: &backlogGate}, want: false},
		{name: "the workspace the user is looking at", ws: Workspace{Current: true}, want: false},
		{name: "a backlogged workspace", ws: Workspace{Backlogged: true}, want: false},
		{name: "a closed workspace", ws: Workspace{Closed: true}, want: false},
		{name: "a merge workspace its async merge withholds", ws: Workspace{MergeOf: &mergeOf}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ws.BacklogsColdGate(backlogGate)

			// Assert
			if got != tt.want {
				t.Fatalf("BacklogsColdGate = %v, want %v", got, tt.want)
			}
		})
	}
}
