package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
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
	if !got.Hidden || got.HiddenAt == nil || !got.HiddenAt.Equal(backlogStamp) {
		t.Fatalf("hidden = %v at %v, want hidden at %v", got.Hidden, got.HiddenAt, backlogStamp)
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
		{name: "already hidden", arrange: func(t *testing.T, s *store, ws Workspace) {
			mustHide(t, s, ws.ID)
		}},
		{name: "closed", arrange: func(t *testing.T, s *store, ws Workspace) {
			mustClose(t, s, ws.ID, CloseByUser)
		}},
		{name: "already backlogged for this gate and shown again", arrange: func(t *testing.T, s *store, ws Workspace) {
			if _, err := s.BacklogColdGate(context.Background(), ws.ID, backlogGate, backlogGate); err != nil {
				t.Fatalf("BacklogColdGate: %v", err)
			}
			if err := s.SetHidden(context.Background(), ws.ID, false, backlogGate); err != nil {
				t.Fatalf("SetHidden: %v", err)
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
			if after.Hidden != before.Hidden || !sameInstant(after.HiddenAt, before.HiddenAt) || !sameInstant(after.BackloggedGateAt, before.BackloggedGateAt) {
				t.Fatalf("the row moved: before hidden=%v at %v gate %v, after hidden=%v at %v gate %v",
					before.Hidden, before.HiddenAt, before.BackloggedGateAt, after.Hidden, after.HiddenAt, after.BackloggedGateAt)
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
	if err := s.SetHidden(context.Background(), ws.ID, false, backlogGate); err != nil {
		t.Fatalf("SetHidden: %v", err)
	}
	newGate := backlogGate.Add(48 * time.Hour)

	// Act
	moved, err := s.BacklogColdGate(context.Background(), ws.ID, newGate, backlogStamp)

	// Assert
	if err != nil || !moved {
		t.Fatalf("BacklogColdGate = %v, %v; want moved", moved, err)
	}
	if got := workspaceNow(t, s, ws.ID); !got.Hidden || got.BackloggedGateAt == nil || !got.BackloggedGateAt.Equal(newGate) {
		t.Fatalf("hidden = %v gate %v, want hidden for %v", got.Hidden, got.BackloggedGateAt, newGate)
	}
}

func TestBacklogColdGateRefusesAParentWithOpenChildren(t *testing.T) {
	// Arrange
	f := newFamily(t)

	// Act
	moved, err := f.s.BacklogColdGate(context.Background(), f.parent.ID, backlogGate, backlogStamp)

	// Assert
	requireHasOpenChildren(t, err, f.parent, f.child)
	if got := workspaceNow(t, f.s, f.parent.ID); moved || got.Hidden || got.BackloggedGateAt != nil {
		t.Fatalf("a refused backlog moved the parent: moved=%v hidden=%v gate=%v", moved, got.Hidden, got.BackloggedGateAt)
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

// sameInstant reports whether two optional instants are the same.
func sameInstant(a, b *time.Time) bool {
	return (a == nil) == (b == nil) && (a == nil || a.Equal(*b))
}
