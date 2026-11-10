package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

func TestMarkIntakeRecordsTheWorkspaceAndHidesIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.MarkIntake(context.Background(), ws.ID, instant)

	// Assert
	if err != nil {
		t.Fatalf("MarkIntake: %v", err)
	}
	intake, err := s.IsIntake(context.Background(), ws.ID)
	if err != nil || !intake {
		t.Fatalf("IsIntake = %v, %v; want the marked workspace", intake, err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || !got.Hidden {
		t.Fatalf("Workspace hidden = %v, %v; want the marked workspace hidden", got.Hidden, err)
	}
}

func TestAWorkspaceNeverMarkedIsNoIntakeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	intake, err := s.IsIntake(context.Background(), ws.ID)

	// Assert
	if err != nil || intake {
		t.Fatalf("IsIntake = %v, %v; want false", intake, err)
	}
}

func TestMarkIntakeFails(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store) WorkspaceID
		wantErr error
	}{
		{name: "an unregistered workspace", arrange: func(*testing.T, *store) WorkspaceID { return "ws-nobody" }, wantErr: ErrNotFound},
		{name: "a workspace marked before", arrange: func(t *testing.T, s *store) WorkspaceID {
			ws := testWorkspace(t, s)
			if err := s.MarkIntake(context.Background(), ws.ID, instant); err != nil {
				t.Fatalf("MarkIntake: %v", err)
			}
			return ws.ID
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			id := tt.arrange(t, s)

			// Act
			err := s.MarkIntake(context.Background(), id, instant)

			// Assert
			if err == nil || (tt.wantErr != nil && !errors.Is(err, tt.wantErr)) {
				t.Fatalf("MarkIntake = %v, want a failure of %v", err, tt.wantErr)
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.mark_intake"}); !ok {
				t.Fatalf("want an ERROR daemon.wsm.mark_intake record, got %+v", logs.Records())
			}
		})
	}
}

func TestForgettingAnIntakeWorkspaceForgetsItsMark(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.MarkIntake(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("MarkIntake: %v", err)
	}
	if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByDaemon}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Act
	_, err := s.Forget(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Forget: %v", err)
	}
	intake, err := s.IsIntake(context.Background(), ws.ID)
	if err != nil || intake {
		t.Fatalf("IsIntake after the forget = %v, %v; want false", intake, err)
	}
}
