package wsm

import (
	"context"
	"path/filepath"
	"testing"
)

// registerBornIntake registers a linked workspace born an intake workspace.
func registerBornIntake(t *testing.T, s *store, repoDir, name string) Workspace {
	t.Helper()
	registerMain(t, s, repoDir)
	ws, _, err := s.RegisterWorkspace(context.Background(), filepath.Join(t.TempDir(), name), RegisterFacts{
		Name: name, Branch: name, ParentBranch: "master", RepoDir: repoDir, Intake: true,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace(%s): %v", name, err)
	}
	return ws
}

func TestAnIntakeWorkspaceIsBornMarkedAndHidden(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := registerBornIntake(t, s, arrangeRepo(t, "repo"), "intake")

	// Assert
	intake, err := s.IsIntake(context.Background(), ws.ID)
	if err != nil || !intake {
		t.Fatalf("IsIntake = %v, %v; want the intake workspace", intake, err)
	}
	if !ws.Hidden || ws.HiddenAt == nil {
		t.Fatalf("the record Register answered = hidden %v at %v, want an intake workspace born hidden", ws.Hidden, ws.HiddenAt)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || !got.Hidden {
		t.Fatalf("Workspace hidden = %v, %v; want the intake workspace hidden", got.Hidden, err)
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

func TestForgettingAnIntakeWorkspaceForgetsItsMark(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := registerBornIntake(t, s, arrangeRepo(t, "repo"), "intake")
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
