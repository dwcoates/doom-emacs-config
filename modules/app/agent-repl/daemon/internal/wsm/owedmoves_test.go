package wsm

import (
	"context"

	"agent-repld/internal/dlog"
	"errors"
	"path/filepath"
	"testing"
)

// moveTarget is a move to a fresh directory under the test's temp dir.
func moveTarget(t *testing.T, branch string) MoveTarget {
	t.Helper()
	return MoveTarget{Branch: branch, Dir: filepath.Join(t.TempDir(), branch)}
}

func TestRenameRecordsTheNameAndOwesTheMove(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
	ctx := context.Background()
	to := moveTarget(t, "renamed")

	// Act
	err := s.Rename(ctx, ws.ID, "renamed", &to)

	// Assert
	if err != nil {
		t.Fatalf("Rename: %v", err)
	}
	got, _ := s.Workspace(ctx, ws.ID)
	if got.Name != "renamed" || got.Branch != ws.Branch || got.Dir != ws.Dir {
		t.Fatalf("workspace = (%q, %q, %q), want the new name over the unmoved branch and dir", got.Name, got.Branch, got.Dir)
	}
	m, found, err := s.OwedMoveOf(ctx, ws.ID)
	if err != nil || !found || m.From != (MoveTarget{Branch: ws.Branch, Dir: ws.Dir}) || m.To.Branch != "renamed" || m.Moving {
		t.Fatalf("OwedMoveOf = (%+v, %v, %v), want a move from %q at %s to the new branch, not yet moving", m, found, err, ws.Branch, ws.Dir)
	}
	if name, owed, _ := s.TakeOwedRename(ctx, ws.ID); !owed || name != "renamed" {
		t.Fatalf("TakeOwedRename = (%q, %v), want the new name owed to the agent", name, owed)
	}
}

func TestRenameWithNoMoveOwesNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	err := s.Rename(ctx, ws.ID, "main-renamed", nil)

	// Assert
	if err != nil {
		t.Fatalf("Rename: %v", err)
	}
	if _, found, err := s.OwedMoveOf(ctx, ws.ID); err != nil || found {
		t.Fatalf("OwedMoveOf = (%v, %v), want none owed", found, err)
	}
}

func TestASecondRenameKeepsWhereTheMoveIsFrom(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
	ctx := context.Background()
	first, second := moveTarget(t, "first"), moveTarget(t, "second")
	if err := s.Rename(ctx, ws.ID, "first", &first); err != nil {
		t.Fatalf("Rename: %v", err)
	}

	// Act
	err := s.Rename(ctx, ws.ID, "second", &second)

	// Assert
	if err != nil {
		t.Fatalf("Rename: %v", err)
	}
	m, _, _ := s.OwedMoveOf(ctx, ws.ID)
	if m.From.Branch != ws.Branch || m.From.Dir != ws.Dir || m.To.Branch != "second" {
		t.Fatalf("owed move = %+v, want it from the original %q to the second target", m, ws.Branch)
	}
}

func TestARenameOverAMoveUnderWayIsRefusedAndWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
	ctx := context.Background()
	first, second := moveTarget(t, "first"), moveTarget(t, "second")
	if err := s.Rename(ctx, ws.ID, "first", &first); err != nil {
		t.Fatalf("Rename: %v", err)
	}
	if marked, err := s.MarkMoving(ctx, ws.ID); err != nil || !marked {
		t.Fatalf("MarkMoving = (%v, %v)", marked, err)
	}

	// Act
	err := s.Rename(ctx, ws.ID, "second", &second)

	// Assert
	if !errors.Is(err, ErrMoveInFlight) {
		t.Fatalf("Rename = %v, want ErrMoveInFlight", err)
	}
	if got, _ := s.Workspace(ctx, ws.ID); got.Name != "first" {
		t.Fatalf("name = %q, want the first name kept", got.Name)
	}
}

func TestRenameRefusesAMoveWithNoBranchOrDir(t *testing.T) {
	tests := []struct {
		name string
		to   MoveTarget
	}{
		{name: "no branch", to: MoveTarget{Dir: "/tmp/x"}},
		{name: "no directory", to: MoveTarget{Branch: "x"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			ws := testLinkedWorkspace(t, s)

			// Act
			err := s.Rename(context.Background(), ws.ID, "x", &tc.to)

			// Assert
			if err == nil {
				t.Fatal("Rename = nil, want the move refused")
			}
			if got, _ := s.Workspace(context.Background(), ws.ID); got.Name != ws.Name {
				t.Fatalf("name = %q, want %q untouched", got.Name, ws.Name)
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.rename"}); !ok {
				t.Fatalf("records = %v, want an error under daemon.wsm.rename", log.Records())
			}
		})
	}
}

func TestMarkMovingOfAWorkspaceOwedNoMoveIsFalse(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)

	// Act
	marked, err := s.MarkMoving(context.Background(), ws.ID)

	// Assert
	if err != nil || marked {
		t.Fatalf("MarkMoving = (%v, %v), want (false, nil)", marked, err)
	}
}

func TestCompleteMoveMovesTheWorkspaceAndItsChildren(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	main := testWorkspaceNamed(t, s, "main")
	ctx := context.Background()
	ws, _, err := s.RegisterWorkspace(ctx, t.TempDir(), RegisterFacts{Name: "parent", Branch: "parent", ParentBranch: "master", RepoDir: main.Dir})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	parent := ws.ID
	child, _, err := s.RegisterWorkspace(ctx, t.TempDir(), RegisterFacts{
		Name: "child", Branch: "child", ParentBranch: ws.Branch, RepoDir: main.Dir, Parent: &parent,
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	to := moveTarget(t, "moved")
	if err := s.Rename(ctx, ws.ID, "moved", &to); err != nil {
		t.Fatalf("Rename: %v", err)
	}

	// Act
	done, found, err := s.CompleteMove(ctx, ws.ID)

	// Assert
	if err != nil || !found || done.To.Branch != "moved" {
		t.Fatalf("CompleteMove = (%+v, %v, %v), want the move completed", done, found, err)
	}
	got, _ := s.Workspace(ctx, ws.ID)
	if got.Branch != "moved" || got.Dir != done.To.Dir {
		t.Fatalf("workspace = (%q, %q), want the moved branch and dir %s", got.Branch, got.Dir, done.To.Dir)
	}
	if c, _ := s.Workspace(ctx, child.ID); c.ParentBranch != "moved" {
		t.Fatalf("child's parent branch = %q, want the moved branch", c.ParentBranch)
	}
	if _, owed, _ := s.OwedMoveOf(ctx, ws.ID); owed {
		t.Fatal("the move is still owed after it completed")
	}
}

func TestCompleteMoveOfAWorkspaceOwedNoMoveIsFalse(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)

	// Act
	_, found, err := s.CompleteMove(context.Background(), ws.ID)

	// Assert
	if err != nil || found {
		t.Fatalf("CompleteMove = (%v, %v), want (false, nil)", found, err)
	}
}

func TestOwedMovesListsEveryOwedMove(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
	ctx := context.Background()
	to := moveTarget(t, "listed")
	if err := s.Rename(ctx, ws.ID, "listed", &to); err != nil {
		t.Fatalf("Rename: %v", err)
	}

	// Act
	moves, err := s.OwedMoves(ctx)

	// Assert
	if err != nil || len(moves) != 1 || moves[0].Workspace != ws.ID {
		t.Fatalf("OwedMoves = (%+v, %v), want the one owed move", moves, err)
	}
}

func TestTwoMovesCannotClaimOneDirectory(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testLinkedWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	ctx := context.Background()
	to := moveTarget(t, "shared")
	if err := s.Rename(ctx, ws.ID, "shared", &to); err != nil {
		t.Fatalf("Rename: %v", err)
	}

	// Act
	err := s.Rename(ctx, other.ID, "shared-too", &to)

	// Assert
	if err == nil {
		t.Fatal("Rename = nil, want the second claim on the directory refused")
	}
}
