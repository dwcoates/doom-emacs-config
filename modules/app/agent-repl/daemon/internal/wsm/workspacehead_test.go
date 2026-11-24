package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
)

// TestAWorkspacesHeadIsItsRowsHead pins the narrow read against the whole
// row: every field the head reads is the one the full row holds, for a
// repository workspace, a closed one and a thread.
func TestAWorkspacesHeadIsItsRowsHead(t *testing.T) {
	tests := []struct {
		name string
		make func(t *testing.T, s *store) WorkspaceID
	}{
		{name: "a repository workspace", make: func(t *testing.T, s *store) WorkspaceID {
			return testWorkspace(t, s).ID
		}},
		{name: "a closed workspace", make: func(t *testing.T, s *store) WorkspaceID {
			id := testWorkspace(t, s).ID
			corrupt(t, s, `UPDATE workspaces SET closed = 1 WHERE id = ?`, id)
			return id
		}},
		{name: "a thread", make: func(t *testing.T, s *store) WorkspaceID {
			ws, err := s.RegisterThread(context.Background(), filepath.Join(t.TempDir(), "hydrogen-ions"), "hydrogen-ions")
			if err != nil {
				t.Fatalf("RegisterThread: %v", err)
			}
			return ws.ID
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			id := tt.make(t, s)
			full, err := s.Workspace(context.Background(), id)
			if err != nil {
				t.Fatalf("Workspace: %v", err)
			}

			// Act
			head, err := s.WorkspaceHead(context.Background(), id)

			// Assert
			if err != nil {
				t.Fatalf("WorkspaceHead: %v", err)
			}
			if head != full.Head() {
				t.Fatalf("head = %+v, want the full row's %+v", head, full.Head())
			}
		})
	}
}

func TestAnUnknownWorkspacesHeadIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.WorkspaceHead(context.Background(), "no-such-workspace")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("WorkspaceHead = %v, want ErrNotFound", err)
	}
}

func TestAHeadNamesItsWorkspaceAsTheFullRowDoes(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	full := testWorkspace(t, s)

	// Act
	head := full.Head()

	// Assert
	if head.IsThread() != full.IsThread() {
		t.Fatalf("head IsThread = %v, want %v", head.IsThread(), full.IsThread())
	}
	if got, want := head.Ref().String(), full.Ref().String(); got != want {
		t.Fatalf("head Ref = %s, want %s", got, want)
	}
}

func TestAnUnknownWorkspaceKindIsADecodeError(t *testing.T) {
	// Act
	_, err := decodeWorkspaceKind("w1", "spaceship")

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "kind" || decode.Row != "w1" {
		t.Fatalf("decodeWorkspaceKind = %v, want a DecodeError on w1's kind", err)
	}
}
