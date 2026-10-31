package wsm

import (
	"context"
	"errors"
	"testing"
)

const originURL = "https://w.slack.com/archives/C1/p1730000000123456"

func TestWorkspaceSlackOriginIsReadBack(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetWorkspaceSlackOrigin(context.Background(), ws.ID, originURL)

	// Assert
	if err != nil {
		t.Fatalf("SetWorkspaceSlackOrigin: %v", err)
	}
	got, found, err := s.WorkspaceSlackOrigin(context.Background(), ws.ID)
	if err != nil || !found || got != originURL {
		t.Fatalf("WorkspaceSlackOrigin = %q, %v, %v; want the recorded thread", got, found, err)
	}
}

func TestWorkspaceSlackOriginOfAWorkspaceCreatedForNoneIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, found, err := s.WorkspaceSlackOrigin(context.Background(), ws.ID)

	// Assert
	if err != nil || found || got != "" {
		t.Fatalf("WorkspaceSlackOrigin = %q, %v, %v; want none", got, found, err)
	}
}

func TestWorkspaceSlackOriginRefusals(t *testing.T) {
	tests := []struct {
		name     string
		act      func(s *store, ws WorkspaceID) error
		notFound bool
	}{
		{name: "an origin with no thread is a defect", act: func(s *store, ws WorkspaceID) error {
			return s.SetWorkspaceSlackOrigin(context.Background(), ws, "")
		}},
		{name: "a second origin is refused", act: func(s *store, ws WorkspaceID) error {
			if err := s.SetWorkspaceSlackOrigin(context.Background(), ws, originURL); err != nil {
				t.Fatalf("first SetWorkspaceSlackOrigin: %v", err)
			}
			return s.SetWorkspaceSlackOrigin(context.Background(), ws, originURL+"9")
		}},
		{name: "an origin of an unknown workspace is not found", act: func(s *store, _ WorkspaceID) error {
			return s.SetWorkspaceSlackOrigin(context.Background(), WorkspaceID("absent"), originURL)
		}, notFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			err := tt.act(s, ws.ID)

			// Assert
			if err == nil {
				t.Fatal("err = nil, want a refusal")
			}
			if tt.notFound != errors.Is(err, ErrNotFound) {
				t.Fatalf("err = %v, want not-found %v", err, tt.notFound)
			}
		})
	}
}

func TestWorkspaceSlackOriginGoesWithItsWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.SetWorkspaceSlackOrigin(context.Background(), ws.ID, originURL); err != nil {
		t.Fatalf("SetWorkspaceSlackOrigin: %v", err)
	}

	// Act
	_, err := s.q(context.Background()).ExecContext(context.Background(), `DELETE FROM workspaces WHERE id = ?`, ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("delete the workspace: %v", err)
	}
	var count int
	if err := s.q(context.Background()).QueryRowContext(context.Background(), `SELECT COUNT(*) FROM workspace_slack_origins`).Scan(&count); err != nil || count != 0 {
		t.Fatalf("origins left = %d, %v; want none", count, err)
	}
}
