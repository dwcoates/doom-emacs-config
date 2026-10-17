package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestTheAwaitingMarkRoundTripsAndClears(t *testing.T) {
	tests := []struct {
		name  string
		clear func(t *testing.T, s *store, ws, awaited Workspace)
	}{
		{name: "cleared by its workspace", clear: func(t *testing.T, s *store, ws, _ Workspace) {
			if err := s.SetAwaiting(context.Background(), ws.ID, nil); err != nil {
				t.Fatalf("SetAwaiting(nil): %v", err)
			}
		}},
		{name: "cleared by the workspace's close", clear: func(t *testing.T, s *store, ws, _ Workspace) {
			if err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser}); err != nil {
				t.Fatalf("MarkClosed: %v", err)
			}
		}},
		{name: "cleared by the awaited workspace's forget", clear: func(t *testing.T, s *store, _, awaited Workspace) {
			if _, err := s.Forget(context.Background(), awaited.ID); err != nil {
				t.Fatalf("Forget: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			awaited := testWorkspaceNamed(t, s, "awaited")
			if err := s.SetAwaiting(context.Background(), ws.ID, &awaited.ID); err != nil {
				t.Fatalf("SetAwaiting: %v", err)
			}
			marked, err := s.Workspace(context.Background(), ws.ID)
			if err != nil || marked.Awaiting == nil || *marked.Awaiting != awaited.ID {
				t.Fatalf("marked = %v, %v; want awaiting %s", marked.Awaiting, err, awaited.ID)
			}

			// Act
			tt.clear(t, s, ws, awaited)

			// Assert
			got, err := s.Workspace(context.Background(), ws.ID)
			if err != nil || got.Awaiting != nil {
				t.Fatalf("after clearing: %v, %v; want awaiting nothing", got.Awaiting, err)
			}
		})
	}
}

func TestSetOneShotMarksAndUnmarksTheCreation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutCreationJob(context.Background(), CreationJob{Workspace: ws.ID, Layout: MergeLayout{SourceBranch: "b", SourceDir: ws.Dir, TargetDir: ws.Dir}}); err != nil {
		t.Fatalf("PutCreationJob: %v", err)
	}

	// Act
	markErr := s.SetOneShot(context.Background(), ws.ID, true)
	marked, _ := s.Workspace(context.Background(), ws.ID)
	unmarkErr := s.SetOneShot(context.Background(), ws.ID, false)
	unmarked, _ := s.Workspace(context.Background(), ws.ID)

	// Assert
	if markErr != nil || unmarkErr != nil {
		t.Fatalf("SetOneShot = %v, %v", markErr, unmarkErr)
	}
	if !marked.OneShot || unmarked.OneShot {
		t.Errorf("one-shot = %v then %v, want true then false", marked.OneShot, unmarked.OneShot)
	}
}

func TestSetOneShotOfAWorkspaceWithNoCreationIsNotFound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.SetOneShot(context.Background(), ws.ID, true)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetOneShot = %v, want ErrNotFound", err)
	}
}
