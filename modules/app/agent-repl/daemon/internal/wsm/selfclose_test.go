package wsm

import (
	"context"
	"testing"
)

func TestTheSelfCloseMarkRoundTripsAndClears(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	at := instant

	// Act
	setErr := s.SetCloseWhenTurnEnds(context.Background(), ws.ID, &at)
	marked, readErr := s.Workspace(context.Background(), ws.ID)
	clearErr := s.SetCloseWhenTurnEnds(context.Background(), ws.ID, nil)
	cleared, clearedErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if setErr != nil || readErr != nil || clearErr != nil || clearedErr != nil {
		t.Fatalf("errors = %v, %v, %v, %v", setErr, readErr, clearErr, clearedErr)
	}
	if marked.CloseWhenTurnEnds == nil || !marked.CloseWhenTurnEnds.Equal(at) {
		t.Errorf("marked = %v, want %v", marked.CloseWhenTurnEnds, at)
	}
	if cleared.CloseWhenTurnEnds != nil {
		t.Errorf("cleared = %v, want nil", cleared.CloseWhenTurnEnds)
	}
}

func TestEveryCloseClearsTheSelfCloseMark(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	at := instant
	if err := s.SetCloseWhenTurnEnds(context.Background(), ws.ID, &at); err != nil {
		t.Fatalf("SetCloseWhenTurnEnds: %v", err)
	}

	// Act
	err := s.MarkClosed(context.Background(), ws.ID, Closure{At: instant, By: CloseByUser})

	// Assert
	if err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil || got.CloseWhenTurnEnds != nil {
		t.Fatalf("after the close: %v, %v; want no mark", got.CloseWhenTurnEnds, err)
	}
}

func TestMarkingAnUnregisteredWorkspaceFails(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	at := instant

	// Act
	err := s.SetCloseWhenTurnEnds(context.Background(), "ws-none", &at)

	// Assert
	if err == nil {
		t.Fatal("SetCloseWhenTurnEnds = nil, want a failure")
	}
}
