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
	setErr := s.MarkCloseWhenTurnEnds(context.Background(), ws.ID, at)
	marked, readErr := s.Workspace(context.Background(), ws.ID)
	first, firstErr := s.TakeCloseWhenTurnEnds(context.Background(), ws.ID)
	second, secondErr := s.TakeCloseWhenTurnEnds(context.Background(), ws.ID)
	cleared, clearedErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if setErr != nil || readErr != nil || firstErr != nil || secondErr != nil || clearedErr != nil {
		t.Fatalf("errors = %v, %v, %v, %v, %v", setErr, readErr, firstErr, secondErr, clearedErr)
	}
	if !first || second {
		t.Errorf("takes = %v, %v; want the first alone to take the mark", first, second)
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
	if err := s.MarkCloseWhenTurnEnds(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("MarkCloseWhenTurnEnds: %v", err)
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
	// Act
	err := s.MarkCloseWhenTurnEnds(context.Background(), "ws-none", instant)

	// Assert
	if err == nil {
		t.Fatal("MarkCloseWhenTurnEnds = nil, want a failure")
	}
}
