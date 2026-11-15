package wsm

import (
	"context"
	"testing"
	"time"
)

func TestTakeOwedPlacementNoteTakesAnOwedNoteOnce(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.OwePlacementNote(ctx, ws.ID, time.Unix(1, 0)); err != nil {
		t.Fatalf("OwePlacementNote: %v", err)
	}

	// Act
	first, err1 := s.TakeOwedPlacementNote(ctx, ws.ID)
	second, err2 := s.TakeOwedPlacementNote(ctx, ws.ID)

	// Assert
	if err1 != nil || !first {
		t.Fatalf("first take = (%v, %v), want the owed note", first, err1)
	}
	if err2 != nil || second {
		t.Fatalf("second take = (%v, %v), want none left", second, err2)
	}
}

func TestTakeOwedPlacementNoteAnswersFalseForAWorkspaceThatOwesNone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	owed, err := s.TakeOwedPlacementNote(context.Background(), ws.ID)

	// Assert
	if err != nil || owed {
		t.Fatalf("TakeOwedPlacementNote = (%v, %v), want (false, nil)", owed, err)
	}
}

func TestOwePlacementNoteTwiceOwesOneNote(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()

	// Act
	for _, at := range []time.Time{time.Unix(1, 0), time.Unix(2, 0)} {
		if err := s.OwePlacementNote(ctx, ws.ID, at); err != nil {
			t.Fatalf("OwePlacementNote: %v", err)
		}
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM owed_placement_notes WHERE workspace_id = ?`, ws.ID); n != 1 {
		t.Fatalf("owed placement notes = %d, want 1", n)
	}
	if at := scalar[int64](t, s, `SELECT owed_at FROM owed_placement_notes WHERE workspace_id = ?`, ws.ID); at != nanos(time.Unix(1, 0)) {
		t.Fatalf("owed_at = %d, want the first instant, which stands", at)
	}
}

func TestOwePlacementNoteRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.OwePlacementNote(context.Background(), "no-such-workspace", time.Unix(1, 0))

	// Assert
	if err == nil {
		t.Fatal("OwePlacementNote accepted a workspace the registry does not hold")
	}
}

func TestForgetRetiresAnOwedPlacementNote(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	ctx := context.Background()
	if err := s.OwePlacementNote(ctx, ws.ID, time.Unix(1, 0)); err != nil {
		t.Fatalf("OwePlacementNote: %v", err)
	}

	// Act
	if _, err := s.Forget(ctx, ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM owed_placement_notes`); n != 0 {
		t.Fatalf("owed placement notes after the forget = %d, want 0", n)
	}
}
