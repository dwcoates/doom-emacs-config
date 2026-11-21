package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestSetSettingsVisitRecordsTheVisitOnTheRow(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	setErr := s.SetSettingsVisit(context.Background(), ws.ID, instant)
	got, readErr := s.Workspace(context.Background(), ws.ID)

	// Assert
	if setErr != nil || readErr != nil {
		t.Fatalf("errors = %v, %v", setErr, readErr)
	}
	if got.SettingsVisitAt == nil || !got.SettingsVisitAt.Equal(instant) {
		t.Fatalf("SettingsVisitAt = %v, want %v", got.SettingsVisitAt, instant)
	}
}

func TestSetSettingsVisitOfAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetSettingsVisit(context.Background(), "no-such-workspace", instant)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("err = %v, want ErrNotFound", err)
	}
}

func TestEndSettingsVisit(t *testing.T) {
	later := instant.Add(time.Minute)
	tests := []struct {
		name      string
		begun     *time.Time
		end       time.Time
		wantEnded bool
		wantLeft  *time.Time
	}{
		{name: "the standing visit ends", begun: &instant, end: instant, wantEnded: true},
		{name: "a visit a later pick began is left standing", begun: &later, end: instant, wantLeft: &later},
		{name: "no visit stands", end: instant},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			if tt.begun != nil {
				if err := s.SetSettingsVisit(context.Background(), ws.ID, *tt.begun); err != nil {
					t.Fatalf("SetSettingsVisit: %v", err)
				}
			}

			// Act
			ended, err := s.EndSettingsVisit(context.Background(), ws.ID, tt.end)
			got, readErr := s.Workspace(context.Background(), ws.ID)

			// Assert
			if err != nil || readErr != nil {
				t.Fatalf("errors = %v, %v", err, readErr)
			}
			if ended != tt.wantEnded {
				t.Fatalf("ended = %v, want %v", ended, tt.wantEnded)
			}
			if (got.SettingsVisitAt == nil) != (tt.wantLeft == nil) || (tt.wantLeft != nil && !got.SettingsVisitAt.Equal(*tt.wantLeft)) {
				t.Fatalf("SettingsVisitAt = %v, want %v", got.SettingsVisitAt, tt.wantLeft)
			}
		})
	}
}
