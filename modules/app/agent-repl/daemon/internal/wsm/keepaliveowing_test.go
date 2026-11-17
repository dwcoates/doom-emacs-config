package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
)

func TestSetKeepaliveOwing(t *testing.T) {
	first := instant
	second := instant.Add(time.Hour)
	third := instant.Add(2 * time.Hour)
	type step struct {
		owing bool
		at    time.Time
	}
	tests := []struct {
		name     string
		steps    []step
		wantNone *time.Time
		wantFrom *time.Time
	}{
		{name: "a row never decided carries neither", wantNone: nil, wantFrom: nil},
		{name: "owing none stamps when it began", steps: []step{{false, first}}, wantNone: &first},
		{name: "a standing decision keeps its earliest instant", steps: []step{{false, first}, {false, second}}, wantNone: &first},
		{name: "owing again stamps when and clears the standing decision", steps: []step{{false, first}, {true, second}}, wantFrom: &second},
		{name: "owing all along stamps nothing", steps: []step{{true, first}}},
		{name: "owing again while owing keeps the last stamp", steps: []step{{false, first}, {true, second}, {true, third}}, wantFrom: &second},
		{name: "owing none after owing again keeps the stamp beside it", steps: []step{{false, first}, {true, second}, {false, third}}, wantNone: &third, wantFrom: &second},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)

			// Act
			for _, st := range tt.steps {
				if err := s.SetKeepaliveOwing(context.Background(), ws.ID, st.owing, st.at); err != nil {
					t.Fatalf("SetKeepaliveOwing(%v, %v) = %v", st.owing, st.at, err)
				}
			}
			got, err := s.Workspace(context.Background(), ws.ID)

			// Assert
			if err != nil {
				t.Fatalf("Workspace = %v", err)
			}
			if !clocktest.SameInstant(got.KeepaliveOwedNoneAt, tt.wantNone) || !clocktest.SameInstant(got.KeepaliveOwedFrom, tt.wantFrom) {
				t.Fatalf("row = (none %v, from %v), want (%v, %v)", got.KeepaliveOwedNoneAt, got.KeepaliveOwedFrom, tt.wantNone, tt.wantFrom)
			}
		})
	}
}

func TestSetKeepaliveOwingRefusesAnUnknownWorkspace(t *testing.T) {
	for _, owing := range []bool{false, true} {
		// Arrange
		s, _ := testStore(t)

		// Act
		err := s.SetKeepaliveOwing(context.Background(), "nobody", owing, instant)

		// Assert
		if !errors.Is(err, ErrNotFound) {
			t.Fatalf("SetKeepaliveOwing(owing %v) = %v, want ErrNotFound", owing, err)
		}
	}
}
