package wsm

import (
	"context"
	"testing"
	"time"
)

func TestMergeRolloutHoldIsNotHeldOnAFreshFile(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.MergeRolloutHold(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("MergeRolloutHold: %v", err)
	}
	if got != (RolloutHold{}) {
		t.Fatalf("hold = %+v, want not held", got)
	}
}

func TestSetMergeRolloutHoldRoundTrips(t *testing.T) {
	at := time.Date(2026, 10, 12, 13, 0, 0, 0, time.UTC)
	tests := []struct {
		name  string
		holds []bool
		want  RolloutHold
	}{
		{name: "a hold is held since its start", holds: []bool{true}, want: RolloutHold{Held: true, Since: at}},
		{name: "a release ends the hold", holds: []bool{true, false}, want: RolloutHold{}},
		{name: "a hold after a release is held again", holds: []bool{true, false, true}, want: RolloutHold{Held: true, Since: at}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			for _, held := range tt.holds {
				if err := s.SetMergeRolloutHold(context.Background(), held, at); err != nil {
					t.Fatalf("SetMergeRolloutHold(%v): %v", held, err)
				}
			}
			got, err := s.MergeRolloutHold(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("MergeRolloutHold: %v", err)
			}
			if got != tt.want {
				t.Fatalf("hold = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestARepeatedHoldKeepsItsOriginalStart(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := time.Date(2026, 10, 12, 13, 0, 0, 0, time.UTC)
	if err := s.SetMergeRolloutHold(context.Background(), true, first); err != nil {
		t.Fatal(err)
	}

	// Act
	if err := s.SetMergeRolloutHold(context.Background(), true, first.Add(time.Hour)); err != nil {
		t.Fatalf("SetMergeRolloutHold: %v", err)
	}
	got, err := s.MergeRolloutHold(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("MergeRolloutHold: %v", err)
	}
	if !got.Since.Equal(first) {
		t.Fatalf("Since = %v, want the original start %v", got.Since, first)
	}
}

func TestTheRolloutHoldSurvivesAReopen(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)
	at := time.Date(2026, 10, 12, 13, 0, 0, 0, time.UTC)
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := handle.SetMergeRolloutHold(context.Background(), true, at); err != nil {
		t.Fatalf("SetMergeRolloutHold: %v", err)
	}
	handle.Close()

	// Act
	handle, err = Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("re-Open: %v", err)
	}
	defer handle.Close()
	got, err := handle.MergeRolloutHold(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("MergeRolloutHold: %v", err)
	}
	if got != (RolloutHold{Held: true, Since: at}) {
		t.Fatalf("hold after a reopen = %+v, want held since %v", got, at)
	}
}
