package wsm

import (
	"context"
	"testing"
	"time"
)

// everySingletonSwitch is every switch kept as a singleton row, each with the
// accessor pair the DB exposes for it. A new switch joins this list, so the
// helper's guarantees are asserted for it too.
var everySingletonSwitch = []struct {
	name string
	sw   singletonSwitch
	set  func(*store, bool, time.Time) error
	get  func(*store) (bool, time.Time, error)
}{
	{
		name: "the racing switch", sw: racingDisabledSwitch,
		set: func(s *store, on bool, at time.Time) error {
			return s.SetMergeRacingDisabled(context.Background(), on, at)
		},
		get: func(s *store) (bool, time.Time, error) {
			racing, err := s.MergeRacing(context.Background())
			return racing.Disabled, racing.Since, err
		},
	},
}

func TestASingletonSwitchIsOffOnAFreshFile(t *testing.T) {
	for _, tt := range everySingletonSwitch {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			on, since, err := tt.sw.get(context.Background(), s, "test.get")

			// Assert
			if err != nil {
				t.Fatalf("get: %v", err)
			}
			if on || !since.IsZero() {
				t.Fatalf("switch = (%v, %v), want off with no start", on, since)
			}
		})
	}
}

func TestASingletonSwitchTurnedOnAgainKeepsItsOriginalStart(t *testing.T) {
	first := time.Date(2026, 10, 14, 13, 0, 0, 0, time.UTC)
	for _, tt := range everySingletonSwitch {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			if err := tt.sw.set(context.Background(), s, "test.set", true, first); err != nil {
				t.Fatal(err)
			}

			// Act
			if err := tt.sw.set(context.Background(), s, "test.set", true, first.Add(time.Hour)); err != nil {
				t.Fatalf("set: %v", err)
			}
			on, since, err := tt.sw.get(context.Background(), s, "test.get")

			// Assert
			if err != nil || !on || !since.Equal(first) {
				t.Fatalf("switch = (%v, %v, %v), want on since %v", on, since, err, first)
			}
		})
	}
}

func TestASingletonSwitchTurnedOffIsOff(t *testing.T) {
	at := time.Date(2026, 10, 14, 13, 0, 0, 0, time.UTC)
	for _, tt := range everySingletonSwitch {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			if err := tt.sw.set(context.Background(), s, "test.set", true, at); err != nil {
				t.Fatal(err)
			}

			// Act
			if err := tt.sw.set(context.Background(), s, "test.set", false, at); err != nil {
				t.Fatalf("set: %v", err)
			}
			on, _, err := tt.sw.get(context.Background(), s, "test.get")

			// Assert
			if err != nil || on {
				t.Fatalf("switch = (%v, %v), want off", on, err)
			}
		})
	}
}

// TestEverySwitchAccessorGoesThroughItsSingletonSwitch pins that each DB
// accessor pair reads and writes the very row its singletonSwitch names, so a
// hand-rolled accessor that drifted to another table or column fails here.
func TestEverySwitchAccessorGoesThroughItsSingletonSwitch(t *testing.T) {
	at := time.Date(2026, 10, 14, 13, 0, 0, 0, time.UTC)
	for _, tt := range everySingletonSwitch {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			if err := tt.set(s, true, at); err != nil {
				t.Fatalf("accessor set: %v", err)
			}
			viaSwitch, sinceViaSwitch, err := tt.sw.get(context.Background(), s, "test.get")
			if err != nil {
				t.Fatalf("switch get: %v", err)
			}
			if err := tt.sw.set(context.Background(), s, "test.set", false, at); err != nil {
				t.Fatalf("switch set: %v", err)
			}
			viaAccessor, _, err := tt.get(s)

			// Assert
			if err != nil {
				t.Fatalf("accessor get: %v", err)
			}
			if !viaSwitch || !sinceViaSwitch.Equal(at) {
				t.Fatalf("the switch reads (%v, %v) after the accessor turned it on at %v", viaSwitch, sinceViaSwitch, at)
			}
			if viaAccessor {
				t.Fatal("the accessor still reads on after the switch turned it off")
			}
		})
	}
}
