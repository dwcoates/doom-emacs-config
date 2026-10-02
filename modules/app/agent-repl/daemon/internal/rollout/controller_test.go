package rollout

import (
	"testing"
	"time"
)

func TestNewRefusesWithoutLogSurfaces(t *testing.T) {
	// Arrange
	deps := Deps{}

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a controller with nowhere to log")
	}
}

func TestNewRefusesWithoutAStateClient(t *testing.T) {
	// Arrange
	h := newHarness(t)
	deps := Deps{Log: h.log}

	// Act
	_, err := New(deps)

	// Assert
	if err == nil {
		t.Fatalf("New accepted a controller with no state client")
	}
}

func TestNewDefaultsEveryWindow(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) {
		d.ExpectedOutage, d.AdoptionWindow, d.HoldoutWarnEvery, d.StandDownWindow = 0, 0, 0, 0
	})

	// Act
	got := h.c.deps

	// Assert
	if got.ExpectedOutage != DefaultExpectedOutage || got.AdoptionWindow != DefaultAdoptionWindow {
		t.Fatalf("windows = %v / %v, want the defaults", got.ExpectedOutage, got.AdoptionWindow)
	}
	if got.HoldoutWarnEvery != DefaultHoldoutWarnEvery || got.StandDownWindow != DefaultStandDownWindow {
		t.Fatalf("windows = %v / %v, want the defaults", got.HoldoutWarnEvery, got.StandDownWindow)
	}
}

func TestTheHoldoutCadenceDefaultsToTheTenMinuteRuling(t *testing.T) {
	// Arrange
	want := 10 * time.Minute

	// Act
	got := DefaultHoldoutWarnEvery

	// Assert
	if got != want {
		t.Fatalf("holdout cadence = %v, want the ruled %v", got, want)
	}
}

func TestNewSubstitutesTheSystemClockWhenNoneIsWired(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.Clock = nil })

	// Act
	_, isSystem := h.c.deps.Clock.(SystemClock)

	// Assert
	if !isSystem {
		t.Fatalf("clock = %T, want the system clock", h.c.deps.Clock)
	}
}

func TestParticipantsCountsTheStreamsThatOweAnAdoptionCall(t *testing.T) {
	// Arrange
	cases := []struct {
		name string
		in   Participants
		want int
	}{
		{"headless", Participants{}, 0},
		{"host only", Participants{Host: true}, 1},
		{"web only", Participants{Web: true}, 1},
		{"both", Participants{Host: true, Web: true}, 2},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.in.Count()

			// Assert
			if got != tc.want {
				t.Fatalf("Count = %d, want %d", got, tc.want)
			}
		})
	}
}

func TestSystemClockAfterFiresForAnAlreadyElapsedDuration(t *testing.T) {
	// Arrange
	clock := SystemClock{}

	// Act
	ch := clock.After(0)

	// Assert
	select {
	case <-ch:
	case <-time.After(time.Second):
		t.Fatalf("After(0) never fired")
	}
}

func TestSystemClockNowAdvances(t *testing.T) {
	// Arrange
	clock := SystemClock{}

	// Act
	first := clock.Now()
	second := clock.Now()

	// Assert
	if second.Before(first) {
		t.Fatalf("the system clock went backwards: %v then %v", first, second)
	}
}

func TestNewRefusesAMissingBringUpCollaborator(t *testing.T) {
	cases := []struct {
		name  string
		strip func(*Deps)
	}{
		{"no session starter", func(d *Deps) { d.StartSession = nil }},
		{"no bring-up marker", func(d *Deps) { d.BringingUp = nil }},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			deps := newHarness(t).c.deps
			tc.strip(&deps)

			// Act
			_, err := New(deps)

			// Assert
			if err == nil {
				t.Fatalf("New accepted a controller that could not start a session-less workspace's session")
			}
		})
	}
}
