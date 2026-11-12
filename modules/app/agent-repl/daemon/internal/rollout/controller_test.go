package rollout

import (
	"errors"
	"strings"
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

func TestNewRefusesADaemonAddrWriterWithNoBootClaimWait(t *testing.T) {
	// Arrange: the harness's whole deps, less the boot-claim wait.
	h := newHarness(t)
	deps := h.c.deps
	deps.AwaitBootClaim = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "boot-claim wait is required") {
		t.Fatalf("New = %v, want the missing boot-claim wait refused", err)
	}
}

func TestNewRefusesWithoutTheOwedLandingReports(t *testing.T) {
	// Arrange: every other dependency in place.
	h := newHarness(t)
	deps := h.c.deps
	deps.Reports = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the owed landing reports are required") {
		t.Fatalf("New = %v, want the missing reports refused", err)
	}
}

func TestNewRefusesWithoutARestartRecorder(t *testing.T) {
	// Arrange
	deps := newHarness(t).c.deps
	deps.Restarts = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "restart recorder is required") {
		t.Fatalf("New = %v, want the missing restart recorder refused", err)
	}
}

func TestNewRefusesWithoutAReconnectTracker(t *testing.T) {
	// Arrange
	deps := newHarness(t).c.deps
	deps.Reconnects = nil

	// Act
	_, err := New(deps)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "reconnect tracker is required") {
		t.Fatalf("New = %v, want the missing reconnect tracker refused", err)
	}
}

func TestAnAdoptionRunSettlesOnceWithItsOutcome(t *testing.T) {
	cases := []struct {
		name    string
		outcome error
	}{
		{name: "a failed run keeps its failure", outcome: errFake},
		{name: "a successful run keeps no failure", outcome: nil},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			e := &entry{run: newAdoptionRun()}
			run := e.run

			// Act: a second settle must not overwrite the first outcome.
			e.settle(tc.outcome)
			e.settle(errors.New("a later settle"))

			// Assert
			if !run.settled() {
				t.Fatal("the run did not settle")
			}
			if run.failed != tc.outcome {
				t.Fatalf("failed = %v, want %v", run.failed, tc.outcome)
			}
		})
	}
}

func TestBeginAdoptionReplacesOnlyASettledRun(t *testing.T) {
	cases := []struct {
		name        string
		settleFirst bool
		wantFresh   bool
	}{
		{name: "a settled run is replaced by a fresh one", settleFirst: true, wantFresh: true},
		{name: "a run still in flight is kept", settleFirst: false, wantFresh: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			e := &entry{run: newAdoptionRun()}
			first := e.run
			if tc.settleFirst {
				e.settle(errFake)
			}

			// Act
			e.beginAdoption()

			// Assert
			if fresh := e.run != first; fresh != tc.wantFresh {
				t.Fatalf("replaced = %v, want %v", fresh, tc.wantFresh)
			}
			if e.run.settled() {
				t.Fatal("the run begun is already settled")
			}
			if tc.settleFirst && first.failed != errFake {
				t.Fatalf("the replaced run's failure = %v, want it left as it settled", first.failed)
			}
		})
	}
}

func TestBeginAdoptionLeavesAnEntryWithNoRunWithout(t *testing.T) {
	// Arrange
	e := &entry{}

	// Act
	e.beginAdoption()

	// Assert
	if e.run != nil || !e.adopting {
		t.Fatalf("run = %v, adopting = %v, want no run and the latch closed", e.run, e.adopting)
	}
}
