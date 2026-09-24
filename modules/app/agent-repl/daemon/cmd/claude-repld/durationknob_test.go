package main

import (
	"strings"
	"testing"
	"time"
)

func TestParsePositiveDurationReadsADuration(t *testing.T) {
	// Arrange, Act.
	got, err := parsePositiveDuration("KNOB", "250ms")

	// Assert.
	if err != nil || got != 250*time.Millisecond {
		t.Fatalf("parsePositiveDuration = (%v, %v), want (250ms, nil)", got, err)
	}
}

func TestParsePositiveDurationRefusesAMalformedValueNamingTheKnob(t *testing.T) {
	// Arrange, Act.
	_, err := parsePositiveDuration("KNOB", "soon")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), `KNOB="soon" is not a duration`) {
		t.Fatalf("parsePositiveDuration(soon) = %v, want a refusal naming the knob and the value", err)
	}
}

func TestParsePositiveDurationRefusesZero(t *testing.T) {
	// Arrange, Act.
	_, err := parsePositiveDuration("KNOB", "0s")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), `KNOB="0s" is not a positive duration`) {
		t.Fatalf("parsePositiveDuration(0s) = %v, want the non-positive refusal", err)
	}
}

func TestParsePositiveDurationRefusesANegativeValue(t *testing.T) {
	// Arrange, Act.
	_, err := parsePositiveDuration("KNOB", "-1s")

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "is not a positive duration") {
		t.Fatalf("parsePositiveDuration(-1s) = %v, want the non-positive refusal", err)
	}
}

// TestEveryDurationKnobRefusesInTheSharedWords pins that the knobs actually go
// through the one parse: a knob that hand-rolled its own would refuse in
// words of its own, and this table would catch the divergence.
func TestEveryDurationKnobRefusesInTheSharedWords(t *testing.T) {
	cases := []struct {
		name    string
		env     string
		resolve func(value string) (time.Duration, error)
	}{
		{"adopt bound", envBootAdoptBound, resolveAdoptBound},
		{"start bound", envStartSessionBound, resolveStartBound},
		{"footer dwell", envFooterMomentaryDwell, func(v string) (time.Duration, error) {
			return resolveFooterMomentaryDwell(0, v)
		}},
		{"holdout warn cadence", HoldoutWarnEnv, func(v string) (time.Duration, error) {
			t.Setenv(HoldoutWarnEnv, v)
			return resolveHoldoutWarnEvery()
		}},
		{"worktree reap idle threshold", envWorktreeReapIdle, resolveWorktreeReapIdle},
		{"worktree reap start delay", envWorktreeReapStartDelay, resolveWorktreeReapStartDelay},
		{"worktree reap cadence", envWorktreeReapEvery, resolveWorktreeReapEvery},
	}
	for _, tc := range cases {
		for _, value := range []string{"soon", "0s"} {
			t.Run(tc.name+"/"+value, func(t *testing.T) {
				// Arrange.
				_, want := parsePositiveDuration(tc.env, value)

				// Act.
				_, got := tc.resolve(value)

				// Assert.
				if got == nil || got.Error() != want.Error() {
					t.Fatalf("%s refused %q with %v, want the shared refusal %v", tc.name, value, got, want)
				}
			})
		}
	}
}
