package drain

import (
	"testing"
	"time"
)

// TestTheSweepCadenceIsClampedToTheIdleCutoff pins that a cutoff shorter than
// the cadence is honored: a sweep running every five minutes could never notice
// a fifty-millisecond cutoff sooner than five minutes late.
func TestTheSweepCadenceIsClampedToTheIdleCutoff(t *testing.T) {
	// Arrange / Act
	h := newHarness(t, func(d *Deps) {
		d.IdleCutoff = 50 * time.Millisecond
		d.SweepEvery = DefaultSweepEvery
	})

	// Assert
	if got := h.c.deps.SweepEvery; got != 50*time.Millisecond {
		t.Fatalf("SweepEvery = %v, want it clamped to the 50ms cutoff", got)
	}
}

// TestTheSweepCadenceIsLeftAloneWhenTheCutoffIsLonger is the other edge: a
// cutoff longer than the cadence needs no clamping, and shortening the cadence
// to it would sweep less often than intended.
func TestTheSweepCadenceIsLeftAloneWhenTheCutoffIsLonger(t *testing.T) {
	// Arrange / Act
	h := newHarness(t, func(d *Deps) {
		d.IdleCutoff = time.Hour
		d.SweepEvery = time.Minute
	})

	// Assert
	if got := h.c.deps.SweepEvery; got != time.Minute {
		t.Fatalf("SweepEvery = %v, want the configured minute", got)
	}
}
