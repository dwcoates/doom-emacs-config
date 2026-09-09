package drain

import (
	"testing"
	"time"

	"claude-repld/internal/shimclient"
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

// TestTheStandBoundStrictlyContainsAGracefulKill pins the nesting the graceful
// stand-down depends on. The sweep's KillSession is a shim teardown followed by
// a process stop, and the stand bound must leave room for BOTH plus the margin
// — never be equal to one of them, which is what made the daemon give up at the
// exact instant the SIGKILL fired and report a shim it was still stopping as
// leaked.
func TestTheStandBoundStrictlyContainsAGracefulKill(t *testing.T) {
	// Arrange.
	contained := shimTeardownWorstCase + shimclient.GracefulKillBound

	// Act.
	headroom := DefaultStandBound - contained

	// Assert.
	if headroom != standBoundMargin {
		t.Fatalf("DefaultStandBound (%v) leaves %v over the %v it must contain, want exactly the %v margin",
			DefaultStandBound, headroom, contained, standBoundMargin)
	}
}

// TestTheStandBoundIsNeverTheKillGrace pins the house rule the defect broke:
// two bounds that promise different things must not be equal by accident. They
// were both 5s, set independently, and an outer bound equal to the inner one
// can never observe what the inner one does.
func TestTheStandBoundIsNeverTheKillGrace(t *testing.T) {
	// Arrange / Act.
	grace := shimclient.DefaultKillGrace

	// Assert.
	if DefaultStandBound <= grace {
		t.Fatalf("DefaultStandBound = %v, kill grace = %v; the stand bound must strictly exceed the grace it contains",
			DefaultStandBound, grace)
	}
}
