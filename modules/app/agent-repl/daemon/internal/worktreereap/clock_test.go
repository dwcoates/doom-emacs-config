package worktreereap

import (
	"testing"
	"time"
)

func TestSystemClockAfterFiresForAnElapsedDuration(t *testing.T) {
	// Arrange.
	clock := SystemClock{}

	// Act.
	fired := clock.After(0)

	// Assert: a zero duration has already passed, so the value is ready.
	if at := <-fired; at.IsZero() {
		t.Fatal("After(0) yielded the zero instant")
	}
}

func TestSystemClockNowIsTheWallClock(t *testing.T) {
	// Arrange.
	before := time.Now()

	// Act.
	got := SystemClock{}.Now()

	// Assert.
	if got.Before(before) {
		t.Fatalf("Now() = %v, before the instant %v taken just prior", got, before)
	}
}
