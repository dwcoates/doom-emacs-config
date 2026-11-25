package clock

import (
	"testing"
	"time"
)

func TestSystemAfterFiresForAnElapsedDuration(t *testing.T) {
	// Arrange.
	clock := System{}

	// Act.
	fired := clock.After(0)

	// Assert: a zero duration has already passed, so the value is ready.
	if at := <-fired; at.IsZero() {
		t.Fatal("After(0) yielded the zero instant")
	}
}

func TestSystemNowIsTheWallClock(t *testing.T) {
	// Arrange.
	before := time.Now()

	// Act.
	got := System{}.Now()

	// Assert.
	if got.Before(before) {
		t.Fatalf("Now() = %v, before the instant %v taken just prior", got, before)
	}
}
