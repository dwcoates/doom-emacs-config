package drain

import (
	"testing"
	"time"
)

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
