package footer

import (
	"testing"
	"time"
)

func TestTheSystemClockAnswersTheRealInstant(t *testing.T) {
	// Arrange
	var clock Clock = SystemClock{}

	// Act
	before := time.Now()
	got := clock.Now()

	// Assert
	if got.Before(before) {
		t.Fatalf("Now() = %v, want an instant at or after %v", got, before)
	}
}

func TestTheSystemClockSchedulesAStoppableDwell(t *testing.T) {
	// Arrange
	var clock Clock = SystemClock{}
	fired := make(chan struct{})

	// Act
	timer := clock.AfterFunc(time.Hour, func() { close(fired) })
	stopped := timer.Stop()

	// Assert
	if !stopped {
		t.Fatalf("Stop() = false, want true for a dwell that had not fired")
	}
	select {
	case <-fired:
		t.Fatalf("the dwell ran after being stopped")
	default:
	}
}
