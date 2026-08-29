package shimclient

import (
	"context"
	"errors"
	"testing"
	"time"
)

// TestBackoffGrowsAndCaps asserts the schedule grows by its factor and stops
// at the cap.
func TestBackoffGrowsAndCaps(t *testing.T) {
	tests := []struct {
		name    string
		attempt int
		want    time.Duration
	}{
		{name: "first", attempt: 0, want: 100 * time.Millisecond},
		{name: "second", attempt: 1, want: 200 * time.Millisecond},
		{name: "third", attempt: 2, want: 400 * time.Millisecond},
		{name: "capped", attempt: 20, want: time.Second},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			b := backoff{Initial: 100 * time.Millisecond, Max: time.Second, Factor: 2}

			// Act.
			got := b.delay(tc.attempt)

			// Assert.
			if got != tc.want {
				t.Fatalf("delay(%d) = %v, want %v", tc.attempt, got, tc.want)
			}
		})
	}
}

// TestBackoffWaitStopsOnDeath asserts a wait ends the moment the process is
// known dead, rather than sitting out the delay.
func TestBackoffWaitStopsOnDeath(t *testing.T) {
	// Arrange.
	b := backoff{Initial: time.Hour, Max: time.Hour, Factor: 1}
	dead := make(chan struct{})
	close(dead)

	// Act.
	err := b.wait(context.Background(), dead, 0)

	// Assert.
	if !errors.Is(err, errProcessDead) {
		t.Fatalf("wait() = %v, want errProcessDead", err)
	}
}

// TestBackoffWaitStopsOnContext asserts a canceled supervision ends the wait.
func TestBackoffWaitStopsOnContext(t *testing.T) {
	// Arrange.
	b := backoff{Initial: time.Hour, Max: time.Hour, Factor: 1}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := b.wait(ctx, make(chan struct{}), 0)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("wait() = %v, want context.Canceled", err)
	}
}
