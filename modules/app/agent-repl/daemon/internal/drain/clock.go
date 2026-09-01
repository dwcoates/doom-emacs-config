package drain

import "time"

// Clock is the controller's view of time. It is injected so a schedule's
// deadline and the sweep's cadence are driven by the test rather than by the
// wall clock: no test of this package sleeps.
type Clock interface {
	// Now is the current instant.
	Now() time.Time
	// After yields one value after d has passed.
	After(d time.Duration) <-chan time.Time
}

// SystemClock is the production Clock.
type SystemClock struct{}

// Now is the wall clock's instant.
func (SystemClock) Now() time.Time { return time.Now() }

// After is time.After.
func (SystemClock) After(d time.Duration) <-chan time.Time { return time.After(d) }
