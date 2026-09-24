// Package clock is the daemon's one injectable view of time for code that
// WAITS: the drain sweep, the rollout controller, the starting-shim wait and
// the landed-worktree reaper. Each takes a Clock so its tests drive every
// window by hand and no test sleeps.
package clock

import "time"

// Clock is a view of time.
type Clock interface {
	// Now is the current instant.
	Now() time.Time
	// After yields one value after d has passed.
	After(d time.Duration) <-chan time.Time
}

// System is the production Clock: the wall clock.
type System struct{}

// Now is the wall clock's instant.
func (System) Now() time.Time { return time.Now() }

// After is time.After.
func (System) After(d time.Duration) <-chan time.Time { return time.After(d) }
