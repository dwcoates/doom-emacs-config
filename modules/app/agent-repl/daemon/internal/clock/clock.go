// Package clock is the daemon's one injectable view of time for code that
// WAITS: the drain sweep, the rollout controller, the starting-shim wait and
// the landed-worktree reaper. Each takes a Clock so its tests drive every
// window by hand and no test sleeps.
package clock

import (
	"context"
	"time"
)

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

// Repeat is the one periodic schedule: it runs fn once after first, then once
// every `every` after the previous run RETURNS, until ctx ends, and answers
// ctx.Err(). Runs happen on the caller's goroutine one after the other, so the
// schedule never overlaps itself, and a run's own failures are the run's to
// record: nothing fn does ends the schedule but ctx.
func Repeat(ctx context.Context, c Clock, first, every time.Duration, fn func(context.Context)) error {
	wait := first
	for {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-c.After(wait):
		}
		fn(ctx)
		wait = every
	}
}
