package worktreereap

import (
	"context"
	"errors"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
)

// Run drives the schedule for the daemon's whole serving lifetime: one sweep
// StartDelay after the start, then one every Every. Sweeps run one after the
// other on this goroutine, so the schedule never overlaps itself. A sweep's
// failures are recorded inside it and never end the loop: a reaper that
// stopped on one bad repository would stop reaping all the others.
func (r *Reaper) Run(ctx context.Context) error {
	r.deps.Log.Debug(opRun, "the landed-worktree reaper is scheduled", dlog.Context{
		"start_delay": r.deps.StartDelay.String(), "every": r.deps.Every.String(),
	})
	err := clock.Repeat(ctx, r.deps.Clock, r.deps.StartDelay, r.deps.Every, func(ctx context.Context) {
		if _, err := r.Sweep(ctx); errors.Is(err, ErrSweepRunning) {
			r.deps.Log.Debug(opRun, "the scheduled sweep found one already running", nil)
		}
	})
	r.deps.Log.Debug(opRun, "the landed-worktree reaper stopped with its context", nil)
	return err
}
