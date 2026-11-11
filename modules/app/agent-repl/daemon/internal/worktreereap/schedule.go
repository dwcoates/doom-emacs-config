package worktreereap

import (
	"context"
	"errors"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
)

// Run drives the schedule for the daemon's whole serving lifetime: the
// closed workspaces' worktrees are swept at once, then one full sweep runs
// StartDelay after the start and one every Every. Sweeps run one after the
// other on this goroutine, so the schedule never overlaps itself. A sweep's
// failures are recorded inside it and never end the loop: a reaper that
// stopped on one bad repository would stop reaping all the others.
//
// THE START'S PASS IS ITS OWN SWEEP, NOT THE FIRST FULL SWEEP MOVED EARLIER
// (owner ruling 2026-11-11: every daemon start expires the closed workspaces'
// worktrees, without blocking startup). A daemon replaced before StartDelay
// runs out never reaches its first full sweep, and on a day of back-to-back
// deploys no daemon did (2026-11-10), so the closed workspaces' worktrees
// would never expire. A full sweep at every start is the wrong answer to
// that: it runs `git status` and a merge-tree in every idle worktree of every
// repository, which a host restarting its daemon every few minutes would pay
// every few minutes. The closed-only pass reads the registry first and asks
// git only about a closed workspace's worktree nobody touched inside the idle
// window, which is nothing at all on most starts. It runs on this goroutine,
// one of the daemon's background loops, so startup never waits for it, and
// under the same guard and kernel lock as every sweep.
func (r *Reaper) Run(ctx context.Context) error {
	r.deps.Log.Debug(opRun, "the landed-worktree reaper is scheduled", dlog.Context{
		"start_delay": r.deps.StartDelay.String(), "every": r.deps.Every.String(),
	})
	switch _, err := r.SweepWith(ctx, Options{ClosedOnly: true}); {
	case errors.Is(err, ErrSweepRunning):
		r.deps.Log.Debug(opRun, "the start's closed-workspace sweep found a sweep already running", nil)
	case errors.Is(err, ErrHeldElsewhere):
		r.deps.Log.Debug(opRun, "the start's closed-workspace sweep found another daemon sweeping", nil)
	}
	err := clock.Repeat(ctx, r.deps.Clock, r.deps.StartDelay, r.deps.Every, func(ctx context.Context) {
		if _, err := r.Sweep(ctx); errors.Is(err, ErrSweepRunning) {
			r.deps.Log.Debug(opRun, "the scheduled sweep found one already running", nil)
		}
	})
	r.deps.Log.Debug(opRun, "the landed-worktree reaper stopped with its context", nil)
	return err
}
