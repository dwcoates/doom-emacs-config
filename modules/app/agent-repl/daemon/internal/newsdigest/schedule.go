package newsdigest

import (
	"context"
	"errors"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/intakegate"
)

// Run drives the schedule for the daemon's whole serving lifetime. It first
// waits StartDelay, then looks at the durable cadence: a run is due Every
// after the previous run's end (at once, on a store no run was ever recorded
// in). A wait is never longer than Recheck, so the wall clock is re-read even
// across a machine sleep.
//
// A due run starts only while this daemon serves, and only one at a time
// across processes (the run lock); a run the other daemon holds, or a daemon
// that does not serve, is looked at again after Recheck. A scheduled run that
// finds, under the lock, that another run ended since the schedule looked
// does not run, and republishes the standing that run left instead.
//
// A run's failures are recorded inside it and never end the loop. A failure
// that left the cadence where it was (the store, the lock) waits Recheck
// before the next look, so it is never retried in a burst.
func (d *Digester) Run(ctx context.Context) error {
	d.deps.Log.Debug(opSchedule, "the news digest is scheduled", dlog.Context{
		"start_delay": d.deps.StartDelay.String(), "every": d.deps.Every.String(), "recheck": d.deps.Recheck.String(),
	})
	gate := intakegate.NewFor(d.deps.Serves, d.deps.Log, opSchedule, "the news digest's scheduled runs")
	wait := d.deps.StartDelay
	for {
		select {
		case <-ctx.Done():
			d.deps.Log.Debug(opSchedule, "the news digest schedule stopped with its context", nil)
			return ctx.Err()
		case <-d.deps.Clock.After(wait):
		}
		next, err := d.tick(ctx, gate)
		if ctx.Err() != nil {
			return ctx.Err()
		}
		if err != nil {
			d.deps.Log.Error(opSchedule, "the news digest schedule could not run; it looks again later", dlog.Context{
				"cause": err.Error(), "recheck": d.deps.Recheck.String(),
			})
		}
		wait = next
	}
}

// tick looks at the cadence once, runs when a run is due and this daemon may
// start it, and answers how long to wait before the next look.
func (d *Digester) tick(ctx context.Context, gate *intakegate.Gate) (time.Duration, error) {
	state, err := d.deps.Store.NewsDigestState(ctx)
	if err != nil {
		return d.deps.Recheck, err
	}
	if wait := d.untilDue(state.LastRunEnd); wait > 0 {
		return min(wait, d.deps.Recheck), nil
	}
	if !gate.Admits() {
		return d.deps.Recheck, nil
	}
	_, err = d.run(ctx, triggerScheduled, true)
	var modelFailed *ModelFailedError
	switch {
	case err == nil, errors.As(err, &modelFailed), errors.Is(err, ErrNoSourceRead):
		// The run recorded its end, so the cadence has moved.
		return d.nextLook(ctx)
	case errors.Is(err, errNotDue):
		// Another run ended since this look: draw what it left standing.
		if err := d.Republish(ctx); err != nil {
			return d.deps.Recheck, err
		}
		return d.nextLook(ctx)
	case errors.Is(err, ErrAlreadyRunning):
		return d.deps.Recheck, nil
	default:
		return d.deps.Recheck, err
	}
}

// nextLook answers how long to wait after a run, from the cadence it left. A
// cadence still due (a record that did not move it) waits Recheck, so nothing
// is retried in a burst.
func (d *Digester) nextLook(ctx context.Context) (time.Duration, error) {
	state, err := d.deps.Store.NewsDigestState(ctx)
	if err != nil {
		return d.deps.Recheck, err
	}
	wait := d.untilDue(state.LastRunEnd)
	if wait <= 0 {
		return d.deps.Recheck, nil
	}
	return min(wait, d.deps.Recheck), nil
}

// untilDue answers how long until a run is due after a run that ended at
// lastRunEnd; zero or less is due now. A store no run was ever recorded in is
// due at once.
func (d *Digester) untilDue(lastRunEnd time.Time) time.Duration {
	if lastRunEnd.IsZero() {
		return 0
	}
	return lastRunEnd.Add(d.deps.Every).Sub(d.deps.Clock.Now())
}
