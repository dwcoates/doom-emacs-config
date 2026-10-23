package rollout

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// closeCreates closes the create gate and waits for every create in flight to
// finish, before a handover or restart lists what this daemon serves.
//
// NO CREATE STRADDLES A STAND-DOWN (2026-10-23). The list is the whole of what
// is transferred or stood down, and a create still running when it was taken
// registered its workspace only afterwards, so that workspace was never
// transferred, and the exit then cut the create off wherever it stood. From
// the close on, a create asked for is refused (transferring_away, naming
// SUCCESSOR, or standing_down when none is listening), and every create
// admitted before it finishes here first. Each step of a create is bounded,
// so this wait is too; the holdout cadence names it while it lasts.
func (c *controller) closeCreates(ctx context.Context, successor string, fields dlog.Context) error {
	c.deps.Creates.Close(successor)
	inFlight := c.deps.Creates.InFlight()
	fields = dlog.Merge(fields, dlog.Context{"creates_in_flight": inFlight})
	if inFlight == 0 {
		c.log.Info(opHandover, "closed the create gate with no create in flight", fields)
		return nil
	}
	c.log.Info(opHandover, "closed the create gate; waiting for the creates in flight to finish before listing what this daemon serves", fields)
	err := c.deps.Creates.AwaitIdle(ctx, c.deps.HoldoutWarnEvery, c.deps.Clock.After, func(warnings, inFlight int) {
		c.log.Warn(opHandover, "still waiting for creates in flight to finish before listing what this daemon serves", dlog.Merge(fields, dlog.Context{
			"creates_in_flight": inFlight,
			"warnings":          warnings,
			"cadence":           c.deps.HoldoutWarnEvery.String(),
		}))
	})
	if err != nil {
		c.log.Info(opHandover, "the daemon's lifetime ended while creates were still finishing", dlog.WithCause(fields, err))
		return fmt.Errorf("rollout: await the creates in flight: %w", err)
	}
	c.log.Info(opHandover, "every create in flight finished; the stand-down proceeds", fields)
	return nil
}

// reopenCreates admits creates again: the stand-down ended with this daemon
// still serving.
func (c *controller) reopenCreates(why string, fields dlog.Context) {
	c.deps.Creates.Reopen()
	c.log.Info(opHandover, "reopened the create gate: "+why, fields)
}
