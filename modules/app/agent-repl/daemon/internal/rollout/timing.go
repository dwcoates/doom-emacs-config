package rollout

import (
	"context"
	"time"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
)

// timeBounce answers req with its run timed: once a run ENDS (finished or
// failed), ended is told how long the bounce waited from being asked until
// that run began, and how long the run took. A run the registry will rerun
// (bounce.ErrDeferred) is no end, so it is not timed; the rerun's wait is
// counted from the original ask.
func (c *controller) timeBounce(req bounce.Request, ended func(outcome restarttiming.Outcome, waited, ran time.Duration)) bounce.Request {
	asked := c.deps.Clock.Now()
	run := req.Run
	req.Run = func(ctx context.Context, ws ids.WorkspaceID) error {
		began := c.deps.Clock.Now()
		err := run(ctx, ws)
		if bounce.OutcomeOf(err) == bounce.OutcomeDeferred {
			return err
		}
		ended(restarttiming.OutcomeOf(err), began.Sub(asked), c.deps.Clock.Now().Sub(began))
		return err
	}
	return req
}

// timeShimBounce times a shim replacement's wait and run for ws.
func (c *controller) timeShimBounce(ws ids.WorkspaceID, reason RelaunchReason, req bounce.Request) bounce.Request {
	return c.timeBounce(req, func(outcome restarttiming.Outcome, waited, ran time.Duration) {
		c.deps.Restarts.ObserveRestart(restarttiming.OfShimRestart(ws, restarttiming.ShimWait, string(reason), outcome, waited))
		c.deps.Restarts.ObserveRestart(restarttiming.OfShimRestart(ws, restarttiming.ShimRun, string(reason), outcome, ran))
	})
}

// timeTransfer times a workspace transfer's wait and run for ws.
func (c *controller) timeTransfer(ws ids.WorkspaceID, req bounce.Request) bounce.Request {
	return c.timeBounce(req, func(outcome restarttiming.Outcome, waited, ran time.Duration) {
		c.deps.Restarts.ObserveRestart(restarttiming.OfTransfer(ws, restarttiming.TransferWait, outcome, waited))
		c.deps.Restarts.ObserveRestart(restarttiming.OfTransfer(ws, restarttiming.TransferRun, outcome, ran))
	})
}

// shimStep times one step of a shim replacement that SUCCEEDED, measured
// from began. A failed step is not timed: the replacement's run carries the
// failure.
func (c *controller) shimStep(ws ids.WorkspaceID, reason RelaunchReason, step restarttiming.ShimPhase, began time.Time) {
	c.deps.Restarts.ObserveRestart(restarttiming.OfShimRestart(ws, step, string(reason), restarttiming.OK, c.deps.Clock.Now().Sub(began)))
}

// timeDeployTakeover times, in the successor that has just taken over from a
// deploy's handover, the handover (from its manifest until now) and the whole
// deploy (from its start until now). An instant the manifest did not carry
// (an older daemon wrote it) is not timed, and the record says so.
func (c *controller) timeDeployTakeover(deployStarted, handoverStarted time.Time, fields dlog.Context) {
	now := c.deps.Clock.Now()
	if handoverStarted.IsZero() || deployStarted.IsZero() {
		c.log.Info(opProgress, "the deploy's manifest did not say when it began; this deploy is not timed", dlog.Merge(fields, dlog.Context{
			"deploy_started_at": deployStarted, "handover_started_at": handoverStarted,
		}))
		return
	}
	c.deps.Restarts.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployHandover, restarttiming.OK, now.Sub(handoverStarted)))
	c.deps.Restarts.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployTotal, restarttiming.OK, now.Sub(deployStarted)))
}
