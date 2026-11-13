package rollout

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// opSpawnPreflight is the operation a daemon's proof that it can spawn its own
// successor is recorded under.
const opSpawnPreflight = "daemon.rollout.spawn_preflight"

// A DAEMON PROVES IT CAN SPAWN ITS OWN SUCCESSOR BEFORE IT TAKES OVER
// (2026-11-13). 0.425.14 built its successors' environment from a requirements
// manifest at a path that did not exist. It deployed fine, because the daemon
// before it spawned it with the old code; the NEXT deploy failed at spawn, and
// from then on the running daemon could not be rolled out from at all. A
// successor that cannot itself spawn a successor is a dead end, so it now
// refuses the handover while its incumbent can still keep serving.

// PreflightSpawn implements Controller: the spawner's Preflight, told this
// daemon's own address as the incumbent its successor would join.
//
// A JOINING SUCCESSOR whose preflight fails answers the failure, and the boot
// that asked ends before the successor ever serves: the incumbent's handover
// sees it exit before it proved it was serving (Successor.Ready), opens the
// `successor_spawn_failed` fault there, stops nothing it has not started, and
// keeps serving -- the channel every successor that does not come up already
// takes.
//
// A DAEMON THAT OWNS ITS STATE (a fresh boot, a restart's replacement) serves
// anyway, with the `successor_spawn_failed` fault opened on it: the deploy it
// cannot hand over from is one capability, and refusing the boot would take
// every workspace down with it, with no predecessor left to keep serving. The
// fault says, on every strip, that no deploy can roll out from this daemon;
// the next daemon boot or a successor that proves it is serving closes it.
// It must run after the daemon-boot recovery edge has fired
// (deploy.Deployer.CloseEarlierFailures), which would otherwise close the
// fault it opens.
func (c *controller) PreflightSpawn(ctx context.Context) error {
	joining := c.deps.IncumbentAddress != ""
	fields := dlog.Context{"self_address": c.deps.SelfAddress, "self_exe": c.deps.SelfExe, "joining": joining}
	err := c.deps.Spawner.Preflight(c.deps.SelfAddress)
	if err == nil {
		c.log.Info(opSpawnPreflight, "this daemon can spawn its own successor", fields)
		return nil
	}
	if joining {
		c.log.Error(opSpawnPreflight, "this successor could not spawn a successor of its own; it refuses the handover before it serves, and its incumbent keeps serving",
			dlog.WithCause(fields, err))
		return fmt.Errorf("rollout: the successor cannot spawn a successor of its own: %w", err)
	}
	c.log.Error(opSpawnPreflight, "this daemon cannot spawn a successor; it serves with the successor_spawn_failed fault open, and no deploy can roll out from it",
		dlog.WithCause(fields, err))
	c.openSpawnFault(ctx, fmt.Sprintf("this daemon cannot spawn a successor, so no deploy can roll out from it: %v", err), err, fields)
	return nil
}
