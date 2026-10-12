package merge

import (
	"context"

	"agent-repld/internal/dlog"
)

// The ROLLOUT HOLD is the operator's daemon-wide switch between a landing and
// the deploy: while it is held, a merge that lands in the daemon's own
// checkout lands exactly as it always does, but selfReload does not tell the
// deploy, so nothing redeploys, hot-reloads or bounces the runtime. It exists
// for a hand-deployed build (a profiling build) that the next landing would
// otherwise replace. It is DURABLE (wsm's merge_rollout_hold), so a restart
// or a handover keeps it; and it is read at each landing rather than cached,
// so a successor answers the hold its predecessor took.

const (
	opHoldRollouts    = "daemon.merge.hold_rollouts"
	opReleaseRollouts = "daemon.merge.release_rollouts"
)

// HoldRollouts implements Orchestrator.
func (o *orchestrator) HoldRollouts(ctx context.Context) error {
	hold, err := o.deps.DB.MergeRolloutHold(ctx)
	if err != nil {
		return err
	}
	if hold.Held {
		o.deps.Log.Global().Warn(opHoldRollouts, "refused a hold of rollouts that are already held",
			dlog.Context{"arm": ArmRolloutsAlreadyHeld, "held_since": hold.Since})
		return &RefusalError{Arm: ArmRolloutsAlreadyHeld, Reason: "the merge queue's rollouts are already held"}
	}
	if err := o.deps.DB.SetMergeRolloutHold(ctx, true, o.deps.Now()); err != nil {
		return err
	}
	o.deps.Log.Global().Info(opHoldRollouts,
		"held the merge queue's rollouts: merges still land, but a landing in this daemon's own checkout no longer tells the deploy",
		dlog.Context{"asked_by": "UpdateMergeQueue.hold_rollouts"})
	return nil
}

// ReleaseRollouts implements Orchestrator.
func (o *orchestrator) ReleaseRollouts(ctx context.Context) error {
	hold, err := o.deps.DB.MergeRolloutHold(ctx)
	if err != nil {
		return err
	}
	if !hold.Held {
		o.deps.Log.Global().Warn(opReleaseRollouts, "refused a release of rollouts that are not held",
			dlog.Context{"arm": ArmRolloutsNotHeld})
		return &RefusalError{Arm: ArmRolloutsNotHeld, Reason: "the merge queue's rollouts are not held"}
	}
	if err := o.deps.DB.SetMergeRolloutHold(ctx, false, o.deps.Now()); err != nil {
		return err
	}
	o.deps.Log.Global().Info(opReleaseRollouts,
		"released the merge queue's rollouts: the next landing in this daemon's own checkout tells the deploy; nothing that landed while held is deployed until then",
		dlog.Context{"asked_by": "UpdateMergeQueue.release_rollouts", "held_since": hold.Since})
	return nil
}
