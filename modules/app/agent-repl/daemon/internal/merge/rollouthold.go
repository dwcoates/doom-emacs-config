package merge

import (
	"context"
	"time"

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

// rolloutHold is the hold as an operator switch: on is held.
func (o *orchestrator) rolloutHold() operatorSwitch {
	return operatorSwitch{
		read: func(ctx context.Context) (bool, time.Time, error) {
			hold, err := o.deps.DB.MergeRolloutHold(ctx)
			return hold.Held, hold.Since, err
		},
		write:      o.deps.DB.SetMergeRolloutHold,
		sinceField: "held_since",
		on: switchSide{
			op:             opHoldRollouts,
			refusedArm:     ArmRolloutsAlreadyHeld,
			refusedReason:  "the merge queue's rollouts are already held",
			refusedMessage: "refused a hold of rollouts that are already held",
			askedBy:        "UpdateMergeQueue.hold_rollouts",
			message:        "held the merge queue's rollouts: merges still land, but a landing in this daemon's own checkout no longer tells the deploy",
		},
		off: switchSide{
			op:             opReleaseRollouts,
			refusedArm:     ArmRolloutsNotHeld,
			refusedReason:  "the merge queue's rollouts are not held",
			refusedMessage: "refused a release of rollouts that are not held",
			askedBy:        "UpdateMergeQueue.release_rollouts",
			message:        "released the merge queue's rollouts: the next landing in this daemon's own checkout tells the deploy; nothing that landed while held is deployed until then",
		},
	}
}

// HoldRollouts implements Orchestrator.
func (o *orchestrator) HoldRollouts(ctx context.Context) error {
	return o.flip(ctx, o.rolloutHold(), true)
}

// ReleaseRollouts implements Orchestrator.
func (o *orchestrator) ReleaseRollouts(ctx context.Context) error {
	return o.flip(ctx, o.rolloutHold(), false)
}

// recordRolloutHold states, once per boot, a hold this daemon inherited: a
// hold taken before a restart or a handover is otherwise invisible until the
// next landing does not deploy. Unheld, it records nothing.
func (o *orchestrator) recordRolloutHold(ctx context.Context) error {
	hold, err := o.deps.DB.MergeRolloutHold(ctx)
	if err != nil {
		return err
	}
	if hold.Held {
		o.deps.Log.Global().Info(opHoldRollouts,
			"the merge queue's rollouts are held: merges land, but a landing in this daemon's own checkout does not tell the deploy until they are released",
			dlog.Context{"held_since": hold.Since})
	}
	return nil
}
