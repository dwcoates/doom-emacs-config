package wsm

import (
	"context"
	"time"
)

// RolloutHold is the daemon-wide hold on merge-queue rollouts: while Held, a
// merge that lands in the daemon's own checkout does not tell the deploy.
type RolloutHold struct {
	// Held reports whether the rollouts are held.
	Held bool
	// Since is when the hold began; zero when not held.
	Since time.Time
}

// rolloutHoldSwitch is the hold's singleton switch (layout 37).
var rolloutHoldSwitch = singletonSwitch{table: "merge_rollout_hold", column: "held_at"}

// SetMergeRolloutHold holds the rollouts from at (held true) or ends the hold
// (held false). Holding an already-held rollout keeps the ORIGINAL start, so
// Since always answers how long the build has been kept; the orchestrator
// refuses that no-op before it gets here.
func (s *store) SetMergeRolloutHold(ctx context.Context, held bool, at time.Time) error {
	return rolloutHoldSwitch.set(ctx, s, "daemon.wsm.set_merge_rollout_hold", held, at)
}

// MergeRolloutHold reports the rollout hold. An absent row is NOT HELD, the
// state every daemon before layout 37 ran in, and is answered without an
// error.
func (s *store) MergeRolloutHold(ctx context.Context) (RolloutHold, error) {
	held, since, err := rolloutHoldSwitch.get(ctx, s, "daemon.wsm.merge_rollout_hold")
	return RolloutHold{Held: held, Since: since}, err
}
