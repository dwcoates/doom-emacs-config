package merge

import (
	"context"
	"time"
)

// The RACING SWITCH is the operator's daemon-wide switch that disables the
// queue's racing: on AC power a queue otherwise works its top two merges,
// which race to land and take turns at the tests (power.go); disabled, every
// queue works its head alone whatever the power source. It is DURABLE (wsm's
// merge_racing_disabled), so a restart or a handover keeps it, and the gate
// governor is born with it (cmd/agent-repld's graph), so no merge is admitted
// to a window it has narrowed. A flip is applied to the governor, which tells
// this orchestrator through modeChanged.

const (
	opDisableRacing = "daemon.merge.disable_racing"
	opEnableRacing  = "daemon.merge.enable_racing"
)

// racingSwitch is the racing switch as an operator switch: on is disabled.
func (o *orchestrator) racingSwitch() operatorSwitch {
	return operatorSwitch{
		read: func(ctx context.Context) (bool, time.Time, error) {
			racing, err := o.deps.DB.MergeRacing(ctx)
			return racing.Disabled, racing.Since, err
		},
		write:      o.deps.DB.SetMergeRacingDisabled,
		apply:      o.deps.Governor.SetRacingDisabled,
		sinceField: "disabled_since",
		on: switchSide{
			op:             opDisableRacing,
			refusedArm:     ArmRacingAlreadyDisabled,
			refusedReason:  "the merge queue's racing is already disabled",
			refusedMessage: "refused a disable of racing that is already disabled",
			askedBy:        "UpdateMergeQueue.disable_racing",
			message:        "disabled the merge queue's racing: every queue works its head alone, whatever the host's power source",
		},
		off: switchSide{
			op:             opEnableRacing,
			refusedArm:     ArmRacingNotDisabled,
			refusedReason:  "the merge queue's racing is not disabled",
			refusedMessage: "refused an enable of racing that is not disabled",
			askedBy:        "UpdateMergeQueue.enable_racing",
			message:        "enabled the merge queue's racing: on AC power each queue works its top two merges again",
		},
		inheritedMessage: "the merge queue's racing is disabled: every queue works its head alone, whatever the host's power source, until it is enabled",
	}
}

// DisableRacing implements Orchestrator.
func (o *orchestrator) DisableRacing(ctx context.Context) error {
	return o.flip(ctx, o.racingSwitch(), true)
}

// EnableRacing implements Orchestrator.
func (o *orchestrator) EnableRacing(ctx context.Context) error {
	return o.flip(ctx, o.racingSwitch(), false)
}
