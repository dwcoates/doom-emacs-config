package wsm

import (
	"context"
	"time"
)

// racingDisabledDDL is the layout-53 addition: the operator's daemon-wide
// switch that DISABLES the merge queue's racing (UpdateMergeQueue's
// disable_racing). A singleton switch (singletonswitch.go): its row present
// means racing is disabled since disabled_at, so every queue works its head
// alone whatever the power source; absent means the power source decides, as
// it always did. Durable because a switch a restart or a handover dropped
// would set two merges racing again behind the operator's back.
//
// IT IS ADDITIVE: the previous build never reads the table, and runs as if
// racing were enabled.
const racingDisabledDDL = `
CREATE TABLE merge_racing_disabled (
  id          INTEGER PRIMARY KEY CHECK (id = 1),
  disabled_at INTEGER NOT NULL
);
`

// RacingSwitch is the daemon-wide racing switch: while Disabled, every merge
// queue works its head alone, whatever the host's power source.
type RacingSwitch struct {
	// Disabled reports whether racing is disabled.
	Disabled bool
	// Since is when racing was disabled; zero when it is not.
	Since time.Time
}

// racingDisabledSwitch is the racing switch's singleton row (layout 53).
var racingDisabledSwitch = singletonSwitch{table: "merge_racing_disabled", column: "disabled_at"}

// SetMergeRacingDisabled disables racing from at (disabled true) or enables
// it again. Disabling it again keeps the ORIGINAL start; the orchestrator
// refuses that no-op before it gets here.
func (s *store) SetMergeRacingDisabled(ctx context.Context, disabled bool, at time.Time) error {
	return racingDisabledSwitch.set(ctx, s, "daemon.wsm.set_merge_racing_disabled", disabled, at)
}

// MergeRacing reports the racing switch. An absent row is ENABLED, the state
// every daemon before layout 53 ran in, and is answered without an error.
func (s *store) MergeRacing(ctx context.Context) (RacingSwitch, error) {
	disabled, since, err := racingDisabledSwitch.get(ctx, s, "daemon.wsm.merge_racing")
	return RacingSwitch{Disabled: disabled, Since: since}, err
}
