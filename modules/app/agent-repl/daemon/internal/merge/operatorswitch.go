package merge

import (
	"context"
	"time"

	"agent-repld/internal/dlog"
)

// An OPERATOR SWITCH is one of the queue's daemon-wide, durable switches an
// operator flips over UpdateMergeQueue: the rollout hold (rollouthold.go) and
// the racing switch (racing.go). Every flip is the one body here: the switch
// is read from the state database, a flip that changes nothing is refused
// with its own arm (and recorded at WARN), the new state is written, then
// applied to whatever reads it live, and recorded at INFO.

// switchSide is one direction of a flip: turning the switch on, or off.
type switchSide struct {
	// op is the operation the flip is recorded under.
	op string
	// refusedArm and refusedReason refuse a flip to the side the switch is
	// already on; refusedMessage records that refusal.
	refusedArm     string
	refusedReason  string
	refusedMessage string
	// askedBy names the UpdateMergeQueue arm that asked for the flip, and
	// message records it.
	askedBy string
	message string
}

// operatorSwitch is one durable switch and its two sides.
type operatorSwitch struct {
	// read answers whether the switch is on, and since when.
	read func(ctx context.Context) (bool, time.Time, error)
	// write turns the switch on from at, or off.
	write func(ctx context.Context, on bool, at time.Time) error
	// apply, when set, hands a written flip to what reads the switch live.
	apply func(on bool)
	// sinceField names the record field that carries when the switch was
	// turned on.
	sinceField string
	on, off    switchSide
}

// flip turns sw on or off.
func (o *orchestrator) flip(ctx context.Context, sw operatorSwitch, on bool) error {
	side := sw.off
	if on {
		side = sw.on
	}
	already, since, err := sw.read(ctx)
	if err != nil {
		return err
	}
	if already == on {
		fields := dlog.Context{"arm": side.refusedArm}
		if already {
			fields[sw.sinceField] = since
		}
		o.deps.Log.Global().Warn(side.op, side.refusedMessage, fields)
		return &RefusalError{Arm: side.refusedArm, Reason: side.refusedReason}
	}
	if err := sw.write(ctx, on, o.deps.Now()); err != nil {
		return err
	}
	if sw.apply != nil {
		sw.apply(on)
	}
	fields := dlog.Context{"asked_by": side.askedBy}
	if !on {
		fields[sw.sinceField] = since
	}
	o.deps.Log.Global().Info(side.op, side.message, fields)
	return nil
}
