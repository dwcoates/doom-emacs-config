package persistentwifi

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opEnergy = "daemon.persistentwifi.energy"

// THE ENERGY MODE FOLLOWS THE LID-CLOSED QUIET STATE AND NOTHING ELSE (owner
// ruling, 2026-11-13): "persistent wifi mode should only update the low vs
// high when the lid closes (switching to low when closing, restoring to
// whatever it was when opening)".
//
//   - While the quiet state stands, the machine runs Low Power Mode. Before
//     the first Low Power write the mode in use (the user's) is recorded,
//     durably (EnergyStore), so a daemon that adopts the standing state
//     across a restart, a handover or a crash restores what the first one
//     recorded.
//   - While it does not stand, a recorded mode is restored, then forgotten.
//     With nothing recorded nothing is written, so with the lid open the
//     user's own choice is never overridden: no beat, no mode change and no
//     power source ever writes the energy mode.
//
// Both halves decide by STATES (holdEnergyLocked), never by transitions: a
// Low Power write or a restore that failed is tried again on the next beat
// while its state lasts, and its record is kept until the restore takes.

// restoreAction is what a restore does with the mode in use and the one
// recorded: THE ONE DECISION, asked by the daemon and the thaw guard alike.
type restoreAction int

const (
	// restoreWait: the mode in use is unread, so nothing can be decided; the
	// record is kept.
	restoreWait restoreAction = iota
	// restoreAlready: the machine already runs the recorded mode; the record
	// is forgotten with nothing written.
	restoreAlready
	// restoreUserMoved: the machine left the Low Power Mode the quiet state
	// set for another mode since; that is the user's choice, which stands,
	// and the record is forgotten with nothing written.
	restoreUserMoved
	// restoreWrite: the recorded mode is written, then forgotten.
	restoreWrite
)

func decideRestore(energy, kept string) restoreAction {
	switch {
	case energy == "":
		return restoreWait
	case energy == kept:
		return restoreAlready
	case energy != energyModeLowPower:
		return restoreUserMoved
	}
	return restoreWrite
}

// holdEnergyLocked holds the energy mode to the quiet state as it stands
// (c.quiet): lowered while it stands, restored while it does not. energy is
// the mode in use as read (reading.energy). Every beat asks it, so a failed
// step is retried by state. Caller holds mu.
func (c *Controller) holdEnergyLocked(ctx context.Context, energy, trigger string) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	if c.quiet {
		return c.lowerEnergyLocked(ctx, energy, trigger)
	}
	return c.restoreEnergyLocked(ctx, energy, trigger, false)
}

// lowerEnergyLocked is the quiet state's half: the mode in use is recorded
// (or the kept record adopted), then Low Power Mode set. A record that cannot
// be read or kept writes nothing. Caller holds mu.
func (c *Controller) lowerEnergyLocked(ctx context.Context, energy, trigger string) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	if energy == "" {
		c.log.Debug(opEnergy, "the energy mode in use is unread or the machine names none; Low Power Mode is not set",
			dlog.Context{"trigger": trigger})
		return energyUnchanged()
	}
	kept, err := c.keepEnergyLocked(energy, trigger)
	if err != nil {
		return c.energyFailedLocked(ctx, trigger,
			"the energy mode in use could not be recorded, so Low Power Mode was not set; it is tried again on the next beat", err)
	}
	if energy == energyModeLowPower {
		c.energyCause = ""
		return energyUnchanged()
	}
	if err := c.pmsetAll(ctx, "powermode", energyModeLowPower); err != nil {
		return c.energyFailedLocked(ctx, trigger,
			"Low Power Mode could not be set for the lid-closed quiet state; it is tried again on the next beat", err)
	}
	c.energyCause = ""
	c.log.Info(opEnergy, "the lid-closed quiet state put the machine in Low Power Mode",
		dlog.Context{"trigger": trigger, "was": energy, "recorded": kept})
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_LowPower{LowPower: &agentreplv1.UpdatePersistentWifiModeEnergyModeLowPower{}},
	}
}

// keepEnergyLocked answers the recorded mode, keeping it durably first: a
// kept record is adopted (a successor daemon's case); one this daemon holds
// but the store lost (a thaw guard's restore raced it) is kept again; with
// neither, the mode in use is the user's and is recorded. Caller holds mu.
func (c *Controller) keepEnergyLocked(energy, trigger string) (string, error) {
	rec, ok, err := c.energyStore.Load()
	if err != nil {
		return "", err
	}
	if ok {
		if c.energyKept != rec.Mode {
			c.log.Info(opEnergy, "the lid-closed quiet state adopted the recorded energy mode",
				dlog.Context{"trigger": trigger, "recorded": rec.Mode})
		}
		c.energyKept = rec.Mode
		return rec.Mode, nil
	}
	kept, why := energy, "the energy mode in use was recorded for the lid-closed quiet state"
	if c.energyKept != "" {
		kept, why = c.energyKept, "the recorded energy mode had left its store; it was kept again"
	}
	if err := c.energyStore.Save(EnergyRecord{Mode: kept}); err != nil {
		return "", err
	}
	c.log.Info(opEnergy, why, dlog.Context{"trigger": trigger, "recorded": kept})
	c.energyKept = kept
	return kept, nil
}

// restoreEnergyLocked is the other half: a recorded mode is restored, then
// forgotten (decideRestore). leaving says the quiet state is being left now,
// when a missing record is worth a record. Caller holds mu.
func (c *Controller) restoreEnergyLocked(ctx context.Context, energy, trigger string, leaving bool) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	rec, ok, err := c.energyStore.Load()
	if err != nil {
		return c.energyFailedLocked(ctx, trigger,
			"the recorded energy mode could not be read; nothing was restored, and it is tried again on the next beat", err)
	}
	kept := c.energyKept
	if ok {
		kept = rec.Mode
	}
	if kept == "" {
		c.energyCause = ""
		if leaving {
			c.log.Info(opEnergy, "the lid-closed quiet state was left with no energy mode recorded (entered by an earlier build, or never recorded); nothing is restored",
				dlog.Context{"trigger": trigger, "energy": energy})
		}
		return energyUnchanged()
	}
	switch decideRestore(energy, kept) {
	case restoreWait:
		c.log.Debug(opEnergy, "the energy mode in use is unread; the recorded mode is restored once it is read",
			dlog.Context{"trigger": trigger, "recorded": kept})
		return energyUnchanged()
	case restoreAlready:
		c.forgetEnergyLocked(ctx, trigger, "the machine already runs the recorded energy mode; the record is forgotten", energy, kept)
		return energyUnchanged()
	case restoreUserMoved:
		c.forgetEnergyLocked(ctx, trigger, "the energy mode was changed after the lid-closed quiet state set Low Power Mode; that choice stands and the recorded mode is forgotten", energy, kept)
		return energyUnchanged()
	}
	if err := c.pmsetAll(ctx, "powermode", kept); err != nil {
		return c.energyFailedLocked(ctx, trigger,
			"the recorded energy mode could not be restored; the record is kept and the restore tried again on the next beat", err)
	}
	c.forgetEnergyLocked(ctx, trigger, "the recorded energy mode was restored", energy, kept)
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_Restored{
			Restored: &agentreplv1.UpdatePersistentWifiModeEnergyModeRestored{PowerMode: kept},
		},
	}
}

// forgetEnergyLocked clears the record once nothing is owed on it, recording
// why at INFO. A record that cannot be cleared stays held, so the next beat
// forgets it again. Caller holds mu.
func (c *Controller) forgetEnergyLocked(ctx context.Context, trigger, why, energy, kept string) {
	if err := c.energyStore.Clear(); err != nil {
		c.energyFailedLocked(ctx, trigger, "the recorded energy mode could not be cleared; it is cleared again on the next beat", err)
		return
	}
	c.energyKept = ""
	c.energyCause = ""
	c.log.Info(opEnergy, why, dlog.Context{"trigger": trigger, "was": energy, "recorded": kept})
}

// energyFailedLocked records a failed energy step at ERROR, once per
// distinct cause (energyCause), and answers its failed outcome. A step its
// caller abandoned records nothing. Caller holds mu.
func (c *Controller) energyFailedLocked(ctx context.Context, trigger, msg string, err error) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	if ctx.Err() == nil {
		if cause := err.Error(); cause != c.energyCause {
			c.log.Error(opEnergy, msg, dlog.Context{"trigger": trigger, "lid": c.lid.name(), "quiet": c.quiet, "cause": cause})
			c.energyCause = cause
		}
	}
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_Failed{
			Failed: &agentreplv1.UpdatePersistentWifiModeEnergyModeFailed{Detail: err.Error()},
		},
	}
}

// energyUnchanged is the step that moved nothing.
func energyUnchanged() *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_Unchanged{Unchanged: &agentreplv1.UpdatePersistentWifiModeEnergyModeUnchanged{}},
	}
}

// GuardRestoreEnergy is the thaw guard's energy step (appfreeze.GuardDeps's
// AlsoRestore): once no daemon is left to leave the quiet state, the mode it
// recorded is restored exactly as a daemon would restore it
// (decideRestore), and the record cleared. A failure keeps the record, so
// the next daemon restores it by state. Every failure is recorded at ERROR
// and answered.
func GuardRestoreEnergy(ctx context.Context, r Runner, tools Tools, store EnergyStore, log dlog.Logger) error {
	const trigger = "thaw_guard"
	rec, ok, err := store.Load()
	if err != nil {
		log.Error(opEnergy, "the thaw guard could not read the recorded energy mode; nothing was restored", dlog.Context{"trigger": trigger, "cause": err.Error()})
		return err
	}
	if !ok {
		return nil
	}
	out, err := run(ctx, r, tools.Pmset, "-g")
	if err != nil {
		log.Error(opEnergy, "the thaw guard could not read the energy mode in use; the record is kept for the next daemon",
			dlog.Context{"trigger": trigger, "recorded": rec.Mode, "cause": err.Error()})
		return err
	}
	energy := parseEnergyMode(out)
	action := decideRestore(energy, rec.Mode)
	switch action {
	case restoreWait:
		err := fmt.Errorf("%s -g printed no powermode line", tools.Pmset)
		log.Error(opEnergy, "the thaw guard read no energy mode in use; the record is kept for the next daemon",
			dlog.Context{"trigger": trigger, "recorded": rec.Mode, "cause": err.Error()})
		return err
	case restoreWrite:
		if err := pmsetWrite(ctx, r, tools, "powermode", rec.Mode); err != nil {
			log.Error(opEnergy, "the thaw guard could not restore the recorded energy mode; the record is kept for the next daemon",
				dlog.Context{"trigger": trigger, "recorded": rec.Mode, "cause": err.Error()})
			return err
		}
	}
	if err := store.Clear(); err != nil {
		log.Error(opEnergy, "the thaw guard could not clear the recorded energy mode", dlog.Context{"trigger": trigger, "recorded": rec.Mode, "cause": err.Error()})
		return err
	}
	log.Info(opEnergy, "the last daemon is gone; the thaw guard settled the recorded energy mode",
		dlog.Context{"trigger": trigger, "was": energy, "recorded": rec.Mode, "restored": action == restoreWrite})
	return nil
}
