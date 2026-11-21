package persistentwifi

import (
	"context"
	"errors"
	"fmt"
	"slices"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opEnergy = "daemon.persistentwifi.energy"

// THE ENERGY MODE FOLLOWS THE LID-CLOSED QUIET STATE AND THE POWER SOURCE
// (owner rulings, 2026-11-13: "persistent wifi mode should only update the
// low vs high when the lid closes (switching to low when closing, restoring
// to whatever it was when opening)"; 2026-11-14: with the lid closed on AC
// power Low Power Mode is never engaged, losing AC engages it, and regaining
// AC with the lid still closed disengages it). energyWanted is the one
// decision:
//
//   - While the quiet state stands on battery power, the machine runs Low
//     Power Mode. Before the first Low Power write the mode in use (the
//     user's) is recorded, durably (EnergyStore), so a daemon that adopts the
//     standing state across a restart, a handover or a crash restores what
//     the first one recorded.
//   - While the quiet state stands on AC power, the machine runs High Power
//     Mode (owner ruling, 2026-11-21: the closed lid on AC power is the
//     machine at work), recorded and restored exactly as Low Power Mode is.
//   - While it does not stand, a recorded mode is restored, then forgotten. With nothing recorded nothing is written, so
//     the user's own choice is never overridden: with the lid open or on AC
//     power no beat and no mode change ever writes the energy mode.
//   - While a fact the decision turns on is not yet heard (the lid, or the
//     power source with the lid closed), nothing is written either way, so a
//     daemon booting under a closed lid never restores a Low Power Mode it is
//     about to hold, only to set it again a moment later.
//
// Every half decides by STATES (holdEnergyLocked), never by transitions: a
// Low Power write or a restore that failed is tried again on the next beat
// while its state lasts, and its record is kept until the restore takes.
// Every write is recorded at INFO with the trigger that asked and the
// decision's reason ("because").

// energyHold is what the energy step does with the facts as they stand.
type energyHold int

const (
	// energyKeep writes nothing: a fact the decision turns on is unheard.
	energyKeep energyHold = iota
	// energyLower records the mode in use, then sets Low Power Mode.
	energyLower
	// energyRaise records the mode in use, then sets High Power Mode.
	energyRaise
	// energyRestore restores a recorded mode, then forgets it.
	energyRestore
)

// energyWanted is THE ONE DECISION of the energy mode, answering the hold
// and its reason for the record. quiet is whether the quiet state stands,
// which only a heard closed lid makes so; an unwatched power source is never
// a battery.
func energyWanted(quiet bool, lid lidKind, source powerSourceKind) (energyHold, string) {
	switch {
	case lid == lidUnheard:
		return energyKeep, "the lid is not yet heard"
	case !quiet:
		return energyRestore, "the lid-closed quiet state does not stand"
	case source == powerSourceBattery:
		return energyLower, "the lid is closed on battery power"
	case source == powerSourceAC:
		return energyRaise, "the lid is closed on AC power"
	case source == powerSourceUnheard:
		return energyKeep, "the power source is not yet heard"
	case source == powerSourceUnwatched:
		return energyRestore, "the power source is not heard"
	}
	panic(fmt.Sprintf("persistentwifi: energyWanted was handed an unnamed power source %d", int(source)))
}

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

// decideRestore's ours are the modes the quiet state itself sets (Low and
// High Power Mode): the machine in any other mode has been moved by the user.
func decideRestore(energy, kept string, ours ...string) restoreAction {
	switch {
	case energy == "":
		return restoreWait
	case energy == kept:
		return restoreAlready
	case !slices.Contains(ours, energy):
		return restoreUserMoved
	}
	return restoreWrite
}

// holdEnergyLocked holds the energy mode to the facts as they stand
// (energyWanted). energy is the mode in use as read (reading.energy). Every
// beat, lid and power source asks it, so a failed step is retried by state.
// Caller holds mu.
func (c *Controller) holdEnergyLocked(ctx context.Context, energy, trigger string) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	hold, because := energyWanted(c.quiet, c.lid.kind, c.powerSource.kind)
	switch hold {
	case energyLower:
		return c.setQuietEnergyLocked(ctx, energy, trigger, because, c.host.LowPowerEnergy(), energyLowPowerOutcome, "Low Power Mode")
	case energyRaise:
		return c.setQuietEnergyLocked(ctx, energy, trigger, because, c.host.HighPowerEnergy(), energyHighPowerOutcome, "High Power Mode")
	case energyRestore:
		return c.restoreEnergyLocked(ctx, energy, trigger, because, false)
	}
	c.log.Debug(opEnergy, "the energy mode is held as it is", dlog.Context{"trigger": trigger, "because": because})
	return energyUnchanged()
}

// setQuietEnergyLocked is the engaging half: the mode in use is recorded (or
// the kept record adopted), then target (Low or High Power Mode, named
// targetName) set. A record that cannot be read or kept writes nothing.
// because is energyWanted's reason. Caller holds mu.
func (c *Controller) setQuietEnergyLocked(ctx context.Context, energy, trigger, because, target string, outcome func() *agentreplv1.UpdatePersistentWifiModeEnergyMode, targetName string) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	if energy == "" {
		c.log.Debug(opEnergy, "the energy mode in use is unread or the machine names none; "+targetName+" is not set",
			dlog.Context{"trigger": trigger})
		return energyUnchanged()
	}
	kept, err := c.keepEnergyLocked(energy, trigger)
	if err != nil {
		return c.energyFailedLocked(ctx, trigger,
			"the energy mode in use could not be recorded, so "+targetName+" was not set; it is tried again on the next beat", err)
	}
	if energy == target {
		c.energyCause = ""
		return energyUnchanged()
	}
	if err := c.host.SetEnergy(ctx, target); err != nil {
		return c.energyFailedLocked(ctx, trigger,
			targetName+" could not be set for the lid-closed quiet state; it is tried again on the next beat", err)
	}
	c.energyCause = ""
	c.log.Info(opEnergy, "the lid-closed quiet state put the machine in "+targetName,
		dlog.Context{"trigger": trigger, "because": because, "power_source": c.powerSource.name(), "was": energy, "recorded": kept})
	return outcome()
}

// energyLowPowerOutcome is the step that set Low Power Mode.
func energyLowPowerOutcome() *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_LowPower{LowPower: &agentreplv1.UpdatePersistentWifiModeEnergyModeLowPower{}},
	}
}

// energyHighPowerOutcome is the step that set High Power Mode.
func energyHighPowerOutcome() *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_HighPowerHeld{HighPowerHeld: &agentreplv1.UpdatePersistentWifiModeEnergyModeHighPowerHeld{}},
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

// restoreEnergyLocked is the disengaging half: a recorded mode is restored,
// then forgotten (decideRestore). because is the reason it is asked.
// leaving says the quiet state is being left now, when a missing record is
// worth a record. Caller holds mu.
func (c *Controller) restoreEnergyLocked(ctx context.Context, energy, trigger, because string, leaving bool) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
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
			c.log.Info(opEnergy, "the lid-closed quiet state was left with no energy mode recorded (no mode was ever set: the quiet state was entered by an earlier build); nothing is restored",
				dlog.Context{"trigger": trigger, "because": because, "power_source": c.powerSource.name(), "energy": energy})
		}
		return energyUnchanged()
	}
	switch decideRestore(energy, kept, c.host.LowPowerEnergy(), c.host.HighPowerEnergy()) {
	case restoreWait:
		c.log.Debug(opEnergy, "the energy mode in use is unread; the recorded mode is restored once it is read",
			dlog.Context{"trigger": trigger, "recorded": kept})
		return energyUnchanged()
	case restoreAlready:
		c.forgetEnergyLocked(ctx, trigger, because, "the machine already runs the recorded energy mode; the record is forgotten", energy, kept)
		return energyUnchanged()
	case restoreUserMoved:
		c.forgetEnergyLocked(ctx, trigger, because, "the energy mode was changed after the lid-closed quiet state set Low Power Mode; that choice stands and the recorded mode is forgotten", energy, kept)
		return energyUnchanged()
	}
	if err := c.host.SetEnergy(ctx, kept); err != nil {
		return c.energyFailedLocked(ctx, trigger,
			"the recorded energy mode could not be restored; the record is kept and the restore tried again on the next beat", err)
	}
	c.forgetEnergyLocked(ctx, trigger, because, "the recorded energy mode was restored", energy, kept)
	return &agentreplv1.UpdatePersistentWifiModeEnergyMode{
		Outcome: &agentreplv1.UpdatePersistentWifiModeEnergyMode_Restored{
			Restored: &agentreplv1.UpdatePersistentWifiModeEnergyModeRestored{PowerMode: kept},
		},
	}
}

// forgetEnergyLocked clears the record once nothing is owed on it, recording
// why at INFO with because, the reason the restore was asked. A record that cannot be cleared stays held, so the next beat
// forgets it again. Caller holds mu.
func (c *Controller) forgetEnergyLocked(ctx context.Context, trigger, because, why, energy, kept string) {
	if err := c.energyStore.Clear(); err != nil {
		c.energyFailedLocked(ctx, trigger, "the recorded energy mode could not be cleared; it is cleared again on the next beat", err)
		return
	}
	c.energyKept = ""
	c.energyCause = ""
	c.log.Info(opEnergy, why, dlog.Context{"trigger": trigger, "because": because, "power_source": c.powerSource.name(), "was": energy, "recorded": kept})
}

// energyFailedLocked records a failed energy step at ERROR, once per
// distinct cause (energyCause), and answers its failed outcome. A step its
// caller abandoned records nothing. Caller holds mu.
func (c *Controller) energyFailedLocked(ctx context.Context, trigger, msg string, err error) *agentreplv1.UpdatePersistentWifiModeEnergyMode {
	if ctx.Err() == nil {
		if cause := err.Error(); cause != c.energyCause {
			c.log.Error(opEnergy, msg, dlog.Context{"trigger": trigger, "lid": c.lid.name(), "power_source": c.powerSource.name(), "quiet": c.quiet, "cause": cause})
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
func GuardRestoreEnergy(ctx context.Context, host Host, store EnergyStore, log dlog.Logger) error {
	const trigger = "thaw_guard"
	rec, ok, err := store.Load()
	if err != nil {
		log.Error(opEnergy, "the thaw guard could not read the recorded energy mode; nothing was restored", dlog.Context{"trigger": trigger, "cause": err.Error()})
		return err
	}
	if !ok {
		return nil
	}
	energy, err := host.ReadEnergy(ctx)
	if err != nil {
		log.Error(opEnergy, "the thaw guard could not read the energy mode in use; the record is kept for the next daemon",
			dlog.Context{"trigger": trigger, "recorded": rec.Mode, "cause": err.Error()})
		return err
	}
	action := decideRestore(energy, rec.Mode, host.LowPowerEnergy(), host.HighPowerEnergy())
	switch action {
	case restoreWait:
		err := errors.New("the host named no energy mode in use")
		log.Error(opEnergy, "the thaw guard read no energy mode in use; the record is kept for the next daemon",
			dlog.Context{"trigger": trigger, "recorded": rec.Mode, "cause": err.Error()})
		return err
	case restoreWrite:
		if err := host.SetEnergy(ctx, rec.Mode); err != nil {
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
