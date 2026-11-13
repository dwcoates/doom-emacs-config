package persistentwifi

import (
	"context"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/appfreeze"
	"agent-repld/internal/dlog"
	"agent-repld/internal/viewquiet"
)

const opQuiet = "daemon.persistentwifi.quiet"

// THE LID-CLOSED QUIET STATE (agentrepl.v1 PersistentWifiState.quiet): with
// the mode on and the lid heard closed, the machine does no work for a screen
// nobody sees, so the battery goes to agent work. Entering it, in order:
//
//  1. the energy mode in use is recorded and Low Power Mode set (energy.go);
//  2. the views stop (Views): the daemon resolves and publishes no view for a
//     local page, and the pages and Emacs idle on the standing's push;
//  3. the configured applications are frozen (Freezer);
//  4. the display sleeps.
//
// Leaving it undoes the four in reverse, so the display is awake before the
// applications move, the views resync at the state everything left, and the
// recorded energy mode is restored last.
// The decision reads STATES, never transitions, so a restart, a handover or a
// repeated lid report reaches the same answer: a frozen set kept by an
// earlier daemon is adopted while the state stands and thawed the moment it
// does not, and so is the energy mode it recorded.

// AppFreezer freezes and thaws the configured applications (appfreeze). It
// freezes a process an application starts while the state stands on its
// own, and tells OnChange's function, outside its own lock, so the standing
// shows what Frozen answers now.
type AppFreezer interface {
	Freeze(ctx context.Context, trigger string) []appfreeze.Frozen
	Thaw(ctx context.Context, trigger string)
	Frozen() []appfreeze.Frozen
	OnChange(fn func())
}

// QuietTelemetry is the quiet state's gauge. The metrics surface is the
// production one; nil records nothing.
type QuietTelemetry interface {
	SetLidQuiet(quiet bool)
}

// quietWanted is THE ONE DECISION of whether the quiet state stands: the lid
// is heard closed and the mode is on. An unknown mode keeps whatever stands,
// since a failed read says nothing about the mode; an unheard or unwatched lid
// is never closed, so it can only ever leave the state.
func quietWanted(lid lidKind, mode modeKnown, standing bool) bool {
	if mode == modeUnknown && lid == lidClosed {
		return standing
	}
	return lid == lidClosed && mode == modeIsOn
}

// quietSteps are the outcomes of the quiet state's two steps an Update
// reports.
type quietSteps struct {
	display *agentreplv1.UpdatePersistentWifiModeDisplay
	energy  *agentreplv1.UpdatePersistentWifiModeEnergyMode
}

// holdQuietLocked holds the quiet state to quietWanted, entering or leaving
// it, and answers the display and energy steps' outcomes for an Update.
// energy is the energy mode in use as read (reading.energy), which the energy
// step holds to the state as it stands (holdEnergyLocked). With the state
// standing and the display awake (a sleep that failed), the sleep is tried
// again; the applications are not frozen again here, since the freezer
// freezes each process started while the state stands itself
// (frozenChanged). With the state not standing, a kept frozen set
// is thawed and a recorded energy mode restored, which is how a daemon
// booting with the lid open undoes what an earlier one left. trigger names
// what asked, for the record. Caller holds mu.
func (c *Controller) holdQuietLocked(ctx context.Context, mode modeKnown, energy, trigger string) quietSteps {
	want := quietWanted(c.lid.kind, mode, c.quiet)
	switch {
	case want && !c.quiet:
		return c.enterQuietLocked(ctx, mode, energy, trigger)
	case want:
		steps := quietSteps{display: displayUnchanged(), energy: c.holdEnergyLocked(ctx, energy, trigger)}
		if !c.displayAsleep {
			steps.display = c.sleepDisplayLocked(ctx, trigger)
		}
		return steps
	case c.quiet:
		return c.leaveQuietLocked(ctx, mode, energy, trigger)
	default:
		c.freezer.Thaw(ctx, trigger)
		return quietSteps{display: displayUnchanged(), energy: c.holdEnergyLocked(ctx, energy, trigger)}
	}
}

func (c *Controller) enterQuietLocked(ctx context.Context, mode modeKnown, energy, trigger string) quietSteps {
	c.quiet = true
	energyStep := c.lowerEnergyLocked(ctx, energy, trigger)
	c.views.Set(true)
	frozen := c.freezer.Freeze(ctx, trigger)
	display := c.sleepDisplayLocked(ctx, trigger)
	if c.telemetry != nil {
		c.telemetry.SetLidQuiet(true)
	}
	c.log.Info(opQuiet, "the lid closed with persistent wifi on; the machine entered the lid-closed quiet state", dlog.Context{
		"trigger": trigger, "lid": c.lid.name(), "mode": modeName(mode),
		"frozen": frozenNames(frozen), "display": displayArm(display), "energy": energyModeArm(energyStep)})
	return quietSteps{display: display, energy: energyStep}
}

func (c *Controller) leaveQuietLocked(ctx context.Context, mode modeKnown, energy, trigger string) quietSteps {
	// THE VIEWS LIFT FIRST, and a failover attempt in flight is ended: no
	// page view waits on the display's wake, and no network is joined once
	// the quiet state is left.
	c.views.Set(false)
	c.quiet = false
	c.abandonFailoverLocked()
	display := c.wakeDisplayLocked(ctx, trigger)
	thawed := c.freezer.Frozen()
	c.freezer.Thaw(ctx, trigger)
	energyStep := c.restoreEnergyLocked(ctx, energy, trigger, true)
	if c.telemetry != nil {
		c.telemetry.SetLidQuiet(false)
	}
	c.log.Info(opQuiet, "the machine left the lid-closed quiet state; every view was brought to its current state", dlog.Context{
		"trigger": trigger, "lid": c.lid.name(), "mode": modeName(mode),
		"thawed": frozenNames(thawed), "display": displayArm(display), "energy": energyModeArm(energyStep)})
	return quietSteps{display: display, energy: energyStep}
}

// stampQuiet sets the standing's quiet message from what the controller
// holds. Caller holds mu.
func (c *Controller) stampQuiet(state *agentreplv1.PersistentWifiState) {
	state.Quiet = nil
	if !c.quiet {
		return
	}
	q := &agentreplv1.PersistentWifiQuiet{}
	for _, f := range c.freezer.Frozen() {
		q.FrozenApps = append(q.FrozenApps, &agentreplv1.PersistentWifiFrozenApp{Name: f.Name, Processes: uint32(f.Processes)})
	}
	if c.displayAsleep {
		q.Display = &agentreplv1.PersistentWifiQuiet_DisplayAsleep{DisplayAsleep: &agentreplv1.PersistentWifiQuietDisplayAsleep{}}
	} else {
		q.Display = &agentreplv1.PersistentWifiQuiet_DisplayAwake{DisplayAwake: &agentreplv1.PersistentWifiQuietDisplayAwake{Detail: c.displayCause}}
	}
	state.Quiet = q
}

// frozenChanged republishes the standing after the freezer froze a process
// started while the state stands, so its frozen_apps are what is frozen now.
// The freezer calls it outside its own lock (AppFreezer).
func (c *Controller) frozenChanged() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.republishLocked()
}

// frozenNames states the frozen applications for a record.
func frozenNames(frozen []appfreeze.Frozen) map[string]int {
	out := map[string]int{}
	for _, f := range frozen {
		out[f.Name] = f.Processes
	}
	return out
}

// NoViews is a view gate nobody follows, for a controller whose daemon draws
// no views (a test of the controller alone).
func NoViews() *viewquiet.Gate { return viewquiet.New(nil) }
