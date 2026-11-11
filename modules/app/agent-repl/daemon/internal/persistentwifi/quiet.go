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
//  1. the views stop (Views): the daemon resolves and publishes no view for a
//     local page, and the pages and Emacs idle on the standing's push;
//  2. the configured applications are frozen (Freezer);
//  3. the display sleeps.
//
// Leaving it undoes the three in reverse, so the display is awake before the
// applications move and the views resync last, at the state everything left.
// The decision reads STATES, never transitions, so a restart, a handover or a
// repeated lid report reaches the same answer: a frozen set kept by an
// earlier daemon is adopted while the state stands and thawed the moment it
// does not.

// AppFreezer freezes and thaws the configured applications (appfreeze).
type AppFreezer interface {
	Freeze(ctx context.Context, trigger string) []appfreeze.Frozen
	Thaw(ctx context.Context, trigger string)
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

// holdQuietLocked holds the quiet state to quietWanted, entering or leaving
// it, and answers the display step's outcome for an Update. With the state
// standing and the display awake (a sleep that failed), the sleep is tried
// again; the applications are not frozen again, since one launched with the
// lid closed is never chased. With the state not standing, a kept frozen set
// is thawed, which is how a daemon booting with the lid open thaws what an
// earlier one froze. trigger names what asked, for the record. Caller holds
// mu.
func (c *Controller) holdQuietLocked(ctx context.Context, mode modeKnown, trigger string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	want := quietWanted(c.lid.kind, mode, c.quiet)
	switch {
	case want && !c.quiet:
		return c.enterQuietLocked(ctx, mode, trigger)
	case want:
		if c.displayAsleep {
			return displayUnchanged()
		}
		return c.sleepDisplayLocked(ctx, trigger)
	case c.quiet:
		return c.leaveQuietLocked(ctx, mode, trigger)
	default:
		c.freezer.Thaw(ctx, trigger)
		return displayUnchanged()
	}
}

func (c *Controller) enterQuietLocked(ctx context.Context, mode modeKnown, trigger string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	c.views.Set(true)
	c.frozen = c.freezer.Freeze(ctx, trigger)
	display := c.sleepDisplayLocked(ctx, trigger)
	c.quiet = true
	if c.telemetry != nil {
		c.telemetry.SetLidQuiet(true)
	}
	c.log.Info(opQuiet, "the lid closed with persistent wifi on; the machine entered the lid-closed quiet state", dlog.Context{
		"trigger": trigger, "lid": c.lid.name(), "mode": modeName(mode),
		"frozen": frozenNames(c.frozen), "display": displayArm(display)})
	return display
}

func (c *Controller) leaveQuietLocked(ctx context.Context, mode modeKnown, trigger string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	display := c.wakeDisplayLocked(ctx, trigger)
	thawed := c.frozen
	c.freezer.Thaw(ctx, trigger)
	c.frozen = nil
	c.views.Set(false)
	c.quiet = false
	if c.telemetry != nil {
		c.telemetry.SetLidQuiet(false)
	}
	c.log.Info(opQuiet, "the machine left the lid-closed quiet state; every view was brought to its current state", dlog.Context{
		"trigger": trigger, "lid": c.lid.name(), "mode": modeName(mode),
		"thawed": frozenNames(thawed), "display": displayArm(display)})
	return display
}

// stampQuiet sets the standing's quiet message from what the controller
// holds. Caller holds mu.
func (c *Controller) stampQuiet(state *agentreplv1.PersistentWifiState) {
	state.Quiet = nil
	if !c.quiet {
		return
	}
	q := &agentreplv1.PersistentWifiQuiet{}
	for _, f := range c.frozen {
		q.FrozenApps = append(q.FrozenApps, &agentreplv1.PersistentWifiFrozenApp{Name: f.Name, Processes: uint32(f.Processes)})
	}
	if c.displayAsleep {
		q.Display = &agentreplv1.PersistentWifiQuiet_DisplayAsleep{DisplayAsleep: &agentreplv1.PersistentWifiQuietDisplayAsleep{}}
	} else {
		q.Display = &agentreplv1.PersistentWifiQuiet_DisplayAwake{DisplayAwake: &agentreplv1.PersistentWifiQuietDisplayAwake{Detail: c.displayCause}}
	}
	state.Quiet = q
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
