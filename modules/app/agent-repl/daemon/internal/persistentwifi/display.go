package persistentwifi

import (
	"context"
	"fmt"
	"math"
	"strconv"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opDisplay = "daemon.persistentwifi.display"

// brightnessLidClosed is the level a closed lid dims the display to.
const brightnessLidClosed = 0.0

// LidDims keeps the pre-close brightness DURABLY: the level the display had
// the moment the lid closed with the mode on, and the ONE source every
// restore reads. Its lifetime is explicit: put when the lid-close dim is
// taken, cleared when the display is restored. Durable because a daemon that
// restarts while the lid is closed and dimmed must still know what to restore
// when it opens. wsm's store is the production one.
type LidDims interface {
	// LidDimLevel answers the kept level, and false when no dim is in force.
	LidDimLevel(ctx context.Context) (float64, bool, error)
	// PutLidDimLevel keeps level as the one to restore.
	PutLidDimLevel(ctx context.Context, level float64) error
	// ClearLidDimLevel forgets the kept level.
	ClearLidDimLevel(ctx context.Context) error
}

// displayMove is what the display must do.
type displayMove int

const (
	moveNone displayMove = iota
	moveDim
	moveRestore
)

func (m displayMove) String() string {
	switch m {
	case moveDim:
		return "dim"
	case moveRestore:
		return "restore"
	}
	return "none"
}

// modeKnown is the mode as the display decision reads it.
type modeKnown int

const (
	modeUnknown modeKnown = iota
	modeIsOn
	modeIsOff
)

// modeOf reads the mode out of a standing.
func modeOf(s *agentreplv1.PersistentWifiState) modeKnown {
	switch {
	case s.GetOn() != nil:
		return modeIsOn
	case s.GetOff() != nil:
		return modeIsOff
	}
	return modeUnknown
}

// displayMoveFor is THE ONE DECISION of what the display does, asked by the
// lid's changes and by every mode change alike:
//   - a dim in force is restored once the lid is open (or the machine has
//     none) or the mode is off;
//   - with no dim in force, a closed lid with the mode on is dimmed;
//   - everything else leaves the user's brightness alone, including an
//     unknown mode or a lid nobody hears.
func displayMoveFor(lid lidKind, mode modeKnown, dimmed bool) displayMove {
	switch {
	case dimmed && (lid == lidOpen || lid == lidNone || mode == modeIsOff):
		return moveRestore
	case !dimmed && lid == lidClosed && mode == modeIsOn:
		return moveDim
	}
	return moveNone
}

// loadDimLocked reads the kept level into the controller's mirror once. A
// read that fails is recorded at ERROR and answered false: the dim in force
// is then unknown, and no display move is made until a later read succeeds.
// trigger names what asked, for the record. Caller holds mu.
func (c *Controller) loadDimLocked(ctx context.Context, trigger string) bool {
	if c.dimLoaded {
		return true
	}
	level, ok, err := c.dims.LidDimLevel(ctx)
	if err != nil {
		c.log.Error(opDisplay, "the kept pre-close brightness could not be read; the display is not moved until it can be",
			dlog.Context{"trigger": trigger, "lid": c.lid.name(), "cause": err.Error()})
		return false
	}
	c.dim = nil
	if ok {
		c.dim = &level
	}
	c.dimLoaded = true
	return true
}

// moveDisplayLocked is THE DISPLAY STEP: decide from the lid, the mode and
// the dim in force, then dim or restore. Every failure is recorded here, once,
// at ERROR, and answered as the failed (or tool_missing) arm. trigger names
// what asked, for the record. Caller holds mu.
func (c *Controller) moveDisplayLocked(ctx context.Context, mode modeKnown, trigger string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	if !c.loadDimLocked(ctx, trigger) {
		return displayFailed("the kept pre-close brightness could not be read")
	}
	move := displayMoveFor(c.lid.kind, mode, c.dim != nil)
	fields := dlog.Context{"trigger": trigger, "move": move.String(), "lid": c.lid.name(), "mode": modeName(mode)}
	if move == moveNone {
		return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Unchanged{
			Unchanged: &agentreplv1.UpdatePersistentWifiModeDisplayUnchanged{},
		}}
	}
	if !c.exists(c.cfg.Tools.Brightness) {
		fields["tool"] = c.cfg.Tools.Brightness
		c.log.Error(opDisplay, "the display had to move and the brightness tool is not installed", fields)
		return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_ToolMissing{
			ToolMissing: &agentreplv1.UpdatePersistentWifiModeDisplayToolMissing{ToolPath: c.cfg.Tools.Brightness},
		}}
	}
	if move == moveDim {
		return c.dimLocked(ctx, fields)
	}
	return c.restoreLocked(ctx, fields)
}

// dimLocked keeps the display's level, then dims it. Nothing is dimmed
// without a kept level to come back to: a level that cannot be read or kept
// leaves the display as it is. Caller holds mu.
func (c *Controller) dimLocked(ctx context.Context, fields dlog.Context) *agentreplv1.UpdatePersistentWifiModeDisplay {
	level, err := c.readBrightness(ctx)
	if err != nil {
		return c.displayFailedLocked("the display's brightness could not be read, so it was not dimmed", err, fields)
	}
	fields["restore_level"] = level
	if err := c.dims.PutLidDimLevel(ctx, level); err != nil {
		return c.displayFailedLocked("the pre-close brightness could not be kept, so the display was not dimmed", err, fields)
	}
	if err := c.setBrightness(ctx, brightnessLidClosed); err != nil {
		// The display was not dimmed, so no dim is in force: forget the level.
		if clearErr := c.dims.ClearLidDimLevel(ctx); clearErr != nil {
			fields["clear_cause"] = clearErr.Error()
			c.dim = &level
		}
		return c.displayFailedLocked("the display could not be dimmed", err, fields)
	}
	c.dim = &level
	c.log.Info(opDisplay, "the display was dimmed for the closed lid", fields)
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Dimmed{
		Dimmed: &agentreplv1.UpdatePersistentWifiModeDisplayDimmed{RestoreLevel: level},
	}}
}

// restoreLocked returns the display to the kept level and forgets it. A
// restore that fails keeps the level, so the next move tries it again. Caller
// holds mu.
func (c *Controller) restoreLocked(ctx context.Context, fields dlog.Context) *agentreplv1.UpdatePersistentWifiModeDisplay {
	level := *c.dim
	fields["restore_level"] = level
	if err := c.setBrightness(ctx, level); err != nil {
		return c.displayFailedLocked("the display could not be restored; the kept level stands", err, fields)
	}
	// The display IS restored, so no dim is in force whatever the store says:
	// a row that could not be cleared is a level the next restore re-applies.
	c.dim = nil
	if err := c.dims.ClearLidDimLevel(ctx); err != nil {
		return c.displayFailedLocked(fmt.Sprintf("the display was restored to %s, but the kept level could not be cleared", formatLevel(level)), err, fields)
	}
	c.log.Info(opDisplay, "the display was restored to its pre-close brightness", fields)
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Restored{
		Restored: &agentreplv1.UpdatePersistentWifiModeDisplayRestored{Level: level},
	}}
}

// displayFailedLocked records a failed display step and answers its arm.
func (c *Controller) displayFailedLocked(what string, err error, fields dlog.Context) *agentreplv1.UpdatePersistentWifiModeDisplay {
	fields["cause"] = err.Error()
	c.log.Error(opDisplay, what, fields)
	return displayFailed(what + ": " + err.Error())
}

func displayFailed(detail string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Failed{
		Failed: &agentreplv1.UpdatePersistentWifiModeDisplayFailed{Detail: detail},
	}}
}

// readBrightness runs the brightness tool with no argument, which prints the
// built-in display's level (0.0 to 1.0).
func (c *Controller) readBrightness(ctx context.Context) (float64, error) {
	out, err := run(ctx, c.runner, c.cfg.Tools.Brightness)
	if err != nil {
		return 0, err
	}
	return parseBrightness(out)
}

// parseBrightness reads the tool's printed level.
func parseBrightness(out string) (float64, error) {
	level, err := strconv.ParseFloat(strings.TrimSpace(out), 64)
	if err != nil || math.IsNaN(level) || level < 0 || level > 1 {
		return 0, fmt.Errorf("the brightness tool printed %q, which is no level between 0 and 1", strings.TrimSpace(out))
	}
	return level, nil
}

// setBrightness sets the built-in display's level.
func (c *Controller) setBrightness(ctx context.Context, level float64) error {
	_, err := run(ctx, c.runner, c.cfg.Tools.Brightness, formatLevel(level))
	return err
}

// formatLevel spells a level as the tool takes it: the shortest decimal.
func formatLevel(level float64) string { return strconv.FormatFloat(level, 'f', -1, 64) }

func modeName(m modeKnown) string {
	switch m {
	case modeIsOn:
		return "on"
	case modeIsOff:
		return "off"
	}
	return "unknown"
}
