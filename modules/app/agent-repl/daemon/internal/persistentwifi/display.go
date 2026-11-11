package persistentwifi

import (
	"context"
	"strconv"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

const opDisplay = "daemon.persistentwifi.display"

// LegacyLidDims is the pre-close brightness an earlier build kept when it
// dimmed the display to 0 for a closed lid (wsm's persistent_wifi_lid_dim
// row). The display now sleeps instead and keeps its brightness, so nothing
// writes the row again: a row left in force is read once at boot, the
// display is set back to it, and the row is cleared (restoreLegacyDimLocked).
type LegacyLidDims interface {
	// LidDimLevel answers the kept level, and false when no dim is in force.
	LidDimLevel(ctx context.Context) (float64, bool, error)
	// ClearLidDimLevel forgets the kept level.
	ClearLidDimLevel(ctx context.Context) error
}

// modeKnown is the mode as the quiet decision reads it.
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

func modeName(m modeKnown) string {
	switch m {
	case modeIsOn:
		return "on"
	case modeIsOff:
		return "off"
	}
	return "unknown"
}

// sleepDisplayLocked puts the built-in display to sleep (`pmset
// displaysleepnow`, which needs no sudo), so nothing is composited for a
// screen nobody sees. Sleeping never touches the brightness, so the display
// wakes at the level it had. A failure is recorded at ERROR and answered as
// the failed arm; the standing then says the display is awake, and the next
// lid or mode change tries again. Caller holds mu.
func (c *Controller) sleepDisplayLocked(ctx context.Context, trigger string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	if _, err := run(ctx, c.runner, c.cfg.Tools.Pmset, "displaysleepnow"); err != nil {
		c.displayCause = err.Error()
		c.log.Error(opDisplay, "the display could not be put to sleep for the closed lid; it stays awake", dlog.Context{
			"trigger": trigger, "tool": c.cfg.Tools.Pmset, "cause": err.Error()})
		return displayFailed(err.Error())
	}
	c.displayAsleep, c.displayCause = true, ""
	c.log.Info(opDisplay, "the display was put to sleep for the closed lid", dlog.Context{"trigger": trigger})
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Slept{
		Slept: &agentreplv1.UpdatePersistentWifiModeDisplaySlept{},
	}}
}

// wakeDisplayLocked wakes a display the quiet state put to sleep, declaring
// user activity (`caffeinate -u -t 1`). Opening the lid wakes it as well, so
// this is what wakes it when the mode is turned off with the lid closed.
// Caller holds mu.
func (c *Controller) wakeDisplayLocked(ctx context.Context, trigger string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	if !c.displayAsleep {
		c.displayCause = ""
		return displayUnchanged()
	}
	if _, err := run(ctx, c.runner, c.cfg.Tools.Caffeinate, "-u", "-t", "1"); err != nil {
		c.log.Error(opDisplay, "the display could not be woken from the quiet state's sleep", dlog.Context{
			"trigger": trigger, "tool": c.cfg.Tools.Caffeinate, "cause": err.Error()})
		c.displayAsleep = false
		return displayFailed(err.Error())
	}
	c.displayAsleep = false
	c.log.Info(opDisplay, "the display was woken from the quiet state's sleep", dlog.Context{"trigger": trigger})
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Woke{
		Woke: &agentreplv1.UpdatePersistentWifiModeDisplayWoke{},
	}}
}

// restoreLegacyDimLocked undoes a lid dim an earlier build left in force:
// the display is set back to the kept level and the level is forgotten. A
// level that cannot be read, set or cleared is recorded at ERROR and kept, so
// the next boot tries again. Caller holds mu.
func (c *Controller) restoreLegacyDimLocked(ctx context.Context) {
	level, ok, err := c.legacy.LidDimLevel(ctx)
	if err != nil {
		c.log.Error(opDisplay, "a lid dim an earlier build kept could not be read; the display is left as it is", dlog.Context{"cause": err.Error()})
		return
	}
	if !ok {
		return
	}
	fields := dlog.Context{"restore_level": level, "tool": c.cfg.Tools.Brightness}
	if !c.exists(c.cfg.Tools.Brightness) {
		c.log.Error(opDisplay, "a lid dim an earlier build left in force cannot be undone: the brightness tool is not installed; set the brightness by hand", fields)
		return
	}
	if _, err := run(ctx, c.runner, c.cfg.Tools.Brightness, formatLevel(level)); err != nil {
		fields["cause"] = err.Error()
		c.log.Error(opDisplay, "a lid dim an earlier build left in force could not be undone; it is tried again at the next boot", fields)
		return
	}
	if err := c.legacy.ClearLidDimLevel(ctx); err != nil {
		fields["cause"] = err.Error()
		c.log.Error(opDisplay, "the display was set back from an earlier build's lid dim, but the kept level could not be cleared", fields)
		return
	}
	c.log.Info(opDisplay, "a lid dim an earlier build left in force was undone; the display sleeps for a closed lid from now on", fields)
}

func displayFailed(detail string) *agentreplv1.UpdatePersistentWifiModeDisplay {
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Failed{
		Failed: &agentreplv1.UpdatePersistentWifiModeDisplayFailed{Detail: detail},
	}}
}

func displayUnchanged() *agentreplv1.UpdatePersistentWifiModeDisplay {
	return &agentreplv1.UpdatePersistentWifiModeDisplay{Outcome: &agentreplv1.UpdatePersistentWifiModeDisplay_Unchanged{
		Unchanged: &agentreplv1.UpdatePersistentWifiModeDisplayUnchanged{},
	}}
}

// formatLevel spells a level as the brightness tool takes it: the shortest
// decimal.
func formatLevel(level float64) string { return strconv.FormatFloat(level, 'f', -1, 64) }
