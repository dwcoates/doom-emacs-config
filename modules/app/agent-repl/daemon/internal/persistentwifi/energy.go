package persistentwifi

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

const opEnergy = "daemon.persistentwifi.energy"

// energyModeFor is THE ONE DECISION of the energy mode, asked by every mode
// change and by every beat and lid change alike: High Power Mode when the mode
// is off, and with it on, High Power Mode only while the lid is closed on AC
// power (a closed machine plugged in has the power to spare, and does its work
// there); Low Power Mode otherwise. The power source is read only when the
// answer depends on it, so an open lid never runs a tool for it.
func (c *Controller) energyModeFor(ctx context.Context, on bool) (string, error) {
	if !on {
		return energyModeHighPower, nil
	}
	if c.lid.kind != lidClosed {
		return energyModeLowPower, nil
	}
	out, err := run(ctx, c.runner, c.cfg.Tools.Pmset, "-g", "ps")
	if err != nil {
		return "", fmt.Errorf("the power source could not be read: %w", err)
	}
	onAC, ok := parsePowerSource(out)
	if !ok {
		return "", fmt.Errorf("%s -g ps printed no power source line", c.cfg.Tools.Pmset)
	}
	if onAC {
		return energyModeHighPower, nil
	}
	return energyModeLowPower, nil
}

// reconcileEnergyLocked holds the energy mode in use to energyModeFor's answer
// while the mode is on: a closed lid plugged in moves to High Power Mode, and
// unplugging it or opening the lid moves back to Low Power Mode. It acts on the
// state read (rd.energy), never on what it last wrote, so a restart or an
// outside change is corrected the same way. A machine that names no energy mode
// has none to move. Every failure is recorded once per distinct cause
// (energyCause) and retried on the next beat. trigger names what asked, for the
// record. Caller holds mu.
func (c *Controller) reconcileEnergyLocked(ctx context.Context, rd reading, trigger string) {
	if modeOf(rd.state) != modeIsOn {
		c.energyCause = ""
		return
	}
	if rd.energy == "" {
		c.log.Debug(opEnergy, "the machine names no energy mode; none is moved", dlog.Context{"trigger": trigger})
		return
	}
	want, err := c.energyModeFor(ctx, true)
	if err == nil && want != rd.energy {
		err = c.pmsetAll(ctx, "powermode", want)
		if err == nil {
			c.log.Info(opEnergy, "the energy mode followed the lid and the power source",
				dlog.Context{"trigger": trigger, "lid": c.lid.name(), "was": rd.energy, "now": want})
		}
	}
	if err == nil {
		c.energyCause = ""
		return
	}
	if ctx.Err() != nil {
		return
	}
	if cause := err.Error(); cause != c.energyCause {
		c.log.Error(opEnergy, "the energy mode could not be moved to follow the lid and the power source; it is tried again on the next beat",
			dlog.Context{"trigger": trigger, "lid": c.lid.name(), "energy": rd.energy, "cause": cause})
		c.energyCause = cause
	}
}
