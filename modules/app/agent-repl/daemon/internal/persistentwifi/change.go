package persistentwifi

import (
	"context"
	"fmt"
	"strings"
	"time"

	"agentrepl/logging/hostpower"
	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/usersetup"
)

// energyModeLowPower is `pmset -a powermode`'s Low Power Mode, which the
// lid-closed quiet state runs; leaving it restores whatever mode was in use
// before (energy.go). The sudoers grant names `powermode 0`, `1` and `2`.
const energyModeLowPower = hostpower.EnergyModeLowPower

// energyModeHighPower is High Power Mode, which the quiet state runs on AC
// power.
const energyModeHighPower = hostpower.EnergyModeHighPower

// powerSettings are the three `pmset -a` settings the mode writes, in order:
// sleep disabled outright, the network kept up over sleep, and wake on network
// access. The sudoers grant names each of these exact invocations.
var powerSettings = []string{"disablesleep", "networkoversleep", "womp"}

// errNoHotspot is THE ONE configuration fault of a host that named no hotspot:
// a user-setup error naming the variable, for every step that needs the
// hotspot.
func errNoHotspot() error {
	return usersetup.Errorf("persistentwifi: no hotspot is configured; set %s to the phone hotspot's name", EnvHotspot)
}

// noHotspotConfigured is the hotspot step of a host that named no hotspot. It
// is a configuration fault, recorded at ERROR and answered as a failed hotspot
// step whose detail names the variable and the user guide; the power step runs
// regardless, exactly as it does after any other failed hotspot step.
func (c *Controller) noHotspotConfigured(on bool) *agentreplv1.UpdatePersistentWifiModeHotspot {
	err := errNoHotspot()
	c.log.Error(opUpdate, "no hotspot is configured; the hotspot step was skipped and the power step runs regardless",
		dlog.Context{"on": on, "env": EnvHotspot, "cause": err.Error()})
	return &agentreplv1.UpdatePersistentWifiModeHotspot{Outcome: hotspotFailed("", err.Error())}
}

// hotspotJoinBound bounds one hotspot join (Host.JoinHotspot). Measured
// 2026-11-26 13:07:49-13:08:03 on this host, lid closed: the helper's ask of
// the phone, its scan for the hotspot and its join took 14s together; the
// bound is three times that. The helper bounds each of its steps inside it
// (wifi-helper/hotspot.swift's HotspotBounds).
const hotspotJoinBound = 42 * time.Second

// hotspotJoinMeasured is the join's measured healthy time hotspotJoinBound is
// sized from.
const hotspotJoinMeasured = "14s"

// hotspotStep is the ON half of the hotspot step's say: its outcome, what the
// host's join steps took, and whether hotspotJoinBound cut the join.
type hotspotStep struct {
	outcome *agentreplv1.UpdatePersistentWifiModeHotspot
	steps   dlog.Context
	cut     bool
}

// joinHotspot is the ON half of the hotspot step, bounded by
// hotspotJoinBound. A failure to join is an outcome, never an error: staying
// awake on whatever network is present is still what was asked for.
func (c *Controller) joinHotspot(ctx context.Context, before reading) hotspotStep {
	h := &agentreplv1.UpdatePersistentWifiModeHotspot{}
	step := hotspotStep{outcome: h}
	switch {
	case before.wifiErr != nil:
		h.Outcome = hotspotFailed(c.cfg.Hotspot, "the Wi-Fi interface could not be read: "+before.wifiErr.Error())
	case before.device == "":
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_NoWifiInterface{
			NoWifiInterface: &agentreplv1.UpdatePersistentWifiModeHotspotNoWifiInterface{},
		}
	case sameNetwork(before.link.name, c.cfg.Hotspot):
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_AlreadyJoined{
			AlreadyJoined: &agentreplv1.UpdatePersistentWifiModeHotspotAlreadyJoined{NetworkName: before.link.name},
		}
	default:
		bounded, cancel := context.WithTimeout(ctx, c.hotspotJoinBound)
		defer cancel()
		join, err := c.host.JoinHotspot(bounded, before.device, c.cfg.Hotspot)
		step.steps, step.cut = join.steps, bounded.Err() != nil && ctx.Err() == nil
		switch {
		case step.cut:
			h.Outcome = hotspotFailed(join.network, fmt.Sprintf("the hotspot join did not answer within %s (measured %s): %v",
				c.hotspotJoinBound, hotspotJoinMeasured, err))
		case err != nil:
			h.Outcome = hotspotFailed(join.network, err.Error())
		default:
			h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_Joined{
				Joined: &agentreplv1.UpdatePersistentWifiModeHotspotJoined{NetworkName: join.network},
			}
		}
	}
	return step
}

// apostrophes folds the apostrophe spellings a network name takes -- the ASCII
// one, and the curly right and left single quotation marks an iPhone and an
// autocorrecting keyboard write -- onto one, for comparison only.
var apostrophes = strings.NewReplacer("\u2019", "'", "\u2018", "'")

// sameNetwork is THE ONE comparison of two network names: equal once their
// apostrophe spellings are folded together. The join, the "already joined"
// check and the "still on the hotspot" check all ask it, so they cannot
// disagree about which network the hotspot is.
func sameNetwork(a, b string) bool {
	return a != "" && apostrophes.Replace(a) == apostrophes.Replace(b)
}

// withNote appends a note to a failure's detail, when there is one.
func withNote(detail, note string) string {
	if note == "" {
		return detail
	}
	return detail + " (" + note + ")"
}

// leaveHotspot is the OFF half of the hotspot step: disassociate with the
// radio left on, and if the machine is still on the hotspot after that (some
// macOS builds refuse the disassociate, or wifi-util is absent), restart the
// radio so auto-join picks a home network. Both are the owner's script's own
// steps.
func (c *Controller) leaveHotspot(ctx context.Context, before reading) *agentreplv1.UpdatePersistentWifiModeHotspot {
	h := &agentreplv1.UpdatePersistentWifiModeHotspot{}
	switch {
	case before.wifiErr != nil:
		h.Outcome = hotspotFailed(c.cfg.Hotspot, "the Wi-Fi interface could not be read: "+before.wifiErr.Error())
		return h
	case before.device == "":
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_NoWifiInterface{
			NoWifiInterface: &agentreplv1.UpdatePersistentWifiModeHotspotNoWifiInterface{},
		}
		return h
	case before.link.joined && before.link.name == "":
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_NetworkUnreadable{
			NetworkUnreadable: &agentreplv1.UpdatePersistentWifiModeHotspotNetworkUnreadable{},
		}
		return h
	case !sameNetwork(before.link.name, c.cfg.Hotspot):
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_NotOnHotspot{
			NotOnHotspot: &agentreplv1.UpdatePersistentWifiModeHotspotNotOnHotspot{},
		}
		return h
	}

	var attempts []string
	if why := c.host.CanDisconnect(); why == nil {
		if err := c.host.Disconnect(ctx, before.device); err != nil {
			attempts = append(attempts, err.Error())
		}
		if c.stillOnHotspot(ctx, before.device, &attempts) {
			c.restartRadio(ctx, before.device, &attempts)
		}
	} else {
		attempts = append(attempts, why.Error())
		c.restartRadio(ctx, before.device, &attempts)
	}
	if c.stillOnHotspot(ctx, before.device, &attempts) {
		h.Outcome = hotspotFailed(c.cfg.Hotspot,
			"still joined after a disconnect and a radio restart: "+strings.Join(attempts, "; "))
		return h
	}
	h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_Left{
		Left: &agentreplv1.UpdatePersistentWifiModeHotspotLeft{NetworkName: c.cfg.Hotspot},
	}
	return h
}

// restartRadio powers the Wi-Fi radio off and on, recording a failed step.
func (c *Controller) restartRadio(ctx context.Context, device string, attempts *[]string) {
	for _, on := range []bool{false, true} {
		if err := c.host.SetRadio(ctx, device, on); err != nil {
			*attempts = append(*attempts, err.Error())
		}
	}
}

// stillOnHotspot re-reads the link and answers whether it still names the
// hotspot. A link that cannot be re-read is recorded and answered as still on
// it: leaving cannot be claimed without seeing it.
func (c *Controller) stillOnHotspot(ctx context.Context, device string, attempts *[]string) bool {
	l, _, err := c.host.ReadLink(ctx, device)
	if err != nil {
		*attempts = append(*attempts, err.Error())
		return true
	}
	return sameNetwork(l.name, c.cfg.Hotspot)
}

// hotspotFailed is the failed arm.
func hotspotFailed(name, detail string) *agentreplv1.UpdatePersistentWifiModeHotspot_Failed {
	return &agentreplv1.UpdatePersistentWifiModeHotspot_Failed{
		Failed: &agentreplv1.UpdatePersistentWifiModeHotspotFailed{NetworkName: name, Detail: detail},
	}
}

// pmsetWrite is THE ONE privileged power write: `sudo -n pmset -a <setting>
// <value>`, the exact shape every line of the sudoers grant names. The error
// names the invocation.
func pmsetWrite(ctx context.Context, r Runner, tools Tools, setting, value string) error {
	if _, err := run(ctx, r, tools.Sudo, "-n", tools.Pmset, "-a", setting, value); err != nil {
		return fmt.Errorf("pmset -a %s %s: %w", setting, value, err)
	}
	return nil
}
