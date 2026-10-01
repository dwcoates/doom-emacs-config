package persistentwifi

import (
	"context"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// Brightness levels the display step sets: dimmed to the second gradation
// above black when the mode goes on, full when it goes off.
const (
	brightnessDimmed   = "0.0625"
	brightnessRestored = "1.0"
)

// powerSettings are the three `pmset -a` settings the mode writes, in order:
// sleep disabled outright, the network kept up over sleep, and wake on network
// access. The sudoers grant names each of these exact invocations.
var powerSettings = []string{"disablesleep", "networkoversleep", "womp"}

// joinHotspot is the ON half of the hotspot step. A failure to join is an
// outcome, never an error: staying awake on whatever network is present is
// still what was asked for.
func (c *Controller) joinHotspot(ctx context.Context, before reading) *agentreplv1.UpdatePersistentWifiModeHotspot {
	h := &agentreplv1.UpdatePersistentWifiModeHotspot{}
	switch {
	case before.wifiErr != nil:
		h.Outcome = hotspotFailed(c.cfg.Hotspot, "the Wi-Fi interface could not be read: "+before.wifiErr.Error())
	case before.device == "":
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_NoWifiInterface{
			NoWifiInterface: &agentreplv1.UpdatePersistentWifiModeHotspotNoWifiInterface{},
		}
	case before.link.name == c.cfg.Hotspot:
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_AlreadyJoined{
			AlreadyJoined: &agentreplv1.UpdatePersistentWifiModeHotspotAlreadyJoined{NetworkName: c.cfg.Hotspot},
		}
	default:
		// `networksetup -setairportnetwork` prints nothing on success and a
		// sentence on failure, whatever its exit code, so both are read.
		out, err := run(ctx, c.runner, c.cfg.Tools.Networksetup, "-setairportnetwork", before.device, c.cfg.Hotspot)
		switch {
		case err != nil:
			h.Outcome = hotspotFailed(c.cfg.Hotspot, err.Error())
		case strings.TrimSpace(out) != "":
			h.Outcome = hotspotFailed(c.cfg.Hotspot, strings.TrimSpace(out))
		default:
			h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_Joined{
				Joined: &agentreplv1.UpdatePersistentWifiModeHotspotJoined{NetworkName: c.cfg.Hotspot},
			}
		}
	}
	return h
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
	case before.link.name != c.cfg.Hotspot:
		h.Outcome = &agentreplv1.UpdatePersistentWifiModeHotspot_NotOnHotspot{
			NotOnHotspot: &agentreplv1.UpdatePersistentWifiModeHotspotNotOnHotspot{},
		}
		return h
	}

	var attempts []string
	if c.exists(c.cfg.Tools.WifiUtil) {
		if _, err := run(ctx, c.runner, c.cfg.Tools.WifiUtil, "disconnect"); err != nil {
			attempts = append(attempts, err.Error())
		}
		if c.stillOnHotspot(ctx, before.device, &attempts) {
			c.restartRadio(ctx, before.device, &attempts)
		}
	} else {
		attempts = append(attempts, c.cfg.Tools.WifiUtil+" is not installed")
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
	for _, power := range []string{"off", "on"} {
		if _, err := run(ctx, c.runner, c.cfg.Tools.Networksetup, "-setairportpower", device, power); err != nil {
			*attempts = append(*attempts, err.Error())
		}
	}
}

// stillOnHotspot re-reads the link and answers whether it still names the
// hotspot. A link that cannot be re-read is recorded and answered as still on
// it: leaving cannot be claimed without seeing it.
func (c *Controller) stillOnHotspot(ctx context.Context, device string, attempts *[]string) bool {
	summary, err := run(ctx, c.runner, c.cfg.Tools.Ipconfig, "getsummary", device)
	if err != nil {
		*attempts = append(*attempts, err.Error())
		return true
	}
	return parseSummary(summary).name == c.cfg.Hotspot
}

// hotspotFailed is the failed arm.
func hotspotFailed(name, detail string) *agentreplv1.UpdatePersistentWifiModeHotspot_Failed {
	return &agentreplv1.UpdatePersistentWifiModeHotspot_Failed{
		Failed: &agentreplv1.UpdatePersistentWifiModeHotspotFailed{NetworkName: name, Detail: detail},
	}
}

// applyPower writes the three power settings through `sudo -n`, stopping at
// the first refusal: the mode is then whatever it was.
func (c *Controller) applyPower(ctx context.Context, on bool) error {
	value := "0"
	if on {
		value = "1"
	}
	for _, setting := range powerSettings {
		if _, err := run(ctx, c.runner, c.cfg.Tools.Sudo, "-n", c.cfg.Tools.Pmset, "-a", setting, value); err != nil {
			return fmt.Errorf("pmset -a %s %s: %w", setting, value, err)
		}
	}
	return nil
}

// setDisplay is the display step. Cosmetic: no outcome of it fails the
// request.
func (c *Controller) setDisplay(ctx context.Context, on bool) *agentreplv1.UpdatePersistentWifiModeDisplay {
	d := &agentreplv1.UpdatePersistentWifiModeDisplay{}
	if !c.exists(c.cfg.Tools.Brightness) {
		d.Outcome = &agentreplv1.UpdatePersistentWifiModeDisplay_ToolMissing{
			ToolMissing: &agentreplv1.UpdatePersistentWifiModeDisplayToolMissing{ToolPath: c.cfg.Tools.Brightness},
		}
		return d
	}
	level := brightnessRestored
	if on {
		level = brightnessDimmed
	}
	if _, err := run(ctx, c.runner, c.cfg.Tools.Brightness, level); err != nil {
		d.Outcome = &agentreplv1.UpdatePersistentWifiModeDisplay_Failed{
			Failed: &agentreplv1.UpdatePersistentWifiModeDisplayFailed{Detail: err.Error()},
		}
		return d
	}
	if on {
		d.Outcome = &agentreplv1.UpdatePersistentWifiModeDisplay_Dimmed{Dimmed: &agentreplv1.UpdatePersistentWifiModeDisplayDimmed{}}
	} else {
		d.Outcome = &agentreplv1.UpdatePersistentWifiModeDisplay_Restored{Restored: &agentreplv1.UpdatePersistentWifiModeDisplayRestored{}}
	}
	return d
}
