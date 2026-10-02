package topbar

import (
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// SetPersistentWifi states the machine's persistent-wifi standing on every
// strip, present and future. It is a daemon-scoped fact like a daemon warning:
// the resolver holds the one standing and every workspace's view projects it.
func (r *resolver) SetPersistentWifi(state *agentreplv1.PersistentWifiState) {
	chip := persistentWifiChip(state)
	r.eachStrip("daemon.topbar.persistent_wifi", "the topbar took the persistent-wifi standing on every strip",
		dlog.Context{"tooltip": chip.GetTooltip().GetText()},
		func() { r.persistentWifi = state },
		func(*wsState) {})
}

// persistentWifiTooltip is the chip's hover text (owner ruling, 2026-10-02).
const persistentWifiTooltip = "Closing laptop lid disables agents"

// persistentWifiChip projects the standing onto the chip, arm for arm. A nil
// standing is two facts nobody read, drawn as unknown.
//
// THE TOOLTIP IS THE OWNER'S ONE SENTENCE. The joined network and the mode are
// already drawn by the chip's own arms (the glyph's color and its disc), so
// the tooltip no longer restates them. A fact the daemon COULD NOT READ is the
// one thing the arms cannot say -- an unassigned arm draws as muted, which the
// contract (topbar.proto, TopbarPersistentWifi) promises the tooltip explains
// -- so only then does a second sentence name what could not be read.
func persistentWifiChip(state *agentreplv1.PersistentWifiState) *frontendv1.TopbarPersistentWifi {
	chip := &frontendv1.TopbarPersistentWifi{}
	var unread []string
	switch {
	case state.GetJoined() != nil:
		chip.Wifi = &frontendv1.TopbarPersistentWifi_Joined{Joined: &frontendv1.TopbarPersistentWifiJoined{}}
	case state.GetNotJoined() != nil:
		chip.Wifi = &frontendv1.TopbarPersistentWifi_NotJoined{NotJoined: &frontendv1.TopbarPersistentWifiNotJoined{}}
	default:
		unread = append(unread, "Wi-Fi")
	}
	switch {
	case state.GetOn() != nil:
		chip.Mode = &frontendv1.TopbarPersistentWifi_On{On: &frontendv1.TopbarPersistentWifiModeOn{}}
	case state.GetOff() != nil:
		chip.Mode = &frontendv1.TopbarPersistentWifi_Off{Off: &frontendv1.TopbarPersistentWifiModeOff{}}
	default:
		unread = append(unread, "persistent wifi mode")
	}
	text := persistentWifiTooltip
	if len(unread) > 0 {
		subject := strings.Join(unread, " and ")
		text += ". " + strings.ToUpper(subject[:1]) + subject[1:] + " could not be read."
	}
	chip.Tooltip = &frontendv1.TopbarPersistentWifiTooltip{Text: text}
	return chip
}
