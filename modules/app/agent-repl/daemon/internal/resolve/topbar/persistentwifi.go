package topbar

import (
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

// persistentWifiChip projects the standing onto the chip, arm for arm, and
// composes its tooltip from both facts. A nil standing is two facts nobody
// read, drawn as unknown.
func persistentWifiChip(state *agentreplv1.PersistentWifiState) *frontendv1.TopbarPersistentWifi {
	chip := &frontendv1.TopbarPersistentWifi{}
	var wifi, mode string
	switch {
	case state.GetJoined() != nil:
		chip.Wifi = &frontendv1.TopbarPersistentWifi_Joined{Joined: &frontendv1.TopbarPersistentWifiJoined{}}
		if name := state.GetJoined().NetworkName; name != nil {
			wifi = "Wi-Fi: joined to " + *name
		} else {
			wifi = "Wi-Fi: joined (network name withheld by macOS)"
		}
	case state.GetNotJoined() != nil:
		chip.Wifi = &frontendv1.TopbarPersistentWifi_NotJoined{NotJoined: &frontendv1.TopbarPersistentWifiNotJoined{}}
		wifi = "Wi-Fi: not joined"
	default:
		wifi = "Wi-Fi: could not be read"
	}
	switch {
	case state.GetOn() != nil:
		chip.Mode = &frontendv1.TopbarPersistentWifi_On{On: &frontendv1.TopbarPersistentWifiModeOn{}}
		mode = "persistent wifi on: the lid can close"
	case state.GetOff() != nil:
		chip.Mode = &frontendv1.TopbarPersistentWifi_Off{Off: &frontendv1.TopbarPersistentWifiModeOff{}}
		mode = "persistent wifi off: closing the lid sleeps"
	default:
		mode = "persistent wifi: could not be read"
	}
	chip.Tooltip = &frontendv1.TopbarPersistentWifiTooltip{Text: wifi + " · " + mode}
	return chip
}
