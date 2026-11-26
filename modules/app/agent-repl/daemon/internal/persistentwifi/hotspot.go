package persistentwifi

import (
	"errors"
	"fmt"
	"strings"
)

// hotspotAnswer is the Wi-Fi helper's hotspot verb's answer
// (wifi-helper/main.swift's Answer, its hotspot field hotspot.swift's
// HotspotAnswer). It names the network only: the hotspot's password never
// leaves the helper.
type hotspotAnswer struct {
	// Authorization is the helper's Location Services standing, as a scan's
	// (wifiScan.Authorization).
	Authorization string `json:"authorization"`
	// Error is why the helper could not try the join at all.
	Error   string             `json:"error"`
	Hotspot *helperHotspotJoin `json:"hotspot"`
}

// helperHotspotJoin is where the helper's join ended (hotspot.swift's
// HotspotStage), the network it joined or tried, the failure's detail, the
// phones sharingd heard when none was the hotspot's, and each step's time.
type helperHotspotJoin struct {
	Stage       string   `json:"stage"`
	Network     string   `json:"network"`
	Error       string   `json:"error"`
	PhonesHeard []string `json:"phones_heard"`
	// JoinedBy is "macos" when macOS's own auto-join took the hotspot before
	// the helper's join.
	JoinedBy string `json:"joined_by"`
	BrowseMs *int64 `json:"browse_ms"`
	EnableMs *int64 `json:"enable_ms"`
	ScanMs   *int64 `json:"scan_ms"`
	JoinMs   *int64 `json:"join_ms"`
}

// hotspotJoined is the stage of a join that took.
const hotspotJoined = "joined"

// hotspotJoinFailed is why the helper's join of HOTSPOT did not take, in words
// naming the stage it stopped at: the record and the hotspot step's failed
// outcome both carry it.
func hotspotJoinFailed(hotspot string, answer hotspotAnswer) error {
	j := answer.Hotspot
	var why string
	switch j.Stage {
	case "browse_failed":
		why = "sharingd could not be asked which phones it hears: " + j.Error
	case "no_phone":
		heard := "none"
		if len(j.PhonesHeard) > 0 {
			heard = strings.Join(j.PhonesHeard, ", ")
		}
		why = fmt.Sprintf("sharingd heard no phone named %s over Bluetooth (heard: %s)", hotspot, heard)
	case "enable_failed":
		why = j.Error
	case "not_visible":
		why = fmt.Sprintf("the phone started its hotspot, but %s was not in range: %s", j.Network, j.Error)
	case "join_failed":
		why = fmt.Sprintf("%s could not be joined: %s", j.Network, j.Error)
	default:
		why = fmt.Sprintf("the Wi-Fi helper named a stage this daemon does not know (%q): %s", j.Stage, j.Error)
	}
	err := fmt.Sprintf("the hotspot join stopped at %s: %s", j.Stage, why)
	if answer.Authorization != "authorized" {
		err += fmt.Sprintf("; the Wi-Fi helper is not allowed Location Services (%s), so it names no network", answer.Authorization)
	}
	return errors.New(err)
}
