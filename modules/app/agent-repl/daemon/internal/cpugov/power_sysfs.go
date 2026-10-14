package cpugov

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// powerFromSysfs reads the source a Linux host runs on from its power-supply
// class directory (/sys/class/power_supply): an online mains supply is AC, a
// mains supply that is not online is battery. A host with no mains supply
// (a desktop or a container that exposes none) is on battery only while one
// of its batteries discharges, and a host with no class directory at all has
// no battery to run on.
func powerFromSysfs(root string) (Power, error) {
	entries, err := os.ReadDir(root)
	if errors.Is(err, fs.ErrNotExist) {
		return PowerAC, nil
	}
	if err != nil {
		return PowerUnknown, fmt.Errorf("cpugov: reading %s: %w", root, err)
	}
	mains, online, discharging := false, false, false
	for _, entry := range entries {
		dir := filepath.Join(root, entry.Name())
		kind, err := readSysfs(dir, "type")
		if err != nil {
			return PowerUnknown, err
		}
		switch kind {
		case "Mains":
			mains = true
			state, err := readSysfs(dir, "online")
			if err != nil {
				return PowerUnknown, err
			}
			online = online || state == "1"
		case "Battery":
			status, err := readSysfs(dir, "status")
			if err != nil {
				return PowerUnknown, err
			}
			discharging = discharging || status == "Discharging"
		}
	}
	switch {
	case online:
		return PowerAC, nil
	case mains, discharging:
		return PowerBattery, nil
	default:
		return PowerAC, nil
	}
}

func readSysfs(dir, name string) (string, error) {
	raw, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		return "", fmt.Errorf("cpugov: reading %s: %w", filepath.Join(dir, name), err)
	}
	return strings.TrimSpace(string(raw)), nil
}
