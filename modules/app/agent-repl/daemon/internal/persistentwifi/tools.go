// Package persistentwifi owns the host machine's persistent wifi mode: the
// laptop's lid-closed operation, in which system sleep is disabled so closing
// the lid neither sleeps the machine nor drops its network.
//
// It is the daemon-side port of the owner's `laptop_keep_alive` script, and it
// keeps that script's behavior and step order (hotspot, power settings,
// display; see endpoint_update_persistent_wifi_mode.proto). It also READS the
// standing, which the script only half did: whether the Wi-Fi interface is
// joined to a network, and whether sleep is disabled. The controller re-reads
// both on a fixed cadence and after every change, and publishes the standing
// on one topic that the daemon-level stream and the topbar both take.
//
// EVERY HOST TOOL RUNS THROUGH ONE INJECTED RUNNER, by absolute path, so a
// unit test scripts every answer and no test ever touches the real machine's
// power or network settings. The integration harness points
// AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR at a directory of fakes.
package persistentwifi

import (
	"fmt"
	"path/filepath"
	"strings"
)

// EnvToolsDir replaces the directory EVERY host tool is resolved from: each
// tool becomes `<dir>/<its base name>`. It is the test and operator knob, the
// sibling of AGENT_REPL_BROWSER_CMD; unset is the production layout.
const EnvToolsDir = "AGENT_REPL_PERSISTENT_WIFI_TOOLS_DIR"

// EnvHotspot names the phone hotspot turning the mode on joins and turning it
// off leaves. It is per-user and has no default: unset leaves the hotspot step
// unconfigured, which every mode change answers as a failed hotspot step
// pointing at the user guide. Either apostrophe spelling joins, because the
// join resolves the name against the saved networks (savedHotspot).
const EnvHotspot = "AGENT_REPL_PERSISTENT_WIFI_HOTSPOT"

// Tools are the absolute paths of every host tool the package runs.
type Tools struct {
	// Sudo runs the power settings change non-interactively (`sudo -n`), so an
	// absent grant fails at once rather than hanging on a password prompt.
	Sudo string
	// Pmset reads and writes the power settings. Its path is part of the
	// sudoers grant, which names the exact invocations, so it is absolute.
	Pmset string
	// Ipconfig reads the Wi-Fi interface's link state and network name.
	Ipconfig string
	// Networksetup finds the Wi-Fi interface, joins a network, and powers the
	// radio.
	Networksetup string
	// WifiUtil disassociates from a network with the radio left on, which
	// macOS has no built-in command for. Optional: absent, leaving the hotspot
	// goes straight to a radio restart.
	WifiUtil string
	// Brightness sets the built-in display's brightness. Optional: absent, the
	// display step is skipped and says so.
	Brightness string
}

// DefaultTools is the production layout: the system tools where macOS ships
// them, and the two helper tools where Homebrew installs them.
func DefaultTools() Tools {
	return Tools{
		Sudo:         "/usr/bin/sudo",
		Pmset:        "/usr/bin/pmset",
		Ipconfig:     "/usr/sbin/ipconfig",
		Networksetup: "/usr/sbin/networksetup",
		WifiUtil:     "/opt/homebrew/bin/wifi-util",
		Brightness:   "/opt/homebrew/bin/mac-brightness",
	}
}

// ToolsIn is every tool resolved from one directory by its base name.
func ToolsIn(dir string) Tools {
	d := DefaultTools()
	in := func(path string) string { return filepath.Join(dir, filepath.Base(path)) }
	return Tools{
		Sudo:         in(d.Sudo),
		Pmset:        in(d.Pmset),
		Ipconfig:     in(d.Ipconfig),
		Networksetup: in(d.Networksetup),
		WifiUtil:     in(d.WifiUtil),
		Brightness:   in(d.Brightness),
	}
}

// Config is the environment's say over the package: which tools, and which
// hotspot.
type Config struct {
	Tools   Tools
	Hotspot string
}

// ConfigFromEnv resolves the Config from getenv. A tools dir that is set but
// not absolute is REFUSED: a relative dir resolves against whatever the
// daemon's working directory happens to be, which is never what was meant.
func ConfigFromEnv(getenv func(string) string) (Config, error) {
	cfg := Config{Tools: DefaultTools()}
	if dir := getenv(EnvToolsDir); dir != "" {
		if !filepath.IsAbs(dir) {
			return Config{}, fmt.Errorf("persistentwifi: %s=%q is not an absolute path", EnvToolsDir, dir)
		}
		cfg.Tools = ToolsIn(dir)
	}
	if hotspot := getenv(EnvHotspot); hotspot != "" {
		if strings.TrimSpace(hotspot) == "" {
			return Config{}, fmt.Errorf("persistentwifi: %s is blank", EnvHotspot)
		}
		cfg.Hotspot = hotspot
	}
	return cfg, nil
}
