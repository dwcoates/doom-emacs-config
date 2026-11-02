// Package persistentwifi owns the host machine's persistent wifi mode: the
// laptop's lid-closed operation, in which system sleep is disabled so closing
// the lid neither sleeps the machine nor drops its network.
//
// It is the daemon-side port of the owner's `laptop_keep_alive` script, and it
// keeps that script's behavior and step order (hotspot, power settings,
// display; see endpoint_update_persistent_wifi_mode.proto), with an energy
// mode step (Low Power Mode on, High Power Mode off) before the display. The
// display FOLLOWS THE LID while the mode is on (display.go): a closed lid
// dims it to 0 and opening it restores the level kept when it closed. The
// lid is HEARD through IOKit's clamshell notification (lid_darwin.go), never
// polled. It also READS the
// standing, which the script only half did: whether the Wi-Fi interface is
// joined to a network, and whether sleep is disabled. The controller re-reads
// both on a fixed cadence and after every change, and publishes the standing
// on one topic that the daemon-level stream and the topbar both take. While
// the mode is on, every re-read that finds no network joined joins the hotspot
// (failover.go).
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
	// Brightness reads (no argument) and sets (one level, 0.0 to 1.0) the
	// built-in display's brightness, for the lid-close dim. Optional: absent,
	// a display move is skipped and says so.
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

// Config is the environment's say over the package: which tools, which
// hotspot, and where the lid is heard from.
type Config struct {
	Tools   Tools
	Hotspot string
	// LidPipe is EnvLidPipe's named pipe; empty is the operating system's lid
	// (SystemLid).
	LidPipe string
}

// Lid is the LidWatcher the config names.
func (c Config) Lid() LidWatcher {
	if c.LidPipe != "" {
		return PipeLid(c.LidPipe)
	}
	return SystemLid()
}

// ConfigFromEnv resolves the Config from getenv. A tools dir that is set but
// not absolute is REFUSED: a relative dir resolves against whatever the
// daemon's working directory happens to be, which is never what was meant.
// Fake tools without a fake lid are REFUSED too: the real lid would then move
// a display the fake tools only pretend to set, and a lid pipe is likewise
// refused without fake tools.
func ConfigFromEnv(getenv func(string) string) (Config, error) {
	cfg := Config{Tools: DefaultTools()}
	dir, pipe := getenv(EnvToolsDir), getenv(EnvLidPipe)
	switch {
	case dir != "" && pipe == "":
		return Config{}, fmt.Errorf("persistentwifi: %s is set without %s; fake host tools never hear the real lid", EnvToolsDir, EnvLidPipe)
	case pipe != "" && dir == "":
		return Config{}, fmt.Errorf("persistentwifi: %s is set without %s; a fake lid never moves the real display", EnvLidPipe, EnvToolsDir)
	case pipe != "" && !filepath.IsAbs(pipe):
		return Config{}, fmt.Errorf("persistentwifi: %s=%q is not an absolute path", EnvLidPipe, pipe)
	}
	cfg.LidPipe = pipe
	if dir != "" {
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
