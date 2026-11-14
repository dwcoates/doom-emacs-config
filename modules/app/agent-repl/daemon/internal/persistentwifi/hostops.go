package persistentwifi

import (
	"context"
	"errors"
	"fmt"
	"os"

	"agent-repld/internal/dlog"
)

// Host is the machine's own controls the controller drives: the standing's
// reads, the sleep and energy settings, the Wi-Fi interface, and the display.
// The controller decides WHAT to do and records it; a Host only does it, so
// the persistent-wifi mode is one design on every platform, and each
// platform's tools live behind one implementation of this interface:
// darwinHost (pmset, networksetup, the Wi-Fi helper app) and linuxHost
// (a logind inhibitor, nmcli, powerprofilesctl, xset).
//
// Every method answers an error naming the tool and what it said; none logs:
// the controller is the one recorder of every step.
type Host interface {
	// Read reads the standing's two facts (the mode, the Wi-Fi link) and the
	// energy mode in use. A fact that cannot be read is left unassigned and
	// its cause kept in the reading; the other fact is still read.
	Read(ctx context.Context) reading
	// ReadLink re-reads device's link, with the tool's whole answer for a
	// record of a loss.
	ReadLink(ctx context.Context, device string) (link, string, error)
	// RadioPower reads whether device's radio is powered: "on" or "off".
	RadioPower(ctx context.Context, device string) (string, error)
	// PreferredNetworks lists the networks the machine has saved for device,
	// in the order it joins them.
	PreferredNetworks(ctx context.Context, device string) ([]string, error)
	// JoinNetwork joins device to the saved network named ssid.
	JoinNetwork(ctx context.Context, device, ssid string) error
	// Disconnect leaves device's network with the radio left on;
	// errNoDisconnect when the host has no way to (CanDisconnect).
	Disconnect(ctx context.Context, device string) error
	// CanDisconnect is nil when Disconnect can run, else why not.
	CanDisconnect() error
	// SetRadio powers device's radio on or off.
	SetRadio(ctx context.Context, device string, on bool) error
	// Scan names the networks visible now and the one joined.
	Scan(ctx context.Context) (wifiScan, error)
	// AskScanPermission asks the owner, once, for the permission a scan whose
	// standing is not determined needs, answering the record's fields.
	AskScanPermission(ctx context.Context) (dlog.Context, error)
	// SetSleepDisabled holds the machine awake with its lid closed, or lets
	// it sleep again.
	SetSleepDisabled(ctx context.Context, on bool) error
	// ReadEnergy reads the energy mode in use; "" when the host names none.
	ReadEnergy(ctx context.Context) (string, error)
	// LowPowerEnergy is the energy mode the quiet state sets.
	LowPowerEnergy() string
	// SetEnergy sets the energy mode in use.
	SetEnergy(ctx context.Context, mode string) error
	// SleepDisplay puts the display to sleep, leaving its brightness alone.
	SleepDisplay(ctx context.Context) error
	// WakeDisplay wakes a display SleepDisplay put to sleep.
	WakeDisplay(ctx context.Context) error
	// SetBrightness sets the built-in display's brightness (0.0 to 1.0), only
	// to undo a lid dim an earlier build left in force; errNoBrightness when
	// the host has no tool to.
	SetBrightness(ctx context.Context, level float64) error
}

// errNoDisconnect and errNoBrightness are a host without the optional tool a
// step would take; the controller words what the step does instead.
var (
	errNoDisconnect = errors.New("persistentwifi: this host has no way to leave a network with the radio on")
	errNoBrightness = errors.New("persistentwifi: this host has no tool to set the display's brightness")
)

// NewHost builds the Host cfg's platform drives, its tools run through runner.
// exists answers whether an optional tool is installed (nil: a stat of a
// regular file); scanDir is where a scan's answer file goes (empty: the system
// temporary directory). A platform with no Host is refused.
func NewHost(cfg Config, runner Runner, exists func(string) bool, scanDir string) (Host, error) {
	if runner == nil {
		return nil, errors.New("persistentwifi: a runner is required")
	}
	if exists == nil {
		exists = regularFile
	}
	if scanDir == "" {
		scanDir = os.TempDir()
	}
	switch cfg.Platform {
	case "darwin":
		return &darwinHost{runner: runner, tools: cfg.Tools, exists: exists, scanDir: scanDir}, nil
	default:
		return nil, fmt.Errorf("persistentwifi: no host drives persistent wifi on %q (darwin and linux do)", cfg.Platform)
	}
}
