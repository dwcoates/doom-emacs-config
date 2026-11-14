package persistentwifi

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"

	"agent-repld/internal/dlog"
)

// darwinHost is macOS's Host: pmset for the power settings, the energy mode
// and the display's sleep, networksetup and ipconfig for the Wi-Fi interface,
// wifi-util to disassociate, caffeinate to wake the display, mac-brightness to
// undo an old dim, and agent-repl's Wi-Fi helper app for the scan. Every tool
// runs through the one injected Runner, by absolute path. It is free of a
// build tag so its parsing and its argv are proven on every platform.
type darwinHost struct {
	runner Runner
	tools  Tools
	// exists answers whether an optional tool is installed.
	exists func(path string) bool
	// scanDir is where the Wi-Fi helper writes its answers.
	scanDir string

	mu sync.Mutex
	// scanSeq names each scan's answer file apart from the last's.
	scanSeq int
}

func (h *darwinHost) Read(ctx context.Context) reading { return probe(ctx, h.runner, h.tools) }

func (h *darwinHost) ReadLink(ctx context.Context, device string) (link, string, error) {
	summary, err := run(ctx, h.runner, h.tools.Ipconfig, "getsummary", device)
	if err != nil {
		return link{}, summary, err
	}
	return parseSummary(summary), summary, nil
}

func (h *darwinHost) RadioPower(ctx context.Context, device string) (string, error) {
	return readRadioPower(ctx, h.runner, h.tools, device)
}

func (h *darwinHost) PreferredNetworks(ctx context.Context, device string) ([]string, error) {
	out, err := run(ctx, h.runner, h.tools.Networksetup, "-listpreferredwirelessnetworks", device)
	if err != nil {
		return nil, err
	}
	return preferredNetworks(out), nil
}

// JoinNetwork: `networksetup -setairportnetwork` prints nothing on success and
// a sentence on failure, whatever its exit code, so both are read.
func (h *darwinHost) JoinNetwork(ctx context.Context, device, ssid string) error {
	out, err := run(ctx, h.runner, h.tools.Networksetup, "-setairportnetwork", device, ssid)
	if err != nil {
		return err
	}
	if said := strings.TrimSpace(out); said != "" {
		return errSaid(said)
	}
	return nil
}

func (h *darwinHost) CanDisconnect() error {
	if !h.exists(h.tools.WifiUtil) {
		return fmt.Errorf("%s is not installed", h.tools.WifiUtil)
	}
	return nil
}

func (h *darwinHost) Disconnect(ctx context.Context, _ string) error {
	if err := h.CanDisconnect(); err != nil {
		return fmt.Errorf("%w: %v", errNoDisconnect, err)
	}
	_, err := run(ctx, h.runner, h.tools.WifiUtil, "disconnect")
	return err
}

func (h *darwinHost) SetRadio(ctx context.Context, device string, on bool) error {
	power := "off"
	if on {
		power = "on"
	}
	_, err := run(ctx, h.runner, h.tools.Networksetup, "-setairportpower", device, power)
	return err
}

// Scan launches the Wi-Fi helper app with `open`, so the Location Services
// permission is the helper's own, and reads the answer it writes to a file,
// since `open` hands back no output.
func (h *darwinHost) Scan(ctx context.Context) (wifiScan, error) {
	h.mu.Lock()
	h.scanSeq++
	out := filepath.Join(h.scanDir, fmt.Sprintf("agent-repl-wifi-scan-%d-%d.json", os.Getpid(), h.scanSeq))
	h.mu.Unlock()
	defer os.Remove(out)
	if _, err := run(ctx, h.runner, helperLaunch(h.tools, helperWaits, verbScan, out)...); err != nil {
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper could not be run: %w", err)
	}
	raw, err := os.ReadFile(out)
	if err != nil {
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper wrote no answer to %s: %w", out, err)
	}
	var scan wifiScan
	if err := json.Unmarshal(raw, &scan); err != nil {
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper's answer could not be read: %w", err)
	}
	if scan.Error != "" {
		return scan, fmt.Errorf("the Wi-Fi helper's %s", scan.Error)
	}
	if !scan.authorized() {
		return scan, fmt.Errorf("%w (its standing: %s)", errScanUnauthorized, scan.Authorization)
	}
	return scan, nil
}

// AskScanPermission launches the helper's authorize verb, left running: macOS
// asks the owner for its Location Services permission, and the prompt waits
// for the answer.
func (h *darwinHost) AskScanPermission(ctx context.Context) (dlog.Context, error) {
	out := filepath.Join(h.scanDir, fmt.Sprintf("agent-repl-wifi-authorize-%d.json", os.Getpid()))
	fields := dlog.Context{"app": h.tools.WifiApp, "answer": out}
	_, err := run(ctx, h.runner, helperLaunch(h.tools, helperLeftRunning, verbAuthorize, out)...)
	return fields, err
}

// SetSleepDisabled writes the three power settings through `sudo -n`,
// stopping at the first refusal: the mode is then whatever it was.
func (h *darwinHost) SetSleepDisabled(ctx context.Context, on bool) error {
	value := "0"
	if on {
		value = "1"
	}
	for _, setting := range powerSettings {
		if err := pmsetWrite(ctx, h.runner, h.tools, setting, value); err != nil {
			return err
		}
	}
	return nil
}

// ReadEnergy reads the energy mode `pmset -g` says is in use.
func (h *darwinHost) ReadEnergy(ctx context.Context) (string, error) {
	out, err := run(ctx, h.runner, h.tools.Pmset, "-g")
	if err != nil {
		return "", err
	}
	return parseEnergyMode(out), nil
}

func (h *darwinHost) LowPowerEnergy() string { return energyModeLowPower }

func (h *darwinHost) SetEnergy(ctx context.Context, mode string) error {
	return pmsetWrite(ctx, h.runner, h.tools, "powermode", mode)
}

// SleepDisplay: `pmset displaysleepnow`, which needs no sudo.
func (h *darwinHost) SleepDisplay(ctx context.Context) error {
	_, err := run(ctx, h.runner, h.tools.Pmset, "displaysleepnow")
	return err
}

// WakeDisplay declares user activity: `caffeinate -u -t 1`.
func (h *darwinHost) WakeDisplay(ctx context.Context) error {
	_, err := run(ctx, h.runner, h.tools.Caffeinate, "-u", "-t", "1")
	return err
}

func (h *darwinHost) SetBrightness(ctx context.Context, level float64) error {
	if !h.exists(h.tools.Brightness) {
		return fmt.Errorf("%w: %s is not installed", errNoBrightness, h.tools.Brightness)
	}
	_, err := run(ctx, h.runner, h.tools.Brightness, formatLevel(level))
	return err
}

// errSaid is a tool's own sentence of failure, answered as the error.
type errSaid string

func (e errSaid) Error() string { return string(e) }
