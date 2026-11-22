package persistentwifi

import (
	"agentrepl/logging/hostpower"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
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

// BluetoothPower reads the Bluetooth controller's state from
// `system_profiler SPBluetoothDataType -json` (parseBluetoothPower).
func (h *darwinHost) BluetoothPower(ctx context.Context) (string, error) {
	out, err := run(ctx, h.runner, h.tools.SystemProfiler, "SPBluetoothDataType", "-json")
	if err != nil {
		return "", err
	}
	return parseBluetoothPower(out)
}

// parseBluetoothPower reads the first controller's `controller_state` from
// `system_profiler SPBluetoothDataType -json`: "attrib_on" is "on" and
// "attrib_off" is "off". An answer naming neither is an error carrying what
// it named, so a record shows what the tool said.
func parseBluetoothPower(out string) (string, error) {
	var answer struct {
		Controllers []struct {
			Properties struct {
				State string `json:"controller_state"`
			} `json:"controller_properties"`
		} `json:"SPBluetoothDataType"`
	}
	if err := json.Unmarshal([]byte(out), &answer); err != nil {
		return "", fmt.Errorf("system_profiler's Bluetooth answer could not be read: %w", err)
	}
	if len(answer.Controllers) == 0 {
		return "", errors.New("system_profiler named no Bluetooth controller")
	}
	switch state := answer.Controllers[0].Properties.State; state {
	case "attrib_on":
		return "on", nil
	case "attrib_off":
		return "off", nil
	default:
		return "", fmt.Errorf("system_profiler named no Bluetooth power state: controller_state %q", state)
	}
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
//
// THE ANSWER FILE IS THE SCAN'S ANSWER, NOT `open`'s EXIT. `open -W` finds
// the helper's process and only then waits on it, so a helper that answered
// and exited in between fails the wait with ESRCH (openWaitLost) though its
// answer is written (2026-11-20 19:06:03, a scan answering in 400ms). That
// failure with an answer in the file is the answer, the lost wait kept in
// the scan's record (wifiScan.WaitLost); without one it is the helper's
// failure. A file left from before is removed first, so the answer read is
// always this launch's.
func (h *darwinHost) Scan(ctx context.Context) (wifiScan, error) {
	h.mu.Lock()
	h.scanSeq++
	out := filepath.Join(h.scanDir, fmt.Sprintf("agent-repl-wifi-scan-%d-%d.json", os.Getpid(), h.scanSeq))
	h.mu.Unlock()
	if err := os.Remove(out); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper's earlier answer at %s could not be removed: %w", out, err)
	}
	defer os.Remove(out)
	said, launchErr := run(ctx, h.runner, helperLaunch(h.tools, helperWaits, verbScan, out)...)
	waitLost := launchErr != nil && strings.Contains(said, openWaitLost)
	if launchErr != nil && !waitLost {
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper could not be run: %w", launchErr)
	}
	raw, err := os.ReadFile(out)
	switch {
	case err != nil && waitLost:
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper exited before `open -W` could wait on it and wrote no answer to %s (%v): %w", out, err, launchErr)
	case err != nil:
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper wrote no answer to %s: %w", out, err)
	}
	var scan wifiScan
	if err := json.Unmarshal(raw, &scan); err != nil {
		return wifiScan{}, fmt.Errorf("the Wi-Fi helper's answer could not be read: %w", err)
	}
	if waitLost {
		scan.WaitLost = launchErr.Error()
	}
	if scan.Error != "" {
		return scan, fmt.Errorf("the Wi-Fi helper's %s", scan.Error)
	}
	if !scan.authorized() {
		return scan, unauthorizedScan(scan.Authorization)
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
	return hostpower.ParseEnergyMode(out), nil
}

func (h *darwinHost) LowPowerEnergy() string { return energyModeLowPower }

func (h *darwinHost) HighPowerEnergy() string { return energyModeHighPower }

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

// openWaitLost is what `open -W` says when the app it launched exited before
// it could wait on it: kevent's ESRCH.
const openWaitLost = "initial call to kevent() failed: No such process"

// errSaid is a tool's own sentence of failure, answered as the error.
type errSaid string

func (e errSaid) Error() string { return string(e) }
