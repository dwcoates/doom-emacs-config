package persistentwifi

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"agent-repld/internal/dlog"
)

// THE WI-FI SCAN (owner ruling, 2026-11-14): the failover joins only networks
// macOS knows that are visible now, then the hotspot. macOS names Wi-Fi
// networks only to an app allowed Location Services (`ipconfig` and
// `system_profiler` print "<redacted>"), so the names come from agent-repl's
// Wi-Fi helper app (wifi-helper/), launched with `open` so the permission is
// the helper's own, and answering in a file, since `open` hands back no
// output.

// scanBound bounds one scan: the helper's launch and its CoreWLAN scan.
// Measured 3.9s (2026-11-14, `open -W` of the helper on this host, lid open);
// the bound is 2.5 times that.
const scanBound = 10 * time.Second

// scanMeasured is the scan's measured healthy time scanBound is sized from.
const scanMeasured = "3.9s"

// wifiScan is the helper's answer (wifi-helper/main.swift's Answer).
type wifiScan struct {
	// Authorization is the helper's Location Services standing:
	// "authorized", or why it names no networks ("not_determined",
	// "denied", "restricted").
	Authorization string `json:"authorization"`
	// Current is the joined network's name; empty while none is joined or
	// names are not readable.
	Current string `json:"current"`
	// Visible is every network the scan saw, strongest first.
	Visible []visibleNetwork `json:"visible"`
	// Error is why the scan itself failed, when it did.
	Error string `json:"error"`
}

// visibleNetwork is one network a scan saw.
type visibleNetwork struct {
	SSID string `json:"ssid"`
	RSSI int    `json:"rssi"`
}

// authorized reports whether the scan could name networks.
func (s wifiScan) authorized() bool { return s.Authorization == "authorized" }

// sees reports whether the scan saw a network named NAME.
func (s wifiScan) sees(name string) bool {
	for _, v := range s.Visible {
		if sameNetwork(v.SSID, name) {
			return true
		}
	}
	return false
}

// errScanUnauthorized is a scan whose helper is not allowed Location
// Services, so it names no network: only the owner can allow it, in System
// Settings, Privacy & Security, Location Services.
var errScanUnauthorized = errors.New("agent-repl's Wi-Fi helper is not allowed Location Services, so no network can be named; allow it in System Settings > Privacy & Security > Location Services")

// scanWifi runs one scan, bounded by scanBound. A scan that could not run,
// did not answer in time, failed, or names no network for want of the
// permission is an error naming why. It runs OFF the lock.
func (c *Controller) scanWifi(ctx context.Context) (wifiScan, error) {
	c.mu.Lock()
	c.scanSeq++
	out := filepath.Join(c.scanDir, fmt.Sprintf("agent-repl-wifi-scan-%d-%d.json", os.Getpid(), c.scanSeq))
	c.mu.Unlock()
	defer os.Remove(out)
	bounded, cancel := context.WithTimeout(ctx, c.scanBound)
	defer cancel()
	start := c.clock.Now()
	_, err := run(bounded, c.runner, c.cfg.Tools.Open, "-W", "-n", "-g", "-a", c.cfg.Tools.WifiApp, "--args", "scan", "--out", out)
	took := c.clock.Now().Sub(start)
	switch {
	case ctx.Err() != nil:
		return wifiScan{}, ctx.Err()
	case bounded.Err() != nil:
		return wifiScan{}, fmt.Errorf("the Wi-Fi scan did not answer within %s (measured %s): %w", c.scanBound, scanMeasured, bounded.Err())
	case err != nil:
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
	c.log.Debug(opFailover, "scanned the visible Wi-Fi networks", dlog.Context{
		"visible": len(scan.Visible), "joined_named": scan.Current != "", "took_ms": took.Milliseconds()})
	return scan, nil
}

// recordScanCauseLocked records a scan's failure once per distinct cause
// (recordStandingCause), at ERROR: a failover that cannot see which networks
// are visible joins only the hotspot. Caller holds mu.
func (c *Controller) recordScanCauseLocked(err error) {
	c.recordStandingCause(&c.scanCause, err.Error(), func(cause string) {
		c.log.Error(opFailover, "the visible Wi-Fi networks could not be read; only the hotspot can be joined", dlog.Context{"cause": cause})
	})
}
