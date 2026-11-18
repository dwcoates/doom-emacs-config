package persistentwifi

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"regexp"
	"strings"
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

// scanBound bounds one scan: the helper's launch, its wait for the client
// seat (wifi-helper/seats.swift) behind at most one scan ahead of it, and its
// own CoreWLAN scan. One scan measured 3.9s (2026-11-14, `open -W` of the
// helper on this host, lid open), so two back to back are 7.8s; the bound is
// 2.5 times that. The helper bounds its own seat wait inside it (seatBound,
// 9s), so a helper always answers before this bound cuts it.
const scanBound = 20 * time.Second

// scanMeasured is the scan's measured healthy time scanBound is sized from.
const scanMeasured = "7.8s"

// wifiScan is the helper's answer (wifi-helper/main.swift's Answer).
type wifiScan struct {
	// Authorization is the helper's Location Services standing:
	// "authorized", or why it names no networks ("not_determined",
	// "denied", "restricted", or "awaiting_owner": an ask holds the helper's
	// one Location Services seat, waiting on the owner's answer).
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
// Services, so it names no network: only the owner can allow it.
var errScanUnauthorized = errors.New("agent-repl's Wi-Fi helper is not allowed Location Services, so no network can be named")

// unauthorizedRemedy is what the owner does to allow the helper Location
// Services from STANDING. The remedy turns on the standing (2026-11-16): a
// helper macOS never asked about is not listed in System Settings, so only
// the helper's own prompt can allow it, while one the owner denied is
// allowed only in System Settings.
func unauthorizedRemedy(standing string) string {
	if standing == standingNotDetermined {
		return "macOS never asked about it, so it is not yet listed in System Settings; allow it in the Location Services prompt agent-repl asks macOS to show, or run `bin/requirements.sh install` to ask again"
	}
	return "allow it in System Settings > Privacy & Security > Location Services"
}

// unauthorizedScan is errScanUnauthorized for a scan whose helper's standing
// is STANDING, naming the standing and its remedy.
func unauthorizedScan(standing string) error {
	return fmt.Errorf("%w; %s (its standing: %s)", errScanUnauthorized, unauthorizedRemedy(standing), standing)
}

// helperWait says whether a helper launch waits for the helper to exit.
type helperWait bool

const (
	// helperWaits is a verb whose answer the launch waits for (`open -W`).
	helperWaits helperWait = true
	// helperLeftRunning is a verb that waits on the owner, launched and left.
	helperLeftRunning helperWait = false
)

// The helper's verbs (wifi-helper/main.swift).
const (
	verbScan      = "scan"
	verbAuthorize = "authorize"
)

// helperLaunch is the one argv that runs the Wi-Fi helper's VERB, answering in
// OUT: a fresh instance (-n) in the background (-g), launched by `open` so
// macOS attributes the permission to the helper's own bundle.
func helperLaunch(tools Tools, wait helperWait, verb, out string) []string {
	argv := []string{tools.Open}
	if wait {
		argv = append(argv, "-W")
	}
	return append(argv, "-n", "-g", "-a", tools.WifiApp, "--args", verb, "--out", out)
}

// scanWifi runs one scan, bounded by scanBound. A scan that could not run,
// did not answer in time, failed, or names no network for want of the
// permission is an error naming why. It runs OFF the lock.
func (c *Controller) scanWifi(ctx context.Context) (wifiScan, error) {
	bounded, cancel := context.WithTimeout(ctx, c.scanBound)
	defer cancel()
	start := c.clock.Now()
	scan, err := c.host.Scan(bounded)
	took := c.clock.Now().Sub(start)
	switch {
	case ctx.Err() != nil:
		return wifiScan{}, ctx.Err()
	case bounded.Err() != nil:
		return wifiScan{}, fmt.Errorf("the Wi-Fi scan did not answer within %s (measured %s): %w", c.scanBound, scanMeasured, bounded.Err())
	case err != nil:
		if scan.Authorization == standingNotDetermined {
			c.askAuthorization(ctx)
		}
		return scan, err
	}
	// AN ALLOWED STANDING ENDS THE ASK: a deploy that installs a rebuilt
	// helper re-signs it, macOS may forget the answer, and the next
	// not_determined is asked for again.
	c.mu.Lock()
	c.authorizeAsked = false
	c.mu.Unlock()
	c.log.Debug(opFailover, "scanned the visible Wi-Fi networks", dlog.Context{
		"visible": len(scan.Visible), "joined_named": scan.Current != "", "took_ms": took.Milliseconds()})
	return scan, nil
}

// standingNotDetermined is the helper's standing while the owner was never
// asked: macOS shows its Location Services prompt only to an app that asks.
const standingNotDetermined = "not_determined"

// askAuthorization asks macOS, once per standing never asked for, to show the owner the helper's
// Location Services prompt, through the helper's authorize verb
// (wifi-helper/main.swift).
//
// A STANDING NO ONE WAS ASKED FOR IS ASKED FOR HERE. The installer asks once
// (bin/requirements.sh, install_location_permission), but macOS forgets an ad
// hoc signed helper's answer when a rebuild re-signs it, and nothing asked
// again: on 2026-11-16 every failover scan reported not_determined and no
// prompt was ever shown. The verb waits for the owner's answer, unbounded, so
// it is launched without -W and left running; the launch itself is bounded by
// scanBound, which a scan's whole launch and scan fit inside. Asked once
// until a scan hears the helper allowed, because a waiting prompt is not
// asked for twice. It runs OFF the lock.
func (c *Controller) askAuthorization(ctx context.Context) {
	c.mu.Lock()
	if c.authorizeAsked {
		c.mu.Unlock()
		return
	}
	c.authorizeAsked = true
	c.mu.Unlock()
	bounded, cancel := context.WithTimeout(ctx, c.scanBound)
	defer cancel()
	fields, err := c.host.AskScanPermission(bounded)
	if err != nil {
		c.log.Error(opFailover, "could not ask macOS for the Wi-Fi helper's Location Services permission", dlog.WithCause(fields, err))
		return
	}
	c.log.Info(opFailover, "asked macOS for the Wi-Fi helper's Location Services permission; its prompt waits for the owner", fields)
}

// Expected time and bound of StopAsks' pkill, spawned the way the daemon
// spawns it. The live daemon deploys on the same host the merge queue's test
// gate runs on, so the healthy worst case is the one under that gate's load.
// Measured 2026-11-17 19:07 in the gate's full run of the selected suites
// (8 slots, 1146s of cpu in 180s): the deploy timed this stop of a one-line
// fake pkill at 200ms, and the daemon's 259 spawns of a fake git across that
// run's kept artifacts had a p99 of 78ms and a worst of 91ms. The expected
// time is the 200ms the gate measured, and the bound 2.5 times it. The
// earlier 140ms bound, measured 2026-11-16 at load average 4 to 6 (1500
// pkills, worst 56ms), fired under that gate.
const (
	stopAsksExpected = 200 * time.Millisecond
	stopAsksBound    = 500 * time.Millisecond
)

// StopAsks stops every authorize ask of the helper app installed in TOOLS
// (TOOLS.WifiApp) that is still running, and reports whether one was. The
// deploy calls it once it installed a rebuilt helper: an ask waits on the
// owner unbounded and holds the bundle's one Location Services client, so an
// ask of the build the install replaced (whose answer would go to the old
// signature) kept every scan of the installed build from hearing its
// standing (2026-11-16 02:53). The next scan asks again, from the installed
// helper. A scan is never stopped: it ends on its own within scanBound.
func StopAsks(ctx context.Context, r Runner, tools Tools) (bool, error) {
	bounded, cancel := context.WithTimeout(ctx, stopAsksBound)
	defer cancel()
	exe := filepath.Join(tools.WifiApp, wifiAppExecutable)
	pattern := "^" + regexp.QuoteMeta(exe) + " " + verbAuthorize + "( |$)"
	out, code, err := r.Run(bounded, runDir, []string{tools.Pkill, "-f", pattern})
	switch {
	case bounded.Err() != nil:
		return false, fmt.Errorf("pkill of the Wi-Fi helper's asks did not end within %s (expected %s): %w", stopAsksBound, stopAsksExpected, bounded.Err())
	case err != nil:
		return false, fmt.Errorf("pkill could not be run: %w", err)
	case code == 0:
		return true, nil
	case code == 1:
		// pkill answers 1 when nothing matched: no ask was waiting.
		return false, nil
	default:
		return false, fmt.Errorf("pkill exited %d: %s", code, strings.TrimSpace(out))
	}
}

// recordScanCauseLocked records a scan's failure once per distinct cause
// (recordStandingCause), at ERROR: a failover that cannot see which networks
// are visible joins only the hotspot. Caller holds mu.
func (c *Controller) recordScanCauseLocked(err error) {
	c.recordStandingCause(&c.scanCause, err.Error(), func(cause string) {
		c.log.Error(opFailover, "the visible Wi-Fi networks could not be read; only the hotspot can be joined", dlog.Context{"cause": cause})
	})
}
