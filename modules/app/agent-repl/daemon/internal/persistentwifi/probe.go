package persistentwifi

import (
	"bufio"
	"context"
	"fmt"
	"regexp"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// Runner runs one host tool and answers its combined output and exit code. An
// error means the run could not be classified at all (the tool could not be
// spawned, the context ended); a tool that ran and failed is a non-zero code
// and a nil error. internal/scriptrunner's Runner is the production one.
type Runner interface {
	Run(ctx context.Context, dir string, argv []string) (string, int, error)
}

// runDir is the directory every tool runs in. None of them reads it; it is
// named because the runner refuses an empty one.
const runDir = "/"

// redactedName is what macOS prints in place of a network name for a process
// without a location-services grant.
const redactedName = "<redacted>"

// sleepDisabled matches `pmset -g`'s system-wide line when sleep is disabled.
var sleepDisabled = regexp.MustCompile(`(?m)^\s*SleepDisabled\s+(\d+)\s*$`)

// run runs argv and folds every way it can fail into one error naming the
// tool, the code and its output, for the probe and change sites alike.
func run(ctx context.Context, r Runner, argv ...string) (string, error) {
	out, code, err := r.Run(ctx, runDir, argv)
	if err != nil {
		return out, fmt.Errorf("%s could not be run: %w", argv[0], err)
	}
	if code != 0 {
		return out, fmt.Errorf("%s exited %d: %s", argv[0], code, strings.TrimSpace(out))
	}
	return out, nil
}

// parseMode reads `pmset -g`'s output. ok is false when the output carries no
// SleepDisabled line, which `pmset -g` always prints.
func parseMode(out string) (on, ok bool) {
	m := sleepDisabled.FindStringSubmatch(out)
	if m == nil {
		return false, false
	}
	return m[1] != "0", true
}

// wifiInterface finds the Wi-Fi interface's device name in
// `networksetup -listallhardwareports` output: the `Device:` line after the
// `Hardware Port: Wi-Fi` line. Empty when the machine has none.
func wifiInterface(out string) string {
	sc := bufio.NewScanner(strings.NewReader(out))
	wifi := false
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		switch {
		case strings.HasPrefix(line, "Hardware Port:"):
			wifi = strings.TrimSpace(strings.TrimPrefix(line, "Hardware Port:")) == "Wi-Fi"
		case wifi && strings.HasPrefix(line, "Device:"):
			return strings.TrimSpace(strings.TrimPrefix(line, "Device:"))
		}
	}
	return ""
}

// link is what `ipconfig getsummary <device>` says about the interface.
type link struct {
	// joined is `LinkStatusActive : TRUE`.
	joined bool
	// name is the joined network's name; empty when withheld or not joined.
	name string
}

// parseSummary reads `ipconfig getsummary <device>`. Only the dictionary's
// TOP-LEVEL keys are read (two-space indent): nested dictionaries carry keys
// of their own, and `BSSID` is a different key from `SSID`.
func parseSummary(out string) link {
	var l link
	sc := bufio.NewScanner(strings.NewReader(out))
	for sc.Scan() {
		line := sc.Text()
		if !strings.HasPrefix(line, "  ") || strings.HasPrefix(line, "   ") {
			continue
		}
		key, value, found := strings.Cut(strings.TrimSpace(line), " : ")
		if !found {
			continue
		}
		switch key {
		case "LinkStatusActive":
			l.joined = value == "TRUE"
		case "SSID":
			if value != redactedName {
				l.name = value
			}
		}
	}
	if !l.joined {
		l.name = ""
	}
	return l
}

// reading is one read of both facts, with why each one could not be read.
type reading struct {
	state *agentreplv1.PersistentWifiState
	// device is the Wi-Fi interface; empty when there is none or it could not
	// be found.
	device string
	// link is the interface's link; zero when device is empty.
	link link
	// wifiErr and modeErr are why a fact was left unassigned; nil when read.
	wifiErr, modeErr error
}

// probe reads both facts. A fact that cannot be read is left unassigned and
// its cause returned in the reading; the other fact is still read.
func probe(ctx context.Context, r Runner, tools Tools) reading {
	rd := reading{state: &agentreplv1.PersistentWifiState{}}

	if out, err := run(ctx, r, tools.Pmset, "-g"); err != nil {
		rd.modeErr = err
	} else if on, ok := parseMode(out); !ok {
		rd.modeErr = fmt.Errorf("%s -g printed no SleepDisabled line", tools.Pmset)
	} else if on {
		rd.state.Mode = &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}
	} else {
		rd.state.Mode = &agentreplv1.PersistentWifiState_Off{Off: &agentreplv1.PersistentWifiModeOff{}}
	}

	rd.device, rd.link, rd.wifiErr = readLink(ctx, r, tools)
	switch {
	case rd.wifiErr != nil:
	case rd.link.joined:
		joined := &agentreplv1.PersistentWifiJoined{}
		if rd.link.name != "" {
			joined.NetworkName = &rd.link.name
		}
		rd.state.Wifi = &agentreplv1.PersistentWifiState_Joined{Joined: joined}
	default:
		rd.state.Wifi = &agentreplv1.PersistentWifiState_NotJoined{NotJoined: &agentreplv1.PersistentWifiNotJoined{}}
	}
	return rd
}

// readLink finds the Wi-Fi interface and reads its link. A machine with no
// Wi-Fi interface is not joined, which is an answer rather than an error.
func readLink(ctx context.Context, r Runner, tools Tools) (string, link, error) {
	ports, err := run(ctx, r, tools.Networksetup, "-listallhardwareports")
	if err != nil {
		return "", link{}, err
	}
	device := wifiInterface(ports)
	if device == "" {
		return "", link{}, nil
	}
	summary, err := run(ctx, r, tools.Ipconfig, "getsummary", device)
	if err != nil {
		return device, link{}, err
	}
	return device, parseSummary(summary), nil
}
