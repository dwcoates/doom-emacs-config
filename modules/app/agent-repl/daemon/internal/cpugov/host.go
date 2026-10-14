package cpugov

import "fmt"

// Host is the real PowerSource: the source this machine runs on, read the
// platform's own way (power_darwin.go, host_linux.go, host_other.go).
type Host struct{}

// FixedPowerEnv names the environment override that makes the governor read a
// FIXED power source instead of the host's own. It exists for test harnesses:
// the power source decides how many merges race, and a suite must not
// inherit whether its machine happens to be plugged in.
const FixedPowerEnv = "AGENT_REPL_FIXED_POWER"

// Fixed is a PowerSource that always answers one source.
type Fixed struct{ Source Power }

// Power answers the fixed source.
func (f Fixed) Power() (Power, error) { return f.Source, nil }

// HostPowerSource answers the power source the daemon's governor reads: the
// host's own, or -- FIXED set, the value of FixedPowerEnv -- a Fixed source,
// "ac" or "battery". Any other value is refused: a misspelled override must
// not quietly read as the real host.
func HostPowerSource(fixed string) (PowerSource, error) {
	switch fixed {
	case "":
		return &Host{}, nil
	case PowerAC.String():
		return Fixed{Source: PowerAC}, nil
	case PowerBattery.String():
		return Fixed{Source: PowerBattery}, nil
	}
	return nil, fmt.Errorf("cpugov: %s=%q is not a power source (ac or battery)", FixedPowerEnv, fixed)
}
