package hostpower

import "regexp"

// The energy modes `pmset`'s powermode names (System Settings, Battery,
// Energy Mode), verbatim.
const (
	// EnergyModeLowPower is Low Power Mode.
	EnergyModeLowPower = "1"
	// EnergyModeHighPower is High Power Mode.
	EnergyModeHighPower = "2"
)

// energyModeLine matches `pmset -g`'s "Currently in use" line naming the
// energy mode. Machines without an energy mode print none.
var energyModeLine = regexp.MustCompile(`(?m)^\s*powermode\s+(\d+)\s*$`)

// ParseEnergyMode reads the energy mode `pmset -g`'s output says is in use,
// powermode's value verbatim; empty when the output names none.
func ParseEnergyMode(out string) string {
	if m := energyModeLine.FindStringSubmatch(out); m != nil {
		return m[1]
	}
	return ""
}

// clamshellLine matches ioreg's AppleClamshellState property; Yes is a closed
// lid.
var clamshellLine = regexp.MustCompile(`(?m)"AppleClamshellState"\s*=\s*(Yes|No)\b`)

// ParseLidClosed reads whether `ioreg -r -k AppleClamshellState`'s output
// says the lid is closed; false for an open lid and for output naming none (a
// machine without a lid).
func ParseLidClosed(out string) bool {
	m := clamshellLine.FindStringSubmatch(out)
	return m != nil && m[1] == "Yes"
}

// sleepDisabledLine matches `pmset -g`'s SleepDisabled line.
var sleepDisabledLine = regexp.MustCompile(`(?m)^\s*SleepDisabled\s+(\d+)\s*$`)

// ParseSleepDisabled reads whether `pmset -g`'s output says system sleep is
// disabled, which persistent wifi mode does.
func ParseSleepDisabled(out string) bool {
	m := sleepDisabledLine.FindStringSubmatch(out)
	return m != nil && m[1] == "1"
}
