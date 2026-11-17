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
