//go:build darwin

package hostpower

// EnergyMode reads the energy mode the host runs in, `pmset -g`'s powermode
// verbatim (EnergyModeLowPower, EnergyModeHighPower); empty when the machine
// has none.
func (*Host) EnergyMode() (string, error) {
	return readEnergyMode("/usr/bin/pmset", EnergyModeBound)
}
