//go:build darwin

package hostpower

// EnergyMode reads the energy mode the host runs in, `pmset -g`'s powermode
// verbatim (EnergyModeLowPower, EnergyModeHighPower); empty when the machine
// has none.
func (*Host) EnergyMode() (string, error) {
	return readEnergyMode("/usr/bin/pmset", EnergyModeBound)
}

// LidClosedAwake answers whether the lid is closed while system sleep is
// disabled: persistent wifi mode's closed lid, which the CPU governor gives
// the machine's whole power to.
func (*Host) LidClosedAwake() (bool, error) {
	return readLidClosedAwake("/usr/sbin/ioreg", "/usr/bin/pmset", EnergyModeBound)
}
