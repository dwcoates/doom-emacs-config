//go:build !darwin

package hostpower

// EnergyMode answers that the host has no energy mode: only macOS names one.
func (*Host) EnergyMode() (string, error) { return "", nil }

// LidClosedAwake answers that no lid is held awake: only macOS reads one.
func (*Host) LidClosedAwake() (bool, error) { return false, nil }
