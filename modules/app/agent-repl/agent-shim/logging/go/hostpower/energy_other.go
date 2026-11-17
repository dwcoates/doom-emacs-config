//go:build !darwin

package hostpower

// EnergyMode answers that the host has no energy mode: only macOS names one.
func (*Host) EnergyMode() (string, error) { return "", nil }
