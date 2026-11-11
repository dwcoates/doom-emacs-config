// Package hostpower reads the source the host runs on -- AC power or its
// battery -- the platform's own way (power_darwin.go, host_linux.go,
// host_other.go). The runtime metrics record it beside every process sample.
package hostpower

// Power is the source the host runs on.
type Power int

const (
	// PowerUnknown is a power source not read, or unreadable.
	PowerUnknown Power = iota
	// PowerAC is a host on external power.
	PowerAC
	// PowerBattery is a host on its battery.
	PowerBattery
)

func (p Power) String() string {
	switch p {
	case PowerAC:
		return "ac"
	case PowerBattery:
		return "battery"
	default:
		return "unknown"
	}
}

// PowerSource reads the source the host runs on.
type PowerSource interface {
	Power() (Power, error)
}

// OnBattery answers whether SOURCE reads the host as running on its battery.
// A source that cannot be read answers its error.
func OnBattery(source PowerSource) (bool, error) {
	power, err := source.Power()
	if err != nil {
		return false, err
	}
	return power == PowerBattery, nil
}
