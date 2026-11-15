package persistentwifi

import (
	"context"
	"fmt"
)

// EnvPowerSource replaces the operating system's power source with a fake
// one, in the lid's two forms (EnvLid): an absolute path is a Unix socket a
// test harness serves (SocketPowerSource), and "ac" or "battery" a power
// source fixed in that state (FixedPowerSource). Like the lid's, it pairs
// with EnvToolsDir, so a daemon running against fake host tools never hears
// the real machine's power source (ConfigFromEnv refuses the one without the
// other).
const EnvPowerSource = "AGENT_REPL_PERSISTENT_WIFI_POWER_SOURCE"

// PowerSource is what the operating system says the machine draws its power
// from.
type PowerSource int

// The power sources a PowerSourceWatcher reports.
const (
	// PowerAC is unlimited power: the wall, a car or an airplane. A machine
	// with no battery always draws it.
	PowerAC PowerSource = iota + 1
	// PowerBattery is limited power: the internal battery, or a UPS.
	PowerBattery
)

// String names the power source for the record and for a fake's lines.
func (p PowerSource) String() string {
	switch p {
	case PowerAC:
		return "ac"
	case PowerBattery:
		return "battery"
	}
	panic(fmt.Sprintf("persistentwifi: PowerSource %d has no name", int(p)))
}

// PowerSourceWatcher HEARS the power source: the operating system tells it of
// every change between AC and battery, so nothing polls. Its contract is the
// LidWatcher's: Watch subscribes, THEN reads, and hands heard that state
// followed by every change after it, in order and on one goroutine, until
// ctx ends; it answers nil once ctx ends, and an error when the subscription
// could not be made or ended while ctx was live.
type PowerSourceWatcher interface {
	Watch(ctx context.Context, heard func(PowerSource)) error
}

// powerSourceKind is the power source as the controller holds it: what was
// heard, or why nothing is.
type powerSourceKind int

const (
	// powerSourceUnheard is the zero value: the watcher has not yet
	// subscribed.
	powerSourceUnheard powerSourceKind = iota
	powerSourceAC
	powerSourceBattery
	// powerSourceUnwatched is a subscription that failed or ended; cause
	// says why.
	powerSourceUnwatched
)

// powerSourceFact is the controller's power source.
type powerSourceFact struct {
	kind  powerSourceKind
	cause string
}

// heardPowerSource is the fact a heard PowerSource states.
func heardPowerSource(p PowerSource) powerSourceFact {
	switch p {
	case PowerAC:
		return powerSourceFact{kind: powerSourceAC}
	case PowerBattery:
		return powerSourceFact{kind: powerSourceBattery}
	}
	panic(fmt.Sprintf("persistentwifi: a watcher heard an unnamed power source %d", int(p)))
}

// name is the fact for the record.
func (f powerSourceFact) name() string {
	switch f.kind {
	case powerSourceAC:
		return "ac"
	case powerSourceBattery:
		return "battery"
	case powerSourceUnwatched:
		return "unwatched"
	}
	return "unheard"
}

// SocketPowerSource is the PowerSourceWatcher over a Unix socket a harness
// serves (watchSocket), whose lines are "ac" or "battery".
func SocketPowerSource(path string) PowerSourceWatcher { return socketPowerSource{path: path} }

type socketPowerSource struct{ path string }

func (s socketPowerSource) Watch(ctx context.Context, heard func(PowerSource)) error {
	return watchSocket(ctx, s.path, "power source", parsePowerSourceLine, heard)
}

// FixedPowerSource is a power source heard once, as p, that never changes.
func FixedPowerSource(p PowerSource) PowerSourceWatcher { return fixedPowerSource{source: p} }

type fixedPowerSource struct{ source PowerSource }

func (f fixedPowerSource) Watch(ctx context.Context, heard func(PowerSource)) error {
	return watchFixed(ctx, f.source, heard)
}

// parsePowerSourceLine reads one power source's name.
func parsePowerSourceLine(line string) (PowerSource, error) {
	return parseState(line, "power source", PowerAC, PowerBattery)
}

// powerSourceOf is the PowerSource the operating system's limited-power
// reading names.
func powerSourceOf(battery bool) PowerSource {
	if battery {
		return PowerBattery
	}
	return PowerAC
}
