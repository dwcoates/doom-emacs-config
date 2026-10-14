package cpugov

import (
	"fmt"
	"os"
	"runtime"
)

// Host is the real Sampler on Linux (the e2e sandbox): the aggregate cpu line
// of /proc/stat, the kernel's own tick count of every processor's states.
type Host struct{}

// Ticks reads the host's processor ticks.
func (*Host) Ticks() (Ticks, error) {
	raw, err := os.ReadFile("/proc/stat")
	if err != nil {
		return Ticks{}, fmt.Errorf("cpugov: reading /proc/stat: %w", err)
	}
	busy, total, err := parseProcStat(string(raw))
	if err != nil {
		return Ticks{}, err
	}
	return Ticks{Busy: busy, Total: total, CPUs: runtime.NumCPU()}, nil
}

// Power reads the source the host runs on from /sys/class/power_supply.
func (*Host) Power() (Power, error) {
	return powerFromSysfs("/sys/class/power_supply")
}
