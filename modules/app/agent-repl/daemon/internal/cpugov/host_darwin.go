package cpugov

import (
	"fmt"

	"golang.org/x/sys/unix"
)

// Host is the real Sampler: the kernel's one-minute load average and the
// host's logical processor count, both read through sysctl. It spawns
// nothing and holds nothing open.
type Host struct{}

// Sample reads the host's load.
func (Host) Sample() (Sample, error) {
	raw, err := unix.SysctlRaw("vm.loadavg")
	if err != nil {
		return Sample{}, fmt.Errorf("cpugov: reading vm.loadavg: %w", err)
	}
	load1, err := parseLoadavg(raw)
	if err != nil {
		return Sample{}, err
	}
	cpus, err := unix.SysctlUint32("hw.logicalcpu")
	if err != nil {
		return Sample{}, fmt.Errorf("cpugov: reading hw.logicalcpu: %w", err)
	}
	if cpus == 0 {
		return Sample{}, fmt.Errorf("cpugov: hw.logicalcpu is 0")
	}
	return Sample{Load1: load1, CPUs: int(cpus)}, nil
}
