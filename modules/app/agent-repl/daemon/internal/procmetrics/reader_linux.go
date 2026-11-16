//go:build linux

package procmetrics

import (
	"fmt"

	"golang.org/x/sys/unix"
)

// Kernel is the real Reader: /proc, read through procfs.
type Kernel struct{}

// proc is the host's /proc.
var proc = newProcfs("/proc")

// Platform implements Reader.
func (Kernel) Platform() Platform { return Linux }

// PIDs implements Reader.
func (Kernel) PIDs() ([]int, error) { return proc.PIDs() }

// Identity implements Reader.
func (Kernel) Identity(pid int) (Identity, error) { return proc.Identity(pid) }

// Usage implements Reader.
func (Kernel) Usage(pid int) (Usage, error) { return proc.Usage(pid) }

// Args implements Reader.
func (Kernel) Args(pid int) ([]string, error) { return proc.Args(pid) }

// Host implements Reader.
func (Kernel) Host(path string) (Host, error) {
	cpu, err := hostCPUSeconds()
	if err != nil {
		return Host{}, err
	}
	memory, total, err := proc.Memory()
	if err != nil {
		return Host{}, err
	}
	var fs unix.Statfs_t
	if err := unix.Statfs(path, &fs); err != nil {
		return Host{}, fmt.Errorf("procmetrics: statfs %s: %w", path, err)
	}
	return Host{
		CPUSeconds:       cpu,
		MemoryBytes:      memory,
		MemoryTotalBytes: total,
		DiskFreeBytes:    fs.Bavail * uint64(fs.Bsize),
		DiskTotalBytes:   fs.Blocks * uint64(fs.Bsize),
	}, nil
}
