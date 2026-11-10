package ramdisk

import (
	"fmt"
	"strconv"
	"strings"
)

// Memory is the host's memory as the seats read it.
type Memory struct {
	// TotalBytes is the host's physical memory, which sizes the ceiling.
	TotalBytes uint64
	// AvailableBytes is what the kernel counts free for use now, which
	// admission keeps above the floor.
	AvailableBytes uint64
}

// HostMemory reads the host's memory from the kernel in one `sysctl` call,
// within InfoBound like the package's other reads of the host: hw.memsize,
// and kern.memorystatus_level, the share of it the kernel counts available
// (the "System-wide memory free percentage" memory_pressure prints, which
// counts free, inactive, purgeable and compressible memory, not merely the
// free pages vm_stat names). RAM disks are made only on macOS, so this is the
// only reader.
func HostMemory() (Memory, error) {
	out, err := RunBounded(InfoBound, "/usr/sbin/sysctl", "-n", "hw.memsize", "kern.memorystatus_level")
	if err != nil {
		return Memory{}, fmt.Errorf("read hw.memsize and kern.memorystatus_level: %w", err)
	}
	return parseMemory(out)
}

// parseMemory reads sysctl's two answers: a positive count of bytes, then a
// percentage from 0 to 100.
func parseMemory(out string) (Memory, error) {
	lines := strings.Fields(out)
	if len(lines) != 2 {
		return Memory{}, fmt.Errorf("sysctl answered %q, not hw.memsize and kern.memorystatus_level", out)
	}
	total, err := strconv.ParseUint(lines[0], 10, 64)
	if err != nil || total == 0 {
		return Memory{}, fmt.Errorf("hw.memsize answered %q, not a positive count of bytes", lines[0])
	}
	level, err := strconv.ParseUint(lines[1], 10, 64)
	if err != nil || level > 100 {
		return Memory{}, fmt.Errorf("kern.memorystatus_level answered %q, not a percentage", lines[1])
	}
	return Memory{TotalBytes: total, AvailableBytes: total / 100 * level}, nil
}
