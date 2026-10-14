package ramdisk

import (
	"fmt"
	"strconv"
	"strings"
)

// HostMemory answers the host's physical memory in bytes, as `sysctl -n
// hw.memsize` states it, read within InfoBound like the package's other
// reads of the host.
func HostMemory() (uint64, error) {
	out, err := RunBounded(InfoBound, "/usr/sbin/sysctl", "-n", "hw.memsize")
	if err != nil {
		return 0, fmt.Errorf("read hw.memsize: %w", err)
	}
	return parseMemsize(out)
}

// parseMemsize reads sysctl's answer for hw.memsize: a positive count of bytes.
func parseMemsize(out string) (uint64, error) {
	n, err := strconv.ParseUint(strings.TrimSpace(out), 10, 64)
	if err != nil || n == 0 {
		return 0, fmt.Errorf("hw.memsize answered %q, not a positive count of bytes", out)
	}
	return n, nil
}
