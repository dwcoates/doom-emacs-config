package main

import (
	"time"

	"golang.org/x/sys/unix"
)

// platformBootTimeMillis derives linux's boot instant from sysinfo's uptime,
// which is the seconds since boot: boot time is now minus that. Linux has no
// kern.boottime sysctl, and sysinfo is a syscall rather than a /proc read, so
// it answers under the same no-filesystem assumptions the darwin path holds.
func platformBootTimeMillis() (int64, error) {
	var info unix.Sysinfo_t
	if err := unix.Sysinfo(&info); err != nil {
		return 0, err
	}
	return time.Now().UnixMilli() - int64(info.Uptime)*1000, nil
}
