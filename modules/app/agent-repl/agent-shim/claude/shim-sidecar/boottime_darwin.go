package main

import "golang.org/x/sys/unix"

// platformBootTimeMillis reads darwin's kern.boottime, a sysctl that answers a
// timeval naming the wall-clock instant the kernel came up.
func platformBootTimeMillis() (int64, error) {
	tv, err := unix.SysctlTimeval("kern.boottime")
	if err != nil {
		return 0, err
	}
	return int64(tv.Sec)*1000 + int64(tv.Usec)/1000, nil
}
