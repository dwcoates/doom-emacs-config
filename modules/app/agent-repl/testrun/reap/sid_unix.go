//go:build darwin || linux

package reap

import "golang.org/x/sys/unix"

// Getsid is getsid(2).
func Getsid(pid int) (int, error) { return unix.Getsid(pid) }
