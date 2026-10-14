package reap

import "syscall"

// Getsid is getsid(2).
func Getsid(pid int) (int, error) { return syscall.Getsid(pid) }
