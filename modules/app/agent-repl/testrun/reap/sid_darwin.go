package reap

import "syscall"

// getsid is getsid(2).
func getsid(pid int) (int, error) { return syscall.Getsid(pid) }
