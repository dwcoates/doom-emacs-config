//go:build !darwin

package reap

import (
	"fmt"
	"runtime"
	"syscall"
)

// getsid reads a process's session. Only darwin's getsid(2) is wired; the
// reap fails loudly anywhere else (see exit_other.go).
func getsid(int) (int, error) {
	return 0, fmt.Errorf("reading a process's session is not implemented on %s: %w", runtime.GOOS, syscall.ENOSYS)
}
