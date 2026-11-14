//go:build !darwin && !linux

package reap

import (
	"fmt"
	"runtime"
	"time"
)

// AwaitExit implements Table. Only darwin's kqueue and linux's pidfds are wired; anywhere else the
// reap fails loudly rather than detach under a process it could not see exit.
func (OSTable) AwaitExit([]int, time.Duration) ([]int, error) {
	return nil, fmt.Errorf("waiting for a process's exit is not implemented on %s", runtime.GOOS)
}
