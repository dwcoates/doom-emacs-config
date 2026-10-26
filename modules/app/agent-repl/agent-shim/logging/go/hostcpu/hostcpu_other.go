//go:build !darwin && !linux

package hostcpu

import (
	"fmt"
	"runtime"
)

// Read refuses: no reader of the host's CPU times exists on this platform.
func Read() (Times, error) {
	return Times{}, fmt.Errorf("hostcpu: no reader of the host's CPU times on %s", runtime.GOOS)
}
