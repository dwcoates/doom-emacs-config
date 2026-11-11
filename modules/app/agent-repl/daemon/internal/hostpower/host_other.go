//go:build !darwin && !linux

package hostpower

import (
	"fmt"
	"runtime"
)

// Power refuses: the queue works only its head.
func (*Host) Power() (Power, error) {
	return PowerUnknown, fmt.Errorf("hostpower: no host power source reader on %s", runtime.GOOS)
}
