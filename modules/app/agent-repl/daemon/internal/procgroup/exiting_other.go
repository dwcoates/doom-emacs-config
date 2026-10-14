//go:build !linux && !darwin

package procgroup

import (
	"fmt"
	"runtime"
)

// Exiting REFUSES on a platform whose process state this package has not
// been taught to read, so a break is never attributed to a death unread.
func Exiting(int) (bool, error) {
	return false, fmt.Errorf("no process-state implementation for GOOS %s", runtime.GOOS)
}
