//go:build !darwin && !linux

package netwatch

import (
	"fmt"
	"runtime"
)

// OpenSystem refuses: this platform has no network-event source here.
func OpenSystem() (Source, error) {
	return nil, fmt.Errorf("netwatch: no network-event source for %s", runtime.GOOS)
}
