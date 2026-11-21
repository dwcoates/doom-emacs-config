//go:build !darwin

package ramdisk

import (
	"fmt"
	"runtime"
)

// WatchProcessExit is WatchExit. RAM disks are made on darwin alone
// (runParent), so anywhere else it fails loudly rather than watch nothing.
func WatchProcessExit(int) (ExitWatch, error) {
	return nil, fmt.Errorf("watching a process's exit is not implemented on %s", runtime.GOOS)
}
