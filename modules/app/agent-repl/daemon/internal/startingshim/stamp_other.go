//go:build !darwin && !linux

package startingshim

import "fmt"

// startStamp has no kernel to ask on this platform, so no spawn can be
// recorded or recognized here.
func startStamp(pid int) (int64, bool, error) {
	return 0, false, fmt.Errorf("startingshim: no start stamp of pid %d can be read on this platform", pid)
}
