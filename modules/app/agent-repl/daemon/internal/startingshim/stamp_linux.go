//go:build linux

package startingshim

import (
	"errors"
	"fmt"

	"agent-repld/internal/procmetrics"
)

// startStamp reads the kernel's start stamp of PID, false when no process
// holds the pid. It is /proc/<pid>/stat, read through procmetrics, which
// every user may read.
func startStamp(pid int) (int64, bool, error) {
	id, err := procmetrics.Kernel{}.Identity(pid)
	if errors.Is(err, procmetrics.ErrGone) {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, fmt.Errorf("startingshim: read the start stamp of pid %d: %w", pid, err)
	}
	return id.StartUnixMicros, true, nil
}
