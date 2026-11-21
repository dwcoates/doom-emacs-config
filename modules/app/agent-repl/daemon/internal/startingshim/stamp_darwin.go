//go:build darwin

package startingshim

import (
	"fmt"

	"agent-repld/internal/procgroup"
)

// startStamp reads the kernel's start stamp of PID, false when no process
// holds the pid. The reading is kern.proc.pid through procgroup, which answers
// for every process on the host whoever owns it -- proc_pidinfo answers
// nothing for a process of another user, which would read a reused pid's root
// daemon as gone for the wrong reason.
func startStamp(pid int) (int64, bool, error) {
	stamp, found, err := procgroup.StartStamp(pid)
	if err != nil {
		return 0, false, fmt.Errorf("startingshim: read the start stamp of pid %d: %w", pid, err)
	}
	return stamp, found, nil
}
