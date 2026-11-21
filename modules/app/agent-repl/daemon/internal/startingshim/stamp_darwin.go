//go:build darwin

package startingshim

import (
	"fmt"

	"golang.org/x/sys/unix"
)

// startStamp reads the kernel's start stamp of PID, false when no process
// holds the pid. It is sysctl kern.proc.pid, which answers for every process
// on the host whoever owns it -- proc_pidinfo answers nothing for a process
// of another user, which would read a reused pid's root daemon as gone for
// the wrong reason.
func startStamp(pid int) (int64, bool, error) {
	procs, err := unix.SysctlKinfoProcSlice("kern.proc.pid", pid)
	if err != nil {
		return 0, false, fmt.Errorf("startingshim: read the start stamp of pid %d: %w", pid, err)
	}
	if len(procs) == 0 {
		return 0, false, nil
	}
	start := procs[0].Proc.P_starttime
	return start.Sec*1e6 + int64(start.Usec), true, nil
}
