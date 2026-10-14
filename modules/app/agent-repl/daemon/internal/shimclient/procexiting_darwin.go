//go:build darwin

package shimclient

import (
	"errors"
	"fmt"

	"golang.org/x/sys/unix"
)

// XNU's process state and flag (bsd/sys/proc.h), which golang.org/x/sys does
// not export: SZOMB is a process that has exited and awaits its reap, and
// P_WEXIT is set as exit1 begins, before the process's descriptors close.
const (
	darwinSZOMB  = 5
	darwinPWEXIT = 0x00002000
)

// procExiting answers whether pid's process has begun exiting: marked
// exiting, a zombie, or gone. kern.proc.pid answers no record for a pid with
// no process, which golang.org/x/sys reports as EIO.
func procExiting(pid int) (bool, error) {
	kp, err := unix.SysctlKinfoProc("kern.proc.pid", pid)
	if errors.Is(err, unix.EIO) {
		return true, nil
	}
	if err != nil {
		return false, fmt.Errorf("sysctl kern.proc.pid %d: %w", pid, err)
	}
	return kp.Proc.P_stat == darwinSZOMB || kp.Proc.P_flag&darwinPWEXIT != 0, nil
}
