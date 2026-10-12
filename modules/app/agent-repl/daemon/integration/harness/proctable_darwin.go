package harness

import (
	"errors"
	"fmt"
	"os"

	"golang.org/x/sys/unix"
)

// processTable answers every live process this user owns, with its argv.
//
// ONLY THIS USER'S PROCESSES ARE LISTED: every stray a test can leave was
// spawned by the test binary or by a process it started, all under this uid.
// A process that exits between the table read and its argv read is dropped,
// as `ps` drops it: it can no longer run, so it is no stray.
func processTable() ([]procEntry, error) {
	procs, err := unix.SysctlKinfoProcSlice("kern.proc.uid", os.Getuid())
	if err != nil {
		return nil, fmt.Errorf("read the process table: %w", err)
	}
	entries := make([]procEntry, 0, len(procs))
	for _, p := range procs {
		pid := int(p.Proc.P_pid)
		buf, err := unix.SysctlRaw("kern.procargs2", pid)
		switch {
		// THE KERNEL HAS NO ARGV FOR THIS PROCESS RIGHT NOW: it exited, is a
		// zombie, is exiting unreaped, or is inside execve (a fork that has
		// not yet become its new image). It answers EINVAL, EIO or ESRCH, and
		// `ps` -- which reads this same sysctl -- then prints only the short
		// command name, which can never name a state directory. So the entry
		// is skipped exactly as `ps` would fail to match it: a process
		// mid-exec is caught by the sweep's next listing, once its argv exists
		// (freezeStrays re-lists until nothing new appears). Every other
		// errno is an error.
		case errors.Is(err, unix.EINVAL), errors.Is(err, unix.EIO), errors.Is(err, unix.ESRCH):
			continue
		case err != nil:
			return nil, fmt.Errorf("read the argv of pid %d: %w", pid, err)
		}
		args, err := parseProcArgs2(buf)
		if err != nil {
			return nil, fmt.Errorf("decode the argv of pid %d: %w", pid, err)
		}
		entries = append(entries, procEntry{pid: pid, args: args})
	}
	return entries, nil
}
