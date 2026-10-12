package harness

import (
	"errors"
	"fmt"
	"os"
	"time"

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
		buf, live, err := procArgs(pid)
		if err != nil {
			return nil, err
		}
		if !live {
			continue
		}
		args, err := parseProcArgs2(buf)
		if err != nil {
			return nil, fmt.Errorf("decode the argv of pid %d: %w", pid, err)
		}
		entries = append(entries, procEntry{pid: pid, args: args})
	}
	return entries, nil
}

// procArgs reads pid's kern.procargs2, answering live=false for a process that
// has exited (gone, a zombie, or exiting unreaped): it can no longer run, so
// it is no stray, and `ps` lists it with no argv either.
//
// A LIVE PROCESS WITH NO ARGV YET IS WAITED FOR, NEVER SKIPPED. Between a
// fork and the end of its exec the kernel answers EINVAL, EIO or ESRCH for a
// process that is still running, and a listing that skipped it would let a
// leak check pass over a stray that is very much alive. The argv appears the
// moment the exec completes, so the read is retried on the kernel's state
// every freezePoll until it is readable or the process has exited, within
// freezeBound; a process still unreadable after that is an error.
func procArgs(pid int) ([]byte, bool, error) {
	deadline := time.NewTimer(freezeBound)
	defer deadline.Stop()
	poll := time.NewTicker(freezePoll)
	defer poll.Stop()
	for {
		buf, err := unix.SysctlRaw("kern.procargs2", pid)
		if err == nil {
			return buf, true, nil
		}
		if !errors.Is(err, unix.EINVAL) && !errors.Is(err, unix.EIO) && !errors.Is(err, unix.ESRCH) {
			return nil, false, fmt.Errorf("read the argv of pid %d: %w", pid, err)
		}
		state, stateErr := readProcessState(pid)
		if stateErr != nil {
			return nil, false, fmt.Errorf("read the state of pid %d, whose argv is unreadable (%v): %w", pid, err, stateErr)
		}
		if state.exited {
			return nil, false, nil
		}
		select {
		case <-deadline.C:
			return nil, false, fmt.Errorf("pid %d is %s but its argv stayed unreadable for %s: %w", pid, state.name, freezeBound, err)
		case <-poll.C:
		}
	}
}
