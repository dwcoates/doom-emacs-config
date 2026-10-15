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
		args, live, err := procArgs(pid, readProcArgs2, readProcessState)
		if err != nil {
			return nil, fmt.Errorf("%w (%s)", err, kinfoIdentity(p))
		}
		if !live {
			continue
		}
		entries = append(entries, procEntry{pid: pid, args: args})
	}
	return entries, nil
}

// readProcArgs2 reads pid's raw kern.procargs2 buffer.
func readProcArgs2(pid int) ([]byte, error) { return unix.SysctlRaw("kern.procargs2", pid) }

// procArgs answers pid's decoded argv, answering live=false for a process
// that has exited (gone, a zombie, or exiting unreaped): it can no longer
// run, so it is no stray, and `ps` lists it with no argv either.
//
// A LIVE PROCESS WITH NO ARGV YET IS WAITED FOR, NEVER SKIPPED. Between a
// fork and the end of its exec the kernel answers EINVAL, EIO or ESRCH for a
// process that is still running -- or answers a buffer the exec has not
// finished writing, whose arguments do not decode yet (seen under a 12-slot
// full run: "procargs2 argument 0 of 3 is unterminated"). A listing that
// skipped it, or failed on it, would let a leak check pass over a stray that
// is very much alive. The argv appears the moment the exec completes, so the
// read is retried on the kernel's state every freezePoll until it decodes or
// the process has exited, within freezeBound; a process still unreadable
// after that is an error.
func procArgs(pid int, read func(int) ([]byte, error), state func(int) (processState, error)) (string, bool, error) {
	deadline := time.NewTimer(freezeBound)
	defer deadline.Stop()
	poll := time.NewTicker(freezePoll)
	defer poll.Stop()
	for {
		buf, err := read(pid)
		if err == nil {
			args, decodeErr := parseProcArgs2(buf)
			if decodeErr == nil {
				return args, true, nil
			}
			err = decodeErr
		} else if !errors.Is(err, unix.EINVAL) && !errors.Is(err, unix.EIO) && !errors.Is(err, unix.ESRCH) {
			return "", false, fmt.Errorf("read the argv of pid %d: %w", pid, err)
		}
		st, stateErr := state(pid)
		if stateErr != nil {
			return "", false, fmt.Errorf("read the state of pid %d, whose argv is unreadable (%v): %w", pid, err, stateErr)
		}
		if st.exited {
			return "", false, nil
		}
		select {
		case <-deadline.C:
			return "", false, fmt.Errorf("pid %d is %s but its argv stayed unreadable for %s: %w", pid, st.name, freezeBound, err)
		case <-poll.C:
		}
	}
}

// kinfoIdentity names a process from the process table alone, for an error
// about a process whose argv cannot be read: its command name, parent, start
// time and kernel flags (P_SUGID among them) are what identify it then.
func kinfoIdentity(p unix.KinfoProc) string {
	comm := unix.ByteSliceToString(p.Proc.P_comm[:])
	start := time.Unix(p.Proc.P_starttime.Sec, int64(p.Proc.P_starttime.Usec)*1000)
	return fmt.Sprintf("comm=%q ppid=%d started=%s flags=%#x", comm, p.Eproc.Ppid, start.Format(time.RFC3339Nano), p.Proc.P_flag)
}
