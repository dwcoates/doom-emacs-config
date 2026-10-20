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
	parents := make(map[int]int, len(procs))
	for _, p := range procs {
		parents[int(p.Proc.P_pid)] = int(p.Eproc.Ppid)
	}
	self := os.Getpid()
	entries := make([]procEntry, 0, len(procs))
	for _, p := range procs {
		pid := int(p.Proc.P_pid)
		if !mayBeThisRunsStray(pid, parents, self) {
			// ANOTHER RUN'S LIVE PROCESS IS READ ONCE, NEVER WAITED ON. Its argv
			// is listed when it reads, so nothing a listing could name is
			// lost, and one caught between its fork and its exec is passed
			// over: it cannot be this run's.
			if args, ok := readArgsOnce(pid); ok {
				entries = append(entries, procEntry{pid: pid, args: args})
			}
			continue
		}
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

// launchdPID is the parent of every orphan.
const launchdPID = 1

// mayBeThisRunsStray reports whether pid can be a process this test binary
// left behind, and so is worth waiting on while its argv is unreadable: a
// descendant of this binary (self), or one whose parent is launchd, gone or
// unknown, which is what a stray is once the daemon that spawned it has exited
// (its parent can read as the exited pid for a moment before launchd adopts
// it). A process whose parent chain is live and never reaches self belongs to
// another test run.
//
// WAITING ON ANOTHER RUN'S PROCESS HELD THIS RUN'S LISTING HOSTAGE TO THAT
// RUN'S LOAD: another run's test binary sat between fork and exec past the
// wait's bound, and every listing here failed (2026-10-20, `008.test`, its
// parent alive).
func mayBeThisRunsStray(pid int, parents map[int]int, self int) bool {
	for at, hops := pid, 0; hops <= len(parents); hops++ {
		if at == self {
			return true
		}
		parent, known := parents[at]
		if !known {
			// Only pid itself can be missing: every later step is a parent
			// the check below found in the table.
			return false
		}
		if _, live := parents[parent]; !live || parent == launchdPID || parent == 0 || parent == at {
			// The chain ends here: at pid itself, it is an orphan or a
			// process whose parent just exited (or is another user's), so it
			// may be a stray of this run; above it, at a live process that is
			// not this binary.
			return at == pid
		}
		at = parent
	}
	return false
}

// readArgsOnce reads pid's argv once, answering ok=false when it cannot be
// read or decoded now.
func readArgsOnce(pid int) (string, bool) {
	buf, err := readProcArgs2(pid)
	if err != nil {
		return "", false
	}
	args, err := parseProcArgs2(buf)
	return args, err == nil
}

// readProcArgs2 reads pid's raw kern.procargs2 buffer.
func readProcArgs2(pid int) ([]byte, error) { return unix.SysctlRaw("kern.procargs2", pid) }

// procArgs answers pid's decoded argv, answering live=false for a process
// that has exited (gone, a zombie, or exiting unreaped): it can no longer
// run, so it is no stray, and `ps` lists it with no argv either. A process
// stopped before its argv was readable answers live=false too (below).
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
		// A STOPPED PROCESS WITH NO ARGV IS NO STRAY OF THIS TEST'S. It was
		// stopped between its fork and the end of its exec, so its argv never
		// appears while it stays stopped, and it can run nothing. This harness
		// stops only processes whose argv it has already read, so another
		// harness stopped it: a concurrent run's reaper freezing its own
		// strays. Waiting on it failed every listing of every test that ran
		// beside that freeze (2026-10-19, a `005.test` of another worktree).
		if st.frozen {
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
