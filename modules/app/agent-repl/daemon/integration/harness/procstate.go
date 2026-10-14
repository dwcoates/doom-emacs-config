package harness

import (
	"agent-repld/internal/procgroup"
	"fmt"
	"syscall"
	"time"
)

// processState is the kernel's own account of whether a process can still run
// an instruction.
type processState struct {
	// frozen is true when the process is stopped, a zombie, or gone: in none of
	// those can it observe anything or write a record.
	frozen bool
	// exited is true when the process is a zombie or gone: it has ended, and
	// only its reaping (or nothing at all) is left. A stopped process is
	// frozen but has NOT exited -- it is still a live process a test owns.
	exited bool
	// name is the kernel's state, for the report when it is not frozen.
	name string
}

// freezeBound bounds the wait for the kernel to report a SIGSTOPped process
// stopped. SIGSTOP cannot be caught, blocked or ignored, and the kernel
// suspends the task as it delivers it, so like reapGrace this covers only the
// scheduling of an already-decided stop, never a process's cooperation.
const freezeBound = reapGrace

// freezePoll is how often awaitFrozen re-reads the state. The first read
// almost always already answers stopped: the loop exists for a kernel that
// takes a moment, not as the expected path.
const freezePoll = time.Millisecond

// awaitFrozen returns once the kernel reports pid unable to run, or an error
// naming the state it was still in when bound ran out.
func awaitFrozen(pid int, bound time.Duration) error {
	deadline := time.NewTimer(bound)
	defer deadline.Stop()
	poll := time.NewTicker(freezePoll)
	defer poll.Stop()
	for {
		state, err := heldState(pid)
		if err != nil {
			return err
		}
		if state.frozen {
			return nil
		}
		select {
		case <-deadline.C:
			return fmt.Errorf("process %d was still %s %s after SIGSTOP", pid, state.name, bound)
		case <-poll.C:
		}
	}
}

// heldState reads pid as a wait for a stop to hold reads it: frozen only once
// the kernel reports it stopped (or exited) AND none of its threads still
// runs.
//
// A STOPPED p_stat IS NOT YET A STOPPED PROCESS. The kernel marks the process
// stopped as it posts SIGSTOP, before its threads are suspended: a thread
// already running finishes its slice first. Under a loaded host a daemon read
// "stopped" went on to read a frame from its shim, log it and close a turn
// (TestTheTakeoversOrphanRecoveryReconcilesItsOpenTurn). The thread check is
// made here, in the wait, and not in readProcessState: a thread the kernel
// wakes in a truly stopped process (a child's exit) reads as running for an
// instant, which a one-shot read would misreport and a wait polls past.
func heldState(pid int) (processState, error) {
	state, err := readProcessState(pid)
	if err != nil {
		return processState{}, fmt.Errorf("read the state of process %d: %w", pid, err)
	}
	if !state.frozen || state.exited {
		return state, nil
	}
	halted, name, err := threadsHalted(pid)
	if err != nil {
		return processState{}, err
	}
	if !halted {
		return processState{name: name}, nil
	}
	return state, nil
}

// stopResendEvery is how often stopFrozen re-sends SIGSTOP to a process that
// is not yet frozen.
const stopResendEvery = 5 * time.Millisecond

// stopFrozenReportAfter is how long a stop may take to hold before stopFrozen
// reports it. A stop that is re-sent until it holds is decided, as an
// accepted SIGKILL's exit is, and only its scheduling is left: that ordinarily
// takes milliseconds, but a test's processes run at background priority, and
// on a loaded host a shim's runnable thread waited past reapGrace for the CPU
// it needed to take its stop (2026-10-14: TestBashDetachedStartAndComplete and
// TestDisplacedTurnCapturedEndedThenResubmittedExactlyOnce, both at the same
// instant, each a shim with "1 thread(s) still running" 2s after SIGSTOP). As
// leaderExitReportAfter, it is fifteen times reapGrace, so it reports a stuck
// stop, never a starved one. It does NOT end the wait.
const stopFrozenReportAfter = 15 * reapGrace

// stopFrozen SIGSTOPs pid and returns once the kernel reports it unable to
// run. It waits UNBOUNDED, calling stalled once, with the state pid was still
// in, if the stop has not held within reportAfter; it answers only a send or
// a read that failed.
//
// THE STOP IS RE-SENT UNTIL IT HOLDS. On Darwin a SIGSTOP that reaches a
// process inside execve is DISCARDED when the exec completes (see Kill), so
// a stray caught between its fork and the end of its exec -- a daemon's
// freshly spawned shim or successor -- could run on through one stop for as
// long as it liked, and the sweep then failed its bound on a process that had
// done nothing wrong. A stop to a process already stopped changes nothing,
// so re-sending it every stopResendEvery until the kernel reports the
// process frozen is safe, and the first send after the exec completes holds.
// signal is the send, a seam for the test that drops the first stop.
func stopFrozen(pid int, reportAfter time.Duration, signal func(int, syscall.Signal) error, stalled func(pid int, state string, waited time.Duration)) error {
	if err := signal(pid, syscall.SIGSTOP); err != nil {
		return err
	}
	report := time.NewTimer(reportAfter)
	defer report.Stop()
	poll := time.NewTicker(freezePoll)
	defer poll.Stop()
	lastSent := time.Now()
	for {
		state, err := heldState(pid)
		if err != nil {
			return err
		}
		if state.frozen {
			return nil
		}
		if time.Since(lastSent) >= stopResendEvery {
			if err := signal(pid, syscall.SIGSTOP); err != nil {
				return err
			}
			lastSent = time.Now()
		}
		select {
		case <-report.C:
			stalled(pid, state.name, reportAfter)
		case <-poll.C:
		}
	}
}

// fromState is the kernel's account as the harness reads it.
func fromState(s procgroup.State) processState {
	return processState{frozen: s.Frozen, exited: s.Exited, name: s.Name}
}
