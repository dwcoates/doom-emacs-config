package harness

import (
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
		state, err := readProcessState(pid)
		if err != nil {
			return fmt.Errorf("read the state of process %d: %w", pid, err)
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

// stopResendEvery is how often stopFrozen re-sends SIGSTOP to a process that
// is not yet frozen.
const stopResendEvery = 5 * time.Millisecond

// stopFrozen SIGSTOPs pid and returns once the kernel reports it unable to
// run, or an error naming the state it was still in when bound ran out.
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
func stopFrozen(pid int, bound time.Duration, signal func(int, syscall.Signal) error) error {
	if err := signal(pid, syscall.SIGSTOP); err != nil {
		return err
	}
	deadline := time.NewTimer(bound)
	defer deadline.Stop()
	poll := time.NewTicker(freezePoll)
	defer poll.Stop()
	lastSent := time.Now()
	for {
		state, err := readProcessState(pid)
		if err != nil {
			return fmt.Errorf("read the state of process %d: %w", pid, err)
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
		case <-deadline.C:
			return fmt.Errorf("process %d was still %s %s after SIGSTOP", pid, state.name, bound)
		case <-poll.C:
		}
	}
}

// groupMember is one process of a process group, as the kernel lists it.
type groupMember struct {
	pid   int
	comm  string
	state string
}
