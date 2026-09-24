package harness

import (
	"fmt"
	"time"
)

// processState is the kernel's own account of whether a process can still run
// an instruction.
type processState struct {
	// frozen is true when the process is stopped, a zombie, or gone: in none of
	// those can it observe anything or write a record.
	frozen bool
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
