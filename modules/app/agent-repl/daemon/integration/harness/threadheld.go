package harness

// linuxThread is one thread of a process as /proc/<pid>/task/<tid>/stat
// gives it: its id and its state letter.
type linuxThread struct {
	task  string
	state string
}

// linuxThreadsHeld classifies the threads of a stopped process: halted when
// every one is stopped or dead, and otherwise the state of the first that is
// not, and whether every thread still to take the stop is runnable (R) and so
// awaits only the CPU. A thread asleep (S) or in an uninterruptible wait (D)
// takes its stop only when the kernel wakes it, which is not decided.
func linuxThreadsHeld(threads []linuxThread) (halted, awaitingCPU bool, name string) {
	awaitingCPU = true
	for _, th := range threads {
		switch th.state {
		case "T", "t", "Z", "X", "x":
			continue
		case "R":
		default:
			awaitingCPU = false
		}
		if name == "" {
			name = "stopping (thread " + th.task + " in state " + th.state + ")"
		}
	}
	if name == "" {
		return true, false, ""
	}
	return false, awaitingCPU, name
}
