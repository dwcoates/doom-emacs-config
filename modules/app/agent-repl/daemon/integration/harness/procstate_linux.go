package harness

import (
	"agent-repld/internal/procgroup"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"
)

// readProcessState reads pid's state from /proc (procgroup.ReadState).
func readProcessState(pid int) (processState, error) {
	st, err := procgroup.ReadState(pid)
	return fromState(st), err
}

// threadsHalted reports whether every thread of a stopped pid is stopped or
// dead, and the state of one that is not. A process's state letter is its
// leader thread's; another thread may still run on until the stop reaches it.
func threadsHalted(pid int) (halted, awaitingCPU bool, name string, err error) {
	tasks, err := os.ReadDir("/proc/" + strconv.Itoa(pid) + "/task")
	if errors.Is(err, fs.ErrNotExist) {
		return true, false, "gone", nil
	}
	if err != nil {
		return false, false, "", fmt.Errorf("list the threads of process %d: %w", pid, err)
	}
	var threads []linuxThread
	for _, task := range tasks {
		raw, err := os.ReadFile("/proc/" + strconv.Itoa(pid) + "/task/" + task.Name() + "/stat")
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return false, false, "", err
		}
		line, err := procgroup.ParseStat(string(raw))
		if err != nil {
			return false, false, "", fmt.Errorf("unparseable /proc/%d/task/%s/stat: %w", pid, task.Name(), err)
		}
		threads = append(threads, linuxThread{task: task.Name(), state: line.Fields[0]})
	}
	halted, awaitingCPU, name = linuxThreadsHeld(threads)
	return halted, awaitingCPU, name, nil
}
