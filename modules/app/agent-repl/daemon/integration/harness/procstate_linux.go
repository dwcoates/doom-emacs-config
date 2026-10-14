package harness

import (
	"agent-repld/internal/procgroup"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"
	"strings"
)

// readProcessState reads pid's state from /proc (procgroup.ReadState).
func readProcessState(pid int) (processState, error) {
	st, err := procgroup.ReadState(pid)
	return fromState(st), err
}

// threadsHalted reports whether every thread of a stopped pid is stopped or
// dead, and the state of one that is not. A process's state letter is its
// leader thread's; another thread may still run on until the stop reaches it.
func threadsHalted(pid int) (bool, string, error) {
	tasks, err := os.ReadDir("/proc/" + strconv.Itoa(pid) + "/task")
	if errors.Is(err, fs.ErrNotExist) {
		return true, "gone", nil
	}
	if err != nil {
		return false, "", fmt.Errorf("list the threads of process %d: %w", pid, err)
	}
	for _, task := range tasks {
		raw, err := os.ReadFile("/proc/" + strconv.Itoa(pid) + "/task/" + task.Name() + "/stat")
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return false, "", err
		}
		stat := string(raw)
		end := strings.LastIndexByte(stat, ')')
		fields := strings.Fields(stat[end+1:])
		if end < 0 || len(fields) == 0 {
			return false, "", fmt.Errorf("unparseable /proc/%d/task/%s/stat %q", pid, task.Name(), stat)
		}
		switch fields[0] {
		case "T", "t", "Z", "X", "x":
		default:
			return false, "stopping (thread " + task.Name() + " in state " + fields[0] + ")", nil
		}
	}
	return true, "", nil
}
