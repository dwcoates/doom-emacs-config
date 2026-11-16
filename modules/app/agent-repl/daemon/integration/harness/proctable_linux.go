package harness

import (
	"errors"
	"fmt"
	"strings"

	"agentrepl/logging/procstat"
)

// processTable answers every live process visible under /proc, with its argv.
// A process that exits between the directory read and its cmdline read is
// dropped, as `ps` drops it: it can no longer run, so it is no stray.
func processTable() ([]procEntry, error) {
	tree := procstat.New("/proc")
	pids, err := tree.PIDs()
	if err != nil {
		return nil, fmt.Errorf("read the process table: %w", err)
	}
	var entries []procEntry
	for _, pid := range pids {
		argv, err := tree.Cmdline(pid)
		switch {
		case errors.Is(err, procstat.ErrGone):
			continue
		case err != nil:
			return nil, fmt.Errorf("read the argv of pid %d: %w", pid, err)
		}
		// A kernel thread or a zombie has an empty cmdline: no argv can name
		// the state directory.
		if len(argv) == 0 {
			continue
		}
		entries = append(entries, procEntry{pid: pid, args: strings.Join(argv, " ")})
	}
	return entries, nil
}
