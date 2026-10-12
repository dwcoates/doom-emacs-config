package harness

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"
)

// processTable answers every live process visible under /proc, with its argv.
// A process that exits between the directory read and its cmdline read is
// dropped, as `ps` drops it: it can no longer run, so it is no stray.
func processTable() ([]procEntry, error) {
	dirs, err := os.ReadDir("/proc")
	if err != nil {
		return nil, fmt.Errorf("read the process table: %w", err)
	}
	var entries []procEntry
	for _, d := range dirs {
		pid, err := strconv.Atoi(d.Name())
		if err != nil {
			continue
		}
		raw, err := os.ReadFile("/proc/" + d.Name() + "/cmdline")
		switch {
		case errors.Is(err, fs.ErrNotExist), errors.Is(err, os.ErrProcessDone):
			continue
		case err != nil:
			return nil, fmt.Errorf("read the argv of pid %d: %w", pid, err)
		}
		// A kernel thread or a zombie has an empty cmdline: no argv can name
		// the state directory.
		if len(raw) == 0 {
			continue
		}
		entries = append(entries, procEntry{pid: pid, args: parseProcCmdline(raw)})
	}
	return entries, nil
}
