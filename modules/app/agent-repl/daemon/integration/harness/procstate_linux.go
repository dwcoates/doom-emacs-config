package harness

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"
	"strings"
)

// readProcessState reads pid's state letter from /proc/<pid>/stat.
func readProcessState(pid int) (processState, error) {
	raw, err := os.ReadFile("/proc/" + strconv.Itoa(pid) + "/stat")
	if errors.Is(err, fs.ErrNotExist) {
		return processState{frozen: true, exited: true, name: "gone"}, nil
	}
	if err != nil {
		return processState{}, err
	}
	// The state follows the parenthesized command name, which may itself hold
	// spaces or parentheses, so it is found after the LAST ')'.
	stat := string(raw)
	end := strings.LastIndexByte(stat, ')')
	fields := strings.Fields(stat[end+1:])
	if end < 0 || len(fields) == 0 {
		return processState{}, fmt.Errorf("unparseable /proc/%d/stat %q", pid, stat)
	}
	switch letter := fields[0]; letter {
	case "T", "t":
		return processState{frozen: true, name: "stopped"}, nil
	case "Z", "X", "x":
		return processState{frozen: true, exited: true, name: "dead"}, nil
	default:
		return processState{name: "in state " + letter}, nil
	}
}

// groupExited reports whether every process in process group pgid has
// exited: a group of zombies, or no group at all.
func groupExited(pgid int) (bool, error) {
	entries, err := os.ReadDir("/proc")
	if err != nil {
		return false, fmt.Errorf("list /proc: %w", err)
	}
	for _, e := range entries {
		pid, err := strconv.Atoi(e.Name())
		if err != nil {
			continue
		}
		raw, err := os.ReadFile("/proc/" + e.Name() + "/stat")
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			return false, err
		}
		// The state, ppid and pgrp follow the parenthesized command name.
		stat := string(raw)
		end := strings.LastIndexByte(stat, ')')
		fields := strings.Fields(stat[end+1:])
		if end < 0 || len(fields) < 3 {
			return false, fmt.Errorf("unparseable /proc/%d/stat %q", pid, stat)
		}
		if fields[2] != strconv.Itoa(pgid) {
			continue
		}
		switch fields[0] {
		case "Z", "X", "x":
		default:
			return false, nil
		}
	}
	return true, nil
}
