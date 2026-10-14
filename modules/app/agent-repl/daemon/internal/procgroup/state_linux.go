package procgroup

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"
	"strings"
)

// ReadState reads pid's state letter from /proc/<pid>/stat.
func ReadState(pid int) (State, error) {
	raw, err := os.ReadFile("/proc/" + strconv.Itoa(pid) + "/stat")
	if errors.Is(err, fs.ErrNotExist) {
		return State{Frozen: true, Exited: true, Name: "gone"}, nil
	}
	if err != nil {
		return State{}, err
	}
	// The state follows the parenthesized command name, which may itself hold
	// spaces or parentheses, so it is found after the LAST ')'.
	stat := string(raw)
	end := strings.LastIndexByte(stat, ')')
	fields := strings.Fields(stat[end+1:])
	if end < 0 || len(fields) == 0 {
		return State{}, fmt.Errorf("unparseable /proc/%d/stat %q", pid, stat)
	}
	switch letter := fields[0]; letter {
	case "T", "t":
		return State{Frozen: true, Name: "stopped"}, nil
	case "Z", "X", "x":
		return State{Frozen: true, Exited: true, Name: "dead"}, nil
	default:
		return State{Name: "in state " + letter}, nil
	}
}

// LiveMembers answers the members of process group pgid that have not
// exited: everything in the group but its zombies and dead entries.
func LiveMembers(pgid int) ([]Member, error) {
	entries, err := os.ReadDir("/proc")
	if err != nil {
		return nil, fmt.Errorf("list /proc: %w", err)
	}
	var live []Member
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
			return nil, err
		}
		// The state, ppid and pgrp follow the parenthesized command name.
		stat := string(raw)
		start := strings.IndexByte(stat, '(')
		end := strings.LastIndexByte(stat, ')')
		if start < 0 || end < start {
			return nil, fmt.Errorf("unparseable /proc/%d/stat %q", pid, stat)
		}
		fields := strings.Fields(stat[end+1:])
		if len(fields) < 3 {
			return nil, fmt.Errorf("unparseable /proc/%d/stat %q", pid, stat)
		}
		if fields[2] != strconv.Itoa(pgid) {
			continue
		}
		switch fields[0] {
		case "Z", "X", "x":
		default:
			live = append(live, Member{Pid: pid, Comm: stat[start+1 : end], State: "in state " + fields[0]})
		}
	}
	return live, nil
}
