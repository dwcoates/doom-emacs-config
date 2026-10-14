package procgroup

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strconv"
)

// readStat reads pid's /proc/<pid>/stat line, answering gone for a pid with
// no process.
func readStat(pid int) (stat string, gone bool, err error) {
	raw, err := os.ReadFile("/proc/" + strconv.Itoa(pid) + "/stat")
	if errors.Is(err, fs.ErrNotExist) {
		return "", true, nil
	}
	if err != nil {
		return "", false, fmt.Errorf("read /proc/%d/stat: %w", pid, err)
	}
	return string(raw), false, nil
}

// ReadState reads pid's state letter from /proc/<pid>/stat.
func ReadState(pid int) (State, error) {
	raw, gone, err := readStat(pid)
	if gone {
		return State{Frozen: true, Exited: true, Name: "gone"}, nil
	}
	if err != nil {
		return State{}, err
	}
	line, err := ParseStat(raw)
	if err != nil {
		return State{}, fmt.Errorf("unparseable /proc/%d/stat: %w", pid, err)
	}
	switch letter := line.Fields[0]; letter {
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
		line, err := ParseStat(string(raw))
		if err != nil {
			return nil, fmt.Errorf("unparseable /proc/%d/stat: %w", pid, err)
		}
		if len(line.Fields) < 3 {
			return nil, fmt.Errorf("unparseable /proc/%d/stat %q: no process group", pid, raw)
		}
		if line.Fields[2] != strconv.Itoa(pgid) {
			continue
		}
		switch line.Fields[0] {
		case "Z", "X", "x":
		default:
			live = append(live, Member{Pid: pid, Comm: line.Comm, State: "in state " + line.Fields[0]})
		}
	}
	return live, nil
}

// Exiting answers whether pid's process has begun exiting: PF_EXITING, a
// zombie or dead state, or gone, read from /proc/<pid>/stat.
func Exiting(pid int) (bool, error) {
	raw, gone, err := readStat(pid)
	if gone {
		return true, nil
	}
	if err != nil {
		return false, err
	}
	return parseStatExiting(raw)
}
