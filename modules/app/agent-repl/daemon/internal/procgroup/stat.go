package procgroup

import (
	"fmt"
	"strconv"
	"strings"
)

// StatLine is one line of /proc/<pid>/stat or /proc/<pid>/task/<tid>/stat
// (proc(5)), split at its command name.
type StatLine struct {
	// Comm is the command name, without its parentheses.
	Comm string
	// Fields are the fields after the command name: Fields[0] is the state
	// (proc(5) field 3), and Fields[i] is field i+3.
	Fields []string
}

// ParseStat splits a /proc stat line at its command name. The name is
// parenthesized and may itself hold spaces or parentheses, so it runs from the
// first '(' to the LAST ')'. A line with no command name, or with no state
// after it, is refused.
func ParseStat(stat string) (StatLine, error) {
	start := strings.IndexByte(stat, '(')
	end := strings.LastIndexByte(stat, ')')
	if start < 0 || end < start {
		return StatLine{}, fmt.Errorf("the stat line has no command name: %q", stat)
	}
	fields := strings.Fields(stat[end+1:])
	if len(fields) == 0 {
		return StatLine{}, fmt.Errorf("the stat line has no state after its command name: %q", stat)
	}
	return StatLine{Comm: stat[start+1 : end], Fields: fields}, nil
}

// linuxPFExiting is PF_EXITING (include/linux/sched.h), set as do_exit
// begins, before the process's descriptors close.
const linuxPFExiting = 0x4

// parseStatExiting reads the state (field 3) and flags (field 9) of one
// /proc/<pid>/stat line: exiting when PF_EXITING is set or the state is a
// zombie or dead one.
func parseStatExiting(stat string) (bool, error) {
	line, err := ParseStat(stat)
	if err != nil {
		return false, err
	}
	fields := line.Fields
	if len(fields) < 7 {
		return false, fmt.Errorf("/proc stat line is short: %q", stat)
	}
	state := fields[0]
	flags, err := strconv.ParseUint(fields[6], 10, 64)
	if err != nil {
		return false, fmt.Errorf("/proc stat flags %q: %w", fields[6], err)
	}
	return state == "Z" || state == "X" || flags&linuxPFExiting != 0, nil
}
