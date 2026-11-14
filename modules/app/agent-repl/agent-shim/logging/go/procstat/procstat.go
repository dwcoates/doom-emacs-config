// Package procstat parses the files of Linux's /proc process table that more
// than one agent-repl program reads: the daemon's process groups and metrics,
// its integration harness, and the test runner's process table all read
// /proc/<pid>/stat and /proc/<pid>/cmdline through it, so the parenthesized
// command name is split one way everywhere.
package procstat

import (
	"fmt"
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

// Field answers proc(5)'s field n (1-based, so 3 is the state), refusing a
// line too short to hold it.
func (l StatLine) Field(n int) (string, error) {
	if n < 3 || n-3 >= len(l.Fields) {
		return "", fmt.Errorf("the stat line of %q has no field %d", l.Comm, n)
	}
	return l.Fields[n-3], nil
}

// ParseCmdline decodes /proc/<pid>/cmdline: argv, each argument
// NUL-terminated; a process that rewrote its title pads it with more NULs.
// A kernel thread's, or a zombie's, is empty: no argv.
func ParseCmdline(raw []byte) []string {
	trimmed := strings.TrimRight(string(raw), "\x00")
	if trimmed == "" {
		return []string{}
	}
	return strings.Split(trimmed, "\x00")
}
