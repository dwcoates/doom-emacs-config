package procgroup

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
