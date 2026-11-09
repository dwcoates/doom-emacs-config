// Package seatwait is the ONE spelling of the runner's RAM disk seat lines:
// the line a run writes on its log each time its wait for a seat begins or
// changes, the line it writes when it takes one, and the parse the merge
// gate (daemon/internal/merge) reads them back with to draw the wait in the
// bubble's tests tab.
//
// THE FORMAT AND ITS PARSE LIVE TOGETHER, in a package both the runner and the
// daemon import, so the two cannot drift: a line the runner writes is a line
// the daemon reads, by construction rather than by two regexes kept in step.
package seatwait

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"
)

// EnvWorkspace names, to a test run the daemon starts, the workspace the run
// is for (its own tests, or its merge's gate): the name the run writes into
// the seat it holds, and the name a waiting run's line gives it.
const EnvWorkspace = "AGENT_REPL_TEST_WORKSPACE"

// Marker opens every RAM disk seat line, after the runner's own prefix.
const Marker = "RAM disk seat: "

// Reason is why a run waits for a seat. The runner checks them in this order:
// a better-ranked run waiting first, then the ceiling, then the memory.
type Reason string

const (
	// Outranked: a run of a better rank waits for a seat, and is granted first.
	Outranked Reason = "outranked"
	// Ceiling: every seat under the ceiling is held.
	Ceiling Reason = "ceiling"
	// Memory: a seat is free, but taking it would leave less memory available
	// than the floor once every seated disk filled.
	Memory Reason = "memory"
)

// explain is each reason as the log line states it for a person.
var explain = map[Reason]string{
	Outranked: "a run that ranks above this one waits first",
	Ceiling:   "every seat under the ceiling is held",
	Memory:    "taking a seat would leave less than the floor available",
}

// Holder is one run holding a seat: a workspace's run, by the workspace's
// name, or, when no workspace started it, by its runner's pid.
type Holder struct {
	// Workspace is the name of the workspace whose run holds the seat, empty
	// for a run started outside agent-repl.
	Workspace string
	// Pid is the holding runner's process id.
	Pid int
}

// String is the holder as a person reads it, and as the line spells it.
func (h Holder) String() string {
	if h.Workspace != "" {
		return "workspace " + strconv.Quote(h.Workspace)
	}
	return "pid " + strconv.Itoa(h.Pid)
}

// Reading is what a seat line states besides its event: who holds seats, and
// the memory figures the admission was decided on.
type Reading struct {
	Holders []Holder
	// AvailableMiB is the host's available memory at the reading.
	AvailableMiB uint64
	// WiredMiB is what every seated disk may wire together: the sum of the
	// held seats' disk sizes.
	WiredMiB uint64
	// CeilingMiB is the most every run's RAM disk together may wire.
	CeilingMiB uint64
	// FloorMiB is the memory that must stay available once every seated disk
	// has filled.
	FloorMiB uint64
	// Rank is the run's rank (zero is the best).
	Rank int
}

func (r Reading) String() string {
	holders := make([]string, len(r.Holders))
	for i, h := range r.Holders {
		holders[i] = h.String()
	}
	return fmt.Sprintf("holders=[%s]; available=%dMiB wired=%dMiB ceiling=%dMiB floor=%dMiB rank=%d",
		strings.Join(holders, ", "), r.AvailableMiB, r.WiredMiB, r.CeilingMiB, r.FloorMiB, r.Rank)
}

// Wait is one state of a run's wait for a seat.
type Wait struct {
	Reason Reason
	Reading
}

// Line is the wait's log line, without the runner's prefix.
func (w Wait) Line() string {
	why, known := explain[w.Reason]
	if !known {
		panic("seatwait: a wait with no reason the package defines: " + string(w.Reason))
	}
	return fmt.Sprintf("%swaiting, reason=%s (%s); %s", Marker, w.Reason, why, w.Reading)
}

// TookLine is the line a run writes when it takes seat i of n, after waiting
// waited (empty when it took one at once); r is read after the take, so its
// wired total counts the seat taken.
func TookLine(i, n int, waited string, r Reading) string {
	after := ""
	if waited != "" {
		after = " after " + waited
	}
	return fmt.Sprintf("%stook seat %d of %d%s; %s", Marker, i, n, after, r)
}

// FailedLine is the line a run writes when its wait for a seat ends without
// one: the seat could not be taken, so the run's root falls back to the disk.
func FailedLine(err error) string {
	return fmt.Sprintf("%sno seat was taken: %v", Marker, err)
}

var waitLine = regexp.MustCompile(regexp.QuoteMeta(Marker) +
	`waiting, reason=([a-z]+) \([^)]*\); holders=\[(.*)\]; available=(\d+)MiB wired=(\d+)MiB ceiling=(\d+)MiB floor=(\d+)MiB rank=(\d+)\s*$`)

var holderItem = regexp.MustCompile(`^workspace ("(?:[^"\\]|\\.)*")$|^pid (\d+)$`)

// Parse reads line as a wait line, wherever in the line the marker stands. A
// line that is not a wait line answers false and no error; a wait line that
// does not read back is an error, since the runner wrote it with Line.
func Parse(line string) (Wait, bool, error) {
	m := waitLine.FindStringSubmatch(line)
	if m == nil {
		return Wait{}, false, nil
	}
	w := Wait{Reason: Reason(m[1])}
	if _, known := explain[w.Reason]; !known {
		return Wait{}, true, fmt.Errorf("seatwait: a wait line names the reason %q, which this build does not know: %q", m[1], line)
	}
	holders, err := parseHolders(m[2])
	if err != nil {
		return Wait{}, true, fmt.Errorf("seatwait: %w: %q", err, line)
	}
	w.Holders = holders
	nums := make([]uint64, 5)
	for i := range nums {
		n, err := strconv.ParseUint(m[3+i], 10, 64)
		if err != nil {
			return Wait{}, true, fmt.Errorf("seatwait: a wait line's figure %q does not read: %q", m[3+i], line)
		}
		nums[i] = n
	}
	w.AvailableMiB, w.WiredMiB, w.CeilingMiB, w.FloorMiB, w.Rank = nums[0], nums[1], nums[2], nums[3], int(nums[4])
	return w, true, nil
}

// parseHolders reads a wait line's holder list.
func parseHolders(list string) ([]Holder, error) {
	if list == "" {
		return nil, nil
	}
	var holders []Holder
	for item := range strings.SplitSeq(splitQuoted(list), "\x00") {
		m := holderItem.FindStringSubmatch(item)
		if m == nil {
			return nil, fmt.Errorf("the holder %q does not read", item)
		}
		if m[1] != "" {
			name, err := strconv.Unquote(m[1])
			if err != nil {
				return nil, fmt.Errorf("the holder %q does not unquote: %w", item, err)
			}
			holders = append(holders, Holder{Workspace: name})
			continue
		}
		pid, err := strconv.Atoi(m[2])
		if err != nil {
			return nil, fmt.Errorf("the holder %q names no pid: %w", item, err)
		}
		holders = append(holders, Holder{Pid: pid})
	}
	return holders, nil
}

// splitQuoted turns the ", " between holders into NULs, leaving any inside a
// quoted workspace name alone.
func splitQuoted(list string) string {
	var b strings.Builder
	quoted, escaped := false, false
	for i := 0; i < len(list); i++ {
		c := list[i]
		switch {
		case escaped:
			escaped = false
		case quoted && c == '\\':
			escaped = true
		case c == '"':
			quoted = !quoted
		case !quoted && c == ',' && i+1 < len(list) && list[i+1] == ' ':
			b.WriteByte(0)
			i++
			continue
		}
		b.WriteByte(c)
	}
	return b.String()
}

// Ended reports whether line ends a run's wait: any seat line that is not a
// wait line (a seat taken, or none taken).
func Ended(line string) bool {
	return strings.Contains(line, Marker) && !waitLine.MatchString(line)
}
