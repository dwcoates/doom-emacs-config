package merge

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"time"
)

// THE GATE THAT RAN LAST BEFORE A DEPLOY is one of the things the deploy
// mediator's brief points it at (intakeBrief): the gate's log, found as the
// newest log that states its run's test reuse. What that reuse means is the
// mediator's to judge from the log (owner ruling, 2026-11-19).

// GateReuse is one gate's reuse, as its log's runner lines state it.
type GateReuse struct {
	// Path is the gate's log, and At when it was last written.
	Path string
	At   time.Time
	// Units, Reused and Run are the run's unit counts; Why counts the run
	// units by why they ran ("inputs changed", "failed before", ...).
	Units, Reused, Run int
	Why                map[string]int
}

// reuseLine reads a runner's reuse line: an optional suite's name, the
// counts, and why the run units ran.
var reuseLine = regexp.MustCompile(`\] (?:([^:\]]+): )?reuse: (\d+) units, (\d+) reused, (\d+) run(?: \(([^)]*)\))?`)

// whyPart reads one "N why" of a reuse line's tally.
var whyPart = regexp.MustCompile(`^(\d+) (.+)$`)

// ParseGateReuse reads a gate log's reuse: its run's total line, or the sum
// of its suites' lines when the run printed none. It answers false when the
// log holds no reuse line.
func ParseGateReuse(text string) (GateReuse, bool) {
	var total, suites GateReuse
	total.Why, suites.Why = map[string]int{}, map[string]int{}
	sawTotal, sawSuite := false, false
	for _, line := range strings.Split(text, "\n") {
		m := reuseLine.FindStringSubmatch(line)
		if m == nil {
			continue
		}
		into := &suites
		if m[1] == "" {
			into, sawTotal = &total, true
		} else {
			sawSuite = true
		}
		units, _ := strconv.Atoi(m[2])
		reused, _ := strconv.Atoi(m[3])
		run, _ := strconv.Atoi(m[4])
		into.Units, into.Reused, into.Run = into.Units+units, into.Reused+reused, into.Run+run
		for _, part := range strings.Split(m[5], ", ") {
			if p := whyPart.FindStringSubmatch(strings.TrimSpace(part)); p != nil {
				n, _ := strconv.Atoi(p[1])
				into.Why[p[2]] += n
			}
		}
	}
	switch {
	case sawTotal:
		return total, true
	case sawSuite:
		return suites, true
	}
	return GateReuse{}, false
}

// lastGateReuseBefore reads the newest gate whose log in DIR was last
// written before AT and states its reuse; false when there is none.
func lastGateReuseBefore(dir string, at time.Time) (GateReuse, bool, error) {
	entries, err := os.ReadDir(dir)
	if os.IsNotExist(err) {
		return GateReuse{}, false, nil
	}
	if err != nil {
		return GateReuse{}, false, fmt.Errorf("merge: list the gate logs in %s: %w", dir, err)
	}
	var logs []GateReuse
	for _, e := range entries {
		if e.IsDir() || !strings.Contains(e.Name(), "-tests-") || !strings.HasSuffix(e.Name(), ".log") {
			continue
		}
		info, err := e.Info()
		if err != nil {
			return GateReuse{}, false, fmt.Errorf("merge: read the gate log %s: %w", e.Name(), err)
		}
		if info.ModTime().After(at) {
			continue
		}
		logs = append(logs, GateReuse{Path: filepath.Join(dir, e.Name()), At: info.ModTime()})
	}
	sort.Slice(logs, func(i, j int) bool { return logs[i].At.After(logs[j].At) })
	for _, g := range logs {
		text, err := os.ReadFile(g.Path)
		if err != nil {
			return GateReuse{}, false, fmt.Errorf("merge: read the gate log %s: %w", g.Path, err)
		}
		reuse, ok := ParseGateReuse(string(text))
		if !ok {
			continue
		}
		reuse.Path, reuse.At = g.Path, g.At
		return reuse, true, nil
	}
	return GateReuse{}, false, nil
}
