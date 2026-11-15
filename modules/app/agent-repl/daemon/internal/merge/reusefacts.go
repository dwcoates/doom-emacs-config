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

// THE GATES' TEST REUSE (owner ruling, 2026-11-15) is one of the facts the
// daemon hands the intake: the reuse of the gate that ran last before the
// deploy, against the gates before it. A rate far below theirs is a NEW fact,
// which the intake escalates; anything else is SEEN BEFORE.

// The bounds of a far-below reuse rate, measured 2026-11-15 over the last 60
// gate logs in the queue's logs directory: gates of 100 units or more reused
// 24-92% of their units (median about 45%) except nine that reused none,
// while gates under 100 units ranged 0-46% with no pattern, so they are never
// judged.
const (
	// reuseRecentGates is how many earlier gates a gate is judged against.
	reuseRecentGates = 20
	// reuseLargeGateUnits is the fewest units a judged gate runs.
	reuseLargeGateUnits = 100
	// reuseFarBelowFraction: a judged gate reusing under this share of the
	// earlier gates' median rate is far below them (under about 11% today).
	reuseFarBelowFraction = 0.25
)

// GateReuse is one gate's reuse, as its log's runner lines state it.
type GateReuse struct {
	// Path is the gate's log, and At when it was last written.
	Path string
	At   time.Time
	// Units, Reused and Run are the run's unit counts; Why counts the run
	// units by why they ran ("inputs changed", "failed before", ...).
	Units, Reused, Run int
	Why               map[string]int
}

// Rate is the share of the gate's units it reused.
func (g GateReuse) Rate() float64 {
	if g.Units == 0 {
		return 0
	}
	return float64(g.Reused) / float64(g.Units)
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

// gateReuseBefore reads the gates whose logs in DIR were last written before
// AT, newest first, up to reuseRecentGates+1 of them that state their reuse.
func gateReuseBefore(dir string, at time.Time) ([]GateReuse, error) {
	entries, err := os.ReadDir(dir)
	if os.IsNotExist(err) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("merge: list the gate logs in %s: %w", dir, err)
	}
	var logs []GateReuse
	for _, e := range entries {
		if e.IsDir() || !strings.Contains(e.Name(), "-tests-") || !strings.HasSuffix(e.Name(), ".log") {
			continue
		}
		info, err := e.Info()
		if err != nil {
			return nil, fmt.Errorf("merge: read the gate log %s: %w", e.Name(), err)
		}
		if info.ModTime().After(at) {
			continue
		}
		logs = append(logs, GateReuse{Path: filepath.Join(dir, e.Name()), At: info.ModTime()})
	}
	sort.Slice(logs, func(i, j int) bool { return logs[i].At.After(logs[j].At) })
	var out []GateReuse
	for _, g := range logs {
		if len(out) > reuseRecentGates {
			break
		}
		text, err := os.ReadFile(g.Path)
		if err != nil {
			return nil, fmt.Errorf("merge: read the gate log %s: %w", g.Path, err)
		}
		reuse, ok := ParseGateReuse(string(text))
		if !ok {
			continue
		}
		reuse.Path, reuse.At = g.Path, g.At
		out = append(out, reuse)
	}
	return out, nil
}

// JudgeReuse answers whether gate THIS reused far below the median rate of
// the RECENT gates before it, and that median: a gate under
// reuseLargeGateUnits, or one with no earlier gate, is never far below.
func JudgeReuse(this GateReuse, recent []GateReuse) (bool, float64) {
	if len(recent) == 0 {
		return false, 0
	}
	rates := make([]float64, 0, len(recent))
	for _, g := range recent {
		rates = append(rates, g.Rate())
	}
	sort.Float64s(rates)
	median := rates[len(rates)/2]
	if len(rates)%2 == 0 {
		median = (rates[len(rates)/2-1] + rates[len(rates)/2]) / 2
	}
	return this.Units >= reuseLargeGateUnits && this.Rate() < reuseFarBelowFraction*median, median
}

// RenderReuse is the brief's reuse fact for GATES, newest first: the first
// is the gate that ran last before the deploy, judged against the rest.
func RenderReuse(gates []GateReuse) string {
	if len(gates) == 0 {
		return "No gate logged its test reuse before this deploy."
	}
	this, recent := gates[0], gates[1:]
	far, median := JudgeReuse(this, recent)
	standing := "SEEN BEFORE"
	if far {
		standing = "NEW"
	}
	whys := make([]string, 0, len(this.Why))
	for why, n := range this.Why {
		whys = append(whys, fmt.Sprintf("%d %s", n, why))
	}
	sort.Strings(whys)
	return fmt.Sprintf("- %s: the last gate before the deploy (%s) reused %d of %d units (%.0f%%) and ran %d (%s); the %d gates before it reused a median %.0f%%, and a gate of %d or more units reusing under %.0f%% of that is far below.",
		standing, this.Path, this.Reused, this.Units, 100*this.Rate(), this.Run, strings.Join(whys, ", "), len(recent), 100*median, reuseLargeGateUnits, 100*reuseFarBelowFraction)
}
