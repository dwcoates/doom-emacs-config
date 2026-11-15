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

	"agentrepl/testrun/reusewhy"
)

// THE GATES' TEST REUSE (owner rulings, 2026-11-15) is one of the facts the
// daemon hands the intake: the reuse of the gate that ran last before the
// deploy. A gate's unit runs rather than being reused for a reason its log
// states (reusewhy). Most reasons EXPLAIN the run: the unit's inputs changed,
// it failed before, or it is never reused by design, and a gate whose branch
// moves a protobuf or a shared library rightly reuses nothing. The rest name
// no change of the unit's own (its record is missing, the run reused nothing,
// the host could not confine it), so a gate with many of them is a NEW fact,
// which the intake escalates; anything else is SEEN BEFORE.

// reuseUnexplainedUnits is the fewest units run for no explaining reason
// that make a gate's reuse NEW. Measured 2026-11-15 over the last 44 gate
// logs in the queue's logs directory: every run unit but at most 3 per gate
// (never recorded) was explained, including the nine gates that reused none;
// the bound is about 3 times that.
const reuseUnexplainedUnits = 10

// explainingWhy are the reasons that explain a unit's run.
var explainingWhy = map[string]bool{
	reusewhy.FailedBefore:  true,
	reusewhy.InputsChanged: true,
	reusewhy.Never:         true,
}

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

// Unexplained is how many of the gate's run units ran for no explaining
// reason (explainingWhy).
func (g GateReuse) Unexplained() int {
	n := 0
	for why, count := range g.Why {
		if !explainingWhy[why] {
			n += count
		}
	}
	return n
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

// JudgeReuse answers whether GATE's reuse is NEW: whether it ran
// reuseUnexplainedUnits or more units for no explaining reason.
func JudgeReuse(gate GateReuse) bool {
	return gate.Unexplained() >= reuseUnexplainedUnits
}

// RenderReuse is the brief's reuse fact for GATE, the gate that ran last
// before the deploy; OK is false when no gate logged its reuse.
func RenderReuse(gate GateReuse, ok bool) string {
	if !ok {
		return "No gate logged its test reuse before this deploy."
	}
	standing := "SEEN BEFORE"
	if JudgeReuse(gate) {
		standing = "NEW"
	}
	whys := make([]string, 0, len(gate.Why))
	for why, n := range gate.Why {
		whys = append(whys, fmt.Sprintf("%d %s", n, why))
	}
	sort.Strings(whys)
	return fmt.Sprintf("- %s: the last gate before the deploy (%s) reused %d of %d units (%.0f%%) and ran %d (%s); %d of them ran for no reason of their own (not %s, %s or %s), and %d or more is NEW.",
		standing, gate.Path, gate.Reused, gate.Units, 100*gate.Rate(), gate.Run, strings.Join(whys, ", "), gate.Unexplained(),
		reusewhy.InputsChanged, reusewhy.FailedBefore, reusewhy.Never, reuseUnexplainedUnits)
}
