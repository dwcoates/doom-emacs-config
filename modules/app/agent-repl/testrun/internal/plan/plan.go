// Package plan joins the suites' units to the host's timing history: it
// estimates every unit and item, has the scheduler choose the chunking, and
// after the run turns the results back into measurements.
//
// ESTIMATES FOR WHAT WAS NEVER MEASURED:
//
//   - an atomic unit is assumed as long as the longest measured atomic unit
//     (UnknownUnit when nothing is measured), so it starts early rather than
//     becoming the tail the whole run waits on;
//   - an item is assumed to cost its group's mean measured item, else its
//     suite's (a new Go package is costed like the module's other packages'
//     tests), else UnknownItem, so a new test joins a chunk at an ordinary
//     size;
//   - a chunk's overhead is assumed to be UnknownOverhead.
//
// After one run every one of them is measured.
//
// EVERY UNIT IS COSTED IN CORES TOO, from its own history: the CPU its
// passing runs used over their wall time (the runner's per-unit CPU
// accounting, run.Usage), which the host's CPU governor admits it by
// (cores.Governor). An atomic unit's cores are its own, a chunk's its
// group's; a unit never measured is costed cores.DefaultCoresPerSlot a slot.
package plan

import (
	"fmt"
	"strings"
	"time"

	"agentrepl/testrun/internal/cores"
	"agentrepl/testrun/internal/history"
	"agentrepl/testrun/internal/run"
	"agentrepl/testrun/internal/sched"
	"agentrepl/testrun/internal/suites"
)

const (
	UnknownUnit     = 60.0
	UnknownItem     = 1.0
	UnknownOverhead = 1.0
)

// unitKeyPrefix starts every atomic unit's history key.
const unitKeyPrefix = "unit:"

// UnitKey is an atomic unit's history key.
func UnitKey(id string) string { return unitKeyPrefix + id }

// ItemKey is a splittable item's history key.
func ItemKey(group, item string) string { return "item:" + group + ":" + item }

// OverheadKey is a group's per-chunk overhead history key.
func OverheadKey(group string) string { return "overhead:" + group }

// CoresKey is the history key of the cores a unit uses while it runs: an
// atomic unit's own ID, or a chunk's group, since a group's chunks are cut
// differently from run to run.
func CoresKey(id string) string {
	if group, _, ok := chunkOf(id); ok {
		return "cores:" + group
	}
	return "cores:" + id
}

// MinCoresWall is the shortest unit, in seconds, whose cores are recorded: a
// shorter one is mostly its process's start, which says little of what it
// uses running, and holds a seat too briefly to show in the governor's
// three-second average.
const MinCoresWall = 1.0

// expectedCores is what the history says a unit uses while it runs.
func expectedCores(est Estimates, s run.Spec) float64 {
	v, ok := est.Get(CoresKey(s.ID))
	return cores.ExpectedCores(s.Width(), v, ok)
}

// Estimates is what the history answers.
type Estimates interface {
	Get(key string) (float64, bool)
	Peak(key string) (float64, bool)
	Longest(prefix string) (float64, bool)
}

var _ Estimates = (*history.Store)(nil)

// Planned is the run to execute.
type Planned struct {
	Specs    []run.Spec
	Makespan float64
	// Chunks is each splittable group's chunk count.
	Chunks map[string]int
}

// Build estimates every unit, chooses the chunking for slots, and returns the
// concrete specs, each bounded by its time limit (bound). With remeasure,
// every unit is bounded as one never measured.
func Build(est Estimates, all []suites.Units, slots int, remeasure bool) (Planned, error) {
	var atomicSpecs []run.Spec
	var splits []suites.Split
	for _, u := range all {
		atomicSpecs = append(atomicSpecs, u.Atomic...)
		splits = append(splits, u.Splits...)
	}

	longest, anyKnown := 0.0, false
	for _, s := range atomicSpecs {
		if v, ok := est.Get(UnitKey(s.ID)); ok {
			anyKnown = true
			longest = max(longest, v)
		}
	}
	unknownUnit := UnknownUnit
	if anyKnown {
		unknownUnit = longest
	}
	bySpec := map[string]run.Spec{}
	var atomic []sched.Unit
	for _, s := range atomicSpecs {
		s.Est = unknownUnit
		if v, ok := est.Get(UnitKey(s.ID)); ok {
			s.Est = v
		}
		bySpec[s.ID] = s
		atomic = append(atomic, s.Unit)
	}

	type mean struct {
		sum float64
		n   int
	}
	groupMean, suiteMean := map[string]*mean{}, map[string]*mean{}
	for _, sp := range splits {
		gm := &mean{}
		groupMean[sp.Group] = gm
		sm := suiteMean[sp.Suite]
		if sm == nil {
			sm = &mean{}
			suiteMean[sp.Suite] = sm
		}
		for _, it := range sp.Items {
			if v, ok := est.Get(ItemKey(sp.Group, it)); ok {
				gm.sum, gm.n = gm.sum+v, gm.n+1
				sm.sum, sm.n = sm.sum+v, sm.n+1
			}
		}
	}
	var chunkables []sched.Chunkable
	splitByGroup := map[string]suites.Split{}
	for _, sp := range splits {
		splitByGroup[sp.Group] = sp
		unknownItem := UnknownItem
		if g := groupMean[sp.Group]; g.n > 0 {
			unknownItem = g.sum / float64(g.n)
		} else if su := suiteMean[sp.Suite]; su.n > 0 {
			unknownItem = su.sum / float64(su.n)
		}
		items := make([]sched.Item, len(sp.Items))
		for i, it := range sp.Items {
			v, ok := est.Get(ItemKey(sp.Group, it))
			if !ok {
				v = unknownItem
			}
			items[i] = sched.Item{Name: it, Est: v}
		}
		overhead, ok := est.Get(OverheadKey(sp.Group))
		if !ok {
			overhead = UnknownOverhead
		}
		if sp.OverheadCap > 0 {
			overhead = min(overhead, sp.OverheadCap)
		}
		chunkables = append(chunkables, sched.Chunkable{
			Group: sp.Group, Suite: sp.Suite, Items: items, Overhead: overhead, Deps: sp.Deps, Slots: sp.Slots,
		})
	}

	p, err := sched.PlanRun(atomic, chunkables, slots)
	if err != nil {
		return Planned{}, err
	}
	out := Planned{Makespan: p.Makespan, Chunks: map[string]int{}}
	for group, chunks := range p.Chunks {
		out.Chunks[group] = len(chunks)
	}
	for _, u := range p.Units {
		if s, ok := bySpec[u.ID]; ok {
			s.Unit = u
			peak, measured := est.Peak(UnitKey(s.ID))
			bound(est, &s, peak, measured && !remeasure)
			s.Cores = expectedCores(est, s)
			out.Specs = append(out.Specs, s)
			continue
		}
		group, idx, ok := chunkOf(u.ID)
		if !ok {
			return Planned{}, fmt.Errorf("plan: the scheduler returned unit %q that no suite built", u.ID)
		}
		sp := splitByGroup[group]
		var names []string
		for _, it := range p.Chunks[group][idx] {
			names = append(names, it.Name)
		}
		s := sp.Chunk(u.ID, names)
		s.Unit = u
		peak, measured := chunkPeak(est, group, names)
		bound(est, &s, peak, measured && !remeasure)
		s.Cores = expectedCores(est, s)
		out.Specs = append(out.Specs, s)
	}
	return out, nil
}

// bound sets a spec's time limit: run.BoundFactor times its expected time.
//
// THE EXPECTED TIME IS THE UNIT'S PEAK, not its mean: a unit's healthy runs
// vary by up to four times their median (cold builds, a loaded host), so a
// limit three times the mean killed about one healthy run in 180 of the 8,807
// recorded (2026-11-09). An atomic unit's peak is its own; a chunk's is its
// group's overhead peak plus every item's.
//
// A UNIT NOT MEASURED (no peak for it or one of its items, or a remeasure)
// is expected to take as long as the longest unit this host has measured, or
// its own estimate if that is longer, or UnknownUnit on a host that has
// measured nothing: the planner's own rule for what was never measured. Its
// passing run records its peak, so its next run is held to its own.
//
// TWO FLOORS HOLD UNDER A MEASURED PEAK:
//   - No unit is expected to finish before its suite's slowest measured
//     process start (startFloor): a unit first measured on a quiet host at
//     25ms (`daemon:internal/rostermirror`) was killed at five times that
//     under governed load, where its test binary's start alone took longer
//     (2026-11-13).
//   - A program build is expected to take as long as the slowest program
//     build the host has measured (programFloor): its own peak is of runs on
//     a warm Go build cache, so the first build after its sources change is
//     cold and could not fit (`program:git`, killed at three times its 202ms
//     peak twice in a row, 2026-11-12), and a killed build never fills the
//     cache, so every later run was cold too.
func bound(est Estimates, s *run.Spec, peak float64, measured bool) {
	expected := peak
	if !measured {
		longest, known := est.Longest(unitKeyPrefix)
		if !known {
			longest = UnknownUnit
		}
		expected = max(longest, s.Est)
	}
	expected = max(expected, startFloor(est, s.Suite), programFloor(est, s.ID))
	s.Expected = time.Duration(expected * float64(time.Second))
	s.Bound = time.Duration(run.BoundFactor * expected * float64(time.Second))
}

// startFloor is the slowest per-process start the host has measured for any
// chunk group of SUITE (OverheadKey), 0 when none is measured.
func startFloor(est Estimates, suite string) float64 {
	floor := 0.0
	for _, sep := range []string{":", "/"} {
		if v, ok := est.Longest(OverheadKey(suite + sep)); ok {
			floor = max(floor, v)
		}
	}
	return floor
}

// programFloor is, for a program build unit ID, the slowest program build
// the host has measured, and 0 for every other unit.
func programFloor(est Estimates, id string) float64 {
	if !strings.HasPrefix(id, suites.ProgramUnitID("")) {
		return 0
	}
	v, _ := est.Longest(UnitKey(suites.ProgramUnitID("")))
	return v
}

// chunkPeak is a chunk's measured peak: its group's overhead peak plus each of
// its items', and whether every one of them was measured.
func chunkPeak(est Estimates, group string, items []string) (float64, bool) {
	total, ok := est.Peak(OverheadKey(group))
	if !ok {
		return 0, false
	}
	for _, it := range items {
		v, ok := est.Peak(ItemKey(group, it))
		if !ok {
			return 0, false
		}
		total += v
	}
	return total, true
}

// chunkOf splits a chunk ID into its group and index.
func chunkOf(id string) (string, int, bool) {
	i := strings.LastIndex(id, "#")
	if i < 0 {
		return "", 0, false
	}
	var n int
	if _, err := fmt.Sscanf(id[i+1:], "%d", &n); err != nil {
		return "", 0, false
	}
	return id[:i], n, true
}

// Measurements turns the passed units' results into history entries. A failed
// unit's time says how long it took to fail, not how long it takes, so it is
// never recorded.
// A group's overhead is the mean over its chunks this run, and its peak the
// largest of them.
func Measurements(results []run.Result) map[string]history.Sample {
	m := map[string]history.Sample{}
	overheadSum, overheadN, overheadMax := map[string]float64{}, map[string]int{}, map[string]float64{}
	// A group's chunks share one cores key: its sample is their mean.
	coresSum, coresN, coresMax := map[string]float64{}, map[string]int{}, map[string]float64{}
	for _, r := range results {
		if r.Outcome != run.Passed {
			continue
		}
		if wall := r.Wall(); wall >= MinCoresWall && r.Usage.CPU > 0 {
			key, c := CoresKey(r.Spec.ID), r.Usage.CPU/wall
			coresSum[key] += c
			coresN[key]++
			coresMax[key] = max(coresMax[key], c)
		}
		group, _, isChunk := chunkOf(r.Spec.ID)
		if !isChunk {
			m[UnitKey(r.Spec.ID)] = history.Sample{Seconds: r.Wall(), Peak: r.Wall()}
			continue
		}
		sum := 0.0
		for item, secs := range r.Items {
			m[ItemKey(group, item)] = history.Sample{Seconds: secs, Peak: secs}
			sum += secs
		}
		overhead := max(0, r.Wall()-sum)
		overheadSum[group] += overhead
		overheadN[group]++
		overheadMax[group] = max(overheadMax[group], overhead)
	}
	for key, total := range coresSum {
		m[key] = history.Sample{Seconds: total / float64(coresN[key]), Peak: coresMax[key]}
	}
	for group, total := range overheadSum {
		m[OverheadKey(group)] = history.Sample{Seconds: total / float64(overheadN[group]), Peak: overheadMax[group]}
	}
	return m
}
