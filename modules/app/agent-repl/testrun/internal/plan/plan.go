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
package plan

import (
	"fmt"
	"strings"

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

// UnitKey is an atomic unit's history key.
func UnitKey(id string) string { return "unit:" + id }

// ItemKey is a splittable item's history key.
func ItemKey(group, item string) string { return "item:" + group + ":" + item }

// OverheadKey is a group's per-chunk overhead history key.
func OverheadKey(group string) string { return "overhead:" + group }

// Estimates is what the history answers.
type Estimates interface {
	Get(key string) (float64, bool)
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
// concrete specs.
func Build(est Estimates, all []suites.Units, slots int) (Planned, error) {
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
		chunkables = append(chunkables, sched.Chunkable{
			Group: sp.Group, Suite: sp.Suite, Items: items, Overhead: overhead, Deps: sp.Deps,
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
		out.Specs = append(out.Specs, s)
	}
	return out, nil
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
// A group's overhead is the mean over its chunks this run.
func Measurements(results []run.Result) map[string]float64 {
	m := map[string]float64{}
	overheadSum, overheadN := map[string]float64{}, map[string]int{}
	for _, r := range results {
		if r.Outcome != run.Passed {
			continue
		}
		group, _, isChunk := chunkOf(r.Spec.ID)
		if !isChunk {
			m[UnitKey(r.Spec.ID)] = r.Wall()
			continue
		}
		sum := 0.0
		for item, secs := range r.Items {
			m[ItemKey(group, item)] = secs
			sum += secs
		}
		overheadSum[group] += max(0, r.Wall()-sum)
		overheadN[group]++
	}
	for group, total := range overheadSum {
		m[OverheadKey(group)] = total / float64(overheadN[group])
	}
	return m
}
