package sched

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

// Item is one indivisible piece of a splittable suite: an ERT test file, a
// vitest file, an e2e top-level test.
type Item struct {
	Name string
	Est  float64
}

// Chunkable is a suite whose items may be split across any number of
// processes. Every process pays Overhead once (an Emacs loading its helpers, a
// vitest booting, an e2e binary starting), so more chunks spread the work
// wider at the price of more overhead. The planner picks the count.
type Chunkable struct {
	// Group names the chunks: chunk i is Group#i, and a unit that lists Group
	// in its Deps waits for every chunk.
	Group    string
	Suite    string
	Items    []Item
	Overhead float64
	Deps     []string
}

// Plan is the schedule's input after chunking: every unit, with group aliases
// expanded to the concrete chunk IDs.
type Plan struct {
	Units []Unit
	// Chunks holds each group's chunk contents, indexed as the chunk IDs are.
	Chunks map[string][][]Item
	// Makespan is the simulated wall time of this plan.
	Makespan float64
}

// ChunkID is chunk i of a group.
func ChunkID(group string, i int) string { return fmt.Sprintf("%s#%02d", group, i) }

// Pack splits items into k chunks, longest item first onto the lightest
// chunk (LPT), so the chunks come out as even as the items allow.
func Pack(items []Item, k int) [][]Item {
	if k < 1 {
		panic(fmt.Sprintf("sched: Pack into %d chunks", k))
	}
	sorted := append([]Item(nil), items...)
	sort.SliceStable(sorted, func(i, j int) bool {
		if sorted[i].Est != sorted[j].Est {
			return sorted[i].Est > sorted[j].Est
		}
		return sorted[i].Name < sorted[j].Name
	})
	return packSorted(sorted, k)
}

// packSorted is Pack after its invariant-preserving sort. PlanRun prepares
// each group once and reuses that ordering across every candidate.
func packSorted(sorted []Item, k int) [][]Item {
	if k > len(sorted) {
		k = len(sorted)
	}
	chunks := make([][]Item, k)
	load := make([]float64, k)
	for _, it := range sorted {
		lightest := 0
		for c := 1; c < k; c++ {
			if load[c] < load[lightest] {
				lightest = c
			}
		}
		chunks[lightest] = append(chunks[lightest], it)
		load[lightest] += it.Est
	}
	return chunks
}

// PlanRun chooses how many chunks each splittable suite gets so that the
// simulated makespan on n slots is smallest, and returns that plan.
//
// Every splittable suite is cut to one common target chunk size: a chunk much
// longer than its peers is the one the run waits on, and a chunk much shorter
// pays its overhead for little work. The candidate sizes are every C/k a
// suite could produce up to the host's slot count, and each is simulated with
// the runner's own policy. A group cannot run more chunks concurrently.
//
// The winner is the plan with the LEAST TOTAL WORK among those whose makespan
// is within MakespanSlack of the best. Pure makespan would buy a 1% faster run
// with ten extra process startups; that overhead is CPU the machine (and any
// concurrent run on it) pays for nothing anyone would notice.
func PlanRun(atomic []Unit, chunkables []Chunkable, n int) (Plan, error) {
	if n < 1 {
		return Plan{}, fmt.Errorf("sched: plan needs at least one slot, got %d", n)
	}
	for _, c := range chunkables {
		if len(c.Items) == 0 {
			return Plan{}, fmt.Errorf("sched: splittable group %q has no items", c.Group)
		}
	}
	prepared := append([]Chunkable(nil), chunkables...)
	for i := range prepared {
		prepared[i].Items = append([]Item(nil), prepared[i].Items...)
		sort.SliceStable(prepared[i].Items, func(a, b int) bool {
			if prepared[i].Items[a].Est != prepared[i].Items[b].Est {
				return prepared[i].Items[a].Est > prepared[i].Items[b].Est
			}
			return prepared[i].Items[a].Name < prepared[i].Items[b].Name
		})
	}
	type scored struct {
		counts   []int
		makespan float64
		work     float64
		units    int
	}
	var all []scored
	bestMakespan := math.Inf(1)
	seen := map[string]bool{}
	for _, size := range candidateSizes(prepared, n) {
		counts := chunkCounts(prepared, size, n)
		key := fmt.Sprint(counts)
		if seen[key] {
			continue
		}
		seen[key] = true
		p, err := build(atomic, prepared, counts)
		if err != nil {
			return Plan{}, err
		}
		p.Makespan, err = Simulate(p.Units, n)
		if err != nil {
			return Plan{}, err
		}
		work := 0.0
		for _, u := range p.Units {
			work += u.Est
		}
		// Retain only the score and the small chunk-count vector. Keeping every
		// candidate's complete plan makes planning memory proportional to the
		// product of candidates and roster units; a full roster can otherwise
		// consume hundreds of megabytes before a single test starts.
		all = append(all, scored{counts: counts, makespan: p.Makespan, work: work, units: len(p.Units)})
		bestMakespan = math.Min(bestMakespan, p.Makespan)
	}
	// better orders plans by less work, then a shorter makespan, then fewer
	// units (fewer processes for the same predicted result).
	better := func(a, b scored) bool {
		if math.Abs(a.work-b.work) > 1e-9 {
			return a.work < b.work
		}
		if math.Abs(a.makespan-b.makespan) > 1e-9 {
			return a.makespan < b.makespan
		}
		return a.units < b.units
	}
	var best *scored
	for i := range all {
		s := &all[i]
		if s.makespan > bestMakespan*(1+MakespanSlack)+1e-9 {
			continue
		}
		if best == nil || better(*s, *best) {
			best = s
		}
	}
	winner, err := build(atomic, prepared, best.counts)
	if err != nil {
		return Plan{}, err
	}
	winner.Makespan = best.makespan
	return winner, nil
}

// MakespanSlack is how much slower than the fastest simulated plan a plan may
// be and still win on doing less total work.
const MakespanSlack = 0.02

// candidateSizes is every chunk size some suite's own split could use on the
// available slots, plus "everything in one chunk".
func candidateSizes(chunkables []Chunkable, slots int) []float64 {
	var sizes []float64
	for _, c := range chunkables {
		total := 0.0
		for _, it := range c.Items {
			total += it.Est
		}
		// More chunks than slots cannot increase this group's concurrency; it
		// only adds process overhead and candidate simulations. Other suites
		// still sequence independently through those same slots.
		for k := 1; k <= min(len(c.Items), slots); k++ {
			sizes = append(sizes, total/float64(k))
		}
	}
	sizes = append(sizes, math.Inf(1))
	sort.Float64s(sizes)
	return sizes
}

// chunkCounts is each suite's chunk count for a target chunk size.
func chunkCounts(chunkables []Chunkable, size float64, slots int) []int {
	counts := make([]int, len(chunkables))
	for i, c := range chunkables {
		total := 0.0
		for _, it := range c.Items {
			total += it.Est
		}
		k := 1
		if !math.IsInf(size, 1) && size > 0 {
			k = int(math.Ceil(total/size - 1e-9))
		}
		k = max(1, min(k, len(c.Items), slots))
		counts[i] = k
	}
	return counts
}

// build lays out the concrete units for one choice of chunk counts.
func build(atomic []Unit, chunkables []Chunkable, counts []int) (Plan, error) {
	p := Plan{Chunks: map[string][][]Item{}}
	groups := map[string][]string{}
	for i, c := range chunkables {
		if _, dup := groups[c.Group]; dup {
			return Plan{}, fmt.Errorf("sched: duplicate splittable group %q", c.Group)
		}
		chunks := packSorted(c.Items, counts[i])
		p.Chunks[c.Group] = chunks
		ids := make([]string, len(chunks))
		for j := range chunks {
			ids[j] = ChunkID(c.Group, j)
		}
		groups[c.Group] = ids
	}
	expand := func(deps []string) []string {
		var out []string
		for _, d := range deps {
			if ids, ok := groups[d]; ok {
				out = append(out, ids...)
				continue
			}
			out = append(out, d)
		}
		return out
	}
	for _, u := range atomic {
		if strings.Contains(u.ID, "#") {
			return Plan{}, fmt.Errorf("sched: atomic unit ID %q may not contain '#', which names chunks", u.ID)
		}
		u.Deps = expand(u.Deps)
		p.Units = append(p.Units, u)
	}
	for _, c := range chunkables {
		for j, chunk := range p.Chunks[c.Group] {
			est := c.Overhead
			for _, it := range chunk {
				est += it.Est
			}
			p.Units = append(p.Units, Unit{ID: ChunkID(c.Group, j), Suite: c.Suite, Deps: expand(c.Deps), Est: est})
		}
	}
	return p, nil
}
