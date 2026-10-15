// Package redundancy is `testrun redundancy`: measure which test cases are
// redundant, judged by the mutants each kills and the code each reaches, and
// find the cheapest set of cases that keeps everything the whole set catches.
//
// The package holds the language-neutral half: the per-case measurement
// (Case), the runtime-weighted minimization (Minimize) and the report. Each
// language's adapter (gomeasure, and later vitest and ERT) produces Cases.
package redundancy

import (
	"fmt"
	"math/bits"
	"slices"
	"sort"
	"time"
)

// Case is one test case measured on its own.
type Case struct {
	// ID names the case the way its runner selects it (Go: "TestX/sub").
	ID string
	// Cost is the case's measured runtime.
	Cost time.Duration
	// Blocks are the production code blocks the case reached, unioned over
	// every measurement run.
	Blocks []string
	// Kills are the mutants the case failed under.
	Kills []string
	// Pinned, when set, is why the case stays regardless of what it covers:
	// it pins an error path and its canonical log record, or it was named on
	// the command line.
	Pinned string
}

// Universe is what the kept set must still catch.
type Universe int

const (
	// KillsOnly keeps every killed mutant and nothing else: a case is
	// removable when the kept cases kill every mutant it kills. It is the
	// default (owner ruling, 2026-10-14).
	KillsOnly Universe = iota
	// Strict keeps every killed mutant AND every reached block. Mutation
	// operators are incomplete, so a line no operator touches still counts.
	Strict
)

func (u Universe) String() string {
	switch u {
	case Strict:
		return "strict"
	case KillsOnly:
		return "kills-only"
	}
	panic(fmt.Sprintf("redundancy: unknown universe %d", int(u)))
}

// ParseUniverse reads a universe's flag spelling.
func ParseUniverse(s string) (Universe, error) {
	switch s {
	case "strict":
		return Strict, nil
	case "kills-only":
		return KillsOnly, nil
	}
	return 0, fmt.Errorf("unknown universe %q (want strict or kills-only)", s)
}

// Removal is one case the minimization drops, and what catches its bugs once
// it is gone.
type Removal struct {
	Case string
	// Saves is the case's cost.
	Saves time.Duration
	// DominatedBy is the cheapest kept case that alone catches everything
	// this one does; empty when no single kept case does, or when this case
	// catches nothing at all.
	DominatedBy string
	// CoveredBy is the kept cases that between them catch everything this
	// one does. It is DominatedBy alone when DominatedBy is set, and empty
	// when this case catches nothing.
	CoveredBy []string
}

// Result is one minimization.
type Result struct {
	Universe Universe
	// Kept are the case IDs that stay, pinned ones included, in input order.
	Kept    []string
	Removed []Removal
	// Optimal says the kept set is proven the cheapest one; false means the
	// search budget ran out and Kept is the best cover found.
	Optimal  bool
	KeptCost time.Duration
	// Elements is the size of the universe the kept set covers.
	Elements int
}

// SearchBudget bounds the exact search's nodes; past it the best cover found
// stands and Result.Optimal is false.
const SearchBudget = 2_000_000

// Minimize finds the cheapest set of cases whose elements (by universe) cover
// every element of the whole set, keeping every pinned case. A case's weight
// is its Cost plus overhead, the fixed price every case pays (its setup and
// its process share), so that among equally fast sets the smaller one wins.
func Minimize(cases []Case, u Universe, overhead time.Duration) (Result, error) {
	return minimize(cases, u, overhead, SearchBudget)
}

func minimize(cases []Case, u Universe, overhead time.Duration, budget int) (Result, error) {
	seen := map[string]bool{}
	for _, c := range cases {
		if seen[c.ID] {
			return Result{}, fmt.Errorf("redundancy: case %q is measured twice", c.ID)
		}
		seen[c.ID] = true
	}
	p := newProblem(cases, u, overhead)
	p.budget = budget
	kept := p.solve()
	res := Result{Universe: u, Optimal: p.optimal, Elements: p.nElem}
	keptSet := make([]bool, len(cases))
	for _, i := range kept {
		keptSet[i] = true
	}
	for i, c := range cases {
		if keptSet[i] {
			res.Kept = append(res.Kept, c.ID)
			res.KeptCost += c.Cost
		}
	}
	for i := range cases {
		if !keptSet[i] {
			res.Removed = append(res.Removed, p.dominators(i, kept))
		}
	}
	sort.SliceStable(res.Removed, func(a, b int) bool { return res.Removed[a].Saves > res.Removed[b].Saves })
	return res, nil
}

// bitset is a fixed-width set of element indices.
type bitset []uint64

func newBitset(n int) bitset { return make(bitset, (n+63)/64) }

func (b bitset) set(i int)      { b[i/64] |= 1 << (uint(i) % 64) }
func (b bitset) has(i int) bool { return b[i/64]&(1<<(uint(i)%64)) != 0 }

func (b bitset) subsetOf(o bitset) bool {
	for i := range b {
		if b[i]&^o[i] != 0 {
			return false
		}
	}
	return true
}

// andNotCount counts b's elements that are not in o.
func (b bitset) andNotCount(o bitset) int {
	n := 0
	for i := range b {
		n += bits.OnesCount64(b[i] &^ o[i])
	}
	return n
}

func (b bitset) or(o bitset) {
	for i := range b {
		b[i] |= o[i]
	}
}

func (b bitset) clone() bitset { return slices.Clone(b) }

type problem struct {
	cases   []Case
	weight  []time.Duration
	elems   []bitset
	nElem   int
	budget  int
	optimal bool
}

func newProblem(cases []Case, u Universe, overhead time.Duration) *problem {
	index := map[string]int{}
	ids := func(c Case) []string {
		out := make([]string, 0, len(c.Kills)+len(c.Blocks))
		for _, k := range c.Kills {
			out = append(out, "mutant:"+k)
		}
		if u == Strict {
			for _, b := range c.Blocks {
				out = append(out, "block:"+b)
			}
		}
		return out
	}
	for _, c := range cases {
		for _, e := range ids(c) {
			if _, ok := index[e]; !ok {
				index[e] = len(index)
			}
		}
	}
	p := &problem{cases: cases, nElem: len(index), optimal: true}
	for _, c := range cases {
		b := newBitset(len(index))
		for _, e := range ids(c) {
			b.set(index[e])
		}
		p.elems = append(p.elems, b)
		p.weight = append(p.weight, c.Cost+overhead)
	}
	return p
}

// solve returns the indices of the kept cases.
func (p *problem) solve() []int {
	n := len(p.cases)
	forced := make([]bool, n)
	covered := newBitset(p.nElem)
	for i, c := range p.cases {
		if c.Pinned != "" {
			forced[i] = true
			covered.or(p.elems[i])
		}
	}
	alive := make([]bool, n)
	for i := range p.cases {
		alive[i] = !forced[i]
	}
	// Reduce to a fixpoint: a case that alone covers an element is forced; a
	// case left with nothing uncovered, or whose uncovered elements another
	// live case covers at no more cost, is dropped.
	for changed := true; changed; {
		changed = false
		for e := 0; e < p.nElem; e++ {
			if covered.has(e) {
				continue
			}
			only := -1
			for i := range p.cases {
				if alive[i] && p.elems[i].has(e) {
					if only != -1 {
						only = -2
						break
					}
					only = i
				}
			}
			if only == -1 {
				panic(fmt.Sprintf("redundancy: element %d is covered by no case", e))
			}
			if only >= 0 {
				forced[only], alive[only] = true, false
				covered.or(p.elems[only])
				changed = true
			}
		}
		for i := range p.cases {
			if !alive[i] {
				continue
			}
			if p.elems[i].andNotCount(covered) == 0 {
				alive[i] = false
				changed = true
				continue
			}
			for j := range p.cases {
				if j == i || !alive[j] {
					continue
				}
				if p.dominates(j, i, covered) {
					alive[i] = false
					changed = true
					break
				}
			}
		}
	}
	var live []int
	for i := range p.cases {
		if alive[i] {
			live = append(live, i)
		}
	}
	best := p.greedy(live, covered)
	s := &search{p: p, live: live, best: best, bestCost: p.cost(best)}
	s.branch(covered.clone(), nil, 0)
	if s.nodes > p.budget {
		p.optimal = false
	}
	var kept []int
	for i := range p.cases {
		if forced[i] {
			kept = append(kept, i)
		}
	}
	kept = append(kept, s.best...)
	sort.Ints(kept)
	return kept
}

// dominates says live case j makes live case i unnecessary: j covers every
// element i still has to offer, at no more weight. Equal sets at equal weight
// keep the earlier case, so exactly one of a tied pair is dropped.
func (p *problem) dominates(j, i int, covered bitset) bool {
	for w := range p.elems[i] {
		if p.elems[i][w]&^covered[w]&^p.elems[j][w] != 0 {
			return false
		}
	}
	if p.weight[j] > p.weight[i] {
		return false
	}
	if p.weight[j] < p.weight[i] {
		return true
	}
	for w := range p.elems[i] {
		if p.elems[j][w]&^covered[w]&^p.elems[i][w] != 0 {
			return true // j strictly covers more
		}
	}
	return j < i
}

func (p *problem) cost(set []int) time.Duration {
	var c time.Duration
	for _, i := range set {
		c += p.weight[i]
	}
	return c
}

// greedy is the weighted greedy cover of what covered leaves open, followed
// by reverse deletion of any case the rest already cover.
func (p *problem) greedy(live []int, covered bitset) []int {
	cov := covered.clone()
	var picked []int
	for {
		bestI := -1
		var bestRatio float64
		for _, i := range live {
			gain := p.elems[i].andNotCount(cov)
			if gain == 0 {
				continue
			}
			ratio := float64(p.weight[i]) / float64(gain)
			if bestI == -1 || ratio < bestRatio {
				bestI, bestRatio = i, ratio
			}
		}
		if bestI == -1 {
			break
		}
		picked = append(picked, bestI)
		cov.or(p.elems[bestI])
	}
	sort.Slice(picked, func(a, b int) bool { return p.weight[picked[a]] > p.weight[picked[b]] })
	for k := 0; k < len(picked); {
		rest := covered.clone()
		for m, j := range picked {
			if m != k {
				rest.or(p.elems[j])
			}
		}
		if p.elems[picked[k]].subsetOf(rest) {
			picked = slices.Delete(picked, k, k+1)
			continue
		}
		k++
	}
	sort.Ints(picked)
	return picked
}

type search struct {
	p        *problem
	live     []int
	best     []int
	bestCost time.Duration
	nodes    int
}

// branch picks the open element with the fewest live cases covering it and
// tries each of them, pruning on a lower bound: the dearest of the open
// elements' cheapest covers.
func (s *search) branch(covered bitset, chosen []int, cost time.Duration) {
	s.nodes++
	if s.nodes > s.p.budget {
		return
	}
	pick, pickN := -1, 0
	var bound time.Duration
	for e := 0; e < s.p.nElem; e++ {
		if covered.has(e) {
			continue
		}
		n := 0
		cheapest := time.Duration(-1)
		for _, i := range s.live {
			if s.p.elems[i].has(e) {
				n++
				if cheapest < 0 || s.p.weight[i] < cheapest {
					cheapest = s.p.weight[i]
				}
			}
		}
		if cheapest > bound {
			bound = cheapest
		}
		if pick == -1 || n < pickN {
			pick, pickN = e, n
		}
	}
	if pick == -1 {
		if cost < s.bestCost {
			s.best, s.bestCost = slices.Clone(chosen), cost
			sort.Ints(s.best)
		}
		return
	}
	if cost+bound >= s.bestCost {
		return
	}
	var options []int
	for _, i := range s.live {
		if s.p.elems[i].has(pick) {
			options = append(options, i)
		}
	}
	sort.Slice(options, func(a, b int) bool { return s.p.weight[options[a]] < s.p.weight[options[b]] })
	for _, i := range options {
		next := covered.clone()
		next.or(s.p.elems[i])
		s.branch(next, append(chosen, i), cost+s.p.weight[i])
	}
}

// dominators names what catches removed case i's elements among the kept.
func (p *problem) dominators(i int, kept []int) Removal {
	r := Removal{Case: p.cases[i].ID, Saves: p.cases[i].Cost}
	if p.elems[i].andNotCount(newBitset(p.nElem)) == 0 {
		return r // it catches nothing, so nothing has to catch it instead
	}
	single := -1
	for _, k := range kept {
		if p.elems[i].subsetOf(p.elems[k]) && (single == -1 || p.weight[k] < p.weight[single]) {
			single = k
		}
	}
	if single >= 0 {
		r.DominatedBy = p.cases[single].ID
		r.CoveredBy = []string{r.DominatedBy}
		return r
	}
	need := p.elems[i].clone()
	got := newBitset(p.nElem)
	for need.andNotCount(got) > 0 {
		bestK, bestGain := -1, 0
		for _, k := range kept {
			gain := 0
			for w := range need {
				gain += bits.OnesCount64(need[w] & p.elems[k][w] &^ got[w])
			}
			if gain > bestGain {
				bestK, bestGain = k, gain
			}
		}
		if bestK == -1 {
			panic(fmt.Sprintf("redundancy: removed case %q has an element no kept case covers", r.Case))
		}
		got.or(p.elems[bestK])
		r.CoveredBy = append(r.CoveredBy, p.cases[bestK].ID)
	}
	return r
}
