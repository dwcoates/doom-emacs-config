package redundancy

import (
	"encoding/json"
	"fmt"
	"os"
	"sort"
	"strings"
	"time"
)

// DefaultMaxDrop is the statement-coverage points one request may cost a
// system before it is reported for arbitration (owner ruling, 2026-10-14):
// the owner's 5-point budget is ONE budget across every workspace, which the
// parent workspace shares out, and each workspace reports at 1 point.
const DefaultMaxDrop = 1.0

// TotalBudget is the owner's whole coverage budget: no system may fall more
// than this many points under its pinned baseline, across every workspace.
// A baseline's floor is its percent minus this.
const TotalBudget = 5.0

// SystemBaseline is one system's pinned all-tier coverage.
type SystemBaseline struct {
	Percent    float64
	Statements int
	// Floor is the least coverage the system may fall to.
	Floor float64
	// Tiers says which tiers' counters the percent counts.
	Tiers string `json:",omitempty"`
}

// Budget is the coverage a removal set may cost.
type Budget struct {
	// MaxDrop is the most points this request may cost any system, off the
	// baseline's percent (never past its pinned floor) or, for a system the
	// baseline does not hold, off the measured population's own coverage.
	MaxDrop float64
	// Baseline is the pinned per-system coverage; a system it holds is
	// estimated against it.
	Baseline map[string]SystemBaseline
}

// SystemCoverage is one system's statement coverage around a removal.
type SystemCoverage struct {
	System string
	// Reference is what the estimate starts from: the pinned baseline, or
	// the measured population's coverage when the baseline lacks the system.
	Reference  string
	Statements int
	// Before is the reference coverage; After the estimate once the removed
	// cases' lost statements are gone; Floor the least After may be.
	Before, After, Floor float64
}

// Drop is the points the removal costs.
func (s SystemCoverage) Drop() float64 { return s.Before - s.After }

// SystemOf names the system a block belongs to: the module, among modules,
// whose path is the longest prefix of the block's file
// ("agentrepl/shim-store/internal/db/x.go:1.1,2.2" is agentrepl/shim-store's).
func SystemOf(block string, modules []string) (string, error) {
	sys, ok := SystemFor(block, modules)
	if !ok {
		return "", fmt.Errorf("redundancy: block %s belongs to none of the measured modules %v", block, modules)
	}
	return sys, nil
}

// SystemFor is SystemOf for a caller to whom a block of no module is
// expected (a merged profile holds the harness's own blocks too): false
// when no module prefixes the block.
func SystemFor(block string, modules []string) (string, bool) {
	best := ""
	for _, mod := range modules {
		if strings.HasPrefix(block, mod+"/") && len(mod) > len(best) {
			best = mod
		}
	}
	return best, best != ""
}

// ApplyBudget holds a minimization to the budget. Statements maps every
// block of the measured scope, reached or not, to its statement count. A
// system's estimate after removal is its reference coverage minus the
// statements only removed cases reach, over the reference's statement count:
// a statement some unmeasured test also reaches is counted lost, so the
// estimate errs low. While any system's estimate is under its floor, the
// removed case that brings back the most of the failing systems' lost
// statements per unit of weight goes back. It returns the budgeted result,
// the cases put back in the order they went back, and each system's
// coverage.
func ApplyBudget(cases []Case, r Result, statements map[string]int, modules []string, b Budget, overhead time.Duration) (Result, []string, []SystemCoverage, error) {
	if len(statements) == 0 {
		return Result{}, nil, nil, fmt.Errorf("redundancy: the measurement carries no statement counts, so no coverage budget can hold")
	}
	systemOf := map[string]string{}
	total := map[string]int{}
	for blk, n := range statements {
		sys, err := SystemOf(blk, modules)
		if err != nil {
			return Result{}, nil, nil, err
		}
		systemOf[blk] = sys
		total[sys] += n
	}
	SystemOf := func(blk string) string { return systemOf[blk] }
	byID := map[string]Case{}
	for _, c := range cases {
		byID[c.ID] = c
		for _, blk := range c.Blocks {
			if _, ok := statements[blk]; !ok {
				return Result{}, nil, nil, fmt.Errorf("redundancy: case %s reaches block %s, which the measurement's statement counts do not hold", c.ID, blk)
			}
		}
	}
	coveredBy := func(ids []string) map[string]bool {
		out := map[string]bool{}
		for _, id := range ids {
			for _, blk := range byID[id].Blocks {
				out[blk] = true
			}
		}
		return out
	}
	pct := func(cov map[string]bool) map[string]float64 {
		got := map[string]int{}
		for blk := range cov {
			got[SystemOf(blk)] += statements[blk]
		}
		out := map[string]float64{}
		for sys, n := range total {
			out[sys] = 100 * float64(got[sys]) / float64(n)
		}
		return out
	}
	all := make([]string, 0, len(cases))
	for _, c := range cases {
		all = append(all, c.ID)
	}
	population := pct(coveredBy(all))
	type ref struct {
		name       string
		pct, floor float64
		stmts      int
	}
	refs := map[string]ref{}
	for sys, n := range total {
		if bl, ok := b.Baseline[sys]; ok {
			// The request's own limit, never past the pinned floor.
			refs[sys] = ref{"baseline", bl.Percent, max(bl.Floor, bl.Percent-b.MaxDrop), bl.Statements}
			continue
		}
		refs[sys] = ref{"measured population", population[sys], population[sys] - b.MaxDrop, n}
	}
	allCov := coveredBy(all)
	estimate := func(cov map[string]bool) map[string]float64 {
		lost := map[string]int{}
		for blk := range allCov {
			if !cov[blk] {
				lost[SystemOf(blk)] += statements[blk]
			}
		}
		out := map[string]float64{}
		for sys, r := range refs {
			out[sys] = r.pct - 100*float64(lost[sys])/float64(r.stmts)
		}
		return out
	}
	failing := func(after map[string]float64) map[string]bool {
		out := map[string]bool{}
		for sys, r := range refs {
			if after[sys] < r.floor-1e-9 {
				out[sys] = true
			}
		}
		return out
	}
	if bad := failing(estimate(allCov)); len(bad) > 0 {
		return Result{}, nil, nil, fmt.Errorf("redundancy: %s already under the coverage floor with every measured case kept, so no removal fits the budget", strings.Join(keys(bad), ", "))
	}
	kept := append([]string(nil), r.Kept...)
	removed := map[string]bool{}
	for _, rm := range r.Removed {
		removed[rm.Case] = true
	}
	var restored []string
	for {
		cov := coveredBy(kept)
		bad := failing(estimate(cov))
		if len(bad) == 0 {
			break
		}
		best, bestScore := "", 0.0
		for _, rm := range r.Removed {
			if !removed[rm.Case] {
				continue
			}
			gain := 0
			for _, blk := range byID[rm.Case].Blocks {
				if !cov[blk] && bad[SystemOf(blk)] {
					gain += statements[blk]
				}
			}
			if gain == 0 {
				continue
			}
			score := float64(gain) / float64(byID[rm.Case].Cost+overhead+1)
			if best == "" || score > bestScore {
				best, bestScore = rm.Case, score
			}
		}
		if best == "" {
			// Every floor held with every case kept (checked above), so some
			// removed case must bring back a lost statement.
			panic(fmt.Sprintf("redundancy: systems %v are over budget and no removed case brings back a statement", keys(bad)))
		}
		removed[best] = false
		restored = append(restored, best)
		kept = append(kept, best)
	}
	out := r
	out.Kept = nil
	out.KeptCost = 0
	keptSet := map[string]bool{}
	for _, id := range kept {
		keptSet[id] = true
	}
	for _, c := range cases {
		if keptSet[c.ID] {
			out.Kept = append(out.Kept, c.ID)
			out.KeptCost += c.Cost
		}
	}
	out.Removed = nil
	for _, rm := range r.Removed {
		if !keptSet[rm.Case] {
			out.Removed = append(out.Removed, rm)
		}
	}
	after := estimate(coveredBy(kept))
	var systems []SystemCoverage
	for sys, r := range refs {
		systems = append(systems, SystemCoverage{System: sys, Reference: r.name, Statements: r.stmts, Before: r.pct, After: after[sys], Floor: r.floor})
	}
	sort.Slice(systems, func(i, j int) bool { return systems[i].System < systems[j].System })
	return out, restored, systems, nil
}

func keys(m map[string]bool) []string {
	var out []string
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}

// BaselineFile is testrun/coverage-baseline.json: each system's all-tier
// statement coverage at one master commit, the one reference every
// workspace measures coverage loss against. It changes only by an explicit
// owner decision; nothing rewrites it after a removal.
type BaselineFile struct {
	Commit   string
	Measured string
	Systems  map[string]SystemBaseline
}

// ReadBaseline loads a baseline file.
func ReadBaseline(path string) (*BaselineFile, error) {
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("redundancy: read the coverage baseline %s: %w", path, err)
	}
	var f BaselineFile
	if err := json.Unmarshal(b, &f); err != nil {
		return nil, fmt.Errorf("redundancy: decode the coverage baseline %s: %w", path, err)
	}
	if f.Commit == "" || len(f.Systems) == 0 {
		return nil, fmt.Errorf("redundancy: the coverage baseline %s names no commit or no system", path)
	}
	for sys, s := range f.Systems {
		if s.Statements <= 0 || s.Percent < 0 || s.Percent > 100 || s.Floor > s.Percent {
			return nil, fmt.Errorf("redundancy: the coverage baseline %s holds an impossible entry for %s: %+v", path, sys, s)
		}
	}
	return &f, nil
}

// LoadBudget is the budget a command line names: maxDrop points, against the
// baseline file at path when path is set.
func LoadBudget(maxDrop float64, path string) (Budget, error) {
	if maxDrop < 0 {
		return Budget{}, fmt.Errorf("redundancy: -max-coverage-drop must not be negative, got %g", maxDrop)
	}
	b := Budget{MaxDrop: maxDrop}
	if path == "" {
		return b, nil
	}
	f, err := ReadBaseline(path)
	if err != nil {
		return Budget{}, err
	}
	b.Baseline = f.Systems
	return b, nil
}

// WriteBaseline writes a baseline file to path.
func WriteBaseline(path string, f *BaselineFile) error {
	b, err := json.MarshalIndent(f, "", "  ")
	if err != nil {
		return fmt.Errorf("redundancy: encode the coverage baseline: %w", err)
	}
	if err := os.WriteFile(path, append(b, '\n'), 0o644); err != nil {
		return fmt.Errorf("redundancy: write the coverage baseline %s: %w", path, err)
	}
	return nil
}
