package gomeasure

import (
	"context"
	"fmt"
	"sort"
	"strings"
	"time"

	"agentrepl/testrun/internal/redundancy"
	"agentrepl/testrun/internal/redundancy/gomut"
)

// Verification mode (owner ruling, 2026-10-15, option (c)): instead of every
// case's whole kill set, prove one deletion set D safe against the kept rest
// K, mutant by mutant, stopping at the first kill wherever one settles it.
// For each mutant some D case reaches:
//
//  1. the kept UNIT-tier cases reaching it (a suite that spawns nothing) run
//     together, one process per package: a kill settles it;
//  2. else the D cases reaching it run cheapest first, stopping at the first
//     kill: none kills it, and deleting D loses nothing here;
//  3. else the kept integration and e2e cases reaching it run cheapest first,
//     stopping at the first kill: none kills it, and that is a LOSS. The D
//     case that killed it (the cheapest one, by step 2's order) is put back
//     and is kept from then on.
//
// R, the mutants step 1 leaves unsettled, is what the rest costs, so an
// estimate stops after step 1 and reports |R| with the cost of steps 2-3
// bounded from the measured runtimes.

// splitDeleted marks the leaves the deletion set names: a leaf by its case
// ID, or every leaf of a top-level test the set names (qualified by package
// when the measurement spans several).
func (m *measurer) splitDeleted(leaves []leaf) (map[leaf]bool, error) {
	named := map[string]bool{}
	for _, d := range m.cfg.Delete {
		named[d] = false
	}
	deleted := map[leaf]bool{}
	for _, l := range leaves {
		id := m.id(l)
		top := m.id(leaf{l.suite, Top(l.name)})
		for _, key := range []string{id, top} {
			if _, ok := named[key]; ok {
				deleted[l] = true
				named[key] = true
			}
		}
	}
	var unknown []string
	for d, found := range named {
		if !found {
			unknown = append(unknown, d)
		}
	}
	if len(unknown) > 0 {
		sort.Strings(unknown)
		return nil, fmt.Errorf("gomeasure: the deletion set names cases the measurement holds none of: %s", strings.Join(unknown, ", "))
	}
	return deleted, nil
}

// verify runs the three steps for every mutant a deleted case reaches.
func (m *measurer) verify(ctx context.Context, schemas []*gomut.Schema, imports map[string]string, leaves []leaf, cases []redundancy.Case, healthy map[leaf]time.Duration, deleted map[leaf]bool) (*redundancy.Verification, []string, error) {
	cost := map[leaf]time.Duration{}
	for i, l := range leaves {
		cost[l] = cases[i].Cost
	}
	reaching, err := newReachIndex(imports, leaves, cases)
	if err != nil {
		return nil, nil, err
	}
	v := &redundancy.Verification{Estimate: m.cfg.Estimate}
	for l := range deleted {
		v.Deleted = append(v.Deleted, m.id(l))
	}
	sort.Strings(v.Deleted)
	putBack := map[leaf]bool{}
	// A pinned case stays whatever it covers (a log-record assertion, or one
	// named with -pin), so a deletion set naming one has it put back first.
	for i, l := range leaves {
		if deleted[l] && cases[i].Pinned != "" {
			putBack[l] = true
			v.PutBack = append(v.PutBack, redundancy.PutBack{Case: m.id(l), Mutant: "(pinned: " + cases[i].Pinned + ")"})
		}
	}
	var anomalies []string
	byCost := func(ls []leaf) []leaf {
		sort.SliceStable(ls, func(i, j int) bool { return cost[ls[i]] < cost[ls[j]] })
		return ls
	}
	// first answers the cheapest of ls that kills mu. Unit-tier leaves cost
	// milliseconds, so they run together, one process per package, and the
	// cheapest that failed is the answer; integration and e2e leaves cost
	// seconds, so they run one at a time, cheapest first, to the first kill.
	first := func(mu gomut.Mutant, ls []leaf) (leaf, bool, error) {
		var unit, tier []leaf
		for _, l := range ls {
			if len(m.suites[l.suite].Programs) == 0 {
				unit = append(unit, l)
			} else {
				tier = append(tier, l)
			}
		}
		if len(unit) > 0 {
			k, anom, err := m.killers(ctx, mu, unit, healthy)
			if err != nil {
				return leaf{}, false, err
			}
			anomalies = append(anomalies, anom...)
			if len(k) > 0 {
				return byCost(k)[0], true, nil
			}
		}
		for _, l := range byCost(tier) {
			k, anom, err := m.killers(ctx, mu, []leaf{l}, healthy)
			if err != nil {
				return leaf{}, false, err
			}
			anomalies = append(anomalies, anom...)
			if len(k) > 0 {
				return l, true, nil
			}
		}
		return leaf{}, false, nil
	}
	for _, s := range schemas {
		for _, mu := range s.Live() {
			var dReach, unitReach, tierReach []leaf
			for _, l := range reaching.of(mu) {
				switch {
				case deleted[l] && !putBack[l]:
					dReach = append(dReach, l)
				case len(m.suites[l.suite].Programs) == 0:
					unitReach = append(unitReach, l)
				default:
					tierReach = append(tierReach, l)
				}
			}
			if len(dReach) == 0 {
				continue
			}
			if err := m.awaitQuiet(ctx, "mutant "+mu.Key()); err != nil {
				return nil, nil, err
			}
			v.Reached++
			if len(unitReach) > 0 {
				k, anom, err := m.killers(ctx, mu, unitReach, healthy)
				if err != nil {
					return nil, nil, err
				}
				anomalies = append(anomalies, anom...)
				if len(k) > 0 {
					v.SettledByUnit++
					continue
				}
			}
			v.Residual++
			var dLow, dHigh time.Duration
			for i, l := range byCost(dReach) {
				if i == 0 {
					dLow = cost[l]
				}
				dHigh += cost[l]
			}
			var kHigh time.Duration
			for _, l := range tierReach {
				kHigh += cost[l]
			}
			v.ResidualCostLow += dLow
			v.ResidualCostHigh += dHigh + kHigh
			if m.cfg.Estimate {
				continue
			}
			killer, killed, err := first(mu, dReach)
			if err != nil {
				return nil, nil, err
			}
			if !killed {
				v.SurvivedDeleted++
				continue
			}
			_, kept, err := first(mu, tierReach)
			if err != nil {
				return nil, nil, err
			}
			if kept {
				v.SettledByTier++
				continue
			}
			putBack[killer] = true
			v.PutBack = append(v.PutBack, redundancy.PutBack{Case: m.id(killer), Mutant: mu.Key()})
		}
		m.log.Infof("redundancy: verify: %d mutants reached by the deletion set so far, %d unsettled by the unit tier, %d put back", v.Reached, v.Residual, len(v.PutBack))
	}
	if !m.cfg.Estimate {
		for l := range deleted {
			if !putBack[l] {
				v.Proven = append(v.Proven, m.id(l))
			}
		}
		sort.Strings(v.Proven)
	}
	return v, anomalies, nil
}
