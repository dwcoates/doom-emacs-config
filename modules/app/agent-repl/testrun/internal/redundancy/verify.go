package redundancy

import (
	"fmt"
	"sort"
	"strings"
)

// VerifyMatrix proves a deletion set against a WHOLE kill matrix (an adapter
// that answers every killer of every mutant, as Stryker does), with the same
// verdicts the Go adapter reaches mutant by mutant: a mutant only deleted
// cases kill is a loss, and the cheapest deleted killer goes back and is kept
// from then on; a pinned case in the set goes back first. deleted names case
// IDs, or prefixes standing for every case they begin (a test file, a
// describe block) when followed by a separator the adapter uses (sep).
func VerifyMatrix(m *Measurement, deleted []string, sep string) (*Verification, error) {
	byID := map[string]Case{}
	for _, c := range m.Cases {
		byID[c.ID] = c
	}
	del := map[string]bool{}
	var unknown []string
	for _, d := range deleted {
		found := false
		for _, c := range m.Cases {
			if c.ID == d || strings.HasPrefix(c.ID, d+sep) {
				del[c.ID] = true
				found = true
			}
		}
		if !found {
			unknown = append(unknown, d)
		}
	}
	if len(unknown) > 0 {
		sort.Strings(unknown)
		return nil, fmt.Errorf("redundancy: the deletion set names cases the measurement holds none of: %s", strings.Join(unknown, ", "))
	}
	v := &Verification{}
	for id := range del {
		v.Deleted = append(v.Deleted, id)
	}
	sort.Strings(v.Deleted)
	back := map[string]bool{}
	for _, id := range v.Deleted {
		if why := byID[id].Pinned; why != "" {
			back[id] = true
			v.PutBack = append(v.PutBack, PutBack{Case: id, Mutant: "(pinned: " + why + ")"})
		}
	}
	for _, r := range m.Mutants {
		if r.Status == "dropped" || r.Status == "unreached" {
			continue
		}
		var dKill, dReach []string
		keptKills := false
		reachedByD := false
		for _, id := range r.KilledBy {
			if del[id] && !back[id] {
				dKill = append(dKill, id)
			} else {
				keptKills = true
			}
		}
		for _, id := range r.CoveredBy {
			if del[id] && !back[id] {
				reachedByD = true
				dReach = append(dReach, id)
			}
		}
		// A mutant detected by a timeout or a runtime error names no
		// killer, so no deleted case reaching it can be shown not to catch
		// it: it counts as killed by each of them.
		if r.Status == "unattributed" {
			dKill = append(dKill, dReach...)
		}
		if len(dKill) > 0 {
			reachedByD = true
		}
		if !reachedByD {
			continue
		}
		v.Reached++
		switch {
		case keptKills:
			v.SettledByTier++
		case len(dKill) == 0:
			v.SurvivedDeleted++
		default:
			sort.Slice(dKill, func(i, j int) bool {
				if byID[dKill[i]].Cost != byID[dKill[j]].Cost {
					return byID[dKill[i]].Cost < byID[dKill[j]].Cost
				}
				return dKill[i] < dKill[j]
			})
			back[dKill[0]] = true
			v.PutBack = append(v.PutBack, PutBack{Case: dKill[0], Mutant: r.Key})
		}
	}
	for _, id := range v.Deleted {
		if !back[id] {
			v.Proven = append(v.Proven, id)
		}
	}
	return v, nil
}
