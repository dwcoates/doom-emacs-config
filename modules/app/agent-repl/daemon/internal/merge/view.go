package merge

import (
	"cmp"
	"fmt"
	"slices"

	"agent-repld/internal/wsm"
)

// THE MERGE'S VIEW IS ONE PROJECTION OF ITS LEDGER (owner ruling,
// 2026-11-16). viewOf is pure: the ledger's rounds and the queue's hold go
// in, the merge's stages come out, and every drawing of the merge -- the
// roster's stage strip, the bubble's tabs, the footer's step -- renders that
// one value. No drawing decides for itself whether a round runs, so no two
// can disagree, and a queue that holds its merges draws every one of their
// rounds held: none can look active while the queue stands still.

// stageState is one stage's state in the view.
type stageState int

const (
	stageStateUnset stageState = iota
	// stageLive runs now.
	stageLive
	// stageHeld would run but stands still: its merge is suspended at it
	// (wsm.TabInterval.HeldSince), or its queue holds every merge in it.
	stageHeld
	// stageSucceeded settled and succeeded.
	stageSucceeded
	// stageFailed settled and did not succeed: failed, restarted by a later
	// step, or abandoned.
	stageFailed
)

// viewStage is one stage of a merge's view: one top-level step, over every
// round the ledger groups under it (stageGroup).
type viewStage struct {
	// kind is the stage's group kind (TabRebasing, TabTests, ...), and label
	// its drawn words.
	kind  string
	label string
	// round is the 1-based round of a stage drawn once per round, always 1
	// for a collapsed one.
	round int
	state stageState
	// startedMS is when its first round began, endedMS when its last round
	// ended (settled only), and heldMS when it was held (held only; zero for
	// a stage the queue's hold holds, which names no instant of its own).
	startedMS, endedMS, heldMS int64
}

// mergeView is a merge's stages, in the order each began.
type mergeView struct {
	stages []viewStage
}

// live reports whether any stage of the view runs.
func (v mergeView) live() bool {
	return slices.ContainsFunc(v.stages, func(s viewStage) bool { return s.state == stageLive })
}

// viewOf projects ROUNDS, one ledger's, into the merge's view. QUEUEHELD
// says the merge's queue holds every merge in it (a pause, a handover, or a
// deploy check it owes: queueHeld), which draws every round that would run
// as held. A kind or an outcome the ledger has no business holding is an
// error: only openTab and closeTab write it.
func viewOf(rounds []wsm.TabInterval, queueHeld bool) (mergeView, error) {
	type acc struct {
		viewStage
		live      bool
		heldMS    int64
		lastStart int64
		outcome   string
	}
	byKey := map[string]*acc{}
	var order []*acc
	for _, r := range rounds {
		group, collapsed, ok := stageGroup(r.Kind)
		if !ok {
			return mergeView{}, fmt.Errorf("merge: the ledger holds a tab of kind %q, which draws no stage", r.Kind)
		}
		n := r.Round
		if collapsed {
			n = 1
		}
		key := roundKey(group, n)
		st, seen := byKey[key]
		started := r.StartedAt.UnixMilli()
		if !seen {
			st = &acc{viewStage: viewStage{kind: group, label: tabWord(group), round: n, startedMS: started}}
			byKey[key] = st
			order = append(order, st)
		}
		st.startedMS = min(st.startedMS, started)
		if r.EndedAt == nil {
			st.live = true
			if r.HeldSince != nil {
				st.heldMS = r.HeldSince.UnixMilli()
			}
		} else {
			st.endedMS = max(st.endedMS, r.EndedAt.UnixMilli())
		}
		if !seen || started >= st.lastStart {
			st.lastStart, st.outcome = started, r.Outcome
		}
	}
	slices.SortStableFunc(order, func(a, b *acc) int { return cmp.Compare(a.startedMS, b.startedMS) })
	out := mergeView{stages: make([]viewStage, 0, len(order))}
	for _, st := range order {
		s := st.viewStage
		switch {
		case st.live && (st.heldMS != 0 || queueHeld):
			s.state, s.heldMS = stageHeld, st.heldMS
		case st.live:
			s.state = stageLive
		case st.outcome == "succeeded":
			s.state = stageSucceeded
		case st.outcome == "failed" || st.outcome == wsm.TabOutcomeRestarted || st.outcome == "abandoned":
			s.state = stageFailed
		default:
			return mergeView{}, fmt.Errorf("merge: the ledger closed a %s tab with outcome %q, which draws no stage state", s.label, st.outcome)
		}
		out.stages = append(out.stages, s)
	}
	return out, nil
}
