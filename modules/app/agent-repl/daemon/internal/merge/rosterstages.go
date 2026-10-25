package merge

import (
	"cmp"
	"fmt"
	"slices"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// This file reads a merge's stages off its ledger for the roster's merge queue
// view (frontend.v1.RosterMergeQueueHead.stages). THE LEDGER IS THE ONE TRUTH:
// every tab round a run opens is recorded there with its start (openTab), and
// its end and outcome when it closes (closeTab), so the stages survive a
// daemon restart exactly as the bubble's durable tabs do.

// stageGroup is the top-level stage a ledger kind is drawn under: a conflict
// resolution under the rebasing stage and a fixing attempt under the tests
// stage, as the bubble draws them (frontTabLabel). COLLAPSED reports a stage
// drawn ONCE over all its rounds, as the bubble's one rebasing and one tests
// tab are. An unknown kind answers false.
func stageGroup(kind string) (group string, collapsed bool, ok bool) {
	switch kind {
	case TabRebasing, TabConflicts:
		return TabRebasing, true, true
	case TabTests, TabFixes:
		return TabTests, true, true
	case TabQueue, TabPick, TabCommitting, TabPreDeploy, TabDeploy, TabPostDeploy:
		return kind, false, true
	}
	return "", false, false
}

// stage is one drawn stage as it is accumulated over its rounds.
type stage struct {
	label   string
	round   int
	started int64
	ended   int64
	live    bool
	// lastStart and outcome are the latest round's, whose outcome a settled
	// collapsed stage takes.
	lastStart int64
	outcome   string
}

// stagesOf answers the stages INTERVALS draw, in the order each began. A kind
// or an outcome the ledger has no business holding is an error: the ledger is
// written only by openTab and closeTab, so either is a contract violation.
func stagesOf(intervals []wsm.TabInterval) ([]*frontendv1.RosterMergeQueueStage, error) {
	byKey := map[string]*stage{}
	var order []*stage
	for _, interval := range intervals {
		group, collapsed, ok := stageGroup(interval.Kind)
		if !ok {
			return nil, fmt.Errorf("merge: the ledger holds a tab of kind %q, which draws no stage", interval.Kind)
		}
		round := interval.Round
		if collapsed {
			round = 1
		}
		key := roundKey(group, round)
		st, seen := byKey[key]
		started := interval.StartedAt.UnixMilli()
		if !seen {
			st = &stage{label: tabWord(group), round: round, started: started}
			byKey[key] = st
			order = append(order, st)
		}
		st.started = min(st.started, started)
		if interval.EndedAt == nil {
			st.live = true
		} else {
			st.ended = max(st.ended, interval.EndedAt.UnixMilli())
		}
		if !seen || started >= st.lastStart {
			st.lastStart = started
			st.outcome = interval.Outcome
		}
	}
	slices.SortStableFunc(order, func(a, b *stage) int { return cmp.Compare(a.started, b.started) })
	out := make([]*frontendv1.RosterMergeQueueStage, 0, len(order))
	for _, st := range order {
		drawn := &frontendv1.RosterMergeQueueStage{Label: st.label, Round: uint32(st.round)}
		switch {
		case st.live:
			drawn.State = &frontendv1.RosterMergeQueueStage_Live{Live: &frontendv1.RosterMergeQueueStageLive{StartedAtMs: st.started}}
		case st.outcome == "succeeded":
			drawn.State = &frontendv1.RosterMergeQueueStage_Succeeded{Succeeded: &frontendv1.RosterMergeQueueStageSucceeded{StartedAtMs: st.started, EndedAtMs: st.ended}}
		case st.outcome == "failed" || st.outcome == "restarted" || st.outcome == "abandoned":
			drawn.State = &frontendv1.RosterMergeQueueStage_Failed{Failed: &frontendv1.RosterMergeQueueStageFailed{StartedAtMs: st.started, EndedAtMs: st.ended}}
		default:
			return nil, fmt.Errorf("merge: the ledger closed a %s tab with outcome %q, which draws no stage state", st.label, st.outcome)
		}
		out = append(out, drawn)
	}
	return out, nil
}
