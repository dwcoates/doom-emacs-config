package merge

import (
	"fmt"

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
// stage, the step each serves. The bubble draws them as tabs of their own
// (frontTabLabel), but a stage is one step of the merge, and the ledger
// closes the rebasing round failed when its conflict is handed over, so a
// rebasing stage of its own rounds alone would read failed while the
// resolution runs. COLLAPSED reports a stage drawn ONCE over all its rounds,
// as the bubble's one rebasing and one tests tab are. An unknown kind answers
// false.
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

// stagesOf answers the stages INTERVALS draw on the roster, in the order each
// began: the rendering of the merge's one view (viewOf) under QUEUEHELD.
func stagesOf(intervals []wsm.TabInterval, queueHeld bool) ([]*frontendv1.RosterMergeQueueStage, error) {
	view, err := viewOf(intervals, queueHeld)
	if err != nil {
		return nil, err
	}
	return rosterStages(view), nil
}

// rosterStages renders VIEW as the roster's stage strip.
func rosterStages(view mergeView) []*frontendv1.RosterMergeQueueStage {
	out := make([]*frontendv1.RosterMergeQueueStage, 0, len(view.stages))
	for _, st := range view.stages {
		drawn := &frontendv1.RosterMergeQueueStage{Label: st.label, Round: uint32(st.round)}
		switch st.state {
		case stageLive:
			drawn.State = &frontendv1.RosterMergeQueueStage_Live{Live: &frontendv1.RosterMergeQueueStageLive{StartedAtMs: st.startedMS}}
		case stageHeld:
			drawn.State = &frontendv1.RosterMergeQueueStage_Held{Held: &frontendv1.RosterMergeQueueStageHeld{StartedAtMs: st.startedMS, HeldSinceMs: st.heldMS}}
		case stageSucceeded:
			drawn.State = &frontendv1.RosterMergeQueueStage_Succeeded{Succeeded: &frontendv1.RosterMergeQueueStageSucceeded{StartedAtMs: st.startedMS, EndedAtMs: st.endedMS}}
		case stageFailed:
			drawn.State = &frontendv1.RosterMergeQueueStage_Failed{Failed: &frontendv1.RosterMergeQueueStageFailed{StartedAtMs: st.startedMS, EndedAtMs: st.endedMS}}
		default:
			panic(fmt.Sprintf("merge: a view stage of %s has no state", st.label))
		}
		out = append(out, drawn)
	}
	return out
}
