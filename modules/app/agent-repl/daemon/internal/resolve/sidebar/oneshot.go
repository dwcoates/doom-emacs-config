package sidebar

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/ladder"
	"claude-repld/internal/wsm"
)

// THE ONE-SHOT OUTCOME (frontend.v1.RosterRowOneShot): whether a one-shot
// workspace has ENDED WITHOUT A LIVE MERGE behind it.
//
// IT IS NOT READ OFF THE STATUS ARM. A merge the turn asked for is RECORDED
// during that turn and put in line only after it ends (merge-landing.md), so
// the arm reads `done` with no merge for at least one push even when the
// one-shot did ask for its merge. The merge orchestrator therefore tells the
// roster the moment a request is recorded (SetMergeRequested), which is always
// before the requesting turn ends: by the time SetTurnEnded asks whether a
// merge is live, the answer is already true.
//
// The outcome is a fact kept per workspace and moved by five edges:
//   - a turn ENDING with no merge live sets it failed;
//   - a merge FAILING sets it failed;
//   - a DETACHED merge ending without landing sets it failed;
//   - a merge being REQUESTED, QUEUED or RUNNING clears it;
//   - a turn reaching THINKING clears it (owner ruling, 2026-10-11).
//
// A DETACHED MERGE RUNS UNDER ANOTHER WORKSPACE'S ID. Once handed off, its
// facts are published for the merge workspace, never for the requester, so
// the requester's merge is live through its request fact: raised when the
// request is recorded, again at the hand-off and on recovery, and lowered
// only by the request's withdrawal or the detached merge's end
// (SetDetachedMergeEnded), which every end of a detached merge reports.
//
// A daemon that saw none of those edges (a restart, a handover's successor)
// seeds it ONCE from the durable record (seedOneShot).

// mergeLive reports a merge standing behind the workspace: requested and
// waiting for its turn's end, waiting in line, or running. A failed or landed
// merge is not live.
func (s *wsState) mergeLive() bool {
	return s.mergeRequested || ladder.MergeClaim(s.merge.State) == ladder.Merging
}

// reachedThinking reports a turn in flight that has moved past `submitting`:
// the fact that draws `thinking`.
func (s *wsState) reachedThinking() bool {
	return s.turn != nil && s.sawActivity
}

// setOneShotFailed moves the outcome, recording the edge that moved it when it
// changes. Every edge marks the outcome settled, so the durable seed never
// overwrites a live decision.
func (s *wsState) setOneShotFailed(failed bool, edge string, log dlog.Logger) {
	s.oneShotSettled = true
	if s.oneShotFailed == failed {
		return
	}
	s.oneShotFailed = failed
	log.Info("daemon.sidebar.one_shot_outcome", "the one-shot outcome changed", dlog.Context{
		"failed": failed,
		"edge":   edge,
	})
}

// noteTurnEndedForOneShot is the turn-end edge: a turn that ends with no merge
// live leaves the one-shot failed, and one with a merge live leaves it armed.
func (s *wsState) noteTurnEndedForOneShot(log dlog.Logger) {
	if s.mergeLive() {
		s.setOneShotFailed(false, "turn_ended_with_merge_live", log)
		return
	}
	s.setOneShotFailed(true, "turn_ended_without_merge", log)
}

// noteMergeForOneShot is the merge-facts edge: a failed merge sets the one-shot
// failed, a queued or running one clears it, and a landed merge or a merge the
// orchestrator forgot moves nothing.
func (s *wsState) noteMergeForOneShot(facts footer.MergeFacts, log dlog.Logger) {
	switch ladder.MergeClaim(facts.State) {
	case ladder.MergeFailed:
		s.setOneShotFailed(true, "merge_failed", log)
	case ladder.Merging:
		s.setOneShotFailed(false, "merge_live", log)
	}
}

// noteDetachedMergeEndedForOneShot is the detached-merge edge: the request
// handed to the merge workspace stands no longer, and a merge that did not
// land -- failed, abandoned, or never forked -- sets the one-shot failed. A
// landed merge closes the requester, so it moves nothing.
func (s *wsState) noteDetachedMergeEndedForOneShot(landed bool, log dlog.Logger) {
	s.mergeRequested = false
	if !landed {
		s.setOneShotFailed(true, "detached_merge_not_landed", log)
	}
}

// noteRenderForOneShot is the thinking edge, taken at every render: a turn
// that has reached `thinking` clears a failed one-shot.
func (s *wsState) noteRenderForOneShot(log dlog.Logger) {
	if s.oneShotFailed && s.reachedThinking() {
		s.setOneShotFailed(false, "turn_reached_thinking", log)
	}
}

// seedOneShot settles the outcome ONCE from the durable record, while no live
// edge has decided it: a workspace whose last turn's result stands, with no
// merge queue entry (its own, or its detached merge's merge workspace's) and
// no landed merge, ended without a live merge. The
// record's MergePending is read in the same read as the result, which is what
// makes the seed right at boot, where the roster is published BEFORE the merge
// orchestrator recovers its queue and so before any live merge fact arrives.
func (s *wsState) seedOneShot(rec wsm.Workspace, log dlog.Logger) {
	if s.oneShotSettled {
		return
	}
	failed := rec.Result != nil && rec.MergedAt == nil && !rec.MergePending
	s.setOneShotFailed(failed, "seeded_from_record", log)
}

// oneShot resolves the row's one-shot mark: nil for a workspace that was not
// created as a one-shot, otherwise its outcome.
func oneShot(rec wsm.Workspace, s *wsState) *frontendv1.RosterRowOneShot {
	if !rec.OneShot {
		return nil
	}
	if s.oneShotFailed {
		return &frontendv1.RosterRowOneShot{Outcome: &frontendv1.RosterRowOneShot_Failed{
			Failed: &frontendv1.RosterRowOneShotFailed{}}}
	}
	return &frontendv1.RosterRowOneShot{Outcome: &frontendv1.RosterRowOneShot_Armed{
		Armed: &frontendv1.RosterRowOneShotArmed{}}}
}
