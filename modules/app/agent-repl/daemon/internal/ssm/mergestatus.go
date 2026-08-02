package ssm

import (
	"database/sql"
	"fmt"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// This file projects the merge axis onto WorkspaceState.merge_status.
//
// THE PROJECTION IS COARSE ON PURPOSE. The merge axis is six tokens over an
// append-only log; MergeStatus is the shape the merge PIPELINE will fill in
// (per-commit progress, the before/after actions, the suite gating each
// landing), and that pipeline does not exist yet. Everything the current
// system genuinely knows is mapped here, and everything it does not is left
// at its zero value rather than invented: `merging` reports cherry_picking
// with no commit counts because no component counts commits yet, and the
// before/after action and testing phases are never produced because nothing
// runs them.
//
// The old trio (merge_phase, merge_queue_position, merge_queue_depth) is still
// stamped beside it. Both forms coexist until the cutover wave retires the
// trio, so nothing downstream has to move in the same change that introduces
// the status.

// mergeTransitionRow is what the newest merge-axis row adds to the token the
// resolution already produced: the instant it was written, and the cause detail
// the transition carried.
//
// The row's own token is deliberately NOT read back. It would be the same token
// `merge_phase` already carries — the resolution's latest_merge CTE and the
// query below order the same rows the same way under the same lock — and a
// second copy of one fact is a second thing that can disagree.
type mergeTransitionRow struct {
	causeKind string
	at        int64
}

// newestMergeTransitionLocked reads the workspace's newest merge-axis row.
// Caller holds mu.
//
// It selects over the SAME token set resolveQuery's latest_merge CTE does
// (mergeAxisStates), which is what makes "the newest merge row" mean one thing
// in the resolver and here. A workspace with no merge history at all reports
// found=false, which is the only reading of absence.
func (m *Manager) newestMergeTransitionLocked(workspace string) (row mergeTransitionRow, found bool, err error) {
	query := `
SELECT cause_kind, at FROM workspace_state
WHERE workspace = ? AND state IN (?,?,?,?,?,?,?)
ORDER BY at DESC LIMIT 1`
	args := make([]any, 0, len(mergeAxisStates)+1)
	args = append(args, workspace)
	for _, s := range mergeAxisStates {
		args = append(args, s)
	}
	var causeKind sql.NullString
	err = m.db.QueryRow(query, args...).Scan(&causeKind, &row.at)
	if err == sql.ErrNoRows {
		return mergeTransitionRow{}, false, nil
	}
	if err != nil {
		return mergeTransitionRow{}, false, fmt.Errorf("ssm: read newest merge transition for workspace %q: %w", workspace, err)
	}
	row.causeKind = causeKind.String
	return row, true, nil
}

// mergeTransitionCause strips the routing prefix off a merge transition's cause
// kind, leaving the human note the transition was recorded with.
//
// ApplyMergeTransition writes `merge_transition` when there was no note and
// `merge_transition:<note>` when there was. The prefix is how the state log
// routes the row; it is not part of what failed, and putting it in front of a
// user-facing cause would report the daemon's own bookkeeping as the reason a
// merge did not land.
func mergeTransitionCause(causeKind string) string {
	if causeKind == causeMergeTransition {
		return ""
	}
	return strings.TrimPrefix(causeKind, causeMergeTransition+":")
}

// mergeRunIDFor mints the run identity every MergeStatus carries.
//
// IT IS A PLACEHOLDER. A real run id is minted by the merge pipeline when it
// accepts a request and stays fixed for the whole run; this one is derived from
// the workspace and the newest merge row's instant, so it changes on every
// phase transition rather than naming one run. That is the best a projection
// over the state log can do — the log has no run identity in it — and it is
// replaced wholesale when the pipeline wave lands. Nothing may correlate on it
// until then.
func mergeRunIDFor(workspace string, at int64) string {
	return fmt.Sprintf("%s@%d", workspace, at)
}

// mergeStatusFor maps a merge-axis token and its row onto the status that
// describes it, coarsely (see the file comment). An unknown or axis-clearing
// token reports nil, which leaves merge_status unset.
func mergeStatusFor(workspace, token string, row mergeTransitionRow, position, depth int32) *frontendv1.MergeStatus {
	status := &frontendv1.MergeStatus{
		RunId:            mergeRunIDFor(workspace, row.at),
		PhaseStartedAtMs: row.at,
		// The SAME instant as the phase's own: the state log records phase
		// transitions and nothing else, so there are no within-phase ticks to
		// report until the merge pipeline produces them.
		UpdatedAtMs: row.at,
	}
	switch token {
	case sigMergeEnqueuing, sigMergeQueued:
		// BOTH map to enqueued, and the queue facts come from the same snapshot
		// that fills merge_queue_position/depth. merge_enqueuing has no durable
		// queue entry behind it yet, so its position and depth are legitimately
		// zero rather than missing.
		status.Phase = &frontendv1.MergeStatus_Enqueued{Enqueued: &frontendv1.MergeStatusEnqueued{
			Position: position,
			Depth:    depth,
		}}
	case sigMerging:
		// Counts stay zero: the current pipeline picks commits without ever
		// reporting how many there are, so a number here would be invented.
		status.Phase = &frontendv1.MergeStatus_CherryPicking{CherryPicking: &frontendv1.MergeStatusCherryPicking{}}
	case sigMergeConflict:
		// The conflicted commit is likewise unknown to the current pipeline.
		status.Phase = &frontendv1.MergeStatus_Conflict{Conflict: &frontendv1.MergeStatusConflict{}}
	case sigMerged:
		status.Phase = &frontendv1.MergeStatus_Merged{Merged: &frontendv1.MergeStatusMerged{}}
	case sigMergeFailed:
		status.Phase = &frontendv1.MergeStatus_Failed{Failed: &frontendv1.MergeStatusFailed{
			Cause: mergeTransitionCause(row.causeKind),
		}}
	default:
		return nil
	}
	return status
}

// stampMergeStatusLocked writes merge_status onto a WorkspaceState about to be
// pushed. Caller holds mu.
//
// IT RIDES THE SAME CONSTRUCTION FUNNEL the rest of the merge facts do
// (stampMergeFactsLocked, called from workspaceMessageLocked and nowhere else),
// so a frame carrying merge_phase but no merge_status is unrepresentable rather
// than merely unlikely.
//
// ABSENCE IS THE ABSENCE OF A MERGE. A workspace whose merge axis has never
// spoken, or whose axis was cleared, gets no status at all — never a
// zero-valued one standing in for "no merge".
func (m *Manager) stampMergeStatusLocked(workspace string, msg *frontendv1.WorkspaceState, position, depth int32) {
	token := msg.GetMergePhase()
	if token == "" {
		return
	}
	row, found, err := m.newestMergeTransitionLocked(workspace)
	if err != nil {
		m.logf("ssm: merge status read FAILED ws=%s merge_phase=%s error=%v — the workspace is merging and its WorkspaceState carries no merge_status, so no frontend can report the run's progress",
			workspace, token, err)
		return
	}
	if !found {
		// The resolved merge phase came OFF a merge row, so its absence here
		// means the two reads disagree about the same log.
		m.logf("ssm: INVARIANT VIOLATION ws=%s resolved merge_phase=%s with NO merge-axis row behind it — merge_status is left unset",
			workspace, token)
		return
	}
	status := mergeStatusFor(workspace, token, row, position, depth)
	if status == nil {
		m.logf("ssm: INVARIANT VIOLATION ws=%s resolved merge_phase=%s which no MergeStatus phase covers — merge_status is left unset",
			workspace, token)
		return
	}
	msg.MergeStatus = status
}
