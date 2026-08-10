package ssm

import (
	"database/sql"
	"fmt"
)

// This file answers ONE question: what ever takes a workspace OFF a terminal
// `merge_failed`.
//
// Nothing did. `merge_failed` ranks above the whole session-status ladder
// (rank 2, resolve.go), the merge axis has no natural closing edge — only
// another merge row supersedes a merge row — and the row is durable, so it
// replays out of the state log on every reload. A workspace whose merge failed
// therefore pushed RENDER_STATE_MERGE_FAILED on every generation change for the
// rest of its life. Observed live on subagent-token-bubbles and
// async-gui-open-oyn: a hard restart brought a healthy session up underneath a
// merge verdict that stayed on screen.
//
// THE VERDICT IS NOT DELETED, IT IS SUPERSEDED. The `merge_failed` row and its
// cause text stay in the append-only log exactly as written, so the failure
// remains readable up to the moment a clear edge runs; what the clear appends is
// a NEWER `merge_none` row saying the axis no longer claims this workspace.
//
// THERE ARE EXACTLY TWO CLEAR EDGES, and both are structural — a user action
// that supersedes the verdict, never a timer and never a heuristic:
//
//   - A NEW MERGE. Enqueuing another merge for the workspace is a fresh attempt
//     whose own phase rows supersede the old verdict (the enqueue's
//     `merge_enqueuing` row is the newest merge row from then on), and the stale
//     run's retained MergeStatus is retired with it — see applyMergeTransition,
//     which routes `merge_enqueuing` through retirePipelineStatusLocked for
//     exactly this reason.
//
//   - AN EXPLICIT SESSION RESTART. A hard restart (RestartSessionCmd) is a user
//     who has seen the failure and asked for an operational session anyway. A
//     terminal verdict the user has already acted on may not mask that session,
//     so the restart clears the axis here.
//
// Once either edge has run, the resolver prefers the live session again with no
// new rule of its own: `merge_none` is excluded from the resolver's merge
// candidate (resolveQuery's `WHERE state <> 'merge_none'`), so the merge axis
// simply stops bidding and the session-status rows win on their own merits.

// ClearFailedMergeAxis appends `merge_none` when workspace's newest merge-axis
// row is the terminal `merge_failed`, and reports whether it appended.
//
// A workspace resting on ANY other merge token is left untouched. A queued,
// merging, or conflicted merge is genuinely in flight and a restart is often
// exactly what such a merge asked for; a `merged` row is retired by its own
// reopen edge (mergereopen.go) and is not a failure the user acted on.
//
// The retained pipeline MergeStatus goes with the axis, AFTER the append is
// durable and BEFORE the push — the same ordering, and for the same reason, as
// the reopen retirement in connectivity.go: a status that outlived its axis
// trips stampMergeStatusLocked's invariant guard on every later resolve.
func (m *Manager) ClearFailedMergeAxis(workspace, cause string) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: ClearFailedMergeAxis got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()

	token, found, err := newestMergeToken(m.db, workspace)
	if err != nil {
		m.warn("ssm: merge axis clear FAILED ws=%q cause=%q branch=read-newest: %v", workspace, cause, err)
		return false, err
	}
	if !found || token != sigMergeFailed {
		m.logf("ssm: merge axis clear SKIPPED ws=%q cause=%q axis=%q — only a terminal `merge_failed` is cleared here; every other merge row describes a merge that is still somebody's business",
			workspace, cause, token)
		return false, nil
	}
	at := m.nextAt()
	if err := appendRow(m.db, workspace, "", sigMergeNone, causeMergeFailedCleared, sql.NullInt64{}, at, ""); err != nil {
		m.warn("ssm: merge axis clear FAILED ws=%q cause=%q branch=append: %v", workspace, cause, err)
		return false, err
	}
	m.retirePipelineStatusLocked(workspace, causeMergeFailedCleared)
	m.clearMergeDequeueOfferLocked(workspace, causeMergeFailedCleared)
	m.logf("ssm: merge axis CLEARED ws=%q cause=%q at=%d — the workspace rested on a terminal `merge_failed` the user has acted on, so the axis stops claiming it and its live session resolves its own state (the failed row and its cause text are untouched in the log)",
		workspace, cause, at)
	if err := m.reresolveLocked(workspace, causeMergeFailedCleared, 0); err != nil {
		return false, err
	}
	return true, nil
}
