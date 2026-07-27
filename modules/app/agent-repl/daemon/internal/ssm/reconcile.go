package ssm

import (
	"database/sql"
	"fmt"
	"sort"
	"strings"
)

// causeTaskReconciled marks the rows ReconcileTasks appends. They carry no
// store seq (they are daemon-local, derived from a snapshot rather than from a
// single event), which is exactly how merge transitions are recorded too.
const causeTaskReconciled = "task_reconciled"

// ReconcileTasks adopts an AUTHORITATIVE live-task set for a session's
// workspace: after it returns, live_task_count equals len(liveTaskIDs).
//
// # Why an authority is needed at all
//
// live_task_count is derived from the append-only log as the distinct
// task-start set EXCEPT the distinct task-end set. That projection cannot go
// negative, but the log can still miss either half of a lifecycle:
//
//   - GHOST STARTS. A task whose start was logged and whose end never arrived
//     (the daemon was not watching when it finished) counts forever, and only a
//     slow LOST staleness sweep eventually closes it.
//   - ORPHAN ENDS. A task whose END is logged with no start anywhere. This is
//     not hypothetical: on 2026-07-26 a sidecar restart raced the store's,
//     cursor recovery failed, every watched transcript re-read from offset 0,
//     and 114 historical `task_ended` events landed without their partners —
//     `IMPOSSIBLE live_task_count=-114`, plus three smaller storms in the same
//     second.
//
// `data.BackgroundTasksChanged` is the SDK's full current live set, so it can
// settle both directions at once, immediately, instead of waiting for sweeps.
//
// # What it appends (and why appending is the honest repair)
//
// The log is append-only, so reconciliation is expressed as rows, never as
// edits:
//
//  1. An orphan end gets a `task_started`. A task cannot end without having
//     started, so recording the start is the ENTAILMENT of the end row already
//     observed — not an invention — and it is what returns the arithmetic to a
//     true set difference.
//  2. An open task absent from the authoritative set gets a `task_ended`. The
//     session says it is not running; the ghost is swept now rather than at the
//     next sweep.
//  3. An id in the set that was never started gets a `task_started`. The task is
//     running and this daemon simply never saw it begin.
//
// An id the set claims is live but whose end IS logged cannot be represented —
// re-adding a `task_started` for an id already in the started set changes no
// DISTINCT count — so it is loud-logged and left alone rather than papered over.
// The SDK reusing a retired task id has not been observed.
//
// Apply and Open repair orphan ends at their respective ingestion boundaries.
// This reconciliation remains the authoritative-set edge that also sweeps
// ghost starts and adopts unseen live tasks.
func (m *Manager) ReconcileTasks(sessionID string, liveTaskIDs []string) error {
	if sessionID == "" {
		return fmt.Errorf("ssm: ReconcileTasks got an empty session id")
	}
	if m.resolver == nil {
		return fmt.Errorf("ssm: no resolver injected; cannot bind session %s to a workspace", sessionID)
	}
	ws, bound := m.resolver.Workspace(sessionID)
	if !bound {
		return fmt.Errorf("ssm: no workspace bound to session %s (task reconciliation)", sessionID)
	}

	live := make(map[string]struct{}, len(liveTaskIDs))
	for _, id := range liveTaskIDs {
		if id != "" {
			live[id] = struct{}{}
		}
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	started, err := taskIDsWithState(m.db, ws, sigTaskStarted)
	if err != nil {
		return err
	}
	ended, err := taskIDsWithState(m.db, ws, sigTaskEnded)
	if err != nil {
		return err
	}

	var (
		orphanEnds  []string // ended, never started (rule 1)
		ghosts      []string // open, not in the live set (rule 2)
		unseenLives []string // live, never started (rule 3)
		reLived     []string // live, but already ended — unrepresentable
	)
	for id := range ended {
		if _, ok := started[id]; !ok {
			orphanEnds = append(orphanEnds, id)
		}
	}
	for id := range started {
		if _, isEnded := ended[id]; isEnded {
			continue
		}
		if _, isLive := live[id]; !isLive {
			ghosts = append(ghosts, id)
		}
	}
	for id := range live {
		_, wasStarted := started[id]
		_, wasEnded := ended[id]
		switch {
		case !wasStarted && !wasEnded:
			unseenLives = append(unseenLives, id)
		case wasEnded:
			reLived = append(reLived, id)
		}
	}
	// Sorted so the appended rows (and the log lines naming them) are
	// deterministic; map iteration order is not.
	sort.Strings(orphanEnds)
	sort.Strings(ghosts)
	sort.Strings(unseenLives)
	sort.Strings(reLived)

	if len(reLived) > 0 {
		m.logf("ssm: task reconciliation ws=%s reports %d task(s) live whose task_ended is already logged (%s); the append-only count cannot re-open them, so they stay closed",
			ws, len(reLived), strings.Join(reLived, ","))
	}
	if len(orphanEnds)+len(ghosts)+len(unseenLives) == 0 {
		return nil // the log already agrees with the session
	}

	for _, id := range orphanEnds {
		if err := m.appendReconciledLocked(ws, sessionID, sigTaskStarted, id); err != nil {
			return err
		}
	}
	for _, id := range unseenLives {
		if err := m.appendReconciledLocked(ws, sessionID, sigTaskStarted, id); err != nil {
			return err
		}
	}
	for _, id := range ghosts {
		if err := m.appendReconciledLocked(ws, sessionID, sigTaskEnded, id); err != nil {
			return err
		}
	}
	m.logf("ssm: task reconciliation ws=%s live=%d swept_ghosts=%d matched_orphan_ends=%d adopted_unseen=%d",
		ws, len(live), len(ghosts), len(orphanEnds), len(unseenLives))
	return m.reresolveLocked(ws, causeTaskReconciled, 0)
}

// appendReconciledLocked appends one reconciliation row. Caller holds m.mu.
func (m *Manager) appendReconciledLocked(workspace, sessionID, state, taskID string) error {
	return appendRow(m.db, workspace, sessionID, state, causeTaskReconciled, sql.NullInt64{}, m.nextAt(), taskID)
}

// taskIDsWithState returns the distinct task ids carrying `state` in the
// workspace's log. A row with no task id is keyed by its `at`, matching how the
// resolve query counts DISTINCT tasks, so a seq-less legacy row still counts
// exactly once.
func taskIDsWithState(db *sql.DB, workspace, state string) (map[string]struct{}, error) {
	rows, err := db.Query(
		`SELECT DISTINCT COALESCE(task_id, 'row:' || at) FROM workspace_state WHERE workspace = ? AND state = ?`,
		workspace, state)
	if err != nil {
		return nil, fmt.Errorf("ssm: list %s task ids for workspace %q: %w", state, workspace, err)
	}
	defer rows.Close()
	out := map[string]struct{}{}
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("ssm: scan %s task id for workspace %q: %w", state, workspace, err)
		}
		out[id] = struct{}{}
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("ssm: iterate %s task ids for workspace %q: %w", state, workspace, err)
	}
	return out, nil
}
