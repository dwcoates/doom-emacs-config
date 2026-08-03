package ssm

import (
	"database/sql"
	"fmt"
	"sort"
	"strings"
)

type orphanTaskEnd struct {
	workspace string
	sessionID string
	taskID    string
}

// persistedOrphanTaskEnds returns ended task ids for which no start exists.
// Rows without task_id are legacy records whose synthetic row identity cannot
// be paired by appending a later row, so they remain excluded from the live set
// by resolve's Starts EXCEPT Ends projection.
func persistedOrphanTaskEnds(db *sql.DB) ([]orphanTaskEnd, error) {
	rows, err := db.Query(`
		SELECT e.workspace, COALESCE(MAX(e.session_id), ''), e.task_id
		FROM workspace_state e
		WHERE e.state = ? AND e.task_id IS NOT NULL
		  AND NOT EXISTS (
		    SELECT 1 FROM workspace_state s
		    WHERE s.workspace = e.workspace AND s.state = ? AND s.task_id = e.task_id
		  )
		GROUP BY e.workspace, e.task_id
		ORDER BY e.workspace, e.task_id`,
		sigTaskEnded, sigTaskStarted)
	if err != nil {
		return nil, fmt.Errorf("ssm: list persisted orphan task ends: %w", err)
	}
	defer rows.Close()
	var out []orphanTaskEnd
	for rows.Next() {
		var orphan orphanTaskEnd
		if err := rows.Scan(&orphan.workspace, &orphan.sessionID, &orphan.taskID); err != nil {
			return nil, fmt.Errorf("ssm: scan persisted orphan task end: %w", err)
		}
		out = append(out, orphan)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("ssm: iterate persisted orphan task ends: %w", err)
	}
	return out, nil
}

// repairPersistedOrphanTaskEndsLocked repairs historical orphan ends once at
// Open. The append-only repair records the start entailed by each observed end;
// a subsequent Open finds no orphan and emits no repeated alarm.
func (m *Manager) repairPersistedOrphanTaskEndsLocked() error {
	orphans, err := persistedOrphanTaskEnds(m.db)
	if err != nil {
		return err
	}
	if len(orphans) == 0 {
		return nil
	}
	tx, err := m.db.Begin()
	if err != nil {
		return fmt.Errorf("ssm: begin persisted orphan repair: %w", err)
	}
	committed := false
	defer func() {
		if !committed {
			_ = tx.Rollback()
		}
	}()
	byWorkspace := make(map[string][]string)
	for _, orphan := range orphans {
		if err := appendRow(tx, orphan.workspace, orphan.sessionID, sigTaskStarted,
			causeTaskReconciled, sql.NullInt64{}, m.nextAt(), orphan.taskID); err != nil {
			return err
		}
		byWorkspace[orphan.workspace] = append(byWorkspace[orphan.workspace], orphan.taskID)
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("ssm: commit persisted orphan repair: %w", err)
	}
	committed = true
	workspaces := make([]string, 0, len(byWorkspace))
	for ws := range byWorkspace {
		workspaces = append(workspaces, ws)
	}
	sort.Strings(workspaces)
	for _, ws := range workspaces {
		ids := byWorkspace[ws]
		m.logf("ssm: repaired persisted orphan task ends ws=%s count=%d task_ids=%s by appending entailed task_started rows",
			ws, len(ids), strings.Join(ids, ","))
	}
	return nil
}

// appendTaskEndLocked appends a TaskEnded event and, when its start was never
// observed, its entailed start in the same transaction. Caller holds m.mu.
//
// sessionID OWNS the rows (the daemon-minted id); eventSessionID is the store's
// own identity for the causing event, carried so the end stays idempotent
// against a replay. The entailed start is the daemon's inference rather than an
// event, so it records no store coordinate.
func (m *Manager) appendTaskEndLocked(workspace, sessionID, eventSessionID, causeKind string, causeSeq sql.NullInt64, taskID string) error {
	if taskID == "" {
		return fmt.Errorf("ssm: task_ended session=%s ws=%s has no task_id; refusing an unpairable lifecycle row",
			sessionID, workspace)
	}
	var started int
	err := m.db.QueryRow(
		`SELECT 1 FROM workspace_state WHERE workspace = ? AND state = ? AND task_id = ? LIMIT 1`,
		workspace, sigTaskStarted, taskID).Scan(&started)
	switch {
	case err == nil:
		return appendEventRow(m.db, workspace, sessionID, eventSessionID, sigTaskEnded, causeKind,
			causeSeq, m.nextAt(), taskID)
	case err != sql.ErrNoRows:
		return fmt.Errorf("ssm: inspect task start ws=%s task_id=%s: %w", workspace, taskID, err)
	}

	tx, err := m.db.Begin()
	if err != nil {
		return fmt.Errorf("ssm: begin orphan task end repair ws=%s task_id=%s: %w", workspace, taskID, err)
	}
	committed := false
	defer func() {
		if !committed {
			_ = tx.Rollback()
		}
	}()
	if err := appendRow(tx, workspace, sessionID, sigTaskStarted, causeTaskReconciled,
		sql.NullInt64{}, m.nextAt(), taskID); err != nil {
		return err
	}
	if err := appendEventRow(tx, workspace, sessionID, eventSessionID, sigTaskEnded, causeKind,
		causeSeq, m.nextAt(), taskID); err != nil {
		return err
	}
	if err := tx.Commit(); err != nil {
		return fmt.Errorf("ssm: commit orphan task end repair ws=%s task_id=%s: %w", workspace, taskID, err)
	}
	committed = true
	m.logf("ssm: repaired orphan task_ended ws=%s session=%s task_id=%s by appending entailed task_started before end",
		workspace, sessionID, taskID)
	return nil
}
