package statedb

import (
	"database/sql"
	"errors"
	"fmt"
)

// This file is the DURABLE half of the scheduled-shutdown drain lease.
//
// Two records live here, and they are separate because they outlive each other
// in opposite directions:
//
//   - shutdown_schedule is the LEASE ITSELF, and there is at most one for the
//     whole daemon. It exists so a daemon that CRASHES mid-drain reboots
//     knowing a shutdown was scheduled, rather than silently dropping a lease
//     every connected client was told about. It is cleared as the drain
//     EXECUTES, before the process leaves — a lease that survived its own
//     successful shutdown would block every prompt on the next daemon forever,
//     with nobody left who remembers asking for it.
//
//   - shutdown_hold_prompt is what the lease is HOLDING. A prompt submitted
//     while the lease stands is not refused and not classified; it is parked.
//     The whole point of parking rather than refusing is that the user loses
//     nothing, and that promise cannot be kept in memory alone, because the
//     very next thing the daemon does is exit. So each held prompt is written
//     here as it is parked, and the daemon that comes back after the bounce
//     reads them back and delivers them. These rows deliberately OUTLIVE the
//     schedule row: the lease is over, the prompts it delayed are not.
//
// The DDL is idempotent and purely ADDITIVE, so it carries no schema version of
// its own — the shared store's schema_meta row belongs to the session-state
// manager, and a table that appears by CREATE TABLE IF NOT EXISTS cannot make
// an older binary's reading of the rest of the database wrong.

// ShutdownSchedule is the daemon-global drain lease as it is persisted.
type ShutdownSchedule struct {
	// ScheduleID is the lease's identity, minted when the lease was taken. It
	// is what a cancel must match and what every held prompt names.
	ScheduleID string
	// ScheduledAtMs is when the lease was taken (epoch ms).
	ScheduledAtMs int64
	// Cause is the free-text display reason. Never parsed.
	Cause string
	// StopShims is the ShutdownCmd.stop_shims decision, fixed at schedule time.
	StopShims bool
}

// HeldPrompt is one prompt parked by a drain lease, as it is persisted.
type HeldPrompt struct {
	EntryID        string
	ScheduleID     string
	Workspace      string
	SessionID      string
	RequestID      string
	Text           string
	PermissionMode string
	PromptOrigin   int32
	QueuedAtMs     int64
}

// ShutdownSchedules owns the two drain-lease tables on the shared state store.
// It does not own the handle.
type ShutdownSchedules struct{ db *sql.DB }

// NewShutdownSchedules installs the drain-lease schema and returns its owner.
func NewShutdownSchedules(db *sql.DB) (*ShutdownSchedules, error) {
	if db == nil {
		return nil, fmt.Errorf("statedb: NewShutdownSchedules needs an open state store")
	}
	if _, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS shutdown_schedule (
			id              INTEGER PRIMARY KEY CHECK (id = 1),
			schedule_id     TEXT    NOT NULL,
			scheduled_at_ms INTEGER NOT NULL,
			cause           TEXT    NOT NULL,
			stop_shims      INTEGER NOT NULL
		);
		CREATE TABLE IF NOT EXISTS shutdown_hold_prompt (
			entry_id        TEXT    PRIMARY KEY,
			schedule_id     TEXT    NOT NULL,
			workspace       TEXT    NOT NULL,
			session_id      TEXT    NOT NULL,
			request_id      TEXT    NOT NULL,
			text            TEXT    NOT NULL,
			permission_mode TEXT    NOT NULL,
			queued_at_ms    INTEGER NOT NULL,
			prompt_origin   INTEGER NOT NULL
		);
		CREATE INDEX IF NOT EXISTS shutdown_hold_prompt_workspace
			ON shutdown_hold_prompt(workspace, queued_at_ms);
	`); err != nil {
		return nil, fmt.Errorf("statedb: create shutdown_schedule schema: %w", err)
	}
	if err := ensureShutdownPromptOriginColumn(db); err != nil {
		return nil, err
	}
	return &ShutdownSchedules{db: db}, nil
}

// ensureShutdownPromptOriginColumn adds the exact prompt attribution to older
// drain-ledger tables. The column is intentionally nullable during migration:
// a legacy row has no truthful origin to synthesize, so reading one fails
// loudly instead of relabeling historical input.
func ensureShutdownPromptOriginColumn(db *sql.DB) error {
	_, err := AddColumnIfMissing(db, "shutdown_hold_prompt", "prompt_origin", `INTEGER`)
	return err
}

// PutSchedule writes (or replaces) the singleton lease row.
func (s *ShutdownSchedules) PutSchedule(rec ShutdownSchedule) error {
	if rec.ScheduleID == "" {
		return fmt.Errorf("statedb: a shutdown schedule needs a schedule id to key a cancel on")
	}
	_, err := s.db.Exec(
		`INSERT INTO shutdown_schedule(id, schedule_id, scheduled_at_ms, cause, stop_shims) VALUES (1,?,?,?,?)
		 ON CONFLICT(id) DO UPDATE SET
		     schedule_id = excluded.schedule_id,
		     scheduled_at_ms = excluded.scheduled_at_ms,
		     cause = excluded.cause,
		     stop_shims = excluded.stop_shims`,
		rec.ScheduleID, rec.ScheduledAtMs, rec.Cause, rec.StopShims)
	if err != nil {
		return fmt.Errorf("statedb: record shutdown schedule %q: %w", rec.ScheduleID, err)
	}
	return nil
}

// Schedule reads back the singleton lease row. The bool reports presence; a
// missing row is the ordinary idle answer, not an error.
func (s *ShutdownSchedules) Schedule() (ShutdownSchedule, bool, error) {
	var rec ShutdownSchedule
	err := s.db.QueryRow(
		`SELECT schedule_id, scheduled_at_ms, cause, stop_shims FROM shutdown_schedule WHERE id = 1`,
	).Scan(&rec.ScheduleID, &rec.ScheduledAtMs, &rec.Cause, &rec.StopShims)
	if errors.Is(err, sql.ErrNoRows) {
		return ShutdownSchedule{}, false, nil
	}
	if err != nil {
		return ShutdownSchedule{}, false, fmt.Errorf("statedb: read the shutdown schedule: %w", err)
	}
	return rec, true, nil
}

// ClearSchedule removes the lease row and reports whether one was there.
func (s *ShutdownSchedules) ClearSchedule() (bool, error) {
	res, err := s.db.Exec(`DELETE FROM shutdown_schedule WHERE id = 1`)
	if err != nil {
		return false, fmt.Errorf("statedb: clear the shutdown schedule: %w", err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: clear the shutdown schedule: rows affected: %w", err)
	}
	return n > 0, nil
}

// RecordHeldPrompt persists one prompt parked by the drain lease.
func (s *ShutdownSchedules) RecordHeldPrompt(p HeldPrompt) error {
	switch {
	case p.EntryID == "":
		return fmt.Errorf("statedb: a drain-held prompt for workspace %q has no entry id", p.Workspace)
	case p.ScheduleID == "":
		return fmt.Errorf("statedb: drain-held prompt %q names no schedule", p.EntryID)
	case p.Workspace == "":
		return fmt.Errorf("statedb: drain-held prompt %q has no workspace", p.EntryID)
	case p.PromptOrigin <= 0:
		return fmt.Errorf("statedb: drain-held prompt %q has invalid prompt origin %d", p.EntryID, p.PromptOrigin)
	}
	_, err := s.db.Exec(
		`INSERT INTO shutdown_hold_prompt(entry_id, schedule_id, workspace, session_id, request_id, text, permission_mode, queued_at_ms, prompt_origin)
		 VALUES (?,?,?,?,?,?,?,?,?)
		 ON CONFLICT(entry_id) DO UPDATE SET
		     schedule_id = excluded.schedule_id,
		     workspace = excluded.workspace,
		     session_id = excluded.session_id,
		     request_id = excluded.request_id,
		     text = excluded.text,
		     permission_mode = excluded.permission_mode,
		     queued_at_ms = excluded.queued_at_ms,
		     prompt_origin = excluded.prompt_origin`,
		p.EntryID, p.ScheduleID, p.Workspace, p.SessionID, p.RequestID, p.Text, p.PermissionMode, p.QueuedAtMs, p.PromptOrigin)
	if err != nil {
		return fmt.Errorf("statedb: record drain-held prompt %q for workspace %q: %w", p.EntryID, p.Workspace, err)
	}
	return nil
}

// DropHeldPrompt removes one held prompt and reports whether a row went. It is
// called when the entry stops being drain-held for ANY reason: forced,
// cancelled, or delivered.
func (s *ShutdownSchedules) DropHeldPrompt(entryID string) (bool, error) {
	res, err := s.db.Exec(`DELETE FROM shutdown_hold_prompt WHERE entry_id = ?`, entryID)
	if err != nil {
		return false, fmt.Errorf("statedb: drop drain-held prompt %q: %w", entryID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("statedb: drop drain-held prompt %q: rows affected: %w", entryID, err)
	}
	return n > 0, nil
}

// DropHeldPromptsForSchedule removes every prompt one schedule was holding and
// reports how many rows went. This is the CANCEL path: the lease is released,
// so the entries it parked are ordinary queue entries again and nothing durable
// should claim otherwise.
func (s *ShutdownSchedules) DropHeldPromptsForSchedule(scheduleID string) (int, error) {
	res, err := s.db.Exec(`DELETE FROM shutdown_hold_prompt WHERE schedule_id = ?`, scheduleID)
	if err != nil {
		return 0, fmt.Errorf("statedb: drop the drain-held prompts of schedule %q: %w", scheduleID, err)
	}
	n, err := res.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("statedb: drop the drain-held prompts of schedule %q: rows affected: %w", scheduleID, err)
	}
	return int(n), nil
}

// HeldPrompts returns one workspace's parked prompts in submit order.
func (s *ShutdownSchedules) HeldPrompts(workspace string) ([]HeldPrompt, error) {
	rows, err := s.db.Query(
		`SELECT entry_id, schedule_id, workspace, session_id, request_id, text, permission_mode, queued_at_ms, prompt_origin
		 FROM shutdown_hold_prompt WHERE workspace = ? ORDER BY queued_at_ms, entry_id`, workspace)
	if err != nil {
		return nil, fmt.Errorf("statedb: read the drain-held prompts of workspace %q: %w", workspace, err)
	}
	return scanHeldPrompts(rows, fmt.Sprintf("workspace %q", workspace))
}

// AllHeldPrompts returns EVERY workspace's parked prompts, ordered by workspace
// and then by submit order within it.
//
// It exists because the ledger has a reader that does not yet know which
// workspaces to ask about: the successor daemon's boot materialization, which
// runs before any session has wired and so cannot enumerate workspaces from the
// fleet. Asking per workspace there would mean deriving the workspace set from
// somewhere other than the ledger itself, and a workspace the derivation missed
// would be a parked prompt no client could ever see.
func (s *ShutdownSchedules) AllHeldPrompts() ([]HeldPrompt, error) {
	rows, err := s.db.Query(
		`SELECT entry_id, schedule_id, workspace, session_id, request_id, text, permission_mode, queued_at_ms, prompt_origin
		 FROM shutdown_hold_prompt ORDER BY workspace, queued_at_ms, entry_id`)
	if err != nil {
		return nil, fmt.Errorf("statedb: read every drain-held prompt: %w", err)
	}
	return scanHeldPrompts(rows, "every workspace")
}

// scanHeldPrompts drains a held-prompt query into records. scope names the
// query's subject for the error text and nothing else.
func scanHeldPrompts(rows *sql.Rows, scope string) ([]HeldPrompt, error) {
	defer rows.Close()
	var out []HeldPrompt
	for rows.Next() {
		var p HeldPrompt
		var promptOrigin sql.NullInt64
		if err := rows.Scan(&p.EntryID, &p.ScheduleID, &p.Workspace, &p.SessionID,
			&p.RequestID, &p.Text, &p.PermissionMode, &p.QueuedAtMs, &promptOrigin); err != nil {
			return nil, fmt.Errorf("statedb: scan a drain-held prompt of %s: %w", scope, err)
		}
		if !promptOrigin.Valid || promptOrigin.Int64 <= 0 {
			return nil, fmt.Errorf("statedb: drain-held prompt %q of %s has invalid prompt_origin %v", p.EntryID, scope, promptOrigin)
		}
		p.PromptOrigin = int32(promptOrigin.Int64)
		out = append(out, p)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("statedb: iterate the drain-held prompts of %s: %w", scope, err)
	}
	return out, nil
}
