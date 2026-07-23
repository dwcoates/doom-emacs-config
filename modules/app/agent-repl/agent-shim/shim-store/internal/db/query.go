package db

import (
	"database/sql"
	"errors"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"google.golang.org/protobuf/proto"
)

// nowMillis is the store's wall clock in unix millis (cursor updated_at).
func nowMillis() int64 { return time.Now().UnixMilli() }

// ReplayFrom returns the session's persisted events with seq > fromSeq, in seq
// order. from_seq is EXCLUSIVE (core.proto Subscribe semantics). Only
// PERSISTENT events are stored, so replay never yields ephemeral events.
func (d *DB) ReplayFrom(sessionID string, fromSeq uint64) ([]*corev1.Event, error) {
	rows, err := d.sql.Query(
		`SELECT payload FROM event WHERE session_id = ? AND seq > ? ORDER BY seq ASC`,
		sessionID, fromSeq)
	if err != nil {
		return nil, fmt.Errorf("shim-store query: replay (session=%q from_seq=%d): %w", sessionID, fromSeq, err)
	}
	defer rows.Close()

	var out []*corev1.Event
	for rows.Next() {
		var blob []byte
		if err := rows.Scan(&blob); err != nil {
			return nil, fmt.Errorf("shim-store query: scanning replay row (session=%q): %w", sessionID, err)
		}
		ev := &corev1.Event{}
		if err := proto.Unmarshal(blob, ev); err != nil {
			return nil, fmt.Errorf("shim-store query: unmarshaling replay payload (session=%q): %w", sessionID, err)
		}
		out = append(out, ev)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("shim-store query: iterating replay rows (session=%q): %w", sessionID, err)
	}
	return out, nil
}

// MaxSeq returns the highest assigned seq for a session (0 if none).
func (d *DB) MaxSeq(sessionID string) (uint64, error) {
	var v uint64
	row := d.sql.QueryRow(`SELECT COALESCE(MAX(seq), 0) FROM event WHERE session_id = ?`, sessionID)
	if err := row.Scan(&v); err != nil {
		return 0, fmt.Errorf("shim-store query: max seq (session=%q): %w", sessionID, err)
	}
	return v, nil
}

// EventsByTask returns a session's events for one extracted task_id, seq order.
// It exercises the event_task index and is used for task-scoped queries.
func (d *DB) EventsByTask(sessionID, taskID string) ([]*corev1.Event, error) {
	rows, err := d.sql.Query(
		`SELECT payload FROM event WHERE session_id = ? AND task_id = ? ORDER BY seq ASC`,
		sessionID, taskID)
	if err != nil {
		return nil, fmt.Errorf("shim-store query: by-task (session=%q task=%q): %w", sessionID, taskID, err)
	}
	defer rows.Close()

	var out []*corev1.Event
	for rows.Next() {
		var blob []byte
		if err := rows.Scan(&blob); err != nil {
			return nil, fmt.Errorf("shim-store query: scanning by-task row: %w", err)
		}
		ev := &corev1.Event{}
		if err := proto.Unmarshal(blob, ev); err != nil {
			return nil, fmt.Errorf("shim-store query: unmarshaling by-task payload: %w", err)
		}
		out = append(out, ev)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("shim-store query: iterating by-task rows: %w", err)
	}
	return out, nil
}

// Cursors returns all persisted file cursors for the sidecar's startup
// recovery (§7.3). The sidecar resumes each file from its stored offset/carry.
func (d *DB) Cursors() ([]*corev1.CursorState, error) {
	rows, err := d.sql.Query(`SELECT file_id, path, offset, carry FROM cursor`)
	if err != nil {
		return nil, fmt.Errorf("shim-store query: listing cursors: %w", err)
	}
	defer rows.Close()

	var out []*corev1.CursorState
	for rows.Next() {
		c := &corev1.CursorState{}
		var carry []byte
		if err := rows.Scan(&c.FileId, &c.Path, &c.Offset, &carry); err != nil {
			return nil, fmt.Errorf("shim-store query: scanning cursor row: %w", err)
		}
		c.Carry = carry
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("shim-store query: iterating cursor rows: %w", err)
	}
	return out, nil
}

// Cursor returns one file's persisted cursor, or (nil, nil) if absent.
func (d *DB) Cursor(fileID string) (*corev1.CursorState, error) {
	c := &corev1.CursorState{}
	var carry []byte
	row := d.sql.QueryRow(`SELECT file_id, path, offset, carry FROM cursor WHERE file_id = ?`, fileID)
	switch err := row.Scan(&c.FileId, &c.Path, &c.Offset, &carry); {
	case err == nil:
		c.Carry = carry
		return c, nil
	case errors.Is(err, sql.ErrNoRows):
		return nil, nil
	default:
		return nil, fmt.Errorf("shim-store query: reading cursor (file_id=%q): %w", fileID, err)
	}
}
