package db

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/logging"
	"google.golang.org/protobuf/proto"
)

// nowMillis is the store's wall clock in unix millis (cursor updated_at).
func nowMillis() int64 { return time.Now().UnixMilli() }

// ReplayStats describes rows successfully handed to a ReplayFrom sink.
type ReplayStats struct {
	Events   uint64
	FirstSeq uint64
	LastSeq  uint64
	Elapsed  time.Duration
}

// ReplayFrom streams the session's persisted events with seq > fromSeq to
// yield in seq order. from_seq is EXCLUSIVE (core.proto Subscribe semantics).
// Only PERSISTENT events are stored, so replay never yields ephemeral events.
//
// The callback-only API is load-bearing: the store server writes each row to
// the subscriber before SQLite advances to the next row. A slice-returning API
// would make it possible to buffer an arbitrarily large replay before the
// first socket write, defeating every downstream activity deadline.
func (d *DB) ReplayFrom(ctx context.Context, sessionID string, fromSeq uint64, yield func(*corev1.Event) error) (ReplayStats, error) {
	if yield == nil {
		panic("shim-store db: ReplayFrom requires a yield callback")
	}
	started := time.Now()
	fields := logging.Fields{Operation: "replay", Table: "event", Session: sessionID}
	d.log.LogVerbose(fields, "streaming replay query from_seq=%d", fromSeq)
	rows, err := d.sql.QueryContext(ctx,
		`SELECT payload FROM event WHERE session_id = ? AND seq > ? ORDER BY seq ASC`,
		sessionID, fromSeq)
	if err != nil {
		return ReplayStats{}, d.queryError("replay", "event", sessionID, fmt.Errorf("shim-store query: replay (session=%q from_seq=%d): %w", sessionID, fromSeq, err))
	}
	defer rows.Close()

	var stats ReplayStats
	for rows.Next() {
		var blob []byte
		if err := rows.Scan(&blob); err != nil {
			return stats, d.queryError("replay-scan", "event", sessionID, fmt.Errorf("shim-store query: scanning replay row (session=%q from_seq=%d delivered=%d): %w", sessionID, fromSeq, stats.Events, err))
		}
		ev := &corev1.Event{}
		if err := proto.Unmarshal(blob, ev); err != nil {
			return stats, d.queryError("replay-unmarshal", "event", sessionID, fmt.Errorf("shim-store query: unmarshaling replay payload (session=%q from_seq=%d delivered=%d): %w", sessionID, fromSeq, stats.Events, err))
		}
		if err := yield(ev); err != nil {
			stats.Elapsed = time.Since(started)
			d.log.LogVerbose(fields, "replay sink stopped stream from_seq=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d cause=%q",
				fromSeq, stats.Events, stats.FirstSeq, stats.LastSeq, stats.Elapsed.Milliseconds(), err)
			return stats, err
		}
		if stats.Events == 0 {
			stats.FirstSeq = ev.GetSeq()
		}
		stats.Events++
		stats.LastSeq = ev.GetSeq()
	}
	if err := rows.Err(); err != nil {
		return stats, d.queryError("replay-iterate", "event", sessionID, fmt.Errorf("shim-store query: iterating replay rows (session=%q from_seq=%d delivered=%d first_seq=%d last_seq=%d): %w",
			sessionID, fromSeq, stats.Events, stats.FirstSeq, stats.LastSeq, err))
	}
	stats.Elapsed = time.Since(started)
	d.log.LogVerbose(fields, "replay query completed from_seq=%d delivered=%d first_seq=%d last_seq=%d elapsed_ms=%d",
		fromSeq, stats.Events, stats.FirstSeq, stats.LastSeq, stats.Elapsed.Milliseconds())
	return stats, nil
}

// MaxSeq returns the highest assigned seq for a session (0 if none).
func (d *DB) MaxSeq(sessionID string) (uint64, error) {
	d.log.LogVerbose(logging.Fields{Operation: "max-seq", Table: "event", Session: sessionID}, "querying highest sequence")
	var v uint64
	row := d.sql.QueryRow(`SELECT COALESCE(MAX(seq), 0) FROM event WHERE session_id = ?`, sessionID)
	if err := row.Scan(&v); err != nil {
		return 0, d.queryError("max-seq", "event", sessionID, fmt.Errorf("shim-store query: max seq (session=%q): %w", sessionID, err))
	}
	d.log.LogVerbose(logging.Fields{Operation: "max-seq", Table: "event", Session: sessionID}, "highest sequence=%d", v)
	return v, nil
}

// EventsByTask returns a session's events for one extracted task_id, seq order.
// It exercises the event_task index and is used for task-scoped queries.
func (d *DB) EventsByTask(sessionID, taskID string) ([]*corev1.Event, error) {
	d.log.LogVerbose(logging.Fields{Operation: "by-task", Table: "event", Session: sessionID}, "querying task_id=%q", taskID)
	rows, err := d.sql.Query(
		`SELECT payload FROM event WHERE session_id = ? AND task_id = ? ORDER BY seq ASC`,
		sessionID, taskID)
	if err != nil {
		return nil, d.queryError("by-task", "event", sessionID, fmt.Errorf("shim-store query: by-task (session=%q task=%q): %w", sessionID, taskID, err))
	}
	defer rows.Close()

	var out []*corev1.Event
	for rows.Next() {
		var blob []byte
		if err := rows.Scan(&blob); err != nil {
			return nil, d.queryError("by-task-scan", "event", sessionID, fmt.Errorf("shim-store query: scanning by-task row: %w", err))
		}
		ev := &corev1.Event{}
		if err := proto.Unmarshal(blob, ev); err != nil {
			return nil, d.queryError("by-task-unmarshal", "event", sessionID, fmt.Errorf("shim-store query: unmarshaling by-task payload: %w", err))
		}
		out = append(out, ev)
	}
	if err := rows.Err(); err != nil {
		return nil, d.queryError("by-task-iterate", "event", sessionID, fmt.Errorf("shim-store query: iterating by-task rows: %w", err))
	}
	d.log.LogVerbose(logging.Fields{Operation: "by-task", Table: "event", Session: sessionID}, "task query returned events=%d task_id=%q", len(out), taskID)
	return out, nil
}

// OpenTasks returns one persisted TaskStarted plus the latest persisted
// task-scoped activity time for every (session_id, task_id) that has no
// persisted TaskEnded. The store uses only indexed envelope columns to select
// lifecycle state; payloads remain opaque except for unmarshalling the selected
// start events back onto the wire.
func (d *DB) OpenTasks() ([]*corev1.OpenTaskState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "open-tasks", Table: "event"}, "querying persisted task lifecycle state")
	rows, err := d.sql.Query(`
		SELECT started.payload, (
		  SELECT active.produced_at
		  FROM event active
		  WHERE active.session_id = started.session_id
		    AND active.task_id = started.task_id
		  ORDER BY active.seq DESC
		  LIMIT 1
		)
		FROM event started
		WHERE started.kind = 'TaskStarted'
		  AND started.task_id IS NOT NULL
		  AND started.seq = (
		    SELECT MIN(first_start.seq)
		    FROM event first_start
		    WHERE first_start.session_id = started.session_id
		      AND first_start.task_id = started.task_id
		      AND first_start.kind = 'TaskStarted'
		  )
		  AND NOT EXISTS (
		    SELECT 1 FROM event ended
		    WHERE ended.session_id = started.session_id
		      AND ended.task_id = started.task_id
		      AND ended.kind = 'TaskEnded'
		  )
		ORDER BY started.session_id, started.task_id`)
	if err != nil {
		return nil, d.queryError("open-tasks", "event", "", fmt.Errorf("shim-store query: open tasks: %w", err))
	}
	defer rows.Close()
	var out []*corev1.OpenTaskState
	for rows.Next() {
		var blob []byte
		var lastActivityAtMs int64
		if err := rows.Scan(&blob, &lastActivityAtMs); err != nil {
			return nil, d.queryError("open-tasks-scan", "event", "", fmt.Errorf("shim-store query: scanning open task: %w", err))
		}
		ev := &corev1.Event{}
		if err := proto.Unmarshal(blob, ev); err != nil {
			return nil, d.queryError("open-tasks-unmarshal", "event", "", fmt.Errorf("shim-store query: unmarshaling open task start: %w", err))
		}
		out = append(out, &corev1.OpenTaskState{
			Started:          ev,
			LastActivityAtMs: lastActivityAtMs,
		})
	}
	if err := rows.Err(); err != nil {
		return nil, d.queryError("open-tasks-iterate", "event", "", fmt.Errorf("shim-store query: iterating open tasks: %w", err))
	}
	d.log.LogVerbose(logging.Fields{Operation: "open-tasks", Table: "event"}, "open task query returned tasks=%d", len(out))
	return out, nil
}

// Cursors returns all persisted file cursors for the sidecar's startup
// recovery (§7.3). The sidecar resumes each file from its stored offset/carry.
func (d *DB) Cursors() ([]*corev1.CursorState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "list-cursors", Table: "cursor"}, "querying all persisted cursors")
	rows, err := d.sql.Query(`SELECT file_id, path, offset, carry FROM cursor`)
	if err != nil {
		return nil, d.queryError("list-cursors", "cursor", "", fmt.Errorf("shim-store query: listing cursors: %w", err))
	}
	defer rows.Close()

	var out []*corev1.CursorState
	for rows.Next() {
		c := &corev1.CursorState{}
		var carry []byte
		if err := rows.Scan(&c.FileId, &c.Path, &c.Offset, &carry); err != nil {
			return nil, d.queryError("list-cursors-scan", "cursor", "", fmt.Errorf("shim-store query: scanning cursor row: %w", err))
		}
		c.Carry = carry
		out = append(out, c)
	}
	if err := rows.Err(); err != nil {
		return nil, d.queryError("list-cursors-iterate", "cursor", "", fmt.Errorf("shim-store query: iterating cursor rows: %w", err))
	}
	d.log.LogVerbose(logging.Fields{Operation: "list-cursors", Table: "cursor"}, "cursor query returned cursors=%d", len(out))
	return out, nil
}

// Cursor returns one file's persisted cursor, or (nil, nil) if absent.
func (d *DB) Cursor(fileID string) (*corev1.CursorState, error) {
	d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "querying file_id=%q", fileID)
	c := &corev1.CursorState{}
	var carry []byte
	row := d.sql.QueryRow(`SELECT file_id, path, offset, carry FROM cursor WHERE file_id = ?`, fileID)
	switch err := row.Scan(&c.FileId, &c.Path, &c.Offset, &carry); {
	case err == nil:
		c.Carry = carry
		d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "cursor found file_id=%q offset=%d", fileID, c.GetOffset())
		return c, nil
	case errors.Is(err, sql.ErrNoRows):
		d.log.LogVerbose(logging.Fields{Operation: "cursor", Table: "cursor"}, "cursor absent file_id=%q", fileID)
		return nil, nil
	default:
		return nil, d.queryError("cursor", "cursor", "", fmt.Errorf("shim-store query: reading cursor (file_id=%q): %w", fileID, err))
	}
}

func (d *DB) queryError(operation, table, session string, err error) error {
	d.log.Log(logging.Fields{Operation: operation, Table: table, Session: session, Level: "error"}, "database query failed: %v", err)
	return err
}
