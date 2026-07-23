package db

import (
	"database/sql"
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/shim-store/internal/dedup"
	"google.golang.org/protobuf/proto"
)

// Result reports the outcome of one Ingest call (mirrors StoreWriteAck).
type Result struct {
	Accepted uint64 // events persisted with a freshly assigned seq
	Deduped  uint64 // events dropped as duplicates (first-writer-wins)
	LastSeq  uint64 // highest seq assigned in this batch (0 if none)
}

// Ingest persists a batch of PERSISTENT events and, atomically in the SAME
// transaction, advances the file cursor when one is supplied (§6.3, §7.3).
//
// Per-session seq is assigned gapless in arrival order: a duplicate (its
// dedup_key already present) consumes NO seq, so the sequence never gaps. On
// return each accepted event has ev.Seq (>0) and ev.DedupKey stamped in place;
// a deduped event is reset to ev.Seq == 0 so the caller can tell which events
// to fan out (accepted → seq>0). EPHEMERAL events must never reach here (the
// server routes them straight to fan-out); one arriving is a loud invariant
// violation that rejects the whole batch.
func (d *DB) Ingest(producer string, events []*corev1.Event, cursor *corev1.CursorState) (Result, error) {
	var res Result

	tx, err := d.sql.Begin()
	if err != nil {
		return res, fmt.Errorf("shim-store ingest: begin tx (producer=%q): %w", producer, err)
	}
	defer tx.Rollback() //nolint:errcheck // no-op after a successful Commit

	// Lazily loaded per-session high-water seq, so multi-event single-session
	// batches read MAX(seq) exactly once.
	sessionSeq := make(map[string]uint64)
	loadSeq := func(sid string) (uint64, error) {
		if v, ok := sessionSeq[sid]; ok {
			return v, nil
		}
		var v uint64
		row := tx.QueryRow(`SELECT COALESCE(MAX(seq), 0) FROM event WHERE session_id = ?`, sid)
		if err := row.Scan(&v); err != nil {
			return 0, fmt.Errorf("shim-store ingest: reading max seq for session %q: %w", sid, err)
		}
		sessionSeq[sid] = v
		return v, nil
	}

	const insertSQL = `INSERT OR IGNORE INTO event
	  (session_id, seq, plane, class, kind, task_id, uuid, dedup_key, produced_at, payload)
	  VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`

	for _, ev := range events {
		if ev.GetClass() == corev1.EventClass_EVENT_CLASS_EPHEMERAL {
			return res, fmt.Errorf("shim-store ingest: EPHEMERAL event reached persistence (session=%q kind=%q) — bug: ephemeral must bypass the DB", ev.GetSessionId(), kindOf(ev))
		}
		sid := ev.GetSessionId()
		if sid == "" {
			return res, fmt.Errorf("shim-store ingest: event with empty session_id (kind=%q)", kindOf(ev))
		}

		cur, err := loadSeq(sid)
		if err != nil {
			return res, err
		}
		candidate := cur + 1

		key := dedup.Derive(ev)
		ev.Seq = candidate
		ev.DedupKey = key

		blob, err := proto.Marshal(ev)
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: marshaling event (session=%q seq=%d): %w", sid, candidate, err)
		}

		out, err := tx.Exec(insertSQL,
			sid, candidate, int32(ev.GetPlane()), int32(ev.GetClass()),
			kindOf(ev), nullStr(taskIDOf(ev)), nullStr(uuidOf(key)), nullStr(key),
			ev.GetProducedAtMs(), blob)
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: inserting event (session=%q seq=%d): %w", sid, candidate, err)
		}
		n, err := out.RowsAffected()
		if err != nil {
			return res, fmt.Errorf("shim-store ingest: rows-affected (session=%q seq=%d): %w", sid, candidate, err)
		}
		if n == 1 {
			res.Accepted++
			sessionSeq[sid] = candidate
			if candidate > res.LastSeq {
				res.LastSeq = candidate
			}
		} else {
			// Duplicate: the dedup index rejected it. Do not consume a seq;
			// mark unpersisted so the caller does not re-fan-out the loser.
			res.Deduped++
			ev.Seq = 0
		}
	}

	if cursor != nil {
		if err := upsertCursor(tx, cursor); err != nil {
			return res, err
		}
	}

	if err := tx.Commit(); err != nil {
		return res, fmt.Errorf("shim-store ingest: commit (producer=%q): %w", producer, err)
	}
	return res, nil
}

func upsertCursor(tx *sql.Tx, c *corev1.CursorState) error {
	const upsertSQL = `INSERT INTO cursor (file_id, path, offset, carry, updated_at)
	  VALUES (?, ?, ?, ?, ?)
	  ON CONFLICT(file_id) DO UPDATE SET
	    path = excluded.path, offset = excluded.offset,
	    carry = excluded.carry, updated_at = excluded.updated_at`
	if _, err := tx.Exec(upsertSQL, c.GetFileId(), c.GetPath(), c.GetOffset(), c.GetCarry(), nowMillis()); err != nil {
		return fmt.Errorf("shim-store ingest: upserting cursor (file_id=%q): %w", c.GetFileId(), err)
	}
	return nil
}

// kindOf returns the §6.2 `kind` column: the vendor Any type-URL suffix, or
// the core payload message name.
func kindOf(ev *corev1.Event) string {
	if v := ev.GetVendor(); v != nil {
		tu := v.GetTypeUrl()
		if i := strings.LastIndexByte(tu, '.'); i >= 0 {
			return tu[i+1:]
		}
		if i := strings.LastIndexByte(tu, '/'); i >= 0 {
			return tu[i+1:]
		}
		return tu
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_SessionStarted:
		return "SessionStarted"
	case *corev1.Event_SessionEnded:
		return "SessionEnded"
	case *corev1.Event_TurnStarted:
		return "TurnStarted"
	case *corev1.Event_TurnEnded:
		return "TurnEnded"
	case *corev1.Event_TaskStarted:
		return "TaskStarted"
	case *corev1.Event_TaskProgress:
		return "TaskProgress"
	case *corev1.Event_TaskEnded:
		return "TaskEnded"
	case *corev1.Event_ContentDelta:
		return "ContentDelta"
	case *corev1.Event_HeartbeatProgress:
		return "HeartbeatProgress"
	case *corev1.Event_DegradedState:
		return "DegradedState"
	case *corev1.Event_Unparsed:
		return "UnparsedEvent"
	default:
		return "Unknown"
	}
}

// taskIDOf extracts the §6.2 `task_id` column from task-scoped core payloads.
func taskIDOf(ev *corev1.Event) string {
	switch p := ev.GetPayload().(type) {
	case *corev1.Event_TaskStarted:
		return p.TaskStarted.GetTaskId()
	case *corev1.Event_TaskProgress:
		return p.TaskProgress.GetTaskId()
	case *corev1.Event_TaskEnded:
		return p.TaskEnded.GetTaskId()
	}
	return ""
}

// uuidOf extracts the §6.2 `uuid` column from a "uuid:<uuid>" dedup key. Events
// keyed otherwise (tur/turn/wf) or unkeyed have no indexed uuid.
func uuidOf(dedupKey string) string {
	if strings.HasPrefix(dedupKey, "uuid:") {
		return dedupKey[len("uuid:"):]
	}
	return ""
}

// nullStr maps "" to a SQL NULL so the partial indexes (WHERE ... IS NOT NULL)
// skip empty extracted columns.
func nullStr(s string) any {
	if s == "" {
		return nil
	}
	return s
}
