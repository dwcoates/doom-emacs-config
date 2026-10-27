package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// queuedCreatesDDL is the layout-86 addition: every create held until the
// network is restored (agentrepl.v1.CreateWorkspaceWaitingForNetwork). A row
// is keyed by its NORMALIZED REQUEST, so two identical creates can never both
// be held: the key is the dedup, enforced by the table rather than by a
// caller remembering to check. spec is the create's request in the workspace
// package's own encoding, and op_ids the option-B progress ops waiting on its
// outcome, so a successor daemon resumes it and answers the same ops. IT IS
// ADDITIVE: a new table the previous build never names.
const queuedCreatesDDL = `
CREATE TABLE queued_creates (
  request_key TEXT PRIMARY KEY,
  create_id   TEXT NOT NULL UNIQUE,
  spec        TEXT NOT NULL,
  op_ids      TEXT NOT NULL,
  queued_at   INTEGER NOT NULL
);
`

// QueuedCreate is one create held until the network is restored.
type QueuedCreate struct {
	// Key is the create's normalized request: the identity two identical
	// requests share.
	Key string
	// ID is the pending create's ref (agentrepl.v1.PendingCreateRef.id).
	ID string
	// Spec is the create's request, encoded by the workspace package, which
	// alone reads it.
	Spec string
	// OpIDs are the option-B progress ops told the create's outcome.
	OpIDs []string
	// QueuedAt is when the create was first held.
	QueuedAt time.Time
}

// PutQueuedCreate holds a create, or replaces the held create of the same
// key (its ops grow as identical requests join it).
func (s *store) PutQueuedCreate(ctx context.Context, q QueuedCreate) error {
	const op = "daemon.wsm.put_queued_create"
	fields := dlog.Context{"create_id": q.ID, "request_key": q.Key, "op_ids": len(q.OpIDs)}
	if q.Key == "" || q.ID == "" || q.Spec == "" {
		err := fmt.Errorf("wsm: a queued create names its key, its id and its request")
		s.log.Error(op, "refused a queued create its caller should never have made", withError(fields, err))
		return err
	}
	ops, err := json.Marshal(nonNilStrings(q.OpIDs))
	if err != nil {
		s.log.Error(op, "refused unencodable progress ops", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO queued_creates (request_key, create_id, spec, op_ids, queued_at) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT(request_key) DO UPDATE SET create_id = excluded.create_id, spec = excluded.spec, op_ids = excluded.op_ids`,
			q.Key, q.ID, q.Spec, string(ops), nanos(q.QueuedAt))
		return err
	})
}

// QueuedCreates answers every held create, oldest first.
func (s *store) QueuedCreates(ctx context.Context) ([]QueuedCreate, error) {
	var out []QueuedCreate
	err := s.read(ctx, "daemon.wsm.queued_creates", nil, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT request_key, create_id, spec, op_ids, queued_at FROM queued_creates ORDER BY queued_at, create_id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		out = nil
		for rows.Next() {
			var (
				q      QueuedCreate
				ops    string
				queued int64
			)
			if err := rows.Scan(&q.Key, &q.ID, &q.Spec, &ops, &queued); err != nil {
				return err
			}
			if err := json.Unmarshal([]byte(ops), &q.OpIDs); err != nil {
				return fmt.Errorf("wsm: queued create %s: unreadable op_ids %q: %w", q.ID, ops, err)
			}
			q.QueuedAt = fromNanos(queued)
			out = append(out, q)
		}
		return rows.Err()
	})
	return out, err
}

// DeleteQueuedCreate releases the held create of KEY. Releasing one that is
// not held is no error: the create's outcome is what the caller acts on.
func (s *store) DeleteQueuedCreate(ctx context.Context, key string) error {
	const op = "daemon.wsm.delete_queued_create"
	return s.write(ctx, op, dlog.Context{"request_key": key}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM queued_creates WHERE request_key = ?`, key)
		return err
	})
}

// nonNilStrings answers S, or an empty slice for nil, so a row's op_ids is
// always a JSON array.
func nonNilStrings(s []string) []string {
	if s == nil {
		return []string{}
	}
	return s
}
