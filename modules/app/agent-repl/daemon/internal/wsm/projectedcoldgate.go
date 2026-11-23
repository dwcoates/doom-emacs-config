package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// projectedColdGateDDL is the layout-194 addition: the cold gate a workspace's
// shim projected, kept on the workspace so the daemon raises the gate the
// moment the cache lapses and not at the next prompt (owner ruling,
// 2026-11-23). The shim states, after every request that moves the cache, when
// the cache lapses if no keep-alive follows and what the gate will state then
// (conversation.v1.SessionCacheLapseGated). Only a projection whose verdict is
// gated is kept: an ungated one has no gate to raise, and clears the row.
// IT IS ADDITIVE: a new table the previous build never names.
//
// cold holds the facts the gate will state, whole, as the shim projected them
// (conversation.v1.SessionCold, its requested model left for the daemon to
// fill from the workspace's model when the gate is raised).
const projectedColdGateDDL = `
CREATE TABLE projected_cold_gate (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  due_at       INTEGER NOT NULL,
  cold         BLOB NOT NULL
);
`

// ProjectedColdGate is one workspace's projected cold gate.
type ProjectedColdGate struct {
	Workspace WorkspaceID
	// DueAt is when the cache lapses if no keep-alive follows: the instant
	// the gate is due.
	DueAt time.Time
	// Cold is the gate's facts as the shim projected them.
	Cold *conversationv1.SessionCold
}

// SetProjectedColdGate records WS's projected cold gate, replacing any earlier
// one. A projection with no cold facts is refused.
func (s *store) SetProjectedColdGate(ctx context.Context, id WorkspaceID, dueAt time.Time, cold *conversationv1.SessionCold) error {
	if cold == nil {
		return fmt.Errorf("wsm: projected cold gate of %s: a projection states its cold facts", id)
	}
	blob, err := proto.Marshal(cold)
	if err != nil {
		return fmt.Errorf("wsm: encode the projected cold gate of %s: %w", id, err)
	}
	fields := dlog.Context{"workspace": string(id), "due_at": dueAt}
	return s.write(ctx, "daemon.wsm.set_projected_cold_gate", fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO projected_cold_gate (workspace_id, due_at, cold) VALUES (?, ?, ?)
			ON CONFLICT (workspace_id) DO UPDATE SET due_at = excluded.due_at, cold = excluded.cold`,
			id, nanos(dueAt), blob)
		return err
	})
}

// ClearProjectedColdGate retires WS's projected cold gate, answering whether
// one stood.
func (s *store) ClearProjectedColdGate(ctx context.Context, id WorkspaceID) (bool, error) {
	cleared := false
	err := s.write(ctx, "daemon.wsm.clear_projected_cold_gate", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM projected_cold_gate WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		cleared, err = touchedRow(res)
		return err
	})
	return cleared, err
}

// ProjectedColdGate answers WS's projected cold gate, false when none is.
func (s *store) ProjectedColdGate(ctx context.Context, id WorkspaceID) (ProjectedColdGate, bool, error) {
	var out ProjectedColdGate
	found := false
	err := s.read(ctx, "daemon.wsm.projected_cold_gate", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var due int64
		var blob []byte
		err := s.q(ctx).QueryRowContext(ctx, `SELECT due_at, cold FROM projected_cold_gate WHERE workspace_id = ?`, id).Scan(&due, &blob)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		out = ProjectedColdGate{Workspace: id, DueAt: fromNanos(due), Cold: &conversationv1.SessionCold{}}
		if err := proto.Unmarshal(blob, out.Cold); err != nil {
			return fmt.Errorf("wsm: decode the projected cold gate of %s: %w", id, err)
		}
		found = true
		return nil
	})
	return out, found, err
}

// ProjectedColdGates answers every workspace's projected cold gate, in no
// particular order: what a boot arms its gate deadlines from.
func (s *store) ProjectedColdGates(ctx context.Context) ([]ProjectedColdGate, error) {
	var all []ProjectedColdGate
	err := s.read(ctx, "daemon.wsm.projected_cold_gates", nil, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx, `SELECT workspace_id, due_at, cold FROM projected_cold_gate`)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var p ProjectedColdGate
			var due int64
			var blob []byte
			if err := rows.Scan(&p.Workspace, &due, &blob); err != nil {
				return err
			}
			p.DueAt = fromNanos(due)
			p.Cold = &conversationv1.SessionCold{}
			if err := proto.Unmarshal(blob, p.Cold); err != nil {
				return fmt.Errorf("wsm: decode the projected cold gate of %s: %w", p.Workspace, err)
			}
			all = append(all, p)
		}
		return rows.Err()
	})
	return all, err
}
