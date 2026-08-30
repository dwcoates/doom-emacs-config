package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// encodeEvidence renders a fault's typed-arm fields as the JSON object the
// column holds. An absent map and an empty one are both stored as "{}", so a
// decode never has to guess.
func encodeEvidence(evidence map[string]string) (string, error) {
	if evidence == nil {
		evidence = map[string]string{}
	}
	out, err := json.Marshal(evidence)
	if err != nil {
		return "", fmt.Errorf("wsm: encode fault evidence: %w", err)
	}
	return string(out), nil
}

// decodeEvidence parses one stored evidence object, failing the WHOLE read when
// the column is not a JSON object of strings — a fault whose typed arm cannot
// be filled is never reported as one that can.
func decodeEvidence(row, raw string) (map[string]string, error) {
	var evidence map[string]string
	if err := json.Unmarshal([]byte(raw), &evidence); err != nil {
		return nil, &DecodeError{Table: "faults", Row: row, Field: "evidence", Err: err}
	}
	return evidence, nil
}

// nowUTC is the store's clock. It exists so every minted instant has one
// spelling.
func nowUTC() time.Time { return time.Now().UTC() }

// OpenFault records a fault and returns its id. A fault is open until it is
// explicitly closed; its resolved-at instant is persisted, so a card that
// reopens unresolved on every boot is unrepresentable.
func (s *store) OpenFault(ctx context.Context, f Fault) (FaultID, error) {
	const op = "daemon.wsm.open_fault"
	fields := dlog.Context{"kind": f.Kind}
	if f.Workspace != nil {
		fields["workspace"] = string(*f.Workspace)
	}
	if f.Kind == "" {
		err := errors.New("wsm: empty fault kind")
		s.log.Error(op, "refused a fault with no kind", withError(fields, err))
		return "", err
	}
	id := f.ID
	if id == "" {
		id = NewFaultID()
	}
	fields["fault"] = string(id)
	openedAt := f.OpenedAt
	if openedAt.IsZero() {
		openedAt = nowUTC()
	}
	evidence, err := encodeEvidence(f.Evidence)
	if err != nil {
		s.log.Error(op, "refused unencodable fault evidence", withError(fields, err))
		return "", err
	}
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var ws any
		if f.Workspace != nil {
			ws = string(*f.Workspace)
		}
		_, err := tx.ExecContext(ctx,
			`INSERT INTO faults (id, workspace_id, kind, detail, evidence, opened_at, resolved_at) VALUES (?, ?, ?, ?, ?, ?, ?)`,
			id, ws, f.Kind, f.Detail, evidence, nanos(openedAt), nullNanos(f.ResolvedAt))
		return err
	})
	if err != nil {
		return "", err
	}
	return id, nil
}

// CloseFault stamps a fault's persisted resolved-at. A fault already resolved is
// refused, so its closing instant is the one the condition actually cleared at.
func (s *store) CloseFault(ctx context.Context, id FaultID, at time.Time) error {
	fields := dlog.Context{"fault": string(id), "resolved_at": at}
	return s.write(ctx, "daemon.wsm.close_fault", fields, func(ctx context.Context, tx *sql.Tx) error {
		var resolved sql.NullInt64
		err := tx.QueryRowContext(ctx, `SELECT resolved_at FROM faults WHERE id = ?`, id).Scan(&resolved)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: fault %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if resolved.Valid {
			return fmt.Errorf("wsm: fault %s was already resolved at %s", id, fromNanos(resolved.Int64))
		}
		res, err := tx.ExecContext(ctx, `UPDATE faults SET resolved_at = ? WHERE id = ?`, nanos(at), id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: fault %s", id))
	})
}

// OpenFaults loads the open faults matching scope, all-or-nothing.
func (s *store) OpenFaults(ctx context.Context, scope FaultScope) ([]Fault, error) {
	fields := dlog.Context{}
	query := `SELECT id, workspace_id, kind, detail, evidence, opened_at, resolved_at FROM faults WHERE resolved_at IS NULL`
	var args []any
	if scope.Workspace != nil {
		query += ` AND workspace_id = ?`
		args = append(args, string(*scope.Workspace))
		fields["workspace"] = string(*scope.Workspace)
	}
	if scope.Kind != "" {
		query += ` AND kind = ?`
		args = append(args, scope.Kind)
		fields["kind"] = scope.Kind
	}
	query += ` ORDER BY opened_at, id`

	var out []Fault
	err := s.read(ctx, "daemon.wsm.open_faults", fields, func(ctx context.Context) error {
		rows, err := s.db.QueryContext(ctx, query, args...)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []Fault
		for rows.Next() {
			var (
				f        Fault
				ws       sql.NullString
				evidence string
				opened   int64
				resolved sql.NullInt64
			)
			if err := rows.Scan(&f.ID, &ws, &f.Kind, &f.Detail, &evidence, &opened, &resolved); err != nil {
				return err
			}
			if f.Evidence, err = decodeEvidence(string(f.ID), evidence); err != nil {
				return err
			}
			if ws.Valid {
				id := WorkspaceID(ws.String)
				f.Workspace = &id
			}
			f.OpenedAt = fromNanos(opened)
			f.ResolvedAt = optTime(resolved)
			loaded = append(loaded, f)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// Fault loads one fault by id, open or resolved. It is how a caller reads back
// the persisted resolved-at instant.
func (s *store) Fault(ctx context.Context, id FaultID) (Fault, error) {
	var out Fault
	err := s.read(ctx, "daemon.wsm.fault", dlog.Context{"fault": string(id)}, func(ctx context.Context) error {
		var (
			f        Fault
			ws       sql.NullString
			evidence string
			opened   int64
			resolved sql.NullInt64
		)
		err := s.db.QueryRowContext(ctx, `SELECT id, workspace_id, kind, detail, evidence, opened_at, resolved_at FROM faults WHERE id = ?`, id).
			Scan(&f.ID, &ws, &f.Kind, &f.Detail, &evidence, &opened, &resolved)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: fault %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		if f.Evidence, err = decodeEvidence(string(f.ID), evidence); err != nil {
			return err
		}
		if ws.Valid {
			wsID := WorkspaceID(ws.String)
			f.Workspace = &wsID
		}
		f.OpenedAt = fromNanos(opened)
		f.ResolvedAt = optTime(resolved)
		out = f
		return nil
	})
	return out, err
}
