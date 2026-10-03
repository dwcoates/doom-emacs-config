package wsm

import (
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"regexp"
	"sort"
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
			// THE WORKSPACE MAY HAVE BEEN FORGOTTEN WHILE ITS SHIM WAS STILL
			// DYING. The link watcher, the health reporter and the lifecycle
			// sink all outlive the registry row, so a shim that dies after its
			// workspace is forgotten arrives here naming a row that is gone.
			// The foreign key already refuses it -- structurally, and it stays
			// -- but it refuses with `FOREIGN KEY constraint failed (787)`,
			// which is an unreadable ERROR three layers deep. The check is
			// inside the transaction, so the answer cannot be stale: this is
			// the same BEGIN IMMEDIATE the insert runs in.
			var exists int
			switch err := tx.QueryRowContext(ctx,
				`SELECT 1 FROM workspaces WHERE id = ?`, ws).Scan(&exists); {
			case errors.Is(err, sql.ErrNoRows):
				return fmt.Errorf("wsm: fault workspace %s: %w", *f.Workspace, ErrNotFound)
			case err != nil:
				return err
			}
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
		rows, err := s.db().QueryContext(ctx, query, args...)
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
		err := s.db().QueryRowContext(ctx, `SELECT id, workspace_id, kind, detail, evidence, opened_at, resolved_at FROM faults WHERE id = ?`, id).
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

// FaultRecorded reports whether any fault matching m was ever recorded, open or
// resolved. A match names its kind and workspace, and every evidence pair it
// names must be present in the record with exactly that value; the evidence
// column is JSON, so each pair is one json_extract comparison.
//
// A match that names no kind or no workspace is refused: it would answer for
// every fault of a kind, or every kind, and no caller means that.
func (s *store) FaultRecorded(ctx context.Context, m FaultMatch) (bool, error) {
	const op = "daemon.wsm.fault_recorded"
	fields := dlog.Context{"kind": m.Kind, "workspace": string(m.Workspace)}
	if m.Kind == "" || m.Workspace == "" {
		err := errors.New("wsm: a fault match must name its kind and workspace")
		s.log.Error(op, "refused a fault match naming no kind or no workspace", withError(fields, err))
		return false, err
	}
	query := `SELECT EXISTS (SELECT 1 FROM faults WHERE workspace_id = ? AND kind = ?`
	args := []any{string(m.Workspace), m.Kind}
	keys := make([]string, 0, len(m.Evidence))
	for key := range m.Evidence {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	for _, key := range keys {
		if !evidenceKey.MatchString(key) {
			err := fmt.Errorf("wsm: evidence key %q is not a plain field name", key)
			s.log.Error(op, "refused a fault match naming an evidence key no record is written under", withError(fields, err))
			return false, err
		}
		query += ` AND json_extract(evidence, ?) = ?`
		args = append(args, "$."+key, m.Evidence[key])
	}
	query += `)`
	var recorded bool
	err := s.read(ctx, op, fields, func(ctx context.Context) error {
		return s.db().QueryRowContext(ctx, query, args...).Scan(&recorded)
	})
	if err != nil {
		return false, err
	}
	return recorded, nil
}

// evidenceKey is the shape of every evidence key a fault is recorded under: a
// proto field name. Only such a key can be spliced into a JSON path unquoted.
var evidenceKey = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)
