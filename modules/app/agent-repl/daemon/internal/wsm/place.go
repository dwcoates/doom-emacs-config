package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"claude-repld/internal/dlog"
)

// THE REGISTRY ORDER is one order of every workspace, held as each row's
// `place` (schema.go). The sidebar sorts each list of rows by it and the
// editor's tab bar lists the open workspaces in it; both are views, and
// nothing else decides order.
//
// Where a workspace goes:
//   - CREATED: second-to-last among the open workspaces of its priority, or
//     among all open workspaces when it has none or none share it
//     (landingPlace).
//   - PRIORITY SET: the same landing, recomputed without the workspace itself;
//     clearing a priority moves nothing.
//   - CLOSED and RE-OPENED: nothing moves; it returns to the place it held.
//   - MOVED (MovePlace): onto another workspace's place, the ones between
//     shifting one step toward the place it left.
//
// Places are unique (the workspaces_by_place index) and at least 1 at rest. 0
// is the one parking slot a write steps a row onto inside its own transaction,
// and negatives are a shift's in-flight values. A row INSERTED on 0 (one that
// states no place) is moved after every other place by the
// workspaces_land_unplaced trigger in the same statement.

// parkedPlace is the slot a row is parked on while a write moves places around
// it. No row rests there.
const parkedPlace = 0

// openPlaces answers the places of the open workspaces other than self, in
// order, restricted to priority p when p is non-nil.
func openPlaces(ctx context.Context, tx *sql.Tx, p *Priority, self WorkspaceID) ([]int64, error) {
	query := `SELECT place FROM workspaces WHERE closed = 0 AND id <> ?`
	args := []any{self}
	if p != nil {
		query += ` AND priority = ?`
		args = append(args, int(*p))
	}
	rows, err := tx.QueryContext(ctx, query+` ORDER BY place`, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []int64
	for rows.Next() {
		var at int64
		if err := rows.Scan(&at); err != nil {
			return nil, err
		}
		out = append(out, at)
	}
	return out, rows.Err()
}

// landingPlace frees and returns the place a workspace of priority p lands on,
// self left out of every count and shift:
//   - among the open workspaces of priority p when p is set and any exist,
//     otherwise among all open workspaces;
//   - with two or more of them, just before the last one (second-to-last);
//   - with exactly one, just after it;
//   - with none open at all, before every place (the first slot).
func landingPlace(ctx context.Context, tx *sql.Tx, p *Priority, self WorkspaceID) (int64, error) {
	var run []int64
	if p != nil {
		bucket, err := openPlaces(ctx, tx, p, self)
		if err != nil {
			return 0, err
		}
		run = bucket
	}
	if len(run) == 0 {
		all, err := openPlaces(ctx, tx, nil, self)
		if err != nil {
			return 0, err
		}
		run = all
	}
	var at int64
	switch len(run) {
	case 0:
		var first sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT MIN(place) FROM workspaces WHERE id <> ? AND place > 0`, self).Scan(&first); err != nil {
			return 0, err
		}
		at = 1
		if first.Valid {
			at = first.Int64
		}
	case 1:
		at = run[0] + 1
	default:
		at = run[len(run)-1]
	}
	if err := shiftFrom(ctx, tx, at, self); err != nil {
		return 0, err
	}
	return at, nil
}

// shiftFrom moves every place at or after `at` up by one, self left out, so
// `at` is free. It runs through negatives so the unique index never sees two
// rows on one place mid-statement.
func shiftFrom(ctx context.Context, tx *sql.Tx, at int64, self WorkspaceID) error {
	if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = -(place + 1) WHERE place >= ? AND id <> ?`, at, self); err != nil {
		return err
	}
	_, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = -place WHERE place < 0`)
	return err
}

// reland moves an existing workspace onto the place a workspace of priority p
// lands on (landingPlace). It parks the row first, so the shift can pass over
// the place it leaves.
func reland(ctx context.Context, tx *sql.Tx, id WorkspaceID, p *Priority) (int64, error) {
	if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, parkedPlace, id); err != nil {
		return 0, err
	}
	at, err := landingPlace(ctx, tx, p, id)
	if err != nil {
		return 0, err
	}
	res, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, at, id)
	if err != nil {
		return 0, err
	}
	return at, requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
}

// MovePlace moves workspace ws onto target's place in one transaction. Every
// workspace between the two shifts one place toward the place ws left, so ws
// ends exactly where target was and target ends beside it, on the side ws came
// from. An unknown id is refused (ErrNotFound) with nothing written.
func (s *store) MovePlace(ctx context.Context, ws, target WorkspaceID) error {
	const op = "daemon.wsm.move_place"
	fields := dlog.Context{"workspace": string(ws), "target": string(target)}
	if ws == target {
		err := fmt.Errorf("wsm: move place: workspace %s onto itself", ws)
		s.log.Error(op, "refused to move a workspace onto its own place", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var from, to int64
		if err := scanWorkspaceRow(ctx, tx, ws, "place", &from); err != nil {
			return err
		}
		if err := scanWorkspaceRow(ctx, tx, target, "place", &to); err != nil {
			return err
		}
		fields["from"], fields["to"] = from, to
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, parkedPlace, ws); err != nil {
			return err
		}
		// Rightward, the places in (from, to] step down one; leftward, the
		// places in [to, from) step up one. Both run through negatives.
		shift := `UPDATE workspaces SET place = -(place - 1) WHERE place > ? AND place <= ?`
		lo, hi := from, to
		if from > to {
			shift = `UPDATE workspaces SET place = -(place + 1) WHERE place >= ? AND place < ?`
			lo, hi = to, from
		}
		if _, err := tx.ExecContext(ctx, shift, lo, hi); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = -place WHERE place < 0`); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, to, ws)
		return err
	})
}
