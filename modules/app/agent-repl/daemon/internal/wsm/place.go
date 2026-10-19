package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
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
//   - SWAPPED (SwapPlaces): it and another workspace exchange places; every
//     other workspace keeps its own.
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

// SwapPlaces exchanges the places of workspaces ws and target in one
// transaction: ws takes target's place, target takes ws's, and every other
// workspace keeps its own. An unknown id is refused (ErrNotFound), and two
// workspaces of different repositories (ErrOtherRepository), with nothing
// written.
func (s *store) SwapPlaces(ctx context.Context, ws, target WorkspaceID) error {
	const op = "daemon.wsm.swap_places"
	fields := dlog.Context{"workspace": string(ws), "target": string(target)}
	if ws == target {
		err := fmt.Errorf("wsm: swap places: workspace %s with itself", ws)
		s.log.Error(op, "refused to swap a workspace with itself", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var from, to int64
		var fromRepo, toRepo RepoID
		var fromParent, toParent, fromSubsection, toSubsection sql.NullString
		if err := scanWorkspaceRow(ctx, tx, ws, "place, repo_id, parent_id, subsection_id", &from, &fromRepo, &fromParent, &fromSubsection); err != nil {
			return err
		}
		if err := scanWorkspaceRow(ctx, tx, target, "place, repo_id, parent_id, subsection_id", &to, &toRepo, &toParent, &toSubsection); err != nil {
			return err
		}
		// THE TAB BAR NEVER INTERLEAVES REPOSITORIES, so a workspace of one has
		// no tab of another to take (owner ruling, 2026-10-19).
		if fromRepo != toRepo {
			return ErrOtherRepository
		}
		// TWO ROOTS EXCHANGE THEIR SUBSECTIONS TOO (arrange.go), so each lands
		// on the tab the other held even across subsections. A swap across
		// families exchanges places alone: a child sits wherever its root does.
		if !fromParent.Valid && !toParent.Valid {
			for id, sub := range map[WorkspaceID]sql.NullString{ws: toSubsection, target: fromSubsection} {
				if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET subsection_id = ? WHERE id = ?`, sub, id); err != nil {
					return err
				}
			}
		}
		fields["from"], fields["to"] = from, to
		// ws steps onto the parking slot first, so the unique index never sees
		// two rows on one place while target takes the place ws left.
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, parkedPlace, ws); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, from, target); err != nil {
			return err
		}
		_, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, to, ws)
		return err
	})
}
