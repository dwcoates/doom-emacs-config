package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// A WORKSPACE'S ROSTER PLACE is the stored order its siblings draw in, on the
// sidebar and on the tab bar that walks it (schema.go's `place`). A created
// workspace LANDS on the place its priority picks; a re-opened one always moves
// to the LAST place; the user's swaps move it after that, and a later priority
// change leaves it where it is.
//
// Places are unique (the workspaces_by_place index), so siblings never tie and
// a swap always moves something. At rest every place is at least 1: 0 is the
// one parking slot a write uses to step a row out of the way inside its own
// transaction, and negatives are the shift's in-flight values. A row INSERTED
// on 0 (one that states no place) is moved after every other place by the
// workspaces_land_unplaced trigger in the same statement.

// parkedPlace is the slot a row is parked on while a write moves places around
// it. No row rests there.
const parkedPlace = 0

// landingPlace frees and returns the place a workspace of priority p lands on
// in repository repo: just before the first workspace of that repository (in
// place order) whose priority is weaker, so it joins the end of its own
// priority's run; or after every place when no weaker one exists, which is
// where an unprioritized workspace always lands. self is left out of the
// search and the shift, so a re-opened workspace never lands relative to its
// own old place.
//
// Every place at or after the landing one moves up by one. The shift runs
// through negatives so the unique index never sees two rows on one place
// mid-statement.
func landingPlace(ctx context.Context, tx *sql.Tx, repo RepoID, p *Priority, self WorkspaceID) (int64, error) {
	if p != nil {
		var at int64
		err := tx.QueryRowContext(ctx,
			`SELECT place FROM workspaces
			 WHERE repo_id = ? AND id <> ? AND (priority IS NULL OR priority > ?)
			 ORDER BY place LIMIT 1`, repo, self, int(*p)).Scan(&at)
		switch {
		case err == nil:
			if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = -(place + 1) WHERE place >= ? AND id <> ?`, at, self); err != nil {
				return 0, err
			}
			if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = -place WHERE place < 0`); err != nil {
				return 0, err
			}
			return at, nil
		case !errors.Is(err, sql.ErrNoRows):
			return 0, err
		}
	}
	return lastPlace(ctx, tx, self)
}

// lastPlace answers the place after every other workspace's, self left out.
func lastPlace(ctx context.Context, tx *sql.Tx, self WorkspaceID) (int64, error) {
	var last sql.NullInt64
	if err := tx.QueryRowContext(ctx, `SELECT MAX(place) FROM workspaces WHERE id <> ?`, self).Scan(&last); err != nil {
		return 0, err
	}
	return last.Int64 + 1, nil
}

// moveLast moves an existing workspace onto the place after every other one:
// where a re-opened workspace always goes, whatever its priority.
func moveLast(ctx context.Context, tx *sql.Tx, id WorkspaceID) (int64, error) {
	at, err := lastPlace(ctx, tx, id)
	if err != nil {
		return 0, err
	}
	res, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, at, id)
	if err != nil {
		return 0, err
	}
	return at, requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
}

// SwapPlaces exchanges two workspaces' roster places in one transaction. An
// unknown id is refused (ErrNotFound) with nothing written. Whether the two are
// siblings is the caller's question: the store holds places, not the drawn
// tree.
func (s *store) SwapPlaces(ctx context.Context, a, b WorkspaceID) error {
	const op = "daemon.wsm.swap_places"
	fields := dlog.Context{"workspace": string(a), "other": string(b)}
	if a == b {
		err := fmt.Errorf("wsm: swap places: workspace %s with itself", a)
		s.log.Error(op, "refused to swap a workspace's place with its own", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		place := func(id WorkspaceID) (int64, error) {
			var at int64
			err := scanWorkspaceRow(ctx, tx, id, "place", &at)
			return at, err
		}
		pa, err := place(a)
		if err != nil {
			return err
		}
		pb, err := place(b)
		if err != nil {
			return err
		}
		fields["from"], fields["to"] = pa, pb
		for _, step := range []struct {
			id WorkspaceID
			at int64
		}{{a, parkedPlace}, {b, pa}, {a, pb}} {
			if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET place = ? WHERE id = ?`, step.at, step.id); err != nil {
				return err
			}
		}
		return nil
	})
}
