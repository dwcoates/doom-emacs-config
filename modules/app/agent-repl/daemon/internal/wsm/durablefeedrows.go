package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
)

// DurableFeedRow is one daemon-synthesized feed row a new daemon draws again:
// the row as last published, and the place it was first drawn at.
type DurableFeedRow struct {
	// Workspace owns the row.
	Workspace WorkspaceID
	// RowID is the row's FeedId value, unique within the workspace.
	RowID string
	// Plane is the feed plane the row was first drawn in, as the feed resolver
	// numbers it.
	Plane int
	// OrderKey is the order key the row was first drawn at, which every later
	// publication restates — or, on the one publication that SETTLES a pinned
	// row, the key it settles at.
	OrderKey string
	// SettledFrom is set on the one publication that settles a pinned row
	// (frontend.v1 FeedRowOrder.pinned): the pin key the row is recorded at
	// now, which OrderKey replaces. The replacement is a compare-and-set — it
	// is refused unless the recorded key is exactly this one — so a row moves
	// at most once and only from where the caller believes it stands. Empty on
	// every other publication, whose key must match the recorded one.
	SettledFrom string
	// Row is the encoded frontend.v1.FeedRow as last published.
	Row []byte
}

// validate refuses a row that could not be drawn again.
func (r DurableFeedRow) validate() error {
	switch {
	case r.Workspace == "":
		return errors.New("wsm: a durable feed row names its workspace")
	case r.RowID == "":
		return errors.New("wsm: a durable feed row names its row")
	case r.OrderKey == "":
		return errors.New("wsm: a durable feed row carries its order key")
	case len(r.Row) == 0:
		return errors.New("wsm: a durable feed row carries the row")
	}
	return nil
}

// PutDurableFeedRow records one durable feed row, replacing the row's previous
// publication. The order key of a row already recorded never changes — a row
// keeps the place it was first drawn at, so a replacement that names another
// is refused — except on the one publication that settles a pinned row
// (DurableFeedRow.SettledFrom), which moves it from its pin key to its settled
// key exactly once.
//
// A READ-ONLY HANDLE HOLDS THE ROW INSTEAD, latest per row, and the promotion
// writes it (writeHeldDurableRowsLocked). A joining successor's children
// tracker and feed draw rows while the incumbent is still the writer
// (2026-11-26: every child's bubble, drawn as the roster installed the
// registry, was refused at ERROR on the read-only handle, and the feed logged
// each as a row a new daemon would not draw).
func (s *store) PutDurableFeedRow(ctx context.Context, row DurableFeedRow) error {
	const op = "daemon.wsm.put_durable_feed_row"
	fields := dlog.Context{"workspace": string(row.Workspace), "row": row.RowID, "order_key": row.OrderKey}
	if err := row.validate(); err != nil {
		s.log.Error(op, "refused a durable feed row that could not be drawn again", withError(fields, err))
		return err
	}
	s.mu.RLock()
	if s.ReadOnly() {
		s.holdDurableRowLocked(row)
		s.mu.RUnlock()
		s.log.Debug(op, "held the durable feed row until this handle writes; the incumbent is still the writer", fields)
		return nil
	}
	s.mu.RUnlock()
	return s.writeDurableFeedRow(ctx, op, fields, row, false)
}

// heldRowKey names one held durable feed row: its workspace and its row.
type heldRowKey struct {
	workspace WorkspaceID
	row       string
}

// holdDurableRowLocked keeps row as its row's latest held publication. The
// caller holds mu's read side.
//
// A HELD SETTLE IS KEPT THROUGH A LATER RESTATEMENT: when the held row carries
// SettledFrom and the replacing publication does not, the replacement keeps
// the earlier SettledFrom. A pinned row settles exactly once, and every later
// publication restates the settled key, so dropping the settle would make the
// promotion's write fail its compare-and-set against the pin key still
// recorded.
func (s *store) holdDurableRowLocked(row DurableFeedRow) {
	s.heldMu.Lock()
	defer s.heldMu.Unlock()
	if s.heldDurableRows == nil {
		s.heldDurableRows = make(map[heldRowKey]DurableFeedRow)
	}
	key := heldRowKey{workspace: row.Workspace, row: row.RowID}
	if prior, held := s.heldDurableRows[key]; held && prior.SettledFrom != "" && row.SettledFrom == "" {
		row.SettledFrom = prior.SettledFrom
	}
	s.heldDurableRows[key] = row
}

// writeHeldDurableRowsLocked writes every held durable feed row and forgets
// it. The caller holds mu EXCLUSIVELY and the handle already writes. A failed
// write is recorded at ERROR by the write itself and is not held again: the
// feed's next publication of that row writes afresh.
func (s *store) writeHeldDurableRowsLocked(ctx context.Context) {
	const op = "daemon.wsm.put_durable_feed_row"
	s.heldMu.Lock()
	held := s.heldDurableRows
	s.heldDurableRows = nil
	s.heldMu.Unlock()
	for _, row := range held {
		fields := dlog.Context{"workspace": string(row.Workspace), "row": row.RowID, "order_key": row.OrderKey, "held": true}
		if err := s.writeDurableFeedRow(ctx, op, fields, row, true); err == nil {
			s.log.Debug(op, "wrote the durable feed row held while this handle was read-only", fields)
		}
	}
}

// writeDurableFeedRow is the row's one write. HELD says the row was held on a
// read-only handle and is written at the promotion: a settle whose pin key
// is recorded nowhere then means the pin itself was held and never written,
// so the row is recorded at its settled key as a first publication. A settle
// of an unrecorded row that was not held is still refused: nothing drew the
// pin it claims to move from.
func (s *store) writeDurableFeedRow(ctx context.Context, op string, fields dlog.Context, row DurableFeedRow, held bool) error {
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var key string
		err := tx.QueryRowContext(ctx, `SELECT order_key FROM durable_feed_rows WHERE workspace_id = ? AND row_id = ?`,
			row.Workspace, row.RowID).Scan(&key)
		switch {
		case errors.Is(err, sql.ErrNoRows):
		case err != nil:
			return err
		case row.SettledFrom != "":
			if key != row.SettledFrom {
				return fmt.Errorf("wsm: durable feed row %s settles from %q but is recorded at %q", row.RowID, row.SettledFrom, key)
			}
			_, err = tx.ExecContext(ctx,
				`UPDATE durable_feed_rows SET order_key = ?, plane = ?, row = ? WHERE workspace_id = ? AND row_id = ?`,
				row.OrderKey, row.Plane, row.Row, row.Workspace, row.RowID)
			return err
		case key != row.OrderKey:
			return fmt.Errorf("wsm: durable feed row %s was drawn at %q, not %q", row.RowID, key, row.OrderKey)
		}
		if row.SettledFrom != "" && !held {
			return fmt.Errorf("wsm: durable feed row %s settles from %q but was never recorded", row.RowID, row.SettledFrom)
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT (workspace_id, row_id) DO UPDATE SET row = excluded.row`,
			row.Workspace, row.RowID, row.Plane, row.OrderKey, row.Row)
		return err
	})
}

// DurableFeedRowRank answers the plane and order key durable row ROWID of
// workspace ID is recorded at, found false when it is not recorded.
func (s *store) DurableFeedRowRank(ctx context.Context, id WorkspaceID, rowID string) (int, string, bool, error) {
	const op = "daemon.wsm.durable_feed_row_rank"
	var (
		plane int
		key   string
		found bool
	)
	err := s.read(ctx, op, dlog.Context{"workspace": string(id), "row": rowID}, func(ctx context.Context) error {
		err := s.q(ctx).QueryRowContext(ctx, `SELECT plane, order_key FROM durable_feed_rows WHERE workspace_id = ? AND row_id = ?`, id, rowID).Scan(&plane, &key)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		found = err == nil
		return err
	})
	return plane, key, found, err
}

// DurableFeedRows loads every durable feed row of one workspace, in order-key
// order.
func (s *store) DurableFeedRows(ctx context.Context, id WorkspaceID) ([]DurableFeedRow, error) {
	var out []DurableFeedRow
	err := s.read(ctx, "daemon.wsm.durable_feed_rows", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.q(ctx).QueryContext(ctx,
			`SELECT row_id, plane, order_key, row FROM durable_feed_rows WHERE workspace_id = ? ORDER BY order_key, row_id`, id)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			row := DurableFeedRow{Workspace: id}
			if err := rows.Scan(&row.RowID, &row.Plane, &row.OrderKey, &row.Row); err != nil {
				return err
			}
			out = append(out, row)
		}
		return rows.Err()
	})
	return out, err
}

// MatchDurableFeedRows loads the durable feed rows MATCH answers true for,
// asked of each row's workspace and id alone, over the workspaces of SCOPE --
// every workspace when SCOPE is empty -- in workspace and order-key order.
// THE IDS ARE READ FIRST AND ONLY THE MATCHES' ROWS AFTER: the rows of every
// workspace together run to gigabytes, and a caller after one kind of row
// (the merge heads) reads only those.
func (s *store) MatchDurableFeedRows(ctx context.Context, scope []WorkspaceID, match func(WorkspaceID, string) bool) ([]DurableFeedRow, error) {
	var out []DurableFeedRow
	fields := dlog.Context{"workspaces": len(scope)}
	err := s.read(ctx, "daemon.wsm.match_durable_feed_rows", fields, func(ctx context.Context) error {
		query, args := `SELECT workspace_id, row_id FROM durable_feed_rows`, []any{}
		if len(scope) > 0 {
			query += ` WHERE workspace_id IN (?` + strings.Repeat(`, ?`, len(scope)-1) + `)`
			for _, ws := range scope {
				args = append(args, ws)
			}
		}
		rows, err := s.q(ctx).QueryContext(ctx, query+` ORDER BY workspace_id, order_key, row_id`, args...)
		if err != nil {
			return err
		}
		var matched []DurableFeedRow
		for rows.Next() {
			var row DurableFeedRow
			if err := rows.Scan(&row.Workspace, &row.RowID); err != nil {
				rows.Close()
				return err
			}
			if match(row.Workspace, row.RowID) {
				matched = append(matched, row)
			}
		}
		if err := rows.Close(); err != nil {
			return err
		}
		if err := rows.Err(); err != nil {
			return err
		}
		for _, row := range matched {
			if err := s.q(ctx).QueryRowContext(ctx, `SELECT plane, order_key, row FROM durable_feed_rows WHERE workspace_id = ? AND row_id = ?`,
				row.Workspace, row.RowID).Scan(&row.Plane, &row.OrderKey, &row.Row); err != nil {
				return fmt.Errorf("wsm: read the durable feed row %s/%s: %w", row.Workspace, row.RowID, err)
			}
			out = append(out, row)
		}
		return nil
	})
	return out, err
}

// ClearDurableFeedRows drops every durable feed row of one workspace. Clearing
// a workspace with none is not a refusal: the rows are gone either way.
func (s *store) ClearDurableFeedRows(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.clear_durable_feed_rows", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM durable_feed_rows WHERE workspace_id = ?`, id)
		return err
	})
}
