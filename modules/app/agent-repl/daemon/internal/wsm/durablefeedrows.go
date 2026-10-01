package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
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
	// publication restates.
	OrderKey string
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
// publication. The order key of a row already recorded never changes: a row
// keeps the place it was first drawn at, so a replacement that names another
// is refused.
func (s *store) PutDurableFeedRow(ctx context.Context, row DurableFeedRow) error {
	const op = "daemon.wsm.put_durable_feed_row"
	fields := dlog.Context{"workspace": string(row.Workspace), "row": row.RowID, "order_key": row.OrderKey}
	if err := row.validate(); err != nil {
		s.log.Error(op, "refused a durable feed row that could not be drawn again", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var key string
		err := tx.QueryRowContext(ctx, `SELECT order_key FROM durable_feed_rows WHERE workspace_id = ? AND row_id = ?`,
			row.Workspace, row.RowID).Scan(&key)
		switch {
		case errors.Is(err, sql.ErrNoRows):
		case err != nil:
			return err
		case key != row.OrderKey:
			return fmt.Errorf("wsm: durable feed row %s was drawn at %q, not %q", row.RowID, key, row.OrderKey)
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES (?, ?, ?, ?, ?)
			 ON CONFLICT (workspace_id, row_id) DO UPDATE SET row = excluded.row`,
			row.Workspace, row.RowID, row.Plane, row.OrderKey, row.Row)
		return err
	})
}

// DurableFeedRows loads every durable feed row of one workspace, in order-key
// order.
func (s *store) DurableFeedRows(ctx context.Context, id WorkspaceID) ([]DurableFeedRow, error) {
	var out []DurableFeedRow
	err := s.read(ctx, "daemon.wsm.durable_feed_rows", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
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

// ClearDurableFeedRows drops every durable feed row of one workspace. Clearing
// a workspace with none is not a refusal: the rows are gone either way.
func (s *store) ClearDurableFeedRows(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.clear_durable_feed_rows", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `DELETE FROM durable_feed_rows WHERE workspace_id = ?`, id)
		return err
	})
}
