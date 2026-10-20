package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
)

// rowQuerier is what a single-row read runs on: a transaction or the handle.
type rowQuerier interface {
	QueryRowContext(ctx context.Context, query string, args ...any) *sql.Row
}

// querier is what a many-row read runs on: a transaction or the handle.
type querier interface {
	QueryContext(ctx context.Context, query string, args ...any) (*sql.Rows, error)
}

// listRows runs QUERY through Q and decodes every row with SCAN,
// all-or-nothing: one row that does not decode fails the whole read.
func listRows[T any](ctx context.Context, q querier, query string, scan func(interface{ Scan(...any) error }) (T, error), args ...any) ([]T, error) {
	rows, err := q.QueryContext(ctx, query, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []T
	for rows.Next() {
		row, err := scan(rows)
		if err != nil {
			return nil, err
		}
		out = append(out, row)
	}
	return out, rows.Err()
}

// scanWorkspaceRow is the one read of named columns of workspace id, scanned
// into dest. An unknown id is ErrNotFound, never a bare sql.ErrNoRows, so
// every caller refuses a missing workspace the same way.
func scanWorkspaceRow(ctx context.Context, q rowQuerier, id WorkspaceID, columns string, dest ...any) error {
	err := q.QueryRowContext(ctx, `SELECT `+columns+` FROM workspaces WHERE id = ?`, id).Scan(dest...)
	if errors.Is(err, sql.ErrNoRows) {
		return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
	}
	return err
}
