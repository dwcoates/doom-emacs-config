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
