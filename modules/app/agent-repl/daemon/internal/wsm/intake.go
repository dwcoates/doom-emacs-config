package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// intakeWorkspacesDDL is the layout-115 addition: intake_workspaces, one row
// per EPHEMERAL INTAKE WORKSPACE, a hidden workspace the daemon creates to
// work out what a request is and hand it on (a card's `?` question, a Slack
// emoji ack), which the daemon closes when a turn of it ends. The row is
// written in the transaction that hides the workspace (MarkIntake), so an
// intake workspace is never one without the other.
//
// IT IS ADDITIVE: one new table the previous build never names.
const intakeWorkspacesDDL = `
CREATE TABLE intake_workspaces (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  marked_at    INTEGER NOT NULL
);
`

// MarkIntake records workspace ID as an ephemeral intake workspace as of AT
// and hides it, in one transaction. A workspace is marked once, at its
// creation, so a second mark is a caller's defect and is refused.
func (s *store) MarkIntake(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.mark_intake"
	return s.write(ctx, op, dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		if err := requireWorkspaceRow(ctx, tx, id); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `INSERT INTO intake_workspaces (workspace_id, marked_at) VALUES (?, ?)`, id, nanos(at)); err != nil {
			return err
		}
		return updateWorkspaceColumns(ctx, tx, id, hideColumns(true, at)...)
	})
}

// IsIntake answers whether workspace ID is an ephemeral intake workspace.
func (s *store) IsIntake(ctx context.Context, id WorkspaceID) (bool, error) {
	intake := false
	err := s.read(ctx, "daemon.wsm.is_intake", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var one int
		err := s.q(ctx).QueryRowContext(ctx, `SELECT 1 FROM intake_workspaces WHERE workspace_id = ?`, id).Scan(&one)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		intake = err == nil
		return err
	})
	return intake, err
}
