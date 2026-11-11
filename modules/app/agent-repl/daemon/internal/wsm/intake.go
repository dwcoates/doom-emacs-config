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
// written in the transaction that inserts the workspace hidden
// (RegisterFacts.Intake), so an intake workspace is never one without the
// other, and never drawn visible.
//
// IT IS ADDITIVE: one new table the previous build never names.
const intakeWorkspacesDDL = `
CREATE TABLE intake_workspaces (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  marked_at    INTEGER NOT NULL
);
`

// insertIntakeRow records workspace ID as an ephemeral intake workspace as of
// AT, inside the transaction that inserts it (RegisterFacts.Intake).
func insertIntakeRow(ctx context.Context, tx *sql.Tx, id WorkspaceID, at time.Time) error {
	_, err := tx.ExecContext(ctx, `INSERT INTO intake_workspaces (workspace_id, marked_at) VALUES (?, ?)`, id, nanos(at))
	return err
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
