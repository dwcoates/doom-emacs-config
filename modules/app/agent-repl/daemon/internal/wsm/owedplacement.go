package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// owedPlacementNotesDDL is the layout-140 addition: the root workspaces whose
// subsection placement note is still owed, because they were created with no
// first message to carry it (a static create, an agent-API create without a
// prompt). The note rides the first prompt the workspace later receives,
// which takes the row; a workspace that leaves the registry takes it with it.
//
// IT IS ADDITIVE: the build before it names no such table.
const owedPlacementNotesDDL = `
CREATE TABLE owed_placement_notes (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  owed_at      INTEGER NOT NULL
);
`

// OwePlacementNote records that workspace ID's next prompt owes the
// subsection placement note, AT being when it came to be owed. A note
// already owed stands as it is.
func (s *store) OwePlacementNote(ctx context.Context, id WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.owe_placement_note"
	return s.write(ctx, op, dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO owed_placement_notes (workspace_id, owed_at) VALUES (?, ?) ON CONFLICT(workspace_id) DO NOTHING`, id, nanos(at))
		return err
	})
}

// TakeOwedPlacementNote takes the placement note workspace ID's next prompt
// owes: it answers whether one was owed and retires it in one transaction,
// so two prompts never both carry it.
func (s *store) TakeOwedPlacementNote(ctx context.Context, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.take_owed_placement_note"
	found := false
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		var at int64
		err := tx.QueryRowContext(ctx, `DELETE FROM owed_placement_notes WHERE workspace_id = ? RETURNING owed_at`, id).Scan(&at)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		found = true
		return nil
	})
	return found, err
}
