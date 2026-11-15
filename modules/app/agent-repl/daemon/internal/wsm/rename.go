package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// SetName records the workspace's new name (agentrepl.v1.SetWorkspaceName),
// and owes it to the workspace's agent in the same transaction.
// It writes the name column only: the branch and the worktree directory keep
// the names the workspace was created with, so after a rename the name and
// the branch may differ. Whether the name is legal and free is the caller's
// check (workspace.verbs.SetName), made under the naming lock. An empty name
// is a defect in the caller and is refused rather than stored, since every
// reader draws the name and the roster refuses a blank one.
func (s *store) SetName(ctx context.Context, id WorkspaceID, name string) error {
	const op = "daemon.wsm.set_name"
	if name == "" {
		err := fmt.Errorf("wsm: name of %s: %w", id, errEmptyName)
		s.log.Error(op, "refused an empty workspace name", withError(dlog.Context{"workspace": string(id)}, err))
		return err
	}
	return s.write(ctx, op, dlog.Context{"workspace": string(id), "name": name}, func(ctx context.Context, tx *sql.Tx) error {
		if err := updateWorkspaceColumns(ctx, tx, id, workspaceColumn{name: "name", value: name}); err != nil {
			return err
		}
		return oweRenameTx(ctx, tx, id, name)
	})
}

// owedRenamesDDL is the layout-146 addition: the name a workspace's agent has
// yet to be told it now has. IT IS ADDITIVE: a new table the previous build
// never names.
//
// A SESSION IS TOLD ITS WORKSPACE'S NAME ONLY AT ITS START, so a rename owes
// the live agent its new name. The rename writes it in the same transaction
// as the name, and the workspace's next prompt carrying a workspace note
// takes it (prompthandler). A later rename replaces it; a later binding
// forgets it, since that conversation's start carries the name.
const owedRenamesDDL = `
CREATE TABLE owed_renames (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  name         TEXT NOT NULL
);
`

// oweRenameTx records NAME as owed to workspace ID's agent, inside the
// caller's transaction.
func oweRenameTx(ctx context.Context, tx *sql.Tx, id WorkspaceID, name string) error {
	_, err := tx.ExecContext(ctx,
		`INSERT INTO owed_renames (workspace_id, name) VALUES (?, ?) ON CONFLICT(workspace_id) DO UPDATE SET name = excluded.name`, id, name)
	return err
}

// TakeOwedRename takes the name workspace ID's next prompt tells its agent:
// it answers it and deletes it in one transaction, so two prompts never both
// carry it. False when none is owed.
func (s *store) TakeOwedRename(ctx context.Context, id WorkspaceID) (string, bool, error) {
	const op = "daemon.wsm.take_owed_rename"
	var (
		name  string
		found bool
	)
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		found = false
		err := tx.QueryRowContext(ctx, `DELETE FROM owed_renames WHERE workspace_id = ? RETURNING name`, id).Scan(&name)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		found = true
		return nil
	})
	return name, found, err
}

// RestoreOwedRename puts back a name TakeOwedRename took for a prompt the
// queue then refused. A name owed again meanwhile is the newer one, and
// stands.
func (s *store) RestoreOwedRename(ctx context.Context, id WorkspaceID, name string) error {
	const op = "daemon.wsm.restore_owed_rename"
	fields := dlog.Context{"workspace": string(id), "name": name}
	if name == "" {
		err := fmt.Errorf("wsm: owed rename of %s: %w", id, errEmptyName)
		s.log.Error(op, "refused to restore an empty owed name", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO owed_renames (workspace_id, name) VALUES (?, ?) ON CONFLICT(workspace_id) DO NOTHING`, id, name)
		return err
	})
}

// errEmptyName is the empty-name refusal.
var errEmptyName = errors.New("a workspace name is never empty")
