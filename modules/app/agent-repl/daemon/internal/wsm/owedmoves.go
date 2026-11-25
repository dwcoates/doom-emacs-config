package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"agent-repld/internal/dlog"
)

// owedMovesDDL is the layout-212 addition: the move a rename owes a
// workspace (agentrepl.v1.SetWorkspaceName). IT IS ADDITIVE: a new table the
// previous build never names.
//
// A RENAME IS TOTAL: the workspace's branch and worktree directory move with
// its name. The name changes at once, but the move waits for the workspace
// to fall free, because the live shim runs in the directory. The row is
// written in the same transaction as the name (Rename), so a rename is never
// recorded without its move, and the move outlives a restart or a handover.
//
// `moving` is set once the move has stood the workspace's shim down
// (MarkMoving): from then on nothing serves the workspace, the steps may have
// moved part of what they move, and the boot finishes the move before it
// brings the workspace up (workspace.Verbs.FinishMovingMoves). A row not yet
// moving is taken again by the daemon that serves the workspace.
//
// `to_dir` is UNIQUE, so two owed moves can never both claim one directory.
const owedMovesDDL = `
CREATE TABLE owed_moves (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  from_branch  TEXT NOT NULL CHECK (from_branch <> ''),
  from_dir     TEXT NOT NULL CHECK (from_dir <> ''),
  to_branch    TEXT NOT NULL CHECK (to_branch <> ''),
  to_dir       TEXT NOT NULL UNIQUE CHECK (to_dir <> ''),
  moving       INTEGER NOT NULL DEFAULT 0
);
`

// MoveTarget is where a rename moves a workspace: the branch and the worktree
// directory its new name resolves to.
type MoveTarget struct {
	Branch string
	Dir    string
}

// OwedMove is the move a rename owes a workspace.
type OwedMove struct {
	Workspace WorkspaceID
	// From is the branch and the directory the workspace had when the move
	// was first owed: what the move moves away from.
	From MoveTarget
	// To is where it moves to.
	To MoveTarget
	// Moving is set once the move stood the workspace's shim down.
	Moving bool
}

// Rename records workspace ID's new NAME, owes it to the workspace's agent
// (owed_renames), and, when TO is set, owes the move of its branch and
// directory to TO, all in one transaction. A move already owed keeps where it
// moves FROM and takes the new TO, since nothing moved yet; one already
// moving is refused (ErrMoveInFlight), since its steps are under way. TO
// unset leaves an owed move standing as it was. Whether the name is legal
// and free is the caller's check, made under the naming lock.
func (s *store) Rename(ctx context.Context, id WorkspaceID, name string, to *MoveTarget) error {
	return s.rename(ctx, "daemon.wsm.rename", id, name, to)
}

// rename is Rename, recorded under OP.
func (s *store) rename(ctx context.Context, op string, id WorkspaceID, name string, to *MoveTarget) error {
	fields := dlog.Context{"workspace": string(id), "name": name}
	if name == "" {
		err := fmt.Errorf("wsm: name of %s: %w", id, errEmptyName)
		s.log.Error(op, "refused an empty workspace name", withError(fields, err))
		return err
	}
	if to != nil {
		if to.Branch == "" || to.Dir == "" {
			err := fmt.Errorf("wsm: move of %s to %+v: a move names its branch and its directory", id, *to)
			s.log.Error(op, "refused a move with no branch or no directory", withError(fields, err))
			return err
		}
		dir, err := normalizeDir(to.Dir)
		if err != nil {
			s.log.Error(op, "could not normalize the directory a move goes to", withError(fields, err))
			return err
		}
		fields["to_branch"], fields["to_dir"] = to.Branch, dir
		to = &MoveTarget{Branch: to.Branch, Dir: dir}
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if err := updateWorkspaceColumns(ctx, tx, id, workspaceColumn{name: "name", value: name}); err != nil {
			return err
		}
		if err := oweRenameTx(ctx, tx, id, name); err != nil {
			return err
		}
		if to == nil {
			return nil
		}
		var moving bool
		err := tx.QueryRowContext(ctx, `SELECT moving FROM owed_moves WHERE workspace_id = ?`, id).Scan(&moving)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			_, err = tx.ExecContext(ctx,
				`INSERT INTO owed_moves (workspace_id, from_branch, from_dir, to_branch, to_dir)
				 SELECT id, branch, dir, ?, ? FROM workspaces WHERE id = ?`, to.Branch, to.Dir, id)
			return err
		case err != nil:
			return err
		case moving:
			return fmt.Errorf("wsm: rename %s: %w", id, ErrMoveInFlight)
		}
		_, err = tx.ExecContext(ctx, `UPDATE owed_moves SET to_branch = ?, to_dir = ? WHERE workspace_id = ?`, to.Branch, to.Dir, id)
		return err
	})
}

// ErrMoveInFlight refuses a second rename of a workspace whose move has
// already stood its shim down: the move's steps are under way, and the
// rename waits for them to end.
var ErrMoveInFlight = errors.New("the workspace's move is under way")

// owedMoveColumns are the columns scanOwedMove decodes.
const owedMoveColumns = `workspace_id, from_branch, from_dir, to_branch, to_dir, moving`

func scanOwedMove(row interface{ Scan(...any) error }) (OwedMove, error) {
	var m OwedMove
	err := row.Scan(&m.Workspace, &m.From.Branch, &m.From.Dir, &m.To.Branch, &m.To.Dir, &m.Moving)
	return m, err
}

// OwedMoveOf answers the move owed workspace ID, and whether one is.
func (s *store) OwedMoveOf(ctx context.Context, id WorkspaceID) (OwedMove, bool, error) {
	const op = "daemon.wsm.owed_move_of"
	var (
		out   OwedMove
		found bool
	)
	err := s.read(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		m, err := scanOwedMove(s.q(ctx).QueryRowContext(ctx, `SELECT `+owedMoveColumns+` FROM owed_moves WHERE workspace_id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		out, found = m, true
		return nil
	})
	return out, found, err
}

// OwedMoves answers every owed move, by workspace.
func (s *store) OwedMoves(ctx context.Context) ([]OwedMove, error) {
	const op = "daemon.wsm.owed_moves"
	var out []OwedMove
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		var err error
		out, err = listRows(ctx, s.q(ctx), `SELECT `+owedMoveColumns+` FROM owed_moves ORDER BY workspace_id`, scanOwedMove)
		return err
	})
	return out, err
}

// MarkMoving records that workspace ID's owed move stood its shim down and
// begins its steps. It answers false when no move is owed.
func (s *store) MarkMoving(ctx context.Context, id WorkspaceID) (bool, error) {
	const op = "daemon.wsm.mark_moving"
	var marked bool
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE owed_moves SET moving = 1 WHERE workspace_id = ?`, id)
		if err != nil {
			return err
		}
		marked, err = touchedRow(res)
		return err
	})
	return marked, err
}

// CompleteMove records that workspace ID's owed move ran: the workspace takes
// the branch and the directory it moved to, every child landing into its old
// branch lands into the new one, and the owed move is gone, in one
// transaction. It answers the move that completed, and false when none was
// owed.
func (s *store) CompleteMove(ctx context.Context, id WorkspaceID) (OwedMove, bool, error) {
	const op = "daemon.wsm.complete_move"
	var (
		done  OwedMove
		found bool
	)
	err := s.write(ctx, op, dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		found = false
		m, err := scanOwedMove(tx.QueryRowContext(ctx, `SELECT `+owedMoveColumns+` FROM owed_moves WHERE workspace_id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if err := updateWorkspaceColumns(ctx, tx, id,
			workspaceColumn{name: "branch", value: m.To.Branch},
			workspaceColumn{name: "dir", value: m.To.Dir}); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx,
			`UPDATE workspaces SET parent_branch = ? WHERE parent_id = ? AND parent_branch = ?`, m.To.Branch, id, m.From.Branch); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM owed_moves WHERE workspace_id = ?`, id); err != nil {
			return err
		}
		done, found = m, true
		return nil
	})
	return done, found, err
}
