package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// childrenFoldedDDL is the layout-108 addition: whether a workspace's
// children are folded (frontend.v1.RosterOpenRow.children_fold).
//
// IT IS ADDITIVE: the previous build names no such column, so a workspace it
// registers takes 0, expanded, which is what every row drew as before.
const childrenFoldedDDL = `
ALTER TABLE workspaces ADD COLUMN children_folded INTEGER NOT NULL DEFAULT 0;
`

// SetChildrenFolded records whether workspace ID's children are folded. A
// folded workspace's open descendants are hidden in the sidebar and their
// tabs are folded off the tab bar, as a folded subsection's are
// (resolve/sidebar's openTree). An unknown workspace is refused (ErrNotFound),
// never a write that touched nothing.
func (s *store) SetChildrenFolded(ctx context.Context, id WorkspaceID, folded bool) error {
	return s.write(ctx, "daemon.wsm.set_children_folded", dlog.Context{"workspace": string(id), "folded": folded},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE workspaces SET children_folded = ? WHERE id = ?`, folded, id)
			if err != nil {
				return err
			}
			return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
		})
}

// expandChildrenFoldsHolding expands, within TX, every collapsed fold of
// workspace ID's ancestors' children, answering whether any was collapsed. It
// is THE ONE EXPANSION of a workspace's children the daemon makes on its own:
// a workspace being selected (expandFoldsHolding) and one starting work
// (ExpandChildrenFoldsHolding) both come here, so a workspace is never left
// hidden under a collapsed parent.
func expandChildrenFoldsHolding(ctx context.Context, tx *sql.Tx, id WorkspaceID) (bool, error) {
	res, err := tx.ExecContext(ctx, `
WITH RECURSIVE up(id, parent_id) AS (
  SELECT own.id, own.parent_id FROM workspaces AS own WHERE own.id = ?
  UNION
  SELECT anc.id, anc.parent_id FROM workspaces AS anc JOIN up ON anc.id = up.parent_id
)
UPDATE workspaces SET children_folded = 0
WHERE children_folded = 1 AND id <> ? AND id IN (SELECT id FROM up)`, id, id)
	if err != nil {
		return false, fmt.Errorf("wsm: expand the children folds holding %s: %w", id, err)
	}
	expanded, err := touchedRow(res)
	if err != nil {
		return false, fmt.Errorf("wsm: expand the children folds holding %s: %w", id, err)
	}
	return expanded, nil
}

// ExpandChildrenFoldsHolding expands the collapsed folds of workspace ID's
// ancestors' children, answering whether any was collapsed. A workspace that
// starts working never stays hidden under a collapsed parent, as it never
// stays inside a collapsed subsection (ExpandSubsectionHolding).
func (s *store) ExpandChildrenFoldsHolding(ctx context.Context, id WorkspaceID) (bool, error) {
	var expanded bool
	err := s.write(ctx, "daemon.wsm.expand_children_folds_holding", dlog.Context{"workspace": string(id)},
		func(ctx context.Context, tx *sql.Tx) error {
			var err error
			expanded, err = expandChildrenFoldsHolding(ctx, tx, id)
			return err
		})
	return expanded, err
}

// ancestorsOf answers, within TX, every ancestor of workspace ID, nearest
// first: the rows whose children folds expandChildrenFoldsHolding expands.
func ancestorsOf(ctx context.Context, tx *sql.Tx, id WorkspaceID) ([]WorkspaceID, error) {
	rows, err := tx.QueryContext(ctx, `
WITH RECURSIVE up(id, parent_id, depth) AS (
  SELECT own.id, own.parent_id, 0 FROM workspaces AS own WHERE own.id = ?
  UNION
  SELECT anc.id, anc.parent_id, up.depth + 1 FROM workspaces AS anc JOIN up ON anc.id = up.parent_id
)
SELECT id FROM up WHERE id <> ? ORDER BY depth`, id, id)
	if err != nil {
		return nil, fmt.Errorf("wsm: read the ancestors of %s: %w", id, err)
	}
	defer rows.Close()
	var out []WorkspaceID
	for rows.Next() {
		var ancestor string
		if err := rows.Scan(&ancestor); err != nil {
			return nil, fmt.Errorf("wsm: read the ancestors of %s: %w", id, err)
		}
		out = append(out, WorkspaceID(ancestor))
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("wsm: read the ancestors of %s: %w", id, err)
	}
	return out, nil
}
