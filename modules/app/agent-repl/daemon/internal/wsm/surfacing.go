package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agent-repld/internal/dlog"
)

// surfacedWorkDDL is the layout-153 addition: workspaces.surfaced, whether
// the workspace started a turn while a fold hid it and that turn is still
// running. The roster draws such a workspace, and only it, inside the fold
// (frontend.v1.RosterOpenRow.surfaced). Every existing row is 0: no turn
// started under a fold.
//
// IT IS ADDITIVE: the previous build's writes name no such column, so a row it
// writes takes 0, a workspace its fold hides.
const surfacedWorkDDL = `
ALTER TABLE workspaces ADD COLUMN surfaced INTEGER NOT NULL DEFAULT 0;
`

// DrawnInsideFold reports whether the roster draws the workspace although a
// fold covers it: it started a turn under the fold and the turn is running
// (Surfaced), or it is the workspace the user is on (Current). A fold hides
// every other workspace under it.
func (ws Workspace) DrawnInsideFold() bool {
	return ws.Surfaced || ws.Current
}

// hiddenByFoldQuery answers, for the workspace its one argument names (twice,
// the second for the subsection), whether a fold hides it: the repository
// section its family's root sits in, the subsection the family sits in
// (rootSubsectionColumn), or the children fold of any ancestor. It is the
// store's reading of what resolve/sidebar's openTree draws as a folded tab.
const hiddenByFoldQuery = `
WITH RECURSIVE up(id, parent_id, repo_id, children_folded, depth) AS (
  SELECT id, parent_id, repo_id, children_folded, 0 FROM workspaces WHERE id = ?
  UNION
  SELECT anc.id, anc.parent_id, anc.repo_id, anc.children_folded, up.depth + 1
  FROM workspaces AS anc JOIN up ON anc.id = up.parent_id
)
SELECT EXISTS (SELECT 1 FROM up WHERE depth > 0 AND children_folded = 1)
    OR EXISTS (SELECT 1 FROM repositories WHERE folded = 1
               AND id = (SELECT repo_id FROM up ORDER BY depth DESC LIMIT 1))
    OR EXISTS (SELECT 1 FROM subsections WHERE folded = 1
               AND id = (SELECT ` + rootSubsectionColumn + ` FROM workspaces WHERE id = ?))`

// SurfaceHeldWork marks workspace ID as surfaced when a fold hides it,
// answering whether it did. A workspace that starts a turn under a fold is
// not unfolded into view: it alone is drawn inside the fold, until its turn
// ends (EndSurfacing), however often the section is unfolded and folded.
// A workspace no fold hides, or an unknown one, is left as it is.
func (s *store) SurfaceHeldWork(ctx context.Context, id WorkspaceID) (bool, error) {
	var surfaced bool
	err := s.write(ctx, "daemon.wsm.surface_held_work", dlog.Context{"workspace": string(id)},
		func(ctx context.Context, tx *sql.Tx) error {
			var hidden bool
			if err := tx.QueryRowContext(ctx, hiddenByFoldQuery, id, id).Scan(&hidden); err != nil {
				return fmt.Errorf("wsm: read whether a fold hides %s: %w", id, err)
			}
			if !hidden {
				return nil
			}
			res, err := tx.ExecContext(ctx, `UPDATE workspaces SET surfaced = 1 WHERE surfaced = 0 AND id = ?`, id)
			if err != nil {
				return fmt.Errorf("wsm: surface %s: %w", id, err)
			}
			surfaced, err = touchedRow(res)
			return err
		})
	return surfaced, err
}

// EndSurfacing takes workspace ID's mark off, answering whether it carried
// one: its turn ended, so the fold hides it again.
func (s *store) EndSurfacing(ctx context.Context, id WorkspaceID) (bool, error) {
	var ended bool
	err := s.write(ctx, "daemon.wsm.end_surfacing", dlog.Context{"workspace": string(id)},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE workspaces SET surfaced = 0 WHERE surfaced = 1 AND id = ?`, id)
			if err != nil {
				return fmt.Errorf("wsm: end the surfacing of %s: %w", id, err)
			}
			ended, err = touchedRow(res)
			return err
		})
	return ended, err
}
