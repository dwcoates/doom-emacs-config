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
