package wsm

import (
	"context"
	"database/sql"

	"agent-repld/internal/dlog"
)

// subsectionFoldsDDL is the layout-59 addition: whether a subsection of a
// repository's section is folded (frontend.v1.RosterSubsection.fold).
//
// IT IS ADDITIVE: the previous build names no such column, so a subsection it
// creates takes 0, expanded, which is what every subsection drew as before.
const subsectionFoldsDDL = `
ALTER TABLE subsections ADD COLUMN folded INTEGER NOT NULL DEFAULT 0;
`

// SetSubsectionFolded records whether subsection ID is folded. A folded
// subsection's rows are hidden in the sidebar and its workspaces' tabs are
// folded off the tab bar, as a folded repository's are (resolve/sidebar's
// openTree). An unknown subsection is refused (UnknownSubsectionError, which
// is ErrNotFound), never a write that touched nothing.
func (s *store) SetSubsectionFolded(ctx context.Context, id SubsectionID, folded bool) error {
	return s.write(ctx, "daemon.wsm.set_subsection_folded", dlog.Context{"subsection": string(id), "folded": folded},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE subsections SET folded = ? WHERE id = ?`, folded, id)
			if err != nil {
				return err
			}
			return requireSubsectionRow(res, id)
		})
}
