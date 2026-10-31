package wsm

import (
	"context"
	"database/sql"
	"fmt"

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

// expandSubsection expands, within TX, subsection ID when it is collapsed,
// answering whether it was. It is THE ONE EXPANSION of a subsection the
// daemon makes on its own: a workspace placed in a subsection
// (placeFamilies, MoveWorkspace) and a workspace starting work or being
// selected (expandSubsectionHolding) all come here, so a workspace is never
// put or left hidden inside a collapsed subsection.
func expandSubsection(ctx context.Context, tx *sql.Tx, id SubsectionID) (bool, error) {
	res, err := tx.ExecContext(ctx, `UPDATE subsections SET folded = 0 WHERE folded = 1 AND id = ?`, id)
	if err != nil {
		return false, fmt.Errorf("wsm: expand subsection %s: %w", id, err)
	}
	expanded, err := touchedRow(res)
	if err != nil {
		return false, fmt.Errorf("wsm: expand subsection %s: %w", id, err)
	}
	return expanded, nil
}
