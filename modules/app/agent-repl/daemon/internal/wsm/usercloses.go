package wsm

import (
	"context"
	"database/sql"
)

// THE USER'S CLOSE OF A SUBSECTION'S LAST OPEN WORKSPACE REMOVES THE
// SUBSECTION, WHOEVER CREATED IT (owner request, 2026-11-18). Closing,
// killing or nuking a workspace is the user saying its work is done, and a
// subsection left with nothing open in it is clutter the user then deleted by
// hand (2026-11-18: `GR Monthly Limit`, a subsection with no recorded
// creator, deleted four seconds after its last workspace was killed). An
// agent-created subsection is removed by every write that empties it
// (agentsubsections.go), the user's close among them, and that prune stays its
// one remover; this one rule extends the removal to the user's subsections,
// for the user's close alone. A user's subsection emptied any
// other way (a drag out, an agent's or a merge's close) still survives empty.
//
// THE REMOVAL RIDES THE ONE CLOSE WRITE (markClosedInTx), so a close, a kill
// and a nuke, which all record a CloseByUser close through it, remove the
// subsection in the same transaction that closes its last workspace.

// opRemoveUserClosedSubsection records each subsection a user's close left
// empty and so removed.
const opRemoveUserClosedSubsection = "daemon.wsm.remove_user_closed_subsection"

// removeSubsectionUserCloseEmptied removes, inside TX, the user-created
// subsection workspace ID sits in (its root's, for a child) when no open
// workspace is left in it, and answers it; nil when ID sits in no subsection,
// in an agent's (the write's prune removes that one), or in one that still has
// an open workspace. ID's close is already written.
func removeSubsectionUserCloseEmptied(ctx context.Context, tx *sql.Tx, id WorkspaceID) (*Subsection, error) {
	var sub sql.NullString
	if err := scanWorkspaceRow(ctx, tx, id, rootSubsectionColumn, &sub); err != nil {
		return nil, err
	}
	if !sub.Valid {
		return nil, nil
	}
	empty, err := listRows(ctx, tx, emptyUserSubsectionsQuery+` AND id = ?`, scanSubsection, sub.String)
	if err != nil || len(empty) == 0 {
		return nil, err
	}
	if err := deleteSubsection(ctx, tx, empty[0].ID); err != nil {
		return nil, err
	}
	return &empty[0], nil
}
