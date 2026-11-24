package wsm

import (
	"context"
)

// THE ONE-SHOTS SUBSECTION GATHERS THE ONE-SHOTS THE USER DISPATCHED (owner
// request, 2026-11-24). A top-level one-shot the user commissions from the
// editor (`SPC TAB o', `SPC TAB O', `SPC TAB M-o') is filed by the insert that
// registers it (RegisterPlacement.OneShots) into its repository's one-shots
// subsection, created by that same insert when the repository has none. So the
// one-shot is never drawn unsectioned, and its agent is never asked to file
// itself.
//
// THE DAEMON OWNS IT, so it records no creating workspace (CreatedBy stays
// nil) and is marked instead (Subsection.OneShots). Like an agent's subsection
// it lasts only while it has a constituent: the prune every write runs
// (agentsubsections.go) removes it once its last workspace closes, is killed,
// lands, or is dragged out.
//
// A REPOSITORY HOLDS AT MOST ONE, which the unique index enforces: two
// one-shots registered at once cannot each create their own, because the
// second insert's transaction finds the first's subsection or fails on the
// index.

// OneShotsSubsectionName is the one-shots subsection's name when the daemon
// creates it. The user may rename it; it stays the one-shots subsection.
const OneShotsSubsectionName = "One-shots"

// oneShotsSubsectionDDL is the layout-203 addition: the mark of the
// repository's one-shots subsection, at most one per repository. IT IS
// ADDITIVE: the previous build's INSERT names no such column, so a subsection
// it creates takes 0, which is all that build can create.
const oneShotsSubsectionDDL = `
ALTER TABLE subsections ADD COLUMN one_shots INTEGER NOT NULL DEFAULT 0;
CREATE UNIQUE INDEX subsections_one_shots ON subsections(repo_id) WHERE one_shots;
`

// oneShotsSubsection answers repository REPO's one-shots subsection, read
// through Q, and false when it has none.
func oneShotsSubsection(ctx context.Context, q querier, repo RepoID) (Subsection, bool, error) {
	subs, err := listRows(ctx, q, `SELECT `+subsectionColumns+` FROM subsections WHERE repo_id = ? AND one_shots`, scanSubsection, repo)
	if err != nil || len(subs) == 0 {
		return Subsection{}, false, err
	}
	return subs[0], true, nil
}
