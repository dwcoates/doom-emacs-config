package sidebar

import (
	"fmt"
	"sort"
	"time"

	"agent-repld/internal/wsm"
)

// sortWorkspaces puts workspaces in ROSTER ORDER, which every client — the
// Emacs tab bar included — follows strictly and never re-sorts: ascending
// STORED PLACE (wsm's place.go), unique across the registry, so no tie-break
// exists to disagree about.
//
// THE PLACE IS THE WORKSPACE'S OWN, NEVER THE SELECTION'S, AND NEVER ITS
// PRIORITY'S. Roster order once broke a priority tie on LAST SELECTED, and
// that made the drawn order move under the user: selecting a workspace stamped
// its last_selected_at, the registry re-pushed, and the row the user had just
// landed on hopped to the front of its priority band, so cycling right and
// then left did not return the user where they started. Priority now decides
// only where a workspace LANDS at create and re-open; after that only the
// user's own swaps move it.
//
// It sorts a copy: the slice belongs to the registry's caller.
func sortWorkspaces(in []wsm.Workspace) []wsm.Workspace {
	out := make([]wsm.Workspace, len(in))
	copy(out, in)
	sort.SliceStable(out, func(i, j int) bool { return lessWorkspace(out[i], out[j]) })
	return out
}

// sortMergedNewestFirst puts the recently-merged band's rows in MERGE ORDER,
// the latest merge first (owner request, 2026-10-13: the band reads as the
// sequence the merges landed in, newest on top). Roster order breaks a tie, so
// two merges stamped the same instant still draw in one order everywhere.
// Every row it is handed is merged; one with no merge instant is a defect, and
// panics. It sorts a copy, as sortWorkspaces does.
func sortMergedNewestFirst(in []wsm.Workspace) []wsm.Workspace {
	return sortNewestFirst(in, "recently-merged", func(ws wsm.Workspace) *time.Time { return ws.MergedAt })
}

// sortBackloggedNewestFirst puts the hidden band's top-level rows in HIDE ORDER,
// the latest hide first (owner request, 2026-10-13), roster order breaking a
// tie. Every row it is handed is hidden, and a hidden workspace always records
// when it was hidden (wsm), so one with no hide instant is a defect, and
// panics. It sorts a copy, as sortWorkspaces does.
func sortBackloggedNewestFirst(in []wsm.Workspace) []wsm.Workspace {
	return sortNewestFirst(in, "backlogged", func(ws wsm.Workspace) *time.Time { return ws.BackloggedAt })
}

// sortNewestFirst sorts a copy of IN by the instant AT names, the latest
// first, roster order breaking a tie. A row with no instant is a defect in
// the band named BAND, and panics: the bands it orders hold only rows that
// carry one.
func sortNewestFirst(in []wsm.Workspace, band string, at func(wsm.Workspace) *time.Time) []wsm.Workspace {
	out := make([]wsm.Workspace, len(in))
	copy(out, in)
	for _, ws := range out {
		if at(ws) == nil {
			panic(fmt.Sprintf("sidebar: %s row %s has no instant to order by", band, ws.ID))
		}
	}
	sort.SliceStable(out, func(i, j int) bool {
		a, b := *at(out[i]), *at(out[j])
		if !a.Equal(b) {
			return a.After(b)
		}
		return lessWorkspace(out[i], out[j])
	})
	return out
}

// lessWorkspace is roster order's comparison, stated once so sections and
// nested children cannot order themselves differently.
func lessWorkspace(a, b wsm.Workspace) bool {
	return a.Place < b.Place
}
