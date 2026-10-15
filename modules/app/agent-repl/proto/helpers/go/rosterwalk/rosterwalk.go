// Package rosterwalk is THE ONE WALK over a frontend.v1.WorkspaceRoster's
// rows. A child workspace's row nests under its parent's
// (RosterRow.children), so a reader that hand-rolls its own loop can miss
// every child while the others see them -- the merge-queue verb once refused
// a child's own merge that way. Every Go reader of the roster, in every
// module, flattens through here.
package rosterwalk

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// FlattenRows lists every row of a rows region depth-first, each row before
// its nested family rows.
func FlattenRows(rows []*frontendv1.RosterRow) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, row := range rows {
		out = append(out, row)
		out = append(out, FlattenRows(row.GetChildren())...)
	}
	return out
}

// AllRows lists every row the roster carries -- the repository sections,
// the hidden section, then the recently-merged section -- each flattened.
// Each workspace appears once.
func AllRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, section := range roster.GetRepository().GetSections() {
		out = append(out, FlattenRows(section.GetRows().GetRows())...)
	}
	out = append(out, FlattenRows(roster.GetHidden().GetRows().GetRows())...)
	return append(out, MergedRows(roster)...)
}

// MergedRows lists every row of the recently-merged band, group by group in
// the band's order, each flattened -- a collapsed repository's group included,
// because the band carries it for every reader but the sidebar's drawing.
func MergedRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, group := range roster.GetRecentlyMerged().GetGroups() {
		out = append(out, FlattenRows(group.GetRows().GetRows())...)
	}
	return out
}
