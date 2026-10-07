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

// AllRows lists every row of every grouping the roster carries -- the
// repository sections, the task sections, then the recently-merged section --
// each flattened. The task view regroups the same workspaces the repository
// view holds, so a workspace can appear more than once.
func AllRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, section := range roster.GetRepository().GetSections() {
		out = append(out, FlattenRows(section.GetRows().GetRows())...)
	}
	for _, section := range roster.GetTask().GetSections() {
		out = append(out, FlattenRows(section.GetRows().GetRows())...)
	}
	return append(out, FlattenRows(roster.GetRecentlyMerged().GetRows().GetRows())...)
}
