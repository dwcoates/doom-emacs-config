// Package rosterwalk is THE ONE WALK over a frontend.v1.WorkspaceRoster's
// rows. A child workspace's row nests under its parent's in its list's own
// row type (RosterOpenRow.children, RosterHiddenRow.children), so a reader
// that hand-rolls its own loop can miss every child while the others see
// them -- the merge-queue verb once refused a child's own merge that way.
// Every Go reader of the roster, in every module, walks through here.
package rosterwalk

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// FlattenOpen lists every open row of a region depth-first, each row before
// its nested family. It is the Open section's drawn order, and so the tab
// bar's.
func FlattenOpen(rows []*frontendv1.RosterOpenRow) []*frontendv1.RosterOpenRow {
	var out []*frontendv1.RosterOpenRow
	for _, row := range rows {
		out = append(out, row)
		out = append(out, FlattenOpen(row.GetChildren())...)
	}
	return out
}

// FlattenHidden lists every hidden row of the hidden section's rows
// depth-first, each row before its nested family.
func FlattenHidden(rows []*frontendv1.RosterHiddenRow) []*frontendv1.RosterHiddenRow {
	var out []*frontendv1.RosterHiddenRow
	for _, row := range rows {
		out = append(out, row)
		out = append(out, FlattenHidden(row.GetChildren())...)
	}
	return out
}

// RepoSectionRegions lists a repository section's open rows regions in drawn
// order: its unsectioned rows, then each subsection's. A reader that walks
// only the section's own `open_rows` misses every workspace in a subsection.
func RepoSectionRegions(section *frontendv1.RosterRepoSection) []*frontendv1.RosterOpenRows {
	out := []*frontendv1.RosterOpenRows{section.GetOpenRows()}
	for _, sub := range section.GetSubsections() {
		out = append(out, sub.GetOpenRows())
	}
	return out
}

// SectionOpenRows lists every open row of one repository section, its own
// rows and then each subsection's, each flattened.
func SectionOpenRows(section *frontendv1.RosterRepoSection) []*frontendv1.RosterOpenRow {
	var out []*frontendv1.RosterOpenRow
	for _, region := range RepoSectionRegions(section) {
		out = append(out, FlattenOpen(region.GetRows())...)
	}
	return out
}

// OpenRows lists every open row of the roster in the Open section's walk --
// repository by repository, each one's own rows then each subsection's,
// every family under its parent. It is the tab bar's order, folded tabs
// included.
func OpenRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterOpenRow {
	var out []*frontendv1.RosterOpenRow
	for _, section := range roster.GetRepository().GetSections() {
		out = append(out, SectionOpenRows(section)...)
	}
	return out
}

// HiddenRows lists every row of the hidden section, flattened.
func HiddenRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, hidden := range FlattenHidden(roster.GetHidden().GetHiddenRows().GetRows()) {
		out = append(out, hidden.GetRow())
	}
	return out
}

// MergedRows lists every row of the recently-merged band, group by group in
// the band's order -- a collapsed repository's group included, because the
// band carries it for every reader but the sidebar's drawing.
func MergedRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, group := range roster.GetRecentlyMerged().GetGroups() {
		for _, merged := range group.GetMergedRows().GetRows() {
			out = append(out, merged.GetRow())
		}
	}
	return out
}

// ClosedRows lists every row of the roster's closed list.
func ClosedRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, closed := range roster.GetClosed().GetRows() {
		out = append(out, closed.GetRow())
	}
	return out
}

// AllRows lists every row the roster carries -- the Open section's walk, the
// hidden section, the recently-merged section, then the closed list. Each
// workspace appears once.
func AllRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, open := range OpenRows(roster) {
		out = append(out, open.GetRow())
	}
	out = append(out, HiddenRows(roster)...)
	out = append(out, MergedRows(roster)...)
	return append(out, ClosedRows(roster)...)
}
