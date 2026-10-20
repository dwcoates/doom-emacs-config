// Package rosterwalk is THE ONE WALK over a frontend.v1.WorkspaceRoster's
// rows. A child workspace's row nests under its parent's in its list's own
// row type (RosterOpenRow.children, RosterHiddenRow.children), so a reader
// that hand-rolls its own loop can miss every child while the others see
// them -- the merge-queue verb once refused a child's own merge that way.
// Every Go reader of the roster, in every module, walks through here.
package rosterwalk

import (
	"fmt"

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

// RowEntries wraps ROWS, top-level open rows in no subsection, as repository
// section entries, one each, in order: the one spelling of a row standing in
// a section's body (frontend.v1.RosterRepoEntry.row).
func RowEntries(rows []*frontendv1.RosterOpenRow) []*frontendv1.RosterRepoEntry {
	out := make([]*frontendv1.RosterRepoEntry, 0, len(rows))
	for _, row := range rows {
		out = append(out, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: row}})
	}
	return out
}

// UnsectionedRows lists a repository section's top-level open rows in no
// subsection, in drawn order, each with its family nested under it.
func UnsectionedRows(section *frontendv1.RosterRepoSection) []*frontendv1.RosterOpenRow {
	var out []*frontendv1.RosterOpenRow
	for _, entry := range section.GetEntries() {
		if row := entry.GetRow(); row != nil {
			out = append(out, row)
		}
	}
	return out
}

// Subsections lists a repository section's subsections in drawn order.
func Subsections(section *frontendv1.RosterRepoSection) []*frontendv1.RosterSubsection {
	var out []*frontendv1.RosterSubsection
	for _, entry := range section.GetEntries() {
		if sub := entry.GetSubsection(); sub != nil {
			out = append(out, sub)
		}
	}
	return out
}

// SectionOpenRows lists every open row of one repository section in drawn
// order: entry by entry (frontend.v1.RosterRepoSection.entries), a row in no
// subsection with its family, a subsection's rows where the subsection
// stands, each flattened. A reader that walks only the rows in no subsection
// misses every workspace in a subsection.
func SectionOpenRows(section *frontendv1.RosterRepoSection) []*frontendv1.RosterOpenRow {
	var out []*frontendv1.RosterOpenRow
	for _, entry := range section.GetEntries() {
		switch e := entry.GetEntry().(type) {
		case *frontendv1.RosterRepoEntry_Row:
			out = append(out, FlattenOpen([]*frontendv1.RosterOpenRow{e.Row})...)
		case *frontendv1.RosterRepoEntry_Subsection:
			out = append(out, FlattenOpen(e.Subsection.GetOpenRows().GetRows())...)
		default:
			panic(fmt.Sprintf("rosterwalk: a repository section entry has no arm (%T)", e))
		}
	}
	return out
}

// OpenRows lists every open row of the roster in the Open section's walk --
// repository by repository, each one's entries in drawn order, every
// family under its parent. It is the tab bar's order, folded tabs
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
