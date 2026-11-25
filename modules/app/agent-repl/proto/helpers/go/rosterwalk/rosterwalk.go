// Package rosterwalk is THE ONE WALK over the rows of a frontend.v1.RosterPush's
// streams. A child workspace's row nests under its parent's in its list's own
// row type (RosterOpenRow.children, RosterBacklogRow.children), so a reader
// that hand-rolls its own loop can miss every child while the others see
// them -- the merge-queue verb once refused a child's own merge that way.
// Every Go reader of the roster, in every module, walks through here.
package rosterwalk

import (
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// FlattenOpen lists every open row of a region depth-first, each row before
// its nested family (OpenFamily). It is the Open section's drawn order, and
// so the tab bar's.
func FlattenOpen(rows []*frontendv1.RosterOpenRow) []*frontendv1.RosterOpenRow {
	var out []*frontendv1.RosterOpenRow
	for _, row := range rows {
		out = append(out, row)
		out = append(out, FlattenOpen(OpenFamily(row))...)
	}
	return out
}

// OpenFamily lists ROW's open children in drawn order: its own-repository
// children, then each repository block's rows, block by block
// (frontend.v1.RosterOpenRow.repository_blocks). A reader that took only
// `children` would miss every child of another repository.
func OpenFamily(row *frontendv1.RosterOpenRow) []*frontendv1.RosterOpenRow {
	out := append([]*frontendv1.RosterOpenRow(nil), row.GetChildren()...)
	for _, block := range row.GetRepositoryBlocks() {
		out = append(out, block.GetRows()...)
	}
	return out
}

// FlattenBacklog lists every hidden row of the hidden section's rows
// depth-first, each row before its nested family.
func FlattenBacklog(rows []*frontendv1.RosterBacklogRow) []*frontendv1.RosterBacklogRow {
	var out []*frontendv1.RosterBacklogRow
	for _, row := range rows {
		out = append(out, row)
		out = append(out, FlattenBacklog(row.GetChildren())...)
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

// BacklogRows lists every row of the hidden section, flattened.
func BacklogRows(roster *frontendv1.WorkspaceRoster) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, backlog := range FlattenBacklog(roster.GetBacklog().GetBacklogRows().GetRows()) {
		out = append(out, backlog.GetRow())
	}
	return out
}

// ClosedRows lists every row of the closed stream's snapshot
// (frontend.v1.RosterPush.closed).
func ClosedRows(closed *frontendv1.RosterClosedRows) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, row := range closed.GetRows() {
		out = append(out, row.GetRow())
	}
	return out
}

// AllRows lists every row a push's streams carry -- the workspaces stream's
// Open section walk and hidden section, then the closed stream. Each
// workspace appears once. A MERGED WORKSPACE IS IN NO STREAM
// (agentrepl.v1.ListMergedWorkspaces pages its row), so none is listed. The
// push must hold every stream: a subscription's first push does, and so does
// every value the daemon's topic carries; a later wire push leaves out the
// streams it did not change.
func AllRows(push *frontendv1.RosterPush) []*frontendv1.RosterRow {
	var out []*frontendv1.RosterRow
	for _, open := range OpenRows(push.GetWorkspaces()) {
		out = append(out, open.GetRow())
	}
	out = append(out, BacklogRows(push.GetWorkspaces())...)
	return append(out, ClosedRows(push.GetClosed())...)
}

// Hold answers the roster HELD with PUSH applied: PUSH's revision, and each
// stream PUSH carries in place of the one held, every stream it leaves out
// kept as held (frontend.v1.RosterPush: an absent stream is unchanged, never
// empty). HELD is not changed, so a value taken from an earlier push stays as
// it was. A nil HELD holds nothing yet. It is the ONE place a Go reader of
// the roster stream folds a push into what it holds.
func Hold(held, push *frontendv1.RosterPush) *frontendv1.RosterPush {
	out := &frontendv1.RosterPush{
		Revision:       push.GetRevision(),
		Workspaces:     held.GetWorkspaces(),
		MergeQueue:     held.GetMergeQueue(),
		RecentlyMerged: held.GetRecentlyMerged(),
		Closed:         held.GetClosed(),
	}
	if w := push.GetWorkspaces(); w != nil {
		out.Workspaces = w
	}
	if q := push.GetMergeQueue(); q != nil {
		out.MergeQueue = q
	}
	if m := push.GetRecentlyMerged(); m != nil {
		out.RecentlyMerged = m
	}
	if c := push.GetClosed(); c != nil {
		out.Closed = c
	}
	return out
}
