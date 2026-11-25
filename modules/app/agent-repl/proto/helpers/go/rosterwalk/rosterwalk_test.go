package rosterwalk

import (
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// row is a shared roster row for id.
func row(id string) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id}}}
}

// open is an open row for id with children.
func open(id string, children ...*frontendv1.RosterOpenRow) *frontendv1.RosterOpenRow {
	return &frontendv1.RosterOpenRow{Row: row(id), Children: children}
}

// withBlocks is ROW with one repository block per entry of BLOCKS, each the
// rows given, in order.
func withBlocks(row *frontendv1.RosterOpenRow, blocks ...[]*frontendv1.RosterOpenRow) *frontendv1.RosterOpenRow {
	for _, rows := range blocks {
		row.RepositoryBlocks = append(row.RepositoryBlocks, &frontendv1.RosterOpenRowRepositoryBlock{Rows: rows})
	}
	return row
}

// backlogRow is a backlogRow row for id with children.
func backlogRow(id string, children ...*frontendv1.RosterBacklogRow) *frontendv1.RosterBacklogRow {
	return &frontendv1.RosterBacklogRow{Row: row(id), Children: children}
}

// ids names the workspace ids of rows, in order.
func ids(rows []*frontendv1.RosterRow) []string {
	var out []string
	for _, r := range rows {
		out = append(out, r.GetWorkspace().GetWorkspace().GetId())
	}
	return out
}

// openIDs names the workspace ids of open rows, in order.
func openIDs(rows []*frontendv1.RosterOpenRow) []string {
	var out []string
	for _, r := range rows {
		out = append(out, r.GetRow().GetWorkspace().GetWorkspace().GetId())
	}
	return out
}

// entry is one section entry: a row in no subsection.
func entry(row *frontendv1.RosterOpenRow) *frontendv1.RosterRepoEntry {
	return RowEntries([]*frontendv1.RosterOpenRow{row})[0]
}

// sub is one section entry: a subsection holding rows.
func sub(rows ...*frontendv1.RosterOpenRow) *frontendv1.RosterRepoEntry {
	return &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Subsection{
		Subsection: &frontendv1.RosterSubsection{OpenRows: &frontendv1.RosterOpenRows{Rows: rows}}}}
}

// section is a repository section of entries, in drawn order.
func section(entries ...*frontendv1.RosterRepoEntry) *frontendv1.RosterRepoSection {
	return &frontendv1.RosterRepoSection{Entries: entries}
}

func TestFlattenOpen(t *testing.T) {
	cases := []struct {
		name string
		rows []*frontendv1.RosterOpenRow
		want []string
	}{
		{name: "an empty region lists nothing", rows: nil, want: nil},
		{name: "flat rows keep their order", rows: []*frontendv1.RosterOpenRow{open("a"), open("b")}, want: []string{"a", "b"}},
		{name: "a child follows its parent, before the parent's next sibling",
			rows: []*frontendv1.RosterOpenRow{open("a", open("a1")), open("b")}, want: []string{"a", "a1", "b"}},
		{name: "a grandchild is listed at its depth",
			rows: []*frontendv1.RosterOpenRow{open("a", open("a1", open("a1x")), open("a2"))}, want: []string{"a", "a1", "a1x", "a2"}},
		{name: "a block's rows follow the own-repository children, block by block",
			rows: []*frontendv1.RosterOpenRow{withBlocks(open("a", open("a1")),
				[]*frontendv1.RosterOpenRow{open("x1"), open("x2")}, []*frontendv1.RosterOpenRow{open("y1")}), open("b")},
			want: []string{"a", "a1", "x1", "x2", "y1", "b"}},
		{name: "a block row's own family is listed under it",
			rows: []*frontendv1.RosterOpenRow{withBlocks(open("a"), []*frontendv1.RosterOpenRow{open("x1", open("x1c"))})},
			want: []string{"a", "x1", "x1c"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := openIDs(FlattenOpen(tc.rows))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("FlattenOpen = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestOpenFamilyListsChildrenThenEveryBlocksRows(t *testing.T) {
	// Arrange.
	parent := withBlocks(open("p", open("own")), []*frontendv1.RosterOpenRow{open("x")}, []*frontendv1.RosterOpenRow{open("y")})

	// Act.
	got := openIDs(OpenFamily(parent))

	// Assert.
	if want := []string{"own", "x", "y"}; !slices.Equal(got, want) {
		t.Fatalf("OpenFamily = %v, want %v", got, want)
	}
}

func TestFlattenBacklogListsAChildAfterItsParent(t *testing.T) {
	// Act.
	got := FlattenBacklog([]*frontendv1.RosterBacklogRow{backlogRow("h", backlogRow("h1")), backlogRow("g")})

	// Assert.
	var names []string
	for _, h := range got {
		names = append(names, h.GetRow().GetWorkspace().GetWorkspace().GetId())
	}
	if want := []string{"h", "h1", "g"}; !slices.Equal(names, want) {
		t.Fatalf("FlattenBacklog = %v, want %v", names, want)
	}
}

func TestSectionOpenRows(t *testing.T) {
	cases := []struct {
		name    string
		section *frontendv1.RosterRepoSection
		want    []string
	}{
		{name: "an empty section lists nothing", section: section(), want: nil},
		{name: "a row's family follows it", section: section(entry(open("a", open("a1")))), want: []string{"a", "a1"}},
		{name: "a subsection's rows are listed where it stands, between two rows",
			section: section(entry(open("a")), sub(open("s", open("s1"))), entry(open("b"))), want: []string{"a", "s", "s1", "b"}},
		{name: "an empty subsection lists nothing", section: section(entry(open("a")), sub(), entry(open("b"))), want: []string{"a", "b"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := openIDs(SectionOpenRows(tc.section))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("SectionOpenRows = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRowEntriesWrapsEachRowInOrder(t *testing.T) {
	// Arrange.
	a, b := open("a"), open("b")

	// Act.
	got := RowEntries([]*frontendv1.RosterOpenRow{a, b})

	// Assert.
	if len(got) != 2 || got[0].GetRow() != a || got[1].GetRow() != b {
		t.Fatalf("RowEntries = %v, want a then b, each on the row arm", got)
	}
}

func TestRowEntriesOfNoRowsIsEmpty(t *testing.T) {
	// Act.
	got := RowEntries(nil)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("RowEntries(nil) = %v, want none", got)
	}
}

func TestUnsectionedRowsListsTheRowEntriesInOrder(t *testing.T) {
	// Arrange.
	s := section(entry(open("a", open("a1"))), sub(open("s")), entry(open("b")))

	// Act / Assert.
	if got, want := openIDs(UnsectionedRows(s)), []string{"a", "b"}; !slices.Equal(got, want) {
		t.Fatalf("UnsectionedRows = %v, want %v", got, want)
	}
}

func TestSubsectionsListsTheSubsectionEntriesInOrder(t *testing.T) {
	// Arrange.
	first, second := sub(open("s")), sub()
	s := section(entry(open("a")), first, entry(open("b")), second)

	// Act.
	got := Subsections(s)

	// Assert.
	if len(got) != 2 || got[0] != first.GetSubsection() || got[1] != second.GetSubsection() {
		t.Fatalf("Subsections = %v, want the two subsections in order", got)
	}
}

func TestSectionOpenRowsRefusesAnEntryWithNoArm(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatalf("SectionOpenRows walked an entry with no arm")
		}
	}()

	// Act.
	SectionOpenRows(section(&frontendv1.RosterRepoEntry{}))
}

func TestOpenRowsWalksTheSectionsInOrder(t *testing.T) {
	// Arrange.
	roster := &frontendv1.WorkspaceRoster{Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
		section(entry(open("a")), sub(open("b"))),
		section(entry(open("c"))),
	}}}

	// Act / Assert.
	if got, want := openIDs(OpenRows(roster)), []string{"a", "b", "c"}; !slices.Equal(got, want) {
		t.Fatalf("OpenRows = %v, want %v", got, want)
	}
}

func TestAllRows(t *testing.T) {
	tests := []struct {
		name string
		push *frontendv1.RosterPush
		want []string
	}{
		{
			name: "open then backlogged then closed, each family under its parent",
			push: &frontendv1.RosterPush{
				Workspaces: &frontendv1.WorkspaceRoster{
					Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{section(entry(open("o", open("o1"))))}},
					Backlog:    &frontendv1.RosterBacklogSection{BacklogRows: &frontendv1.RosterBacklogRows{Rows: []*frontendv1.RosterBacklogRow{backlogRow("h", backlogRow("h1"))}}},
				},
				Closed: &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{Row: row("c")}}},
			},
			want: []string{"o", "o1", "h", "h1", "c"},
		},
		{
			name: "a push with no closed stream lists the workspaces stream alone",
			push: &frontendv1.RosterPush{Workspaces: &frontendv1.WorkspaceRoster{
				Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{section(entry(open("o")))}},
			}},
			want: []string{"o"},
		},
		{
			name: "a push with no workspaces stream lists the closed stream alone",
			push: &frontendv1.RosterPush{Closed: &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{Row: row("c")}}}},
			want: []string{"c"},
		},
		{
			name: "the merged summary contributes no row",
			push: &frontendv1.RosterPush{RecentlyMerged: &frontendv1.RosterMergedSection{Groups: []*frontendv1.RosterMergedRepoGroup{{}}}},
			want: nil,
		},
		{name: "an empty push lists none", push: &frontendv1.RosterPush{}, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := ids(AllRows(tt.push))

			// Assert.
			if !slices.Equal(got, tt.want) {
				t.Fatalf("AllRows = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestClosedRowsListsTheClosedStreamInOrder(t *testing.T) {
	// Arrange.
	closed := &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{Row: row("a")}, {Row: row("b")}}}

	// Act / Assert.
	if got, want := ids(ClosedRows(closed)), []string{"a", "b"}; !slices.Equal(got, want) {
		t.Fatalf("ClosedRows = %v, want %v", got, want)
	}
}

func TestHold(t *testing.T) {
	open := &frontendv1.WorkspaceRoster{Open: &frontendv1.RosterOpenSection{}}
	reopen := &frontendv1.WorkspaceRoster{Backlog: &frontendv1.RosterBacklogSection{}}
	queue := &frontendv1.RosterMergeQueueSection{}
	merged := &frontendv1.RosterMergedSection{}
	closed := &frontendv1.RosterClosedRows{}
	full := &frontendv1.RosterPush{Revision: 3, Workspaces: open, MergeQueue: queue, RecentlyMerged: merged, Closed: closed}
	tests := []struct {
		name string
		held *frontendv1.RosterPush
		push *frontendv1.RosterPush
		want *frontendv1.RosterPush
	}{
		{"nothing held takes every stream of the first push", nil, full, full},
		{"a partial push replaces only what it carries", full,
			&frontendv1.RosterPush{Revision: 4, Workspaces: reopen},
			&frontendv1.RosterPush{Revision: 4, Workspaces: reopen, MergeQueue: queue, RecentlyMerged: merged, Closed: closed}},
		{"a push with no stream keeps every stream and takes its revision", full,
			&frontendv1.RosterPush{Revision: 5},
			&frontendv1.RosterPush{Revision: 5, Workspaces: open, MergeQueue: queue, RecentlyMerged: merged, Closed: closed}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Hold(tt.held, tt.push)

			// Assert
			if got.Revision != tt.want.Revision || got.Workspaces != tt.want.Workspaces || got.MergeQueue != tt.want.MergeQueue ||
				got.RecentlyMerged != tt.want.RecentlyMerged || got.Closed != tt.want.Closed {
				t.Fatalf("Hold = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestHoldLeavesWhatWasHeldUnchanged(t *testing.T) {
	// Arrange
	before := &frontendv1.WorkspaceRoster{}
	held := &frontendv1.RosterPush{Revision: 1, Workspaces: before}

	// Act
	Hold(held, &frontendv1.RosterPush{Revision: 2, Workspaces: &frontendv1.WorkspaceRoster{}})

	// Assert
	if held.Revision != 1 || held.Workspaces != before {
		t.Fatalf("Hold changed what was held: %v", held)
	}
}
