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

// hidden is a hidden row for id with children.
func hidden(id string, children ...*frontendv1.RosterHiddenRow) *frontendv1.RosterHiddenRow {
	return &frontendv1.RosterHiddenRow{Row: row(id), Children: children}
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

func TestFlattenHiddenListsAChildAfterItsParent(t *testing.T) {
	// Act.
	got := FlattenHidden([]*frontendv1.RosterHiddenRow{hidden("h", hidden("h1")), hidden("g")})

	// Assert.
	var names []string
	for _, h := range got {
		names = append(names, h.GetRow().GetWorkspace().GetWorkspace().GetId())
	}
	if want := []string{"h", "h1", "g"}; !slices.Equal(names, want) {
		t.Fatalf("FlattenHidden = %v, want %v", names, want)
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

func TestAllRowsListsOpenThenHiddenThenMergedThenClosed(t *testing.T) {
	// Arrange.
	roster := &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{section(entry(open("o", open("o1"))))}},
		Hidden:     &frontendv1.RosterHiddenSection{HiddenRows: &frontendv1.RosterHiddenRows{Rows: []*frontendv1.RosterHiddenRow{hidden("h", hidden("h1"))}}},
		RecentlyMerged: &frontendv1.RosterMergedSection{Groups: []*frontendv1.RosterMergedRepoGroup{
			{MergedRows: &frontendv1.RosterMergedRows{Rows: []*frontendv1.RosterMergedRow{{Row: row("m")}}}},
			{MergedRows: &frontendv1.RosterMergedRows{Rows: []*frontendv1.RosterMergedRow{{Row: row("n")}}}},
		}},
		Closed: &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{Row: row("c")}}},
	}

	// Act / Assert.
	if got, want := ids(AllRows(roster)), []string{"o", "o1", "h", "h1", "m", "n", "c"}; !slices.Equal(got, want) {
		t.Fatalf("AllRows = %v, want %v", got, want)
	}
}

func TestAllRowsOfAnEmptyRosterIsEmpty(t *testing.T) {
	// Act / Assert.
	if got := AllRows(&frontendv1.WorkspaceRoster{}); len(got) != 0 {
		t.Fatalf("AllRows = %v, want none", ids(got))
	}
}
