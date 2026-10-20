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

// section is a repository section with its own rows and subsections' rows.
func section(own []*frontendv1.RosterOpenRow, subs ...[]*frontendv1.RosterOpenRow) *frontendv1.RosterRepoSection {
	s := &frontendv1.RosterRepoSection{OpenRows: &frontendv1.RosterOpenRows{Rows: own}}
	for _, rows := range subs {
		s.Subsections = append(s.Subsections, &frontendv1.RosterSubsection{OpenRows: &frontendv1.RosterOpenRows{Rows: rows}})
	}
	return s
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

func TestSectionOpenRowsListsTheOwnRowsThenEachSubsections(t *testing.T) {
	// Arrange.
	s := section([]*frontendv1.RosterOpenRow{open("a", open("a1"))}, []*frontendv1.RosterOpenRow{open("s")}, nil)

	// Act / Assert.
	if got, want := openIDs(SectionOpenRows(s)), []string{"a", "a1", "s"}; !slices.Equal(got, want) {
		t.Fatalf("SectionOpenRows = %v, want %v", got, want)
	}
}

func TestOpenRowsWalksTheSectionsInOrder(t *testing.T) {
	// Arrange.
	roster := &frontendv1.WorkspaceRoster{Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
		section([]*frontendv1.RosterOpenRow{open("a")}, []*frontendv1.RosterOpenRow{open("b")}),
		section([]*frontendv1.RosterOpenRow{open("c")}),
	}}}

	// Act / Assert.
	if got, want := openIDs(OpenRows(roster)), []string{"a", "b", "c"}; !slices.Equal(got, want) {
		t.Fatalf("OpenRows = %v, want %v", got, want)
	}
}

func TestAllRowsListsOpenThenHiddenThenMergedThenClosed(t *testing.T) {
	// Arrange.
	roster := &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{section([]*frontendv1.RosterOpenRow{open("o", open("o1"))})}},
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
