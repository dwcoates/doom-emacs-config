package rosterwalk

import (
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// row is a roster row for id with children.
func row(id string, children ...*frontendv1.RosterRow) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id}},
		Children:  children,
	}
}

// ids names the workspace ids of rows, in order.
func ids(rows []*frontendv1.RosterRow) []string {
	var out []string
	for _, r := range rows {
		out = append(out, r.GetWorkspace().GetWorkspace().GetId())
	}
	return out
}

// mergedOf is a recently-merged band of one group per rows region.
func mergedOf(groups ...*frontendv1.RosterRows) *frontendv1.RosterMergedSection {
	section := &frontendv1.RosterMergedSection{}
	for _, rows := range groups {
		section.Groups = append(section.Groups, &frontendv1.RosterMergedRepoGroup{Rows: rows})
	}
	return section
}

// rowsOf wraps rows in a rows region.
func rowsOf(rows ...*frontendv1.RosterRow) *frontendv1.RosterRows {
	return &frontendv1.RosterRows{Rows: rows}
}

func TestFlattenRows(t *testing.T) {
	cases := []struct {
		name string
		rows []*frontendv1.RosterRow
		want []string
	}{
		{name: "an empty region lists nothing", rows: nil, want: nil},
		{name: "flat rows keep their order", rows: []*frontendv1.RosterRow{row("a"), row("b")}, want: []string{"a", "b"}},
		{name: "a child follows its parent, before the parent's next sibling",
			rows: []*frontendv1.RosterRow{row("a", row("a1")), row("b")}, want: []string{"a", "a1", "b"}},
		{name: "a grandchild is listed at its depth",
			rows: []*frontendv1.RosterRow{row("a", row("a1", row("a1x")), row("a2"))}, want: []string{"a", "a1", "a1x", "a2"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ids(FlattenRows(tc.rows))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("FlattenRows = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestAllRows(t *testing.T) {
	cases := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{name: "a nil roster lists nothing", roster: nil, want: nil},
		{name: "repository sections, then hidden, then recently merged, in that order",
			roster: &frontendv1.WorkspaceRoster{
				RecentlyMerged: mergedOf(rowsOf(row("m"))),
				Hidden:         &frontendv1.RosterHiddenSection{Rows: rowsOf(row("h", row("h1")))},
				Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
					{Rows: rowsOf(row("r1"))}, {Rows: rowsOf(row("r2"))}}},
			},
			want: []string{"r1", "r2", "h", "h1", "m"}},
		{name: "nested rows are flattened in every section",
			roster: &frontendv1.WorkspaceRoster{
				Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
					{Rows: rowsOf(row("r", row("r1")))}}},
				RecentlyMerged: mergedOf(rowsOf(row("m", row("m1")))),
			},
			want: []string{"r", "r1", "m", "m1"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ids(AllRows(tc.roster))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("AllRows = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestMergedRows(t *testing.T) {
	collapsed := &frontendv1.RosterMergedRepoGroup{
		Rows:  rowsOf(row("c")),
		Drawn: &frontendv1.RosterMergedRepoGroup_RepositoryCollapsed{RepositoryCollapsed: &frontendv1.RosterMergedRepoGroupRepositoryCollapsed{}},
	}
	cases := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{name: "a nil roster lists nothing", roster: nil, want: nil},
		{name: "a band with no groups lists nothing",
			roster: &frontendv1.WorkspaceRoster{RecentlyMerged: &frontendv1.RosterMergedSection{}}, want: nil},
		{name: "groups are listed in the band's order",
			roster: &frontendv1.WorkspaceRoster{RecentlyMerged: mergedOf(rowsOf(row("a1"), row("a2")), rowsOf(row("b1")))},
			want:   []string{"a1", "a2", "b1"}},
		{name: "a collapsed repository's group is listed too",
			roster: &frontendv1.WorkspaceRoster{RecentlyMerged: &frontendv1.RosterMergedSection{
				Groups: []*frontendv1.RosterMergedRepoGroup{collapsed}}},
			want: []string{"c"}},
		{name: "nested rows in a group are flattened",
			roster: &frontendv1.WorkspaceRoster{RecentlyMerged: mergedOf(rowsOf(row("m", row("m1"))))},
			want:   []string{"m", "m1"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := ids(MergedRows(tc.roster))

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("MergedRows = %v, want %v", got, tc.want)
			}
		})
	}
}
