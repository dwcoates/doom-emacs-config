package sidebar

import (
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// tabRow is a roster row for id, closed or not, with children.
func tabRow(id string, closed bool, children ...*frontendv1.RosterRow) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id}},
		Name:      &frontendv1.RosterRowName{Text: "name-" + id},
		Closed:    &frontendv1.RosterRowClosed{Closed: closed},
		Children:  children,
	}
}

// tabIDs is the order's workspace ids.
func tabIDs(order []TabEntry) []string {
	out := []string{}
	for _, e := range order {
		out = append(out, e.Ref.GetId())
	}
	return out
}

func TestTabOrder(t *testing.T) {
	section := func(rows ...*frontendv1.RosterRow) *frontendv1.RosterRepoSection {
		return &frontendv1.RosterRepoSection{Rows: &frontendv1.RosterRows{Rows: rows}}
	}
	tests := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{name: "no roster has no order", roster: nil, want: []string{}},
		{name: "sections in order, rows in order", roster: &frontendv1.WorkspaceRoster{
			Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
				section(tabRow("a", false), tabRow("b", false)), section(tabRow("c", false))}},
		}, want: []string{"a", "b", "c"}},
		{name: "children follow their parent depth-first", roster: &frontendv1.WorkspaceRoster{
			Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
				section(tabRow("a", false, tabRow("a1", false, tabRow("a11", false))), tabRow("b", false))}},
		}, want: []string{"a", "a1", "a11", "b"}},
		{name: "a closed row takes no tab but its open children do", roster: &frontendv1.WorkspaceRoster{
			Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
				section(tabRow("a", true, tabRow("a1", false)), tabRow("b", false))}},
		}, want: []string{"a1", "b"}},
		{name: "recently merged comes last", roster: &frontendv1.WorkspaceRoster{
			Repository:     &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{section(tabRow("a", false))}},
			RecentlyMerged: &frontendv1.RosterMergedSection{Rows: &frontendv1.RosterRows{Rows: []*frontendv1.RosterRow{tabRow("m", false), tabRow("gone", true)}}},
		}, want: []string{"a", "m"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tabIDs(TabOrder(tt.roster))

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("TabOrder = %v, want %v", got, tt.want)
			}
		})
	}
}
