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
	tab := func(id string) *frontendv1.RosterTab {
		return &frontendv1.RosterTab{Workspace: &workspacev1.WorkspaceRef{Id: id}}
	}
	tests := []struct {
		name      string
		roster    *frontendv1.WorkspaceRoster
		wantIDs   []string
		wantNames []string
	}{
		{name: "no roster has no order", roster: nil, wantIDs: []string{}, wantNames: []string{}},
		{name: "the published tab bar's order, not the sections' walk", roster: &frontendv1.WorkspaceRoster{
			Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{
				{Rows: &frontendv1.RosterRows{Rows: []*frontendv1.RosterRow{tabRow("a", false, tabRow("a1", false)), tabRow("b", false)}}}}},
			TabBar: &frontendv1.RosterTabBar{Tabs: []*frontendv1.RosterTab{tab("b"), tab("a1"), tab("a")}},
		}, wantIDs: []string{"b", "a1", "a"}, wantNames: []string{"name-b", "name-a1", "name-a"}},
		{name: "a tab is named from its row in any grouping", roster: &frontendv1.WorkspaceRoster{
			Hidden:         &frontendv1.RosterHiddenSection{Rows: &frontendv1.RosterRows{Rows: []*frontendv1.RosterRow{tabRow("h", false)}}},
			RecentlyMerged: mergedBand([]*frontendv1.RosterRow{tabRow("m", false)}),
			TabBar:         &frontendv1.RosterTabBar{Tabs: []*frontendv1.RosterTab{tab("m"), tab("h")}},
		}, wantIDs: []string{"m", "h"}, wantNames: []string{"name-m", "name-h"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			order := TabOrder(tt.roster)

			// Assert
			if got := tabIDs(order); !slices.Equal(got, tt.wantIDs) {
				t.Fatalf("TabOrder ids = %v, want %v", got, tt.wantIDs)
			}
			names := []string{}
			for _, e := range order {
				names = append(names, e.Name)
			}
			if !slices.Equal(names, tt.wantNames) {
				t.Fatalf("TabOrder names = %v, want %v", names, tt.wantNames)
			}
		})
	}
}

// mergedBand is a recently-merged band of one group per rows list.
func mergedBand(groups ...[]*frontendv1.RosterRow) *frontendv1.RosterMergedSection {
	band := &frontendv1.RosterMergedSection{}
	for _, rows := range groups {
		band.Groups = append(band.Groups, &frontendv1.RosterMergedRepoGroup{Rows: &frontendv1.RosterRows{Rows: rows}})
	}
	return band
}
