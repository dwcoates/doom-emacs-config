package composermentions

import (
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

func namedRow(name string) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{Name: &frontendv1.RosterRowName{Text: name}}
}

// rosterOf is a roster drawing OPEN in the open section, HIDDEN in the hidden
// section and MERGED in the recently merged band.
func rosterOf(open, hidden, merged []string) *frontendv1.WorkspaceRoster {
	var openRows []*frontendv1.RosterRepoEntry
	for _, name := range open {
		openRows = append(openRows, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: &frontendv1.RosterOpenRow{Row: namedRow(name)}}})
	}
	var hiddenRows []*frontendv1.RosterHiddenRow
	for _, name := range hidden {
		hiddenRows = append(hiddenRows, &frontendv1.RosterHiddenRow{Row: namedRow(name)})
	}
	var mergedRows []*frontendv1.RosterMergedRow
	for _, name := range merged {
		mergedRows = append(mergedRows, &frontendv1.RosterMergedRow{Row: namedRow(name)})
	}
	return &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{Entries: openRows}}},
		Hidden:     &frontendv1.RosterHiddenSection{HiddenRows: &frontendv1.RosterHiddenRows{Rows: hiddenRows}},
		RecentlyMerged: &frontendv1.RosterMergedSection{Groups: []*frontendv1.RosterMergedRepoGroup{
			{MergedRows: &frontendv1.RosterMergedRows{Rows: mergedRows}}}},
	}
}

func TestRosterNames(t *testing.T) {
	tests := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{name: "no roster yet holds none", roster: nil},
		{name: "open then hidden, merged left out", roster: rosterOf([]string{"a", "b"}, []string{"c"}, []string{"d"}), want: []string{"a", "b", "c"}},
		{name: "a name two rows share is listed once", roster: rosterOf([]string{"a", "a"}, []string{"a"}, nil), want: []string{"a"}},
		{name: "a row with no name is left out", roster: rosterOf([]string{""}, nil, nil)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RosterNames(tt.roster)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("RosterNames = %q, want %q", got, tt.want)
			}
		})
	}
}
