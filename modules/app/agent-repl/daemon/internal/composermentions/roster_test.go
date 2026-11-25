package composermentions

import (
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

func namedRow(name string) *frontendv1.RosterRow {
	return &frontendv1.RosterRow{Name: &frontendv1.RosterRowName{Text: name}}
}

// rosterOf is a workspaces stream drawing OPEN in the open section and
// HIDDEN in the hidden section. A merged workspace is in no stream.
func rosterOf(open, backlog []string) *frontendv1.WorkspaceRoster {
	var openRows []*frontendv1.RosterRepoEntry
	for _, name := range open {
		openRows = append(openRows, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: &frontendv1.RosterOpenRow{Row: namedRow(name)}}})
	}
	var backlogRows []*frontendv1.RosterBacklogRow
	for _, name := range backlog {
		backlogRows = append(backlogRows, &frontendv1.RosterBacklogRow{Row: namedRow(name)})
	}
	return &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{Entries: openRows}}},
		Backlog:    &frontendv1.RosterBacklogSection{BacklogRows: &frontendv1.RosterBacklogRows{Rows: backlogRows}},
	}
}

func TestRosterNames(t *testing.T) {
	tests := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   []string
	}{
		{name: "no roster yet holds none", roster: nil},
		{name: "open then hidden", roster: rosterOf([]string{"a", "b"}, []string{"c"}), want: []string{"a", "b", "c"}},
		{name: "a name two rows share is listed once", roster: rosterOf([]string{"a", "a"}, []string{"a"}), want: []string{"a"}},
		{name: "a row with no name is left out", roster: rosterOf([]string{""}, nil)},
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
