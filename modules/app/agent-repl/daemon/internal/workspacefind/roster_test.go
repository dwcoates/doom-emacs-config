package workspacefind

import (
	"reflect"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/wsm"
)

// rosterRow is a row of workspace ID drawn with the thinking status, or with
// no status arm when THINKING is false.
func rosterRow(id string, thinking bool) *frontendv1.RosterRow {
	row := &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: id, Dir: "/w/" + id}}}
	if thinking {
		row.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
	}
	return row
}

// rosterOf is a roster drawing OPEN in the open section, HIDDEN in the hidden
// section and MERGED in the recently merged band.
func rosterOf(open, hidden, merged []*frontendv1.RosterRow) *frontendv1.WorkspaceRoster {
	var openRows []*frontendv1.RosterRepoEntry
	for _, row := range open {
		openRows = append(openRows, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: &frontendv1.RosterOpenRow{Row: row}}})
	}
	var hiddenRows []*frontendv1.RosterHiddenRow
	for _, row := range hidden {
		hiddenRows = append(hiddenRows, &frontendv1.RosterHiddenRow{Row: row})
	}
	var mergedRows []*frontendv1.RosterMergedRow
	for _, row := range merged {
		mergedRows = append(mergedRows, &frontendv1.RosterMergedRow{Row: row})
	}
	return &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{Entries: openRows}}},
		Hidden:     &frontendv1.RosterHiddenSection{HiddenRows: &frontendv1.RosterHiddenRows{Rows: hiddenRows}},
		RecentlyMerged: &frontendv1.RosterMergedSection{Groups: []*frontendv1.RosterMergedRepoGroup{
			{MergedRows: &frontendv1.RosterMergedRows{Rows: mergedRows}}}},
	}
}

func TestPlacementsAnswersEachDrawnWorkspacesSectionAndStatus(t *testing.T) {
	// Arrange
	roster := rosterOf(
		[]*frontendv1.RosterRow{rosterRow("a", true)},
		[]*frontendv1.RosterRow{rosterRow("b", true)},
		[]*frontendv1.RosterRow{rosterRow("c", true)},
	)

	// Act
	got, err := Placements(roster)

	// Assert
	if err != nil {
		t.Fatalf("Placements: %v", err)
	}
	want := map[wsm.WorkspaceID]Drawn{
		"a": {Section: OpenSection, Status: "thinking"},
		"b": {Section: HiddenSection, Status: "thinking"},
		"c": {Section: MergedSection, Status: "thinking"},
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("Placements = %+v, want %+v", got, want)
	}
}

func TestPlacementsFailsOnABrokenRoster(t *testing.T) {
	tests := []struct {
		name   string
		roster *frontendv1.WorkspaceRoster
		want   string
	}{
		{
			name:   "a row with no status arm",
			roster: rosterOf([]*frontendv1.RosterRow{rosterRow("a", false)}, nil, nil),
			want:   "open row of workspace a sets no status arm",
		},
		{
			name:   "a workspace drawn twice",
			roster: rosterOf([]*frontendv1.RosterRow{rosterRow("a", true)}, []*frontendv1.RosterRow{rosterRow("a", true)}, nil),
			want:   "draws workspace a twice, in its open and hidden sections",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := Placements(tt.roster)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Placements err = %v, want one containing %q", err, tt.want)
			}
			if got != nil {
				t.Fatalf("a failed Placements answered %+v", got)
			}
		})
	}
}
