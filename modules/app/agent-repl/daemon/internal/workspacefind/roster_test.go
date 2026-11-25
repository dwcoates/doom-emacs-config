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

// pushOf is a push whose workspaces stream draws OPEN in the open section
// and HIDDEN in the hidden section.
func pushOf(open, hidden []*frontendv1.RosterRow) *frontendv1.RosterPush {
	var openRows []*frontendv1.RosterRepoEntry
	for _, row := range open {
		openRows = append(openRows, &frontendv1.RosterRepoEntry{Entry: &frontendv1.RosterRepoEntry_Row{Row: &frontendv1.RosterOpenRow{Row: row}}})
	}
	var backlogRows []*frontendv1.RosterBacklogRow
	for _, row := range hidden {
		backlogRows = append(backlogRows, &frontendv1.RosterBacklogRow{Row: row})
	}
	return &frontendv1.RosterPush{Workspaces: &frontendv1.WorkspaceRoster{
		Repository: &frontendv1.RosterRepositoryView{Sections: []*frontendv1.RosterRepoSection{{Entries: openRows}}},
		Backlog:    &frontendv1.RosterBacklogSection{BacklogRows: &frontendv1.RosterBacklogRows{Rows: backlogRows}},
	}}
}

func TestPlacementsAnswersEachDrawnWorkspacesSectionAndStatus(t *testing.T) {
	// Arrange: a and b on the workspaces stream, c a merged workspace's page
	// row.
	push := pushOf([]*frontendv1.RosterRow{rosterRow("a", true)}, []*frontendv1.RosterRow{rosterRow("b", true)})

	// Act
	got, err := Placements(push, []*frontendv1.RosterRow{rosterRow("c", true)})

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
		push   *frontendv1.RosterPush
		merged []*frontendv1.RosterRow
		want   string
	}{
		{
			name: "an open row with no status arm",
			push: pushOf([]*frontendv1.RosterRow{rosterRow("a", false)}, nil),
			want: "open row of workspace a sets no status arm",
		},
		{
			name:   "a merged row with no status arm",
			push:   pushOf(nil, nil),
			merged: []*frontendv1.RosterRow{rosterRow("m", false)},
			want:   "merged row of workspace m sets no status arm",
		},
		{
			name: "a workspace drawn twice",
			push: pushOf([]*frontendv1.RosterRow{rosterRow("a", true)}, []*frontendv1.RosterRow{rosterRow("a", true)}),
			want: "draws workspace a twice, in its open and hidden sections",
		},
		{
			name:   "a workspace both open and merged",
			push:   pushOf([]*frontendv1.RosterRow{rosterRow("a", true)}, nil),
			merged: []*frontendv1.RosterRow{rosterRow("a", true)},
			want:   "draws workspace a twice, in its open and merged sections",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := Placements(tt.push, tt.merged)

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
