package workspacefind

import (
	"reflect"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/wsm"
)

// locateRegistry is a registry of an open workspace, a hidden merge
// workspace, an open child nested inside the open one's worktree, a closed
// workspace the roster still draws, a closed one it no longer draws, and a
// thread sharing the open workspace's directory.
func locateRegistry() []wsm.Workspace {
	return []wsm.Workspace{
		{ID: "ws-open", Dir: "/wt/open", Name: "open-one"},
		{ID: "ws-merge", Dir: "/wt/merge", Name: "merge-one", Hidden: true, MergeOf: &wsm.MergeOrigin{Requester: "ws-open"}},
		{ID: "ws-nested", Dir: "/wt/open/nested", Name: "nested-one"},
		{ID: "ws-drawn", Dir: "/wt/drawn", Name: "drawn-one", Closed: true},
		{ID: "ws-gone", Dir: "/wt/gone", Name: "gone-one", Closed: true},
		{ID: "ws-thread", Dir: "/wt/open", Name: "open-one", Kind: wsm.WorkspaceKindThread},
	}
}

// locateRoster draws the closed ws-drawn in the recently merged band.
func locateRoster() *frontendv1.WorkspaceRoster {
	row := &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-drawn", Dir: "/wt/drawn"}}}
	return &frontendv1.WorkspaceRoster{RecentlyMerged: &frontendv1.RosterMergedSection{Groups: []*frontendv1.RosterMergedRepoGroup{{
		MergedRows: &frontendv1.RosterMergedRows{Rows: []*frontendv1.RosterMergedRow{{Row: row}}}}}}}
}

func TestLocate(t *testing.T) {
	tests := []struct {
		name string
		by   LocateBy
		want []wsm.WorkspaceID
	}{
		{name: "an open workspace by name, its thread left out", by: LocateBy{Name: "open-one"}, want: []wsm.WorkspaceID{"ws-open"}},
		{name: "a hidden merge workspace by name", by: LocateBy{Name: "merge-one"}, want: []wsm.WorkspaceID{"ws-merge"}},
		{name: "a closed workspace the roster draws by name", by: LocateBy{Name: "drawn-one"}, want: []wsm.WorkspaceID{"ws-drawn"}},
		{name: "a closed workspace the roster no longer draws is not located", by: LocateBy{Name: "gone-one"}},
		{name: "an unknown name", by: LocateBy{Name: "nobody"}},
		{name: "a hidden merge workspace at its directory", by: LocateBy{At: "/wt/merge"}, want: []wsm.WorkspaceID{"ws-merge"}},
		{name: "an exact directory reads a trailing slash as the same", by: LocateBy{At: "/wt/open/"}, want: []wsm.WorkspaceID{"ws-open"}},
		{name: "a directory inside a worktree is not at it", by: LocateBy{At: "/wt/open/src"}},
		{name: "a hidden merge workspace contains its worktree's subdirectory", by: LocateBy{Containing: "/wt/merge/modules/app"}, want: []wsm.WorkspaceID{"ws-merge"}},
		{name: "the deepest of nested worktrees contains", by: LocateBy{Containing: "/wt/open/nested/src"}, want: []wsm.WorkspaceID{"ws-nested"}},
		{name: "the outer worktree contains outside the nested one", by: LocateBy{Containing: "/wt/open/src"}, want: []wsm.WorkspaceID{"ws-open"}},
		{name: "a sibling sharing a prefix is not contained", by: LocateBy{Containing: "/wt/openly"}},
		{name: "a directory in no worktree", by: LocateBy{Containing: "/elsewhere"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			located := Locate(locateRegistry(), locateRoster(), tt.by)

			// Assert
			var got []wsm.WorkspaceID
			for _, ws := range located {
				got = append(got, ws.ID)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Locate(%+v) = %v, want %v", tt.by, got, tt.want)
			}
		})
	}
}

func TestLocateAnswersEveryWorkspaceOfAName(t *testing.T) {
	// Arrange
	registry := []wsm.Workspace{{ID: "ws-1", Dir: "/wt/one", Name: "twin"}, {ID: "ws-2", Dir: "/wt/two", Name: "twin"}}

	// Act
	located := Locate(registry, &frontendv1.WorkspaceRoster{}, LocateBy{Name: "twin"})

	// Assert
	if len(located) != 2 || located[0].ID != "ws-1" || located[1].ID != "ws-2" {
		t.Fatalf("Locate = %v, want both twins in registry order", located)
	}
}

func TestLocateContainingPrefersTheOpenWorkspaceAtAReusedDirectory(t *testing.T) {
	// Arrange: a drawn closed workspace and an open one at the same worktree,
	// the closed one first in registry order.
	registry := []wsm.Workspace{{ID: "ws-old", Dir: "/wt/reused", Closed: true}, {ID: "ws-new", Dir: "/wt/reused"}}
	roster := &frontendv1.WorkspaceRoster{Closed: &frontendv1.RosterClosedRows{Rows: []*frontendv1.RosterClosedRow{{
		Row: &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-old"}}}}}}}

	// Act
	located := Locate(registry, roster, LocateBy{Containing: "/wt/reused/src"})

	// Assert
	if len(located) != 1 || located[0].ID != "ws-new" {
		t.Fatalf("Locate = %v, want the open ws-new", located)
	}
}

func TestLocatePanicsOnALocateThatNamesNothing(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("Locate answered a locate naming nothing; want a panic")
		}
	}()

	// Act
	Locate(locateRegistry(), &frontendv1.WorkspaceRoster{}, LocateBy{})
}
