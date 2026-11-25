package sidebar

import (
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// landingAt is minute MINUTE of the landing tests' day.
func landingAt(minute int) *time.Time {
	v := time.Date(2026, 11, 24, 12, minute, 0, 0, time.UTC)
	return &v
}

// landingWorkspace is a workspace of repository "r1" at /src/ID, the shape
// every landing test's workspace starts from.
func landingWorkspace(id ids.WorkspaceID) wsm.Workspace {
	return wsm.Workspace{ID: id, Repo: "r1", Dir: "/src/" + string(id)}
}

// landingRegistry is REG over WORKSPACES, with repositories r1 (main
// worktree /src/main) and r2 (main worktree /src/main2).
func landingRegistry(workspaces ...wsm.Workspace) Registry {
	return Registry{
		Workspaces: workspaces,
		Repositories: []wsm.Repository{
			{ID: "r1", Dir: "/src/main"},
			{ID: "r2", Dir: "/src/main2"},
		},
	}
}

// landingTabs draws every workspace of REG, in order, as a drawn tab.
func landingTabs(reg Registry) []Tab {
	tabs := make([]Tab, 0, len(reg.Workspaces))
	for _, ws := range reg.Workspaces {
		tabs = append(tabs, Tab{Workspace: ws, Withheld: wsm.Drawn})
	}
	return tabs
}

func TestLandingTargetTiers(t *testing.T) {
	sub := wsm.SubsectionID("s1")
	other := wsm.SubsectionID("s2")
	w := func(id ids.WorkspaceID, opts ...func(*wsm.Workspace)) wsm.Workspace {
		ws := landingWorkspace(id)
		for _, opt := range opts {
			opt(&ws)
		}
		return ws
	}
	selected := func(minute int) func(*wsm.Workspace) {
		return func(ws *wsm.Workspace) { ws.LastSelectedAt = landingAt(minute) }
	}
	parent := func(p ids.WorkspaceID) func(*wsm.Workspace) {
		return func(ws *wsm.Workspace) { ws.Parent = &p }
	}
	in := func(s wsm.SubsectionID) func(*wsm.Workspace) {
		return func(ws *wsm.Workspace) { ws.Subsection = &s }
	}
	repo := func(r ids.RepoID) func(*wsm.Workspace) {
		return func(ws *wsm.Workspace) { ws.Repo = r }
	}
	main := func(dir string) func(*wsm.Workspace) {
		return func(ws *wsm.Workspace) { ws.Dir = dir }
	}
	closed := func(ws *wsm.Workspace) { ws.Closed = true }
	tests := []struct {
		name       string
		workspaces []wsm.Workspace
		// drawn, when set, are the only workspaces with a tab; nil draws all.
		drawn  []ids.WorkspaceID
		want   ids.WorkspaceID
		wantBy Landing
	}{
		{
			name: "an ancestor wins over a more recently selected sibling",
			workspaces: []wsm.Workspace{
				w("parent", selected(1)), w("from", parent("parent"), selected(9)), w("sibling", selected(5)),
			},
			want: "parent", wantBy: LandedByAncestor,
		},
		{
			name: "the most recently selected ancestor wins among several",
			workspaces: []wsm.Workspace{
				w("grand", selected(4)), w("parent", parent("grand"), selected(2)), w("from", parent("parent"), selected(9)),
			},
			want: "grand", wantBy: LandedByAncestor,
		},
		{
			name: "an ancestor is reached through a closed parent",
			workspaces: []wsm.Workspace{
				w("grand", selected(1)), w("parent", parent("grand"), closed), w("from", parent("parent")), w("sibling", selected(5)),
			},
			drawn: []ids.WorkspaceID{"grand", "from", "sibling"},
			want:  "grand", wantBy: LandedByAncestor,
		},
		{
			name: "an ancestor in another repository still wins",
			workspaces: []wsm.Workspace{
				w("parent", repo("r2"), selected(1)), w("from", parent("parent")), w("sibling", selected(5)),
			},
			want: "parent", wantBy: LandedByAncestor,
		},
		{
			name: "a never-selected ancestor passes to the subsection",
			workspaces: []wsm.Workspace{
				w("parent", in(sub)), w("from", parent("parent"), in(sub)), w("mate", in(sub), selected(2)), w("repo-mate", selected(5)),
			},
			want: "mate", wantBy: LandedBySubsection,
		},
		{
			name: "the subsection wins over a more recently selected repository workspace",
			workspaces: []wsm.Workspace{
				w("mate", in(sub), selected(1)), w("from", in(sub), selected(9)), w("outside", in(other), selected(5)), w("loose", selected(6)),
			},
			want: "mate", wantBy: LandedBySubsection,
		},
		{
			name: "a subsection of the same id in another repository is not the subsection",
			workspaces: []wsm.Workspace{
				w("from", in(sub)), w("elsewhere", repo("r2"), in(sub), selected(7)), w("loose", selected(2)),
			},
			want: "loose", wantBy: LandedByRepository,
		},
		{
			name: "a workspace in no subsection passes straight to its repository",
			workspaces: []wsm.Workspace{
				w("from"), w("in-sub", in(sub), selected(3)), w("loose", selected(2)), w("foreign", repo("r2"), selected(8)),
			},
			want: "in-sub", wantBy: LandedByRepository,
		},
		{
			name: "the repository wins over a more recently selected other repository",
			workspaces: []wsm.Workspace{
				w("from"), w("mate", selected(1)), w("foreign", repo("r2"), selected(8)),
			},
			want: "mate", wantBy: LandedByRepository,
		},
		{
			name: "with no repository workspace ever selected its main workspace wins",
			workspaces: []wsm.Workspace{
				w("first"), w("main", main("/src/main")), w("from"), w("foreign", repo("r2"), selected(8)),
			},
			want: "main", wantBy: LandedByMain,
		},
		{
			name: "with the repository drawing nothing else any repository's most recent wins",
			workspaces: []wsm.Workspace{
				w("foreign-old", repo("r2"), selected(1)), w("from"), w("foreign-new", repo("r2"), selected(8)),
			},
			want: "foreign-new", wantBy: LandedByRecency,
		},
		{
			name: "with no tab ever selected the tab immediately left wins",
			workspaces: []wsm.Workspace{
				w("far", repo("r2")), w("left", repo("r2")), w("from"), w("right", repo("r2")),
			},
			want: "left", wantBy: LandedByNeighbour,
		},
		{
			name: "with no tab ever selected the leftmost tab lands on its right neighbour",
			workspaces: []wsm.Workspace{
				w("from"), w("right", repo("r2")), w("further", repo("r2")),
			},
			want: "right", wantBy: LandedByNeighbour,
		},
		{
			name: "a workspace with no tab (a closed one) still lands by its tiers",
			workspaces: []wsm.Workspace{
				w("parent", selected(1)), w("from", parent("parent"), closed), w("sibling", selected(5)),
			},
			drawn: []ids.WorkspaceID{"parent", "sibling"},
			want:  "parent", wantBy: LandedByAncestor,
		},
		{
			name: "a workspace with no tab and nothing ever selected lands on the leftmost tab",
			workspaces: []wsm.Workspace{
				w("first", repo("r2")), w("second", repo("r2")), w("from", closed),
			},
			drawn: []ids.WorkspaceID{"first", "second"},
			want:  "first", wantBy: LandedByNeighbour,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			reg := landingRegistry(tt.workspaces...)
			tabs := landingTabs(reg)
			if tt.drawn != nil {
				tabs = tabs[:0]
				for _, ws := range reg.Workspaces {
					for _, id := range tt.drawn {
						if ws.ID == id {
							tabs = append(tabs, Tab{Workspace: ws, Withheld: wsm.Drawn})
						}
					}
				}
			}
			var from wsm.Workspace
			for _, ws := range reg.Workspaces {
				if ws.ID == "from" {
					from = ws
				}
			}

			// Act
			got, by, found := LandingTarget(reg, tabs, from, nil)

			// Assert
			if !found || got.ID != tt.want || by != tt.wantBy {
				t.Fatalf("landing = (%q, %q, %v), want (%q, %q, true)", got.ID, by, found, tt.want, tt.wantBy)
			}
		})
	}
}

func TestLandingTargetPassesOverWithheldTabs(t *testing.T) {
	tests := []struct {
		name     string
		withheld wsm.Withholding
	}{
		{name: "a hidden ancestor is no landing", withheld: wsm.WithheldBacklogged},
		{name: "a collapsed ancestor is no landing", withheld: wsm.WithheldCollapsed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			p := ids.WorkspaceID("parent")
			parentWS := landingWorkspace("parent")
			parentWS.LastSelectedAt = landingAt(9)
			from := landingWorkspace("from")
			from.Parent = &p
			mate := landingWorkspace("mate")
			mate.LastSelectedAt = landingAt(1)
			reg := landingRegistry(parentWS, from, mate)
			tabs := landingTabs(reg)
			tabs[0].Withheld = tt.withheld

			// Act
			got, by, found := LandingTarget(reg, tabs, from, nil)

			// Assert
			if !found || got.ID != "mate" || by != LandedByRepository {
				t.Fatalf("landing = (%q, %q, %v), want (mate, repository, true)", got.ID, by, found)
			}
		})
	}
}

func TestLandingTargetFindsNoneWithNoOtherDrawnTab(t *testing.T) {
	tests := []struct {
		name string
		tabs func(Registry) []Tab
	}{
		{name: "only the leaving workspace is drawn", tabs: func(reg Registry) []Tab { return landingTabs(reg)[1:2] }},
		{name: "every other tab is withheld", tabs: func(reg Registry) []Tab {
			tabs := landingTabs(reg)
			tabs[0].Withheld = wsm.WithheldBacklogged
			tabs[2].Withheld = wsm.WithheldCollapsed
			return tabs
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			other := landingWorkspace("other")
			other.LastSelectedAt = landingAt(1)
			reg := landingRegistry(other, landingWorkspace("from"), landingWorkspace("third"))

			// Act
			_, _, found := LandingTarget(reg, tt.tabs(reg), reg.Workspaces[1], nil)

			// Assert
			if found {
				t.Fatal("found a landing, want none")
			}
		})
	}
}

func TestLandingTargetWithinAFoldPassesOverTheFoldsMembers(t *testing.T) {
	sub := wsm.SubsectionID("s1")
	inSub := func(id ids.WorkspaceID, minute int) wsm.Workspace {
		ws := landingWorkspace(id)
		ws.Subsection = &sub
		ws.LastSelectedAt = landingAt(minute)
		return ws
	}
	outside := func(id ids.WorkspaceID, minute int) wsm.Workspace {
		ws := landingWorkspace(id)
		ws.LastSelectedAt = landingAt(minute)
		return ws
	}
	neverSelected := func(id ids.WorkspaceID) wsm.Workspace {
		ws := landingWorkspace(id)
		ws.Repo = "r2"
		return ws
	}
	from := inSub("from", 9)
	tests := []struct {
		name       string
		workspaces []wsm.Workspace
		want       ids.WorkspaceID
		wantBy     Landing
		wantFound  bool
	}{
		{
			name:       "the repository's most recent tab outside the fold wins over a more recent member",
			workspaces: []wsm.Workspace{outside("older", 1), from, inSub("member", 8), outside("newer", 5)},
			want:       "newer", wantBy: LandedByRepository, wantFound: true,
		},
		{
			name:       "with no tab outside ever selected a member neighbour is passed over",
			workspaces: []wsm.Workspace{neverSelected("far-left"), inSub("member", 4), from},
			want:       "far-left", wantBy: LandedByNeighbour, wantFound: true,
		},
		{
			name:       "a fold holding every drawn tab leaves nowhere to land",
			workspaces: []wsm.Workspace{inSub("member", 4), from},
			wantFound:  false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			reg := landingRegistry(tt.workspaces...)

			// Act
			got, by, found := LandingTarget(reg, landingTabs(reg), from, wsm.SubsectionMembers(sub))

			// Assert
			if found != tt.wantFound || found && (got.ID != tt.want || by != tt.wantBy) {
				t.Fatalf("landing = (%q, %q, %v), want (%q, %q, %v)", got.ID, by, found, tt.want, tt.wantBy, tt.wantFound)
			}
		})
	}
}
