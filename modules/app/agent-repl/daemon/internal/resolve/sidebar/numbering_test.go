package sidebar_test

import (
	"testing"

	"agentrepl/protohelpers/rosterwalk"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/wsm"
)

// openRowsAndDrawnTabs answers the Open section's rows top to bottom, as the
// sidebar draws them (a collapsed repository draws none), and the tab bar's
// drawn tabs in order.
func openRowsAndDrawnTabs(roster *frontendv1.WorkspaceRoster) (rows, tabs []string) {
	rows, tabs = []string{}, []string{}
	for _, section := range roster.GetRepository().GetSections() {
		if section.GetCollapsed() != nil {
			continue
		}
		for _, region := range rosterwalk.RepoSectionRegions(section) {
			for _, row := range rosterwalk.FlattenRows(region.GetRows()) {
				if !row.GetClosed().GetClosed() {
					rows = append(rows, row.GetWorkspace().GetWorkspace().GetId())
				}
			}
		}
	}
	for _, tab := range roster.GetTabBar().GetTabs() {
		if tab.GetDrawn() != nil {
			tabs = append(tabs, tab.GetWorkspace().GetId())
		}
	}
	return rows, tabs
}

func TestTheDrawnTabsAreTheOpenRowsInTheirDrawnOrder(t *testing.T) {
	// Arrange: two repositories whose workspaces interleave in the registry,
	// a subsection, a family placed apart, a hidden workspace, and a
	// collapsed repository's workspace. Read down the Open section, the
	// numbered rows must be exactly the drawn tabs in order, so the
	// sidebar's numbers run 1, 2, 3, ... with no gap.
	later := subsection("sub-later", "later", 1)
	b1 := placed(workspace("b1", "b1"), 2)
	b1.Repo = otherRepo.ID
	parent := placed(workspace("p", "parent"), 6)
	kid := placed(workspace("k", "kid"), 1)
	kid.Parent = &parent.ID
	folded := wsm.Repository{ID: "repo-3", Dir: "/repos/three", Name: "gamma", DefaultBranch: "main", Folded: true, Place: 3}
	c1 := placed(workspace("c1", "c1"), 8)
	c1.Repo = folded.ID
	reg := registry(placed(workspace("a1", "a1"), 3), b1, in(placed(workspace("a2", "a2"), 5), later), parent, kid,
		placed(hiddenWorkspace("h", "h"), 7), c1)
	reg.Repositories = append(reg.Repositories, otherRepo, folded)
	reg.Subsections = []wsm.Subsection{later}
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(reg)

	// Assert.
	rows, tabs := openRowsAndDrawnTabs(latest(t, r))
	if want := []string{"a1", "p", "k", "a2", "b1"}; !equal(rows, want) || !equal(tabs, want) {
		t.Fatalf("open rows %v, drawn tabs %v; want both %v", rows, tabs, want)
	}
}
