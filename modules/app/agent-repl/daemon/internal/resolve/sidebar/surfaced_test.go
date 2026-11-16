package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// surfacedIn marks the workspaces of IDS surfaced in REG.
func surfacedIn(reg sidebar.Registry, marked ...ids.WorkspaceID) sidebar.Registry {
	for i := range reg.Workspaces {
		for _, id := range marked {
			if reg.Workspaces[i].ID == id {
				reg.Workspaces[i].Surfaced = true
			}
		}
	}
	return reg
}

func TestASurfacedWorkspacesTabIsDrawnInsideItsFold(t *testing.T) {
	tests := []struct {
		name      string
		reg       sidebar.Registry
		surfaced  ids.WorkspaceID
		wantDrawn []ids.WorkspaceID
	}{
		{name: "a child of a folded parent", reg: foldedBlockFamily(), surfaced: "w-a1", wantDrawn: []ids.WorkspaceID{"w-p", "w-a1"}},
		{name: "a member of a folded subsection", reg: foldedSubsectionRegistry(), surfaced: "a2", wantDrawn: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			plain := sidebar.Tabs(tt.reg, dlog.NewTestLogger())
			reg := surfacedIn(tt.reg, tt.surfaced)

			// Act
			tabs := sidebar.Tabs(reg, dlog.NewTestLogger())

			// Assert
			for i, tab := range tabs {
				want := plain[i].Withheld
				if tab.Workspace.ID == tt.surfaced {
					want = wsm.Drawn
				}
				if tab.Withheld != want {
					t.Errorf("%s's tab = %v, want %v", tab.Workspace.ID, tab.Withheld, want)
				}
			}
		})
	}
}

func TestASurfacedChildLeavesItsSiblingsAndItsOwnFamilyFolded(t *testing.T) {
	// Arrange
	reg := surfacedIn(foldedBlockFamily(), "w-a1")

	// Act
	tabs := sidebar.Tabs(reg, dlog.NewTestLogger())

	// Assert
	for _, tab := range tabs {
		want := wsm.WithheldCollapsed
		if tab.Workspace.ID == "w-p" || tab.Workspace.ID == "w-a1" {
			want = wsm.Drawn
		}
		if tab.Withheld != want {
			t.Errorf("%s's tab = %v, want %v", tab.Workspace.ID, tab.Withheld, want)
		}
	}
}

func TestASurfacedRowCarriesANumberedTabAndTheSurfacedMark(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(surfacedIn(foldedBlockFamily(), "w-a1"))

	// Assert
	section := latest(t, r).GetRepository().GetSections()[0]
	seen := false
	for _, row := range rosterwalk.FlattenOpen(rosterwalk.UnsectionedRows(section)) {
		id := ids.WorkspaceID(row.GetRow().GetWorkspace().GetWorkspace().GetId())
		surfaced := id == "w-a1"
		seen = seen || surfaced
		if row.GetSurfaced() != surfaced {
			t.Errorf("%s surfaced = %v, want %v", id, row.GetSurfaced(), surfaced)
		}
		if _, numbered := row.GetTab().GetTab().(*frontendv1.RosterOpenRowTab_Numbered); numbered != (surfaced || id == "w-p") {
			t.Errorf("%s numbered tab = %v, want %v", id, numbered, surfaced || id == "w-p")
		}
	}
	if !seen {
		t.Fatal("the surfaced workspace has no open row")
	}
}

func TestASurfacedMarkNoFoldCoversIsNotDrawnSurfaced(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(surfacedIn(numberingRegistry(), "a1"))

	// Assert
	section := latest(t, r).GetRepository().GetSections()[0]
	for _, row := range rosterwalk.FlattenOpen(rosterwalk.UnsectionedRows(section)) {
		if row.GetSurfaced() {
			t.Errorf("%s is drawn surfaced with no fold over it", row.GetRow().GetWorkspace().GetWorkspace().GetId())
		}
	}
}

func TestTheCurrentWorkspaceIsDrawnInsideAFoldThatCoversIt(t *testing.T) {
	tests := []struct {
		name      string
		reg       sidebar.Registry
		current   ids.WorkspaceID
		wantDrawn []ids.WorkspaceID
	}{
		{name: "a child of a folded parent", reg: foldedBlockFamily(), current: "w-a1", wantDrawn: []ids.WorkspaceID{"w-p", "w-a1"}},
		{name: "a member of a folded subsection", reg: foldedSubsectionRegistry(), current: "a2", wantDrawn: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			plain := sidebar.Tabs(tt.reg, dlog.NewTestLogger())
			reg := tt.reg
			reg.Current = &tt.current

			// Act
			tabs := sidebar.Tabs(reg, dlog.NewTestLogger())

			// Assert
			for i, tab := range tabs {
				want := plain[i].Withheld
				if tab.Workspace.ID == tt.current {
					want = wsm.Drawn
				}
				if tab.Withheld != want {
					t.Errorf("%s's tab = %v, want %v", tab.Workspace.ID, tab.Withheld, want)
				}
			}
		})
	}
}

func TestSwitchingOutOfAFoldHidesTheWorkspaceItDrewInsideIt(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	reg := foldedBlockFamily()
	inside := ids.WorkspaceID("w-a1")
	reg.Current = &inside
	r.SetRegistry(reg)
	section := func() []*frontendv1.RosterOpenRow {
		return rosterwalk.FlattenOpen(rosterwalk.UnsectionedRows(latest(t, r).GetRepository().GetSections()[0]))
	}
	surfaced := func() int {
		n := 0
		for _, row := range section() {
			if row.GetSurfaced() {
				n++
			}
		}
		return n
	}
	if surfaced() != 1 {
		t.Fatalf("surfaced rows while current = %d, want 1", surfaced())
	}

	// Act
	r.SetSelected("w-p")

	// Assert
	if surfaced() != 0 {
		t.Errorf("surfaced rows after switching out = %d, want 0", surfaced())
	}
}
