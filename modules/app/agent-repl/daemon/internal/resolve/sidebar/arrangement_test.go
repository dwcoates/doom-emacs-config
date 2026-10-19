package sidebar_test

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// subsection builds one of repo's subsections at PLACE.
func subsection(id, name string, place int64) wsm.Subsection {
	return wsm.Subsection{ID: ids.SubsectionID(id), Repo: repo.ID, Name: name, Place: place}
}

// in puts WS into subsection SUB.
func in(ws wsm.Workspace, sub wsm.Subsection) wsm.Workspace {
	id := sub.ID
	ws.Subsection = &id
	return ws
}

// arrangedRegistry is the owner's example: three workspaces of one
// repository, the FIRST of them moved into a subsection at the bottom, and a
// second, empty subsection placed BEFORE it.
func arrangedRegistry() sidebar.Registry {
	later := subsection("sub-later", "later", 2)
	empty := subsection("sub-empty", "empty", 1)
	reg := registry(
		in(placed(workspace("w1", "first"), 1), later),
		placed(workspace("w2", "second"), 2),
		placed(workspace("w3", "third"), 3),
	)
	reg.Subsections = []wsm.Subsection{later, empty}
	return reg
}

// subsectionNamesOf is SECTION's subsections' labels and row names.
func subsectionNamesOf(section *frontendv1.RosterRepoSection) []string {
	var out []string
	for _, sub := range section.GetSubsections() {
		out = append(out, sub.GetHeader().GetLabel().GetText()+"="+joinNames(rowNames(innerRows(sub.GetOpenRows().GetRows()))))
	}
	return out
}

func joinNames(names []string) string {
	out := ""
	for i, name := range names {
		if i > 0 {
			out += ","
		}
		out += name
	}
	return out
}

func TestTheRepositorySectionDrawsItsUnsectionedRowsAboveItsSubsections(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(arrangedRegistry())

	// Assert
	section := latest(t, r).GetRepository().GetSections()[0]
	if got := rowNames(innerRows(section.GetOpenRows().GetRows())); !equal(got, []string{"second", "third"}) {
		t.Fatalf("unsectioned rows = %v, want [second third]", got)
	}
	if got := subsectionNamesOf(section); !equal(got, []string{"empty=", "later=first"}) {
		t.Fatalf("subsections = %v, want the empty one, then later holding first", got)
	}
}

func TestASubsectionCarriesItsKeyAndCount(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(arrangedRegistry())

	// Assert
	later := latest(t, r).GetRepository().GetSections()[0].GetSubsections()[1]
	if got := later.GetKey().GetSubsectionId(); got != "sub-later" {
		t.Fatalf("key = %q, want sub-later", got)
	}
	if got := later.GetHeader().GetCount().GetWorkspaces(); got != 1 {
		t.Fatalf("count = %d, want 1", got)
	}
}

func TestTheRepositorySectionsCountsCoverItsSubsections(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(arrangedRegistry())

	// Assert
	roster := latest(t, r)
	if got := roster.GetRepository().GetSections()[0].GetHeader().GetCount().GetWorkspaces(); got != 3 {
		t.Fatalf("section count = %d, want 3", got)
	}
	if got := roster.GetRepository().GetOpenCount().GetWorkspaces(); got != 3 {
		t.Fatalf("open count = %d, want 3", got)
	}
}

func TestTheTabBarPutsASubsectionsWorkspacesAfterTheUnsectionedOnes(t *testing.T) {
	// Arrange -- the owner's example: the first workspace, dragged into a
	// subsection at the bottom of its repository, becomes the last tab.
	reg := arrangedRegistry()

	// Act
	got := sidebar.Tabs(reg, dlog.NewTestLogger())

	// Assert
	var ids []string
	for _, tab := range got {
		ids = append(ids, string(tab.Workspace.ID))
	}
	if !equal(ids, []string{"w2", "w3", "w1"}) {
		t.Fatalf("tabs = %v, want [w2 w3 w1]", ids)
	}
}

func TestTheTabBarWalksSubsectionsInTheirPlace(t *testing.T) {
	// Arrange
	a, b := subsection("sub-a", "a", 2), subsection("sub-b", "b", 1)
	reg := registry(in(placed(workspace("w1", "one"), 1), a), in(placed(workspace("w2", "two"), 2), b))
	reg.Subsections = []wsm.Subsection{a, b}

	// Act
	got := sidebar.Tabs(reg, dlog.NewTestLogger())

	// Assert
	if len(got) != 2 || got[0].Workspace.ID != "w2" || got[1].Workspace.ID != "w1" {
		t.Fatalf("tabs = %+v, want w2 (subsection b) then w1 (subsection a)", got)
	}
}

func TestASubsectionNestsAChildUnderItsParent(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	later := subsection("sub-later", "later", 1)
	parent := in(placed(workspace("w-p", "parent"), 1), later)
	child := in(placed(workspace("w-k", "child"), 2), later)
	child.Parent = &parent.ID
	reg := registry(parent, child)
	reg.Subsections = []wsm.Subsection{later}

	// Act
	r.SetRegistry(reg)

	// Assert
	rows := latest(t, r).GetRepository().GetSections()[0].GetSubsections()[0].GetOpenRows().GetRows()
	if len(rows) != 1 || rowNames(innerRows(rows[0].GetChildren())) == nil || rowNames(innerRows(rows[0].GetChildren()))[0] != "child" {
		t.Fatalf("subsection rows = %v, want parent with child nested", rowNames(innerRows(rows)))
	}
}

func TestSetRegistryRefusesAnArrangementItCannotDraw(t *testing.T) {
	foreign := wsm.Subsection{ID: "sub-foreign", Repo: otherRepo.ID, Name: "foreign", Place: 1}
	tests := []struct {
		name    string
		reg     func() sidebar.Registry
		message string
	}{
		{
			name: "a workspace naming a subsection the registry does not carry",
			reg: func() sidebar.Registry {
				return registry(in(placed(workspace("w1", "one"), 1), subsection("sub-gone", "gone", 1)))
			},
			message: "a workspace names a subsection the registry does not carry",
		},
		{
			name: "a workspace in another repository's subsection",
			reg: func() sidebar.Registry {
				reg := registry(in(placed(workspace("w1", "one"), 1), foreign))
				reg.Repositories = append(reg.Repositories, otherRepo)
				reg.Subsections = []wsm.Subsection{foreign}
				return reg
			},
			message: "a workspace sits in another repository's subsection",
		},
		{
			name: "a subsection of an unregistered repository",
			reg: func() sidebar.Registry {
				reg := registry()
				reg.Subsections = []wsm.Subsection{foreign}
				return reg
			},
			message: "a subsection names a repository the registry does not carry",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, surfaces := newResolver(t)
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("the registry was installed, not refused")
				}
				if _, ok := dlog.FindRecord(surfaces.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.sidebar.set_registry", Message: tt.message}); !ok {
					t.Fatalf("no %q error record: %v", tt.message, surfaces.Records())
				}
			}()

			// Act
			r.SetRegistry(tt.reg())
		})
	}
}

func TestTheTabBarKeepsAFamilyContiguous(t *testing.T) {
	// Arrange: the parent's sibling sits between its children in the registry.
	parent := placed(workspace("w-p", "parent"), 1)
	sibling := placed(workspace("w-s", "sibling"), 2)
	first := placed(workspace("w-k1", "first"), 3)
	first.Parent = &parent.ID
	grandchild := placed(workspace("w-g", "grand"), 4)
	grandchild.Parent = &first.ID
	second := placed(workspace("w-k2", "second"), 5)
	second.Parent = &parent.ID

	// Act
	got := sidebar.Tabs(registry(second, sibling, grandchild, first, parent), dlog.NewTestLogger())

	// Assert
	var tabs []string
	for _, tab := range got {
		tabs = append(tabs, string(tab.Workspace.ID))
	}
	if !equal(tabs, []string{"w-p", "w-k1", "w-g", "w-k2", "w-s"}) {
		t.Fatalf("tabs = %v, want the family depth-first, then the sibling", tabs)
	}
}

func TestTheTabBarPutsAClosedParentsChildrenInItsPlace(t *testing.T) {
	// Arrange: the sidebar hoists a closed parent's live child into its place.
	parent := placed(workspace("w-p", "parent"), 1)
	parent.Closed = true
	sibling := placed(workspace("w-s", "sibling"), 2)
	child := placed(workspace("w-k", "child"), 3)
	child.Parent = &parent.ID

	// Act
	got := sidebar.Tabs(registry(parent, sibling, child), dlog.NewTestLogger())

	// Assert
	if len(got) != 2 || got[0].Workspace.ID != "w-k" || got[1].Workspace.ID != "w-s" {
		t.Fatalf("tabs = %+v, want the hoisted child before the sibling", got)
	}
}

func TestTheTabBarMatchesTheSidebarsRowOrder(t *testing.T) {
	// Arrange: a family split across the registry, in a subsection too.
	later := subsection("sub-later", "later", 1)
	parent := placed(workspace("w-p", "parent"), 4)
	child := placed(workspace("w-k", "child"), 1)
	child.Parent = &parent.ID
	other := placed(workspace("w-o", "other"), 2)
	moved := in(placed(workspace("w-m", "moved"), 3), later)
	reg := registry(parent, child, other, moved)
	reg.Subsections = []wsm.Subsection{later}
	r, _ := newResolver(t)

	// Act
	r.SetRegistry(reg)

	// Assert
	rows, numbers := openRowsAndNumbers(latest(t, r))
	if want := []string{"w-o", "w-p", "w-k", "w-m"}; !equal(rows, want) || !equal(numbers, []string{"1", "2", "3", "4"}) {
		t.Fatalf("open rows %v numbered %v, want %v numbered 1..4", rows, numbers, want)
	}
}

func TestArrangedListsEachRunsFamiliesInDrawnOrder(t *testing.T) {
	// Arrange
	later := subsection("sub-later", "later", 1)
	parent := placed(workspace("w-p", "parent"), 1)
	child := placed(workspace("w-k", "child"), 2)
	child.Parent = &parent.ID
	moved := in(placed(workspace("w-m", "moved"), 3), later)
	reg := registry(child, moved, parent)
	reg.Subsections = []wsm.Subsection{later}

	// Act
	got := sidebar.Arranged(reg, dlog.NewTestLogger())

	// Assert
	if len(got) != 1 || len(got[0].Workspaces) != 1 || got[0].Workspaces[0].Workspace.ID != "w-p" ||
		len(got[0].Workspaces[0].Children) != 1 || got[0].Workspaces[0].Children[0].Workspace.ID != "w-k" {
		t.Fatalf("unsectioned = %+v, want parent with its child", got)
	}
	if len(got[0].Subsections) != 1 || got[0].Subsections[0].Subsection.ID != "sub-later" ||
		len(got[0].Subsections[0].Workspaces) != 1 || got[0].Subsections[0].Workspaces[0].Workspace.ID != "w-m" {
		t.Fatalf("subsections = %+v, want later holding moved", got[0].Subsections)
	}
}

func TestArrangedListsAnEmptySubsection(t *testing.T) {
	// Arrange
	reg := registry()
	reg.Subsections = []wsm.Subsection{subsection("sub-empty", "empty", 1)}

	// Act
	got := sidebar.Arranged(reg, dlog.NewTestLogger())

	// Assert
	if len(got) != 1 || len(got[0].Subsections) != 1 || len(got[0].Subsections[0].Workspaces) != 0 {
		t.Fatalf("got %+v, want one empty subsection", got)
	}
}

func TestArrangedLeavesOutClosedAndMergedWorkspacesAndHoistsAClosedParentsChildren(t *testing.T) {
	// Arrange
	parent := placed(workspace("w-p", "parent"), 1)
	parent.Closed = true
	child := placed(workspace("w-k", "child"), 2)
	child.Parent = &parent.ID
	merged := placed(workspace("w-m", "merged"), 3)
	merged.MergedAt = at(0)
	merged.Closed = true
	hidden := placed(hiddenWorkspace("w-h", "hidden"), 4)

	// Act
	got := sidebar.Arranged(registry(parent, child, merged, hidden), dlog.NewTestLogger())

	// Assert
	var ids []string
	for _, ws := range got[0].Workspaces {
		ids = append(ids, string(ws.Workspace.ID))
	}
	if !equal(ids, []string{"w-k", "w-h"}) {
		t.Fatalf("listed = %v, want the hoisted child and the hidden one", ids)
	}
}
