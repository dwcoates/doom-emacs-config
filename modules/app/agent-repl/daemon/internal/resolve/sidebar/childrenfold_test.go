package sidebar_test

import (
	"testing"

	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// childrenFoldedRegistry is numberingRegistry with the children of its
// workspace "p" (whose open child is "k") folded.
func childrenFoldedRegistry() sidebar.Registry {
	reg := numberingRegistry()
	for i := range reg.Workspaces {
		if reg.Workspaces[i].ID == "p" {
			reg.Workspaces[i].ChildrenFolded = true
		}
	}
	return reg
}

// childrenFoldArm names the fold arm on workspace ID's open row.
func childrenFoldArm(t *testing.T, r sidebar.Resolver, id ids.WorkspaceID) string {
	t.Helper()
	for _, open := range rosterwalk.OpenRows(latest(t, r)) {
		if open.GetRow().GetWorkspace().GetWorkspace().GetId() != string(id) {
			continue
		}
		fold := open.GetChildrenFold()
		switch {
		case fold == nil:
			return "none"
		case fold.GetCollapsed() != nil:
			return "collapsed"
		case fold.GetExpanded().GetHoldsCurrent() != nil:
			return "holds_current"
		case fold.GetExpanded().GetCollapsible() != nil:
			return "collapsible"
		default:
			return "unset"
		}
	}
	t.Fatalf("workspace %s has no open row", id)
	return ""
}

func TestAFoldedParentsChildRowHasAFoldedTabAndTheRestRunUnbroken(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(childrenFoldedRegistry())

	// Assert: k (the folded parent's child) is off the bar, and the numbers
	// still run 1..N with no gap.
	rows, numbers := openRowsAndNumbers(latest(t, r))
	if want := []string{"a1", "p", "m", "a2", "b1"}; !equal(rows, want) {
		t.Fatalf("numbered rows = %v, want %v", rows, want)
	}
	if want := []string{"1", "2", "3", "4", "5"}; !equal(numbers, want) {
		t.Fatalf("numbers = %v, want %v", numbers, want)
	}
}

func TestAFoldedParentsChildTabIsWithheldCollapsed(t *testing.T) {
	// Act.
	tabs := sidebar.Tabs(childrenFoldedRegistry(), dlog.NewTestLogger())

	// Assert.
	for _, tab := range tabs {
		want := wsm.Drawn
		if tab.Workspace.ID == "k" || tab.Workspace.ID == "c1" {
			want = wsm.WithheldCollapsed
		}
		if tab.Withheld != want {
			t.Fatalf("%s's tab = %v, want %v", tab.Workspace.ID, tab.Withheld, want)
		}
	}
}

func TestAFoldedAncestorFoldsEveryDescendantsTab(t *testing.T) {
	// Arrange: g is k's child, so it sits below the folded p too.
	reg := childrenFoldedRegistry()
	var kid wsm.Workspace
	for _, ws := range reg.Workspaces {
		if ws.ID == "k" {
			kid = ws
		}
	}
	grand := placed(workspace("g", "grand"), 2)
	grand.Parent = &kid.ID
	reg.Workspaces = append(reg.Workspaces, grand)

	// Act.
	tabs := sidebar.Tabs(reg, dlog.NewTestLogger())

	// Assert.
	for _, tab := range tabs {
		if (tab.Workspace.ID == "k" || tab.Workspace.ID == "g") && tab.Withheld != wsm.WithheldCollapsed {
			t.Fatalf("%s's tab = %v, want %v below a folded ancestor", tab.Workspace.ID, tab.Withheld, wsm.WithheldCollapsed)
		}
	}
}

func TestARowsChildrenFoldIsStatedOnlyWhenItHasChildren(t *testing.T) {
	tests := []struct {
		name     string
		registry func() sidebar.Registry
		row      ids.WorkspaceID
		want     string
	}{
		{name: "a parent with expanded children is collapsible", registry: numberingRegistry, row: "p", want: "collapsible"},
		{name: "a parent with folded children is collapsed", registry: childrenFoldedRegistry, row: "p", want: "collapsed"},
		{name: "a row with no children states no fold", registry: numberingRegistry, row: "a1", want: "none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(tt.registry())

			// Assert.
			if got := childrenFoldArm(t, r, tt.row); got != tt.want {
				t.Fatalf("children fold of %s = %s, want %s", tt.row, got, tt.want)
			}
		})
	}
}

func TestAParentWhoseChildIsCurrentHoldsTheSelection(t *testing.T) {
	tests := []struct {
		name     string
		selected ids.WorkspaceID
		want     string
	}{
		{name: "the current child holds its parent's fold open", selected: "k", want: "holds_current"},
		{name: "the current parent does not", selected: "p", want: "collapsible"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistrySelected(numberingRegistry(), tt.selected, sidebar.SelectStamp{})

			// Assert.
			if got := childrenFoldArm(t, r, "p"); got != tt.want {
				t.Fatalf("children fold of p = %s, want %s", got, tt.want)
			}
		})
	}
}
