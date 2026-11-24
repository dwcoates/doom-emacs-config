package sidebar_test

import (
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// foldOf answers the fold the tab bar names on workspace ID's tab.
func foldOf(t *testing.T, reg sidebar.Registry, id ids.WorkspaceID) *sidebar.Fold {
	t.Helper()
	for _, tab := range sidebar.Tabs(reg, dlog.NewTestLogger()) {
		if tab.Workspace.ID == id {
			if (tab.Fold != nil) != (tab.Withheld == wsm.WithheldCollapsed) {
				t.Fatalf("%s's tab: withheld %v with fold %+v, want a fold exactly when collapsed", id, tab.Withheld, tab.Fold)
			}
			return tab.Fold
		}
	}
	t.Fatalf("workspace %s has no tab", id)
	return nil
}

func TestEveryFoldedTabNamesItsFold(t *testing.T) {
	tests := []struct {
		name string
		reg  func() sidebar.Registry
		id   ids.WorkspaceID
		want *sidebar.Fold
	}{
		{name: "a drawn tab names none", reg: numberingRegistry, id: "a1", want: nil},
		{name: "a collapsed repository's tab names the repository", reg: numberingRegistry, id: "c1",
			want: &sidebar.Fold{By: sidebar.FoldedByRepository, Repository: "gamma"}},
		{name: "a collapsed subsection's tab names the subsection", id: "a2", reg: func() sidebar.Registry {
			reg := numberingRegistry()
			for i := range reg.Subsections {
				reg.Subsections[i].Folded = true
			}
			return reg
		}, want: &sidebar.Fold{By: sidebar.FoldedBySubsection, Repository: repo.Name, Subsection: "later"}},
		{name: "an unfolded subsection's tab in a collapsed repository names none", id: "a2", reg: func() sidebar.Registry {
			reg := numberingRegistry()
			reg.Repositories[0].Folded = true
			return reg
		}, want: nil},
		{name: "a collapsed subsection's tab in a collapsed repository names the subsection", id: "a2", reg: func() sidebar.Registry {
			reg := numberingRegistry()
			reg.Repositories[0].Folded = true
			for i := range reg.Subsections {
				reg.Subsections[i].Folded = true
			}
			return reg
		}, want: &sidebar.Fold{By: sidebar.FoldedBySubsection, Repository: repo.Name, Subsection: "later"}},
		{name: "a collapsed repository's tab in no subsection names the repository", id: "a1", reg: func() sidebar.Registry {
			reg := numberingRegistry()
			reg.Repositories[0].Folded = true
			return reg
		}, want: &sidebar.Fold{By: sidebar.FoldedByRepository, Repository: repo.Name}},
		{name: "a folded parent's child in an unfolded subsection of a collapsed repository names the parent", id: "k2", reg: func() sidebar.Registry {
			reg := numberingRegistry()
			reg.Repositories[0].Folded = true
			sub := reg.Subsections[0]
			parent := in(placed(workspace("p2", "parent2"), 10), sub)
			parent.ChildrenFolded = true
			kid := in(placed(workspace("k2", "kid2"), 1), sub)
			kid.Parent = &parent.ID
			reg.Workspaces = append(reg.Workspaces, parent, kid)
			return reg
		}, want: &sidebar.Fold{By: sidebar.FoldedByParent, Parent: "p2", ParentName: "parent2"}},
		{name: "a folded parent's child names the parent", reg: childrenFoldedRegistry, id: "k",
			want: &sidebar.Fold{By: sidebar.FoldedByParent, Parent: "p", ParentName: "parent"}},
		{name: "a folded parent's own tab names none", reg: childrenFoldedRegistry, id: "p", want: nil},
		{name: "a grandchild names the folded ancestor", id: "g", reg: func() sidebar.Registry {
			reg := childrenFoldedRegistry()
			grand := placed(workspace("g", "grand"), 2)
			kid := ids.WorkspaceID("k")
			grand.Parent = &kid
			reg.Workspaces = append(reg.Workspaces, grand)
			return reg
		}, want: &sidebar.Fold{By: sidebar.FoldedByParent, Parent: "p", ParentName: "parent"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := foldOf(t, tt.reg(), tt.id)

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("fold = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestAFoldsFieldsNameItsCollapse(t *testing.T) {
	tests := []struct {
		name string
		fold sidebar.Fold
		want dlog.Context
	}{
		{name: "a repository", fold: sidebar.Fold{By: sidebar.FoldedByRepository, Repository: "alpha"},
			want: dlog.Context{"fold": "repository", "fold_repository": "alpha"}},
		{name: "a subsection", fold: sidebar.Fold{By: sidebar.FoldedBySubsection, Repository: "alpha", Subsection: "later"},
			want: dlog.Context{"fold": "subsection", "fold_repository": "alpha", "fold_subsection": "later"}},
		{name: "a parent", fold: sidebar.Fold{By: sidebar.FoldedByParent, Parent: "p", ParentName: "parent"},
			want: dlog.Context{"fold": "parent", "fold_parent": "p", "fold_parent_name": "parent"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.fold.Fields()

			// Assert
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("Fields = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAFoldNamingNoCollapseRefusesItsFields(t *testing.T) {
	// Arrange
	fold := sidebar.Fold{}

	// Act
	defer func() {
		// Assert
		if recovered := recover(); recovered == nil || !strings.Contains(recovered.(string), "names no collapse") {
			t.Fatalf("recovered = %v, want the fold with no collapse refused", recovered)
		}
	}()
	fold.Fields()
}
