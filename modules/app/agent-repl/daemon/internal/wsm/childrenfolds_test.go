package wsm

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/dlog"
)

// childrenFoldedOf reads whether workspace ID's children are folded.
func childrenFoldedOf(t *testing.T, s *store, id WorkspaceID) bool {
	t.Helper()
	ws, err := s.Workspace(context.Background(), id)
	if err != nil {
		t.Fatalf("Workspace(%s): %v", id, err)
	}
	return ws.ChildrenFolded
}

// childrenFoldWorld is a three-deep family (root > child > grandchild) and an
// unrelated sibling of root.
type childrenFoldWorld struct {
	root, child, grandchild, sibling Workspace
}

func newChildrenFoldWorld(t *testing.T, s *store) childrenFoldWorld {
	t.Helper()
	repo := arrangeRepo(t, "repo")
	root := registerIn(t, s, repo, "root", nil)
	child := registerChild(t, s, repo, "child", root.ID)
	grandchild := registerChild(t, s, repo, "grandchild", child.ID)
	sibling := registerIn(t, s, repo, "sibling", nil)
	return childrenFoldWorld{root: root, child: child, grandchild: grandchild, sibling: sibling}
}

func TestSetChildrenFoldedRecordsTheFold(t *testing.T) {
	for _, folded := range []bool{true, false} {
		t.Run(map[bool]string{true: "collapsed", false: "expanded"}[folded], func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newChildrenFoldWorld(t, s)
			if err := s.SetChildrenFolded(context.Background(), w.root.ID, !folded); err != nil {
				t.Fatalf("SetChildrenFolded: %v", err)
			}

			// Act
			err := s.SetChildrenFolded(context.Background(), w.root.ID, folded)

			// Assert
			if err != nil {
				t.Fatalf("SetChildrenFolded: %v", err)
			}
			if got := childrenFoldedOf(t, s, w.root.ID); got != folded {
				t.Fatalf("children folded = %v, want %v", got, folded)
			}
			if childrenFoldedOf(t, s, w.sibling.ID) {
				t.Fatal("the fold reached an unrelated workspace")
			}
		})
	}
}

func TestANewWorkspaceHasItsChildrenExpanded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	w := newChildrenFoldWorld(t, s)

	// Assert
	if childrenFoldedOf(t, s, w.root.ID) {
		t.Fatal("a new workspace's children are folded")
	}
}

func TestSetChildrenFoldedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetChildrenFolded(context.Background(), "absent", true)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetChildrenFolded = %v, want ErrNotFound", err)
	}
}

func TestExpandChildrenFoldsHolding(t *testing.T) {
	tests := []struct {
		name         string
		pick         func(childrenFoldWorld) Workspace
		want         bool
		wantRootOpen bool
		wantKidOpen  bool
	}{
		{name: "a child expands its parent's fold", pick: func(w childrenFoldWorld) Workspace { return w.child }, want: true, wantRootOpen: true},
		{name: "a grandchild expands every ancestor's fold", pick: func(w childrenFoldWorld) Workspace { return w.grandchild }, want: true, wantRootOpen: true, wantKidOpen: true},
		{name: "a workspace never expands its own children", pick: func(w childrenFoldWorld) Workspace { return w.root }},
		{name: "an unrelated workspace expands nothing", pick: func(w childrenFoldWorld) Workspace { return w.sibling }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			w := newChildrenFoldWorld(t, s)
			for _, id := range []WorkspaceID{w.root.ID, w.child.ID} {
				if err := s.SetChildrenFolded(context.Background(), id, true); err != nil {
					t.Fatalf("SetChildrenFolded: %v", err)
				}
			}

			// Act
			got, err := s.ExpandChildrenFoldsHolding(context.Background(), tt.pick(w).ID)

			// Assert
			if err != nil {
				t.Fatalf("ExpandChildrenFoldsHolding: %v", err)
			}
			if got != tt.want {
				t.Errorf("expanded = %v, want %v", got, tt.want)
			}
			if open := !childrenFoldedOf(t, s, w.root.ID); open != tt.wantRootOpen {
				t.Errorf("root's children expanded = %v, want %v", open, tt.wantRootOpen)
			}
			if open := !childrenFoldedOf(t, s, w.child.ID); open != tt.wantKidOpen {
				t.Errorf("child's children expanded = %v, want %v", open, tt.wantKidOpen)
			}
		})
	}
}

func TestSelectingADescendantExpandsTheChildrenFoldsHoldingIt(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	w := newChildrenFoldWorld(t, s)
	if err := s.SetChildrenFolded(context.Background(), w.root.ID, true); err != nil {
		t.Fatalf("SetChildrenFolded: %v", err)
	}

	// Act
	err := s.SetCurrent(context.Background(), w.grandchild.ID, instant)

	// Assert
	if err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}
	if childrenFoldedOf(t, s, w.root.ID) {
		t.Fatal("the selection left the collapsed fold holding the current workspace")
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.wsm.set_current",
		Message: "the selection expanded the collapsed folds holding the workspace"})
	if !ok || rec.Context["expanded_children"] != true {
		t.Fatalf("the expansion was not recorded at info naming the children fold: %v", log.Records())
	}
}

func TestDescendantMembers(t *testing.T) {
	p := func(id WorkspaceID) *WorkspaceID { return &id }
	all := []Workspace{
		{ID: "root"}, {ID: "child", Parent: p("root")}, {ID: "grand", Parent: p("child")},
		{ID: "other"}, {ID: "other-kid", Parent: p("other")},
		{ID: "loop-a", Parent: p("loop-b")}, {ID: "loop-b", Parent: p("loop-a")},
	}
	tests := []struct {
		name string
		root WorkspaceID
		in   map[WorkspaceID]bool
	}{
		{name: "every depth below the root", root: "root", in: map[WorkspaceID]bool{"child": true, "grand": true}},
		{name: "the root is not its own member", root: "child", in: map[WorkspaceID]bool{"grand": true}},
		{name: "a leaf has none", root: "grand", in: map[WorkspaceID]bool{}},
		{name: "a parent cycle ends the walk", root: "loop-a", in: map[WorkspaceID]bool{"loop-b": true}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			members := DescendantMembers(all, tt.root)

			// Assert
			for _, ws := range all {
				if got := members(ws); got != tt.in[ws.ID] {
					t.Errorf("member(%s) = %v, want %v", ws.ID, got, tt.in[ws.ID])
				}
			}
		})
	}
}

func TestDescendantMembersHoldTheCurrentWorkspaceOnlyBelowTheRoot(t *testing.T) {
	// Arrange
	root := WorkspaceID("root")
	child := Workspace{ID: "child", Parent: &root, Current: true}
	members := DescendantMembers([]Workspace{{ID: root}, child}, root)

	// Act and Assert
	if !members.HoldsCurrent(&child) {
		t.Fatal("the current child is not held by its parent's fold")
	}
	if members.HoldsCurrent(&Workspace{ID: root, Current: true}) {
		t.Fatal("the current parent is held by its own children's fold")
	}
	closed := Workspace{ID: "child", Parent: &root, Current: true, Closed: true}
	if members.HoldsCurrent(&closed) {
		t.Fatal("a closed current child is held by the fold")
	}
}
