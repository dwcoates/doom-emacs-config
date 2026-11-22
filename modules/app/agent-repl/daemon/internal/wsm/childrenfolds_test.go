package wsm

import (
	"context"
	"errors"
	"slices"
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

func TestSelectingADescendantLeavesTheChildrenFoldsHoldingItCollapsed(t *testing.T) {
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
	if !childrenFoldedOf(t, s, w.root.ID) {
		t.Fatal("the selection opened the fold holding the current workspace")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: "daemon.wsm.set_current",
		Message: "the selection expanded the collapsed folds holding the workspace"}); ok {
		t.Fatalf("a selection recorded an expansion it no longer makes: %v", log.Records())
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

// foldAboveGrandchild files root in a new subsection and folds everything
// above the grandchild: root's and child's children, the subsection, and the
// repository's section. It answers the subsection.
func foldAboveGrandchild(t *testing.T, s *store, w childrenFoldWorld) Subsection {
	t.Helper()
	ctx := context.Background()
	sub := mustSubsection(t, s, w.root.Repo, "later")
	mustPlace(t, s, w.root.ID, &sub.ID, AtEnd{})
	for _, fold := range []func() error{
		func() error { return s.SetChildrenFolded(ctx, w.root.ID, true) },
		func() error { return s.SetChildrenFolded(ctx, w.child.ID, true) },
		func() error { return s.SetSubsectionFolded(ctx, sub.ID, true) },
		func() error { return s.SetRepositoryFolded(ctx, w.root.Repo, true) },
	} {
		if err := fold(); err != nil {
			t.Fatalf("fold: %v", err)
		}
	}
	return sub
}

// foldsAbove reads (root's children, child's children, subsection, repository)
// folded.
func foldsAbove(t *testing.T, s *store, w childrenFoldWorld, sub Subsection) [4]bool {
	t.Helper()
	subs, err := s.ListSubsections(context.Background())
	if err != nil {
		t.Fatalf("ListSubsections: %v", err)
	}
	repos, err := s.ListRepositories(context.Background())
	if err != nil {
		t.Fatalf("ListRepositories: %v", err)
	}
	var out [4]bool
	out[0], out[1] = childrenFoldedOf(t, s, w.root.ID), childrenFoldedOf(t, s, w.child.ID)
	for _, got := range subs {
		if got.ID == sub.ID {
			out[2] = got.Folded
		}
	}
	for _, got := range repos {
		if got.ID == w.root.Repo {
			out[3] = got.Folded
		}
	}
	return out
}

func TestExpandChildrenWithAncestryOpensEveryFoldAbove(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newChildrenFoldWorld(t, s)
	sub := foldAboveGrandchild(t, s, w)
	if err := s.SetChildrenFolded(context.Background(), w.grandchild.ID, true); err != nil {
		t.Fatalf("SetChildrenFolded: %v", err)
	}

	// Act
	opened, err := s.ExpandChildrenWithAncestry(context.Background(), w.grandchild.ID)

	// Assert
	if err != nil {
		t.Fatalf("ExpandChildrenWithAncestry: %v", err)
	}
	want := []OpenedFold{
		{Kind: AttentionOpenedChildren, Target: string(w.child.ID)},
		{Kind: AttentionOpenedChildren, Target: string(w.root.ID)},
		{Kind: AttentionOpenedSubsection, Target: string(sub.ID)},
		{Kind: AttentionOpenedRepository, Target: string(w.root.Repo)},
	}
	if !slices.Equal(opened, want) {
		t.Fatalf("opened = %v, want %v", opened, want)
	}
	if folds := foldsAbove(t, s, w, sub); folds != [4]bool{} || childrenFoldedOf(t, s, w.grandchild.ID) {
		t.Fatalf("folds above (root, child, subsection, repository) = %v, grandchild folded = %v; want everything open", folds, childrenFoldedOf(t, s, w.grandchild.ID))
	}
}

func TestExpandChildrenWithAncestryLeavesWhatIsNotAboveFolded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newChildrenFoldWorld(t, s)
	sub := foldAboveGrandchild(t, s, w)
	if err := s.SetChildrenFolded(context.Background(), w.sibling.ID, true); err != nil {
		t.Fatalf("SetChildrenFolded: %v", err)
	}
	if err := s.SetChildrenFolded(context.Background(), w.grandchild.ID, true); err != nil {
		t.Fatalf("SetChildrenFolded: %v", err)
	}

	// Act
	_, err := s.ExpandChildrenWithAncestry(context.Background(), w.child.ID)

	// Assert: child's own and everything above it opens; root's sibling stays.
	if err != nil {
		t.Fatalf("ExpandChildrenWithAncestry: %v", err)
	}
	if !childrenFoldedOf(t, s, w.sibling.ID) || !childrenFoldedOf(t, s, w.grandchild.ID) {
		t.Fatal("the expand opened a fold that is not above the workspace")
	}
	if folds := foldsAbove(t, s, w, sub); folds != [4]bool{} {
		t.Fatalf("folds above (root, child, subsection, repository) = %v, want all open", folds)
	}
}

func TestExpandChildrenWithAncestryReportsOnlyTheFoldsItOpened(t *testing.T) {
	// Arrange: only the repository's section is folded.
	s, _ := testStore(t)
	w := newChildrenFoldWorld(t, s)
	if err := s.SetRepositoryFolded(context.Background(), w.root.Repo, true); err != nil {
		t.Fatalf("SetRepositoryFolded: %v", err)
	}

	// Act
	opened, err := s.ExpandChildrenWithAncestry(context.Background(), w.grandchild.ID)

	// Assert
	if err != nil {
		t.Fatalf("ExpandChildrenWithAncestry: %v", err)
	}
	want := []OpenedFold{{Kind: AttentionOpenedRepository, Target: string(w.root.Repo)}}
	if !slices.Equal(opened, want) {
		t.Fatalf("opened = %v, want %v", opened, want)
	}
}

func TestExpandChildrenWithAncestryOfATopLevelWorkspaceOpensItsSectionFolds(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newChildrenFoldWorld(t, s)
	sub := foldAboveGrandchild(t, s, w)

	// Act
	opened, err := s.ExpandChildrenWithAncestry(context.Background(), w.root.ID)

	// Assert
	if err != nil {
		t.Fatalf("ExpandChildrenWithAncestry: %v", err)
	}
	want := []OpenedFold{
		{Kind: AttentionOpenedSubsection, Target: string(sub.ID)},
		{Kind: AttentionOpenedRepository, Target: string(w.root.Repo)},
	}
	if !slices.Equal(opened, want) {
		t.Fatalf("opened = %v, want %v", opened, want)
	}
	if !childrenFoldedOf(t, s, w.child.ID) {
		t.Fatal("the expand opened a descendant's fold")
	}
}

func TestExpandChildrenWithAncestryRefusesAnUnknownWorkspaceAndWritesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	w := newChildrenFoldWorld(t, s)
	sub := foldAboveGrandchild(t, s, w)

	// Act
	opened, err := s.ExpandChildrenWithAncestry(context.Background(), "w-nope")

	// Assert
	if !errors.Is(err, ErrNotFound) || opened != nil {
		t.Fatalf("ExpandChildrenWithAncestry = %v, %v; want ErrNotFound and nothing opened", opened, err)
	}
	if folds := foldsAbove(t, s, w, sub); folds != [4]bool{true, true, true, true} {
		t.Fatalf("folds above (root, child, subsection, repository) = %v, want all still folded", folds)
	}
}
