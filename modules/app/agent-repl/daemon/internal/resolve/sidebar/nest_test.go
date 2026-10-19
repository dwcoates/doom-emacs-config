package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"

	"agent-repld/internal/ids"
)

func TestRosterDoesNotNestAWorkspaceUnderTheBranchItWasCutFrom(t *testing.T) {
	// Arrange: cut from another workspace's branch, with no recorded parent,
	// so it merges into the primary branch, not into that workspace.
	r, _ := newResolver(t)
	feature := workspace("w-feature", "feature")
	cut := workspace("w-cut", "cut")
	cut.ParentBranch = feature.Branch

	// Act.
	r.SetRegistry(registry(feature, cut))

	// Assert: a row nests only under the workspace it merges into.
	rows := repoOpen(t, latest(t, r))
	if got := rowNames(innerRows(rows)); !equal(got, []string{"feature", "cut"}) || len(rows[0].GetChildren()) != 0 {
		t.Fatalf("rows = %v, want both at the top level", got)
	}
}

func TestRosterNestsAWorkspaceUnderItsRecordedParent(t *testing.T) {
	// Arrange: created through the daemon, so the parent is a recorded fact
	// and the branches say nothing about the family.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	child := workspace("w-child", "child")
	child.Parent = &parent.ID

	// Act.
	r.SetRegistry(registry(parent, child))

	// Assert.
	rows := repoOpen(t, latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the section drew %d top-level rows, want only the parent", len(rows))
	}
	if got := rowNames(innerRows(rows[0].GetChildren())); !equal(got, []string{"child"}) {
		t.Fatalf("children = %v, want the workspace its parent spawned", got)
	}
}

func TestRosterDrawsAWorkspaceWhoseRecordedParentIsNotInTheSectionAtTheTopLevel(t *testing.T) {
	// Arrange: a row can only nest under a row drawn beside it.
	r, _ := newResolver(t)
	absent := ids.WorkspaceID("w-elsewhere")
	child := workspace("w-child", "child")
	child.Parent = &absent

	// Act.
	r.SetRegistry(registry(child))

	// Assert.
	if got := len(repoRows(t, latest(t, r))); got != 1 {
		t.Fatalf("the section drew %d rows, want the child at the top level", got)
	}
}

func TestRosterNestsAGrandchild(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	child := workspace("w-child", "child")
	child.Parent = &parent.ID
	grandchild := workspace("w-grandchild", "grandchild")
	grandchild.Parent = &child.ID

	// Act.
	r.SetRegistry(registry(parent, child, grandchild))

	// Assert.
	rows := repoOpen(t, latest(t, r))
	got := rowNames(innerRows(rows[0].GetChildren()[0].GetChildren()))
	if !equal(got, []string{"grandchild"}) {
		t.Fatalf("grandchildren = %v, want the family nested two deep", got)
	}
}

func TestRosterDrawsAWorkspaceCutFromTheDefaultBranchAtTheTopLevel(t *testing.T) {
	// Arrange: the ordinary case — no workspace occupies the default branch.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	if got := len(repoRows(t, latest(t, r))); got != 2 {
		t.Fatalf("the section drew %d top-level rows, want both", got)
	}
}

func TestRosterDrawsAWorkspaceWithNoParentBranchAtTheTopLevel(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	orphan := workspace("w-orphan", "orphan")
	orphan.ParentBranch = ""

	// Act.
	r.SetRegistry(registry(orphan))

	// Assert.
	if got := len(repoRows(t, latest(t, r))); got != 1 {
		t.Fatalf("the section drew %d rows, want the parentless workspace", got)
	}
}

func TestRosterNeverNestsAWorkspaceUnderItself(t *testing.T) {
	// Arrange: a workspace recorded as its own parent is degenerate.
	r, _ := newResolver(t)
	ws := workspace("w-self", "self")
	ws.Parent = &ws.ID

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	rows := repoOpen(t, latest(t, r))
	if len(rows) != 1 || len(rows[0].GetChildren()) != 0 {
		t.Fatal("a workspace nested under itself")
	}
}

func TestRosterRecordsACyclingParentChain(t *testing.T) {
	// Arrange: two workspaces each recorded as the other's parent.
	r, surfaces := newResolver(t)
	a := workspace("w-a", "a")
	b := workspace("w-b", "b")
	a.Parent = &b.ID
	b.Parent = &a.ID

	// Act.
	r.SetRegistry(registry(a, b))

	// Assert.
	var warned bool
	for _, rec := range surfaces.Records() {
		if rec.Level == "warn" && rec.Operation == "daemon.sidebar.nest" {
			warned = true
		}
	}
	if !warned {
		t.Fatal("a cycling parent chain was not recorded")
	}
}

func TestRosterStillDrawsACyclingParentChainsRows(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	a := workspace("w-a", "a")
	b := workspace("w-b", "b")
	a.Parent = &b.ID
	b.Parent = &a.ID

	// Act.
	r.SetRegistry(registry(a, b))

	// Assert: an ill-formed chain recedes a row to the top level, never
	// drops it.
	rows := repoRows(t, latest(t, r))
	if rowFor(rows, "w-a") == nil || rowFor(rows, "w-b") == nil {
		t.Fatal("a cycling parent chain lost a row")
	}
}

func TestRecentlyMergedIsFlat(t *testing.T) {
	// Arrange: a merged parent and a merged child.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	parent.MergedAt = at(0)
	parent.Closed = true
	child := workspace("w-child", "child")
	child.Parent = &parent.ID
	child.MergedAt = at(0)
	child.Closed = true

	// Act.
	r.SetRegistry(registry(parent, child))

	// Assert: a settled merge has no family left to draw.
	rows := rosterwalk.MergedRows(latest(t, r))
	if len(rows) != 2 {
		t.Fatalf("the merged section drew %d rows, want both flat", len(rows))
	}
}

// TestRosterDrawsACutFromTheDefaultBranchAtTheTopLevelEvenWhenAWorkspaceOccupiesIt
// pins the one case that made every ordinary workspace a child: a repository's
// own main worktree is a registered workspace sitting ON the default branch,
// and every workspace created in that repository is cut from it.
func TestRosterDrawsACutFromTheDefaultBranchAtTheTopLevelEvenWhenAWorkspaceOccupiesIt(t *testing.T) {
	// Arrange: the repository's main worktree, on the default branch, beside a
	// workspace cut from that same branch.
	r, _ := newResolver(t)
	main := workspace("w-main", "alpha")
	main.Branch = repo.DefaultBranch
	cut := workspace("w-cut", "second")
	cut.ParentBranch = repo.DefaultBranch

	// Act.
	r.SetRegistry(registry(main, cut))

	// Assert.
	rows := repoRows(t, latest(t, r))
	if got := rowNames(rows); len(got) != 2 {
		t.Fatalf("the section drew %d top-level rows (%v), want both at the top level", len(got), got)
	}
}

// TestRosterLetsPlaceReorderTwoWorkspacesCutFromTheDefaultBranch is
// realtest 8's shape: the repository row and a workspace created inside it,
// given places that contradict the order they were drawn in.
func TestRosterLetsPlaceReorderTwoWorkspacesCutFromTheDefaultBranch(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	main := workspace("w-main", "alpha")
	main.Branch = repo.DefaultBranch
	second := workspace("w-second", "second")
	second.ParentBranch = repo.DefaultBranch

	// Act.
	r.SetRegistry(registry(
		placed(main, 2),
		placed(second, 1),
	))

	// Assert.
	want := []string{"second", "alpha"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want %v: place orders the roster and nesting must not outrank it", got, want)
	}
}

// TestRosterStillNestsARecordedChildCutFromTheDefaultBranch pins that a
// recorded parent nests whatever branch the child records: the parent is the
// workspace it merges into.
func TestRosterStillNestsARecordedChildCutFromTheDefaultBranch(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	main := workspace("w-main", "alpha")
	main.Branch = repo.DefaultBranch
	fork := workspace("w-fork", "fork")
	fork.ParentBranch = repo.DefaultBranch
	fork.Parent = &main.ID

	// Act.
	r.SetRegistry(registry(main, fork))

	// Assert.
	rows := repoOpen(t, latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the section drew %d top-level rows, want only the recorded parent", len(rows))
	}
	if got := rowNames(innerRows(rows[0].GetChildren())); !equal(got, []string{"fork"}) {
		t.Fatalf("children = %v, want the recorded child", got)
	}
}
