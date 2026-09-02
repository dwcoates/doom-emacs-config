package sidebar_test

import (
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestRosterNestsAWorkspaceUnderTheBranchItWasCutFrom(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	child := workspace("w-child", "child")
	child.ParentBranch = parent.Branch

	// Act.
	r.SetRegistry(registry(parent, child))

	// Assert.
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the section drew %d top-level rows, want only the parent", len(rows))
	}
	if got := rowNames(rows[0].GetChildren()); !equal(got, []string{"child"}) {
		t.Fatalf("children = %v, want the spawned workspace", got)
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
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 {
		t.Fatalf("the section drew %d top-level rows, want only the parent", len(rows))
	}
	if got := rowNames(rows[0].GetChildren()); !equal(got, []string{"child"}) {
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
	child.ParentBranch = parent.Branch
	grandchild := workspace("w-grandchild", "grandchild")
	grandchild.ParentBranch = child.Branch

	// Act.
	r.SetRegistry(registry(parent, child, grandchild))

	// Assert.
	rows := repoRows(t, latest(t, r))
	got := rowNames(rows[0].GetChildren()[0].GetChildren())
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
	// Arrange: a branch cut from itself is degenerate, not a family.
	r, _ := newResolver(t)
	ws := workspace("w-self", "self")
	ws.ParentBranch = ws.Branch

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	rows := repoRows(t, latest(t, r))
	if len(rows) != 1 || len(rows[0].GetChildren()) != 0 {
		t.Fatal("a workspace nested under itself")
	}
}

func TestRosterRecordsACyclingLineage(t *testing.T) {
	// Arrange: two workspaces each cut from the other's branch.
	r, surfaces := newResolver(t)
	a := workspace("w-a", "a")
	b := workspace("w-b", "b")
	a.ParentBranch = b.Branch
	b.ParentBranch = a.Branch

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
		t.Fatal("a cycling branch lineage was not recorded")
	}
}

func TestRosterStillDrawsACyclingLineagesRows(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	a := workspace("w-a", "a")
	b := workspace("w-b", "b")
	a.ParentBranch = b.Branch
	b.ParentBranch = a.Branch

	// Act.
	r.SetRegistry(registry(a, b))

	// Assert: an ill-formed lineage recedes a row to the top level, never
	// drops it.
	rows := repoRows(t, latest(t, r))
	if rowFor(rows, "w-a") == nil || rowFor(rows, "w-b") == nil {
		t.Fatal("a cycling lineage lost a row")
	}
}

func TestRosterRecordsTwoWorkspacesClaimingOneBranch(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	first := workspace("w-first", "first")
	second := workspace("w-second", "second")
	second.Branch = first.Branch

	// Act.
	r.SetRegistry(registry(first, second))

	// Assert.
	var warned bool
	for _, rec := range surfaces.Records() {
		if rec.Level == "warn" && rec.Context["branch"] == first.Branch {
			warned = true
		}
	}
	if !warned {
		t.Fatal("two workspaces claiming one branch was not recorded")
	}
}

func TestRosterNestsOnlyWithinOneRepository(t *testing.T) {
	// Arrange: a same-named branch in another repository is another branch.
	r, _ := newResolver(t)
	other := wsm.Repository{ID: "repo-2", Dir: "/repos/two", Name: "beta", DefaultBranch: "main"}
	parent := workspace("w-parent", "parent")
	stranger := workspace("w-stranger", "stranger")
	stranger.Repo = other.ID
	stranger.ParentBranch = parent.Branch
	reg := registry(parent, stranger)
	reg.Repositories = []wsm.Repository{repo, other}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	sections := latest(t, r).GetRepository().GetSections()
	for _, section := range sections {
		for _, row := range section.GetRows().GetRows() {
			if len(row.GetChildren()) != 0 {
				t.Fatal("a workspace nested under a branch in another repository")
			}
		}
	}
}

func TestTaskViewNestsOnlyAmongTheRowsItDraws(t *testing.T) {
	// Arrange: the child is on a task, its parent is not.
	r, _ := newResolver(t)
	task := wsm.Task{ID: ids.TaskID("task-1"), Title: "the task", CreatedAt: epoch}
	parent := workspace("w-parent", "parent")
	child := workspace("w-child", "child")
	child.ParentBranch = parent.Branch
	taskID := task.ID
	child.Task = &taskID
	reg := registry(parent, child)
	reg.Tasks = []wsm.Task{task}

	// Act.
	r.SetRegistry(reg)

	// Assert: a row can only nest under a row drawn beside it.
	sections := latest(t, r).GetTask().GetSections()
	if len(sections) != 1 {
		t.Fatalf("the task view carried %d sections, want 1", len(sections))
	}
	rows := sections[0].GetRows().GetRows()
	if got := rowNames(rows); !equal(got, []string{"child"}) {
		t.Fatalf("task rows = %v, want the child at the section's top level", got)
	}
}

func TestRecentlyMergedIsFlat(t *testing.T) {
	// Arrange: a merged parent and a merged child.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	parent.MergedAt = at(0)
	child := workspace("w-child", "child")
	child.ParentBranch = parent.Branch
	child.MergedAt = at(0)

	// Act.
	r.SetRegistry(registry(parent, child))

	// Assert: a settled merge has no family left to draw.
	rows := latest(t, r).GetRecentlyMerged().GetRows().GetRows()
	if len(rows) != 2 {
		t.Fatalf("the merged section drew %d rows, want both flat", len(rows))
	}
}
