package sidebar_test

import (
	"testing"
	"time"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/wsm"
)

func TestRosterOrdersByPriorityStrongestFirst(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(
		prioritized(workspace("w3", "three"), wsm.PriorityP3),
		prioritized(workspace("w05", "half"), wsm.PriorityP05),
		prioritized(workspace("w2", "two"), wsm.PriorityP2),
		prioritized(workspace("w1", "one"), wsm.PriorityP1),
	)

	// Act.
	r.SetRegistry(reg)

	// Assert.
	want := []string{"half", "one", "two", "three"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want %v", got, want)
	}
}

func TestRosterOrdersEveryPriorityBeforeTheUnprioritized(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(
		workspace("w-none", "aaa-unprioritized"),
		prioritized(workspace("w3", "zzz-p3"), wsm.PriorityP3),
	)

	// Act.
	r.SetRegistry(reg)

	// Assert: the name would order these the other way round, so only priority can.
	want := []string{"zzz-p3", "aaa-unprioritized"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want the prioritized workspace first", got)
	}
}

func TestRosterOrderDoesNotMoveWhenAWorkspaceIsSelected(t *testing.T) {
	// Arrange: three workspaces alike in priority, none selected yet.
	r, _ := newResolver(t)
	reg := registry(workspace("w-a", "alpha"), workspace("w-b", "bravo"),
		workspace("w-c", "charlie"))
	r.SetRegistry(reg)
	before := rowNames(repoRows(t, latest(t, r)))

	// Act: the user selects the middle tab, which stamps its selection instant
	// and re-pushes the registry exactly as WSM does.
	r.SetRegistry(selectIn(reg, "w-b", at(time.Hour)))
	r.SetSelected("w-b")

	// Assert: selection changes what is underlined, never what is where.
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, before) {
		t.Fatalf("order after selecting = %v, want the order before it, %v", got, before)
	}
}

func TestRosterOrderDoesNotMoveWhenAPrioritizedWorkspaceIsSelected(t *testing.T) {
	// Arrange: a selection instant must not reorder within a priority band either.
	r, _ := newResolver(t)
	reg := registry(
		prioritized(workspace("w-a", "alpha"), wsm.PriorityP1),
		prioritized(workspace("w-b", "bravo"), wsm.PriorityP1),
	)
	r.SetRegistry(reg)
	before := rowNames(repoRows(t, latest(t, r)))

	// Act.
	r.SetRegistry(selectIn(reg, "w-b", at(time.Hour)))
	r.SetSelected("w-b")

	// Assert.
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, before) {
		t.Fatalf("order after selecting = %v, want %v", got, before)
	}
}

func TestCyclingRightThenLeftReturnsToTheStartingTab(t *testing.T) {
	// Arrange: the bar realtest 4 drew, in the order it drew it.
	r, _ := newResolver(t)
	reg := registry(
		workspace("w-ee", "explanation-engine"),
		workspace("w-abc", "ABC/chess960-review-failures-enm"),
		workspace("w-rt4", "rt4-bootstrap-1"),
	)
	r.SetRegistry(reg)
	start := rowNames(repoRows(t, latest(t, r)))[0]

	// Act: cycle RIGHT off the first tab and select what it landed on, exactly
	// as a switch does, then cycle LEFT off that tab reading the bar as it is
	// drawn at that moment.
	right := cycle(t, r, start, 1)
	selected := selectNamed(t, reg, right, at(time.Hour))
	r.SetRegistry(selected.reg)
	r.SetSelected(selected.id)
	back := cycle(t, r, right, -1)

	// Assert: right then left is the identity.
	if back != start {
		t.Fatalf("cycling right to %q then left landed on %q, want the starting tab %q",
			right, back, start)
	}
}

// cycle answers the tab n steps from `from` in the DRAWN order, which is what
// the Emacs tab bar walks.
func cycle(t *testing.T, r sidebar.Resolver, from string, n int) string {
	t.Helper()
	names := rowNames(repoRows(t, latest(t, r)))
	for i, name := range names {
		if name == from {
			return names[((i+n)%len(names)+len(names))%len(names)]
		}
	}
	t.Fatalf("%q is not on the bar %v", from, names)
	return ""
}

// selectIn answers a copy of reg with one workspace selected: its selection
// instant stamped and the registry pointed at it, which is what WSM's
// SetCurrent re-pushes.
func selectIn(reg sidebar.Registry, id ids.WorkspaceID, when *time.Time) sidebar.Registry {
	out := reg
	out.Workspaces = make([]wsm.Workspace, len(reg.Workspaces))
	copy(out.Workspaces, reg.Workspaces)
	for i := range out.Workspaces {
		if out.Workspaces[i].ID == id {
			out.Workspaces[i].LastSelectedAt = when
		}
	}
	out.Current = &id
	return out
}

// selection is one selected workspace: the registry WSM would re-push and the
// id the resolver is told about.
type selection struct {
	reg sidebar.Registry
	id  ids.WorkspaceID
}

// selectNamed selects the workspace drawn under name, which is what a client
// hands back after cycling: the bar names tabs, the daemon addresses ids.
func selectNamed(t *testing.T, reg sidebar.Registry, name string, when *time.Time) selection {
	t.Helper()
	for _, ws := range reg.Workspaces {
		if ws.Name == name {
			return selection{reg: selectIn(reg, ws.ID, when), id: ws.ID}
		}
	}
	t.Fatalf("no workspace is named %q", name)
	return selection{}
}

func TestRosterBreaksASelectionTieOnTheName(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	b := workspace("w-b", "bravo")
	b.LastSelectedAt = at(0)
	a := workspace("w-a", "alpha")
	a.LastSelectedAt = at(0)

	// Act.
	r.SetRegistry(registry(b, a))

	// Assert.
	want := []string{"alpha", "bravo"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want %v", got, want)
	}
}

func TestRosterBreaksANameTieOnTheWorkspaceId(t *testing.T) {
	// Arrange: two workspaces alike in every drawn field.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w-b", "same"), workspace("w-a", "same")))

	// Assert.
	rows := repoRows(t, latest(t, r))
	if rows[0].GetWorkspace().GetWorkspace().GetId() != "w-a" {
		t.Fatal("two identical rows did not draw in one fixed order")
	}
}

func TestRosterOrdersChildrenTheSameWayWithinTheirParent(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	childLow := workspace("w-low", "aaa-child")
	childLow.ParentBranch = parent.Branch
	childHigh := prioritized(workspace("w-high", "zzz-child"), wsm.PriorityP05)
	childHigh.ParentBranch = parent.Branch

	// Act.
	r.SetRegistry(registry(parent, childLow, childHigh))

	// Assert: the same rule applies inside the family as outside it.
	rows := repoRows(t, latest(t, r))
	want := []string{"zzz-child", "aaa-child"}
	if got := rowNames(rows[0].GetChildren()); !equal(got, want) {
		t.Fatalf("children order = %v, want %v", got, want)
	}
}

func TestRosterComposesThePriorityBadgeLabel(t *testing.T) {
	tests := []struct {
		name string
		ws   wsm.Workspace
		want string
	}{
		{name: "p05", ws: prioritized(workspace("w", "n"), wsm.PriorityP05), want: "P0.5"},
		{name: "p1", ws: prioritized(workspace("w", "n"), wsm.PriorityP1), want: "P1"},
		{name: "p2", ws: prioritized(workspace("w", "n"), wsm.PriorityP2), want: "P2"},
		{name: "p3", ws: prioritized(workspace("w", "n"), wsm.PriorityP3), want: "P3"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)

			// Act.
			r.SetRegistry(registry(tc.ws))

			// Assert.
			if got := onlyRow(t, r).GetPriority().GetLabel(); got != tc.want {
				t.Fatalf("badge = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRosterDrawsNoBadgeForAnUnprioritizedWorkspace(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w", "n")))

	// Assert: unset is no badge, never an empty one.
	if got := onlyRow(t, r).GetPriority(); got != nil {
		t.Fatalf("an unprioritized workspace drew a badge: %v", got)
	}
}

func TestRosterNeverReordersTheRegistrysSlice(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	given := []wsm.Workspace{
		prioritized(workspace("w3", "three"), wsm.PriorityP3),
		prioritized(workspace("w1", "one"), wsm.PriorityP1),
	}
	reg := registry(given...)

	// Act.
	r.SetRegistry(reg)

	// Assert: the slice belongs to the registry's caller.
	if reg.Workspaces[0].Name != "three" {
		t.Fatal("the resolver sorted its caller's memory")
	}
}

func TestRecentlyMergedOrdersMostRecentFirst(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	older := workspace("w-older", "aaa-older")
	older.MergedAt = at(0)
	newer := workspace("w-newer", "zzz-newer")
	newer.MergedAt = at(time.Hour)

	// Act.
	r.SetRegistry(registry(older, newer))

	// Assert: recency is the section's whole meaning.
	got := rowNames(latest(t, r).GetRecentlyMerged().GetRows().GetRows())
	if !equal(got, []string{"zzz-newer", "aaa-older"}) {
		t.Fatalf("merged order = %v, want the most recent merge first", got)
	}
}

func TestRosterOrdersRepositorySectionsByName(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	second := wsm.Repository{ID: "repo-2", Dir: "/repos/two", Name: "beta", DefaultBranch: "main"}
	reg := registry(workspace("w1", "one"))
	reg.Repositories = []wsm.Repository{second, repo}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	sections := latest(t, r).GetRepository().GetSections()
	if len(sections) != 2 || sections[0].GetHeader().GetLabel().GetText() != "alpha" {
		t.Fatal("repository sections did not order by name")
	}
}

func TestRosterOrdersTaskSectionsByCreation(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	first := wsm.Task{ID: "task-z", Title: "zzz", CreatedAt: epoch}
	second := wsm.Task{ID: "task-a", Title: "aaa", CreatedAt: epoch.Add(time.Hour)}
	reg := registry()
	reg.Tasks = []wsm.Task{second, first}

	// Act.
	r.SetRegistry(reg)

	// Assert: the title would order these the other way round.
	sections := latest(t, r).GetTask().GetSections()
	if len(sections) != 2 || sections[0].GetKey().GetTaskId() != "task-z" {
		t.Fatal("task sections did not order by creation")
	}
}

// equal reports whether two name lists match, in order.
func equal(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
