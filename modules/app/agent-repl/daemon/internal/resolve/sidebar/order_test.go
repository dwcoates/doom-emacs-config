package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

func TestRosterOrdersByPlace(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(
		placed(workspace("w3", "three"), 3),
		placed(workspace("w1", "one"), 1),
		placed(workspace("w2", "two"), 2),
	)

	// Act.
	r.SetRegistry(reg)

	// Assert.
	want := []string{"one", "two", "three"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want %v", got, want)
	}
}

func TestRosterOrderIgnoresPriority(t *testing.T) {
	// Arrange: a P0.5 workspace placed after an unprioritized one.
	r, _ := newResolver(t)
	reg := registry(
		placed(prioritized(workspace("w-high", "high"), wsm.PriorityP05), 2),
		placed(workspace("w-none", "none"), 1),
	)

	// Act.
	r.SetRegistry(reg)

	// Assert: priority only picks a landing place; the place orders.
	want := []string{"none", "high"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want %v", got, want)
	}
}

func TestRosterOrderIgnoresTheName(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(
		placed(workspace("w-a", "alpha"), 2),
		placed(workspace("w-z", "zulu"), 1),
	)

	// Act.
	r.SetRegistry(reg)

	// Assert.
	want := []string{"zulu", "alpha"}
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, want) {
		t.Fatalf("order = %v, want %v", got, want)
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

func TestRosterOrdersChildrenTheSameWayWithinTheirParent(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	parent = placed(parent, 1)
	childLate := placed(workspace("w-late", "aaa-child"), 3)
	childLate.Parent = &parent.ID
	childEarly := placed(workspace("w-early", "zzz-child"), 2)
	childEarly.Parent = &parent.ID

	// Act.
	r.SetRegistry(registry(parent, childLate, childEarly))

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

func TestRecentlyMergedOrdersByMergeTimeNewestFirst(t *testing.T) {
	// Arrange: the earlier merge holds the EARLIER place, so place and merge
	// time disagree.
	r, _ := newResolver(t)
	older := placed(workspace("w-older", "older"), 1)
	older.MergedAt = at(0)
	newer := placed(workspace("w-newer", "newer"), 2)
	newer.MergedAt = at(time.Hour)

	// Act.
	r.SetRegistry(registry(older, newer))

	// Assert: merge time, newest first (owner request, 2026-10-13).
	got := rowNames(rosterwalk.MergedRows(latest(t, r)))
	if !equal(got, []string{"newer", "older"}) {
		t.Fatalf("merged order = %v, want the latest merge first [newer older]", got)
	}
}

func TestSortMergedNewestFirst(t *testing.T) {
	tests := []struct {
		name string
		in   []wsm.Workspace
		want []string
	}{
		{
			name: "the latest merge leads whatever its place",
			in: []wsm.Workspace{
				withMergedAt(placed(workspace("w1", "first"), 1), at(0)),
				withMergedAt(placed(workspace("w2", "second"), 2), at(2*time.Hour)),
				withMergedAt(placed(workspace("w3", "third"), 3), at(time.Hour)),
			},
			want: []string{"second", "third", "first"},
		},
		{
			name: "a tie keeps roster order",
			in: []wsm.Workspace{
				withMergedAt(placed(workspace("w2", "later-place"), 2), at(0)),
				withMergedAt(placed(workspace("w1", "earlier-place"), 1), at(0)),
			},
			want: []string{"earlier-place", "later-place"},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := sidebar.SortMergedNewestFirst(tc.in)

			// Assert.
			var names []string
			for _, ws := range got {
				names = append(names, ws.Name)
			}
			if !equal(names, tc.want) {
				t.Fatalf("order = %v, want %v", names, tc.want)
			}
		})
	}
}

func TestSortMergedNewestFirstRefusesARowWithNoMergeInstant(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a merged row with no merge instant was sorted, not refused")
		}
	}()

	// Act.
	sidebar.SortMergedNewestFirst([]wsm.Workspace{workspace("w1", "unmerged")})
}

// withMergedAt stamps WS merged at AT.
func withMergedAt(ws wsm.Workspace, at *time.Time) wsm.Workspace {
	ws.MergedAt = at
	return ws
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

// tabIDsOf is the published tab bar's workspace ids and their visibility arm.
func tabIDsOf(roster *frontendv1.WorkspaceRoster) []string {
	out := []string{}
	for _, tab := range roster.GetTabBar().GetTabs() {
		arm := "drawn"
		switch {
		case tab.GetWithheldHidden() != nil:
			arm = "hidden"
		case tab.GetWithheldCollapsed() != nil:
			arm = "collapsed"
		case tab.GetDrawn() == nil:
			arm = "unset"
		}
		out = append(out, tab.GetWorkspace().GetId()+":"+arm)
	}
	return out
}

func TestTheTabBarFollowsRegistryOrderAcrossRepositories(t *testing.T) {
	// Arrange: two repositories whose workspaces interleave in the registry.
	r, _ := newResolver(t)
	second := wsm.Repository{ID: "repo-2", Dir: "/repos/two", Name: "beta", DefaultBranch: "main"}
	other := placed(workspace("w-b", "b"), 2)
	other.Repo = second.ID
	reg := registry(placed(workspace("w-a", "a"), 1), other, placed(workspace("w-c", "c"), 3))
	reg.Repositories = append(reg.Repositories, second)

	// Act.
	r.SetRegistry(reg)

	// Assert.
	want := []string{"w-a:drawn", "w-b:drawn", "w-c:drawn"}
	if got := tabIDsOf(latest(t, r)); !equal(got, want) {
		t.Fatalf("tab bar = %v, want %v", got, want)
	}
}

func TestTheTabBarPutsAChildWhereTheRegistryDoes(t *testing.T) {
	// Arrange: a child placed after its parent's sibling.
	r, _ := newResolver(t)
	parent := placed(workspace("w-p", "parent"), 1)
	child := placed(workspace("w-k", "child"), 3)
	child.Parent = &parent.ID
	sibling := placed(workspace("w-s", "sibling"), 2)

	// Act.
	r.SetRegistry(registry(parent, child, sibling))

	// Assert: the sidebar nests the child, the tab bar does not.
	want := []string{"w-p:drawn", "w-s:drawn", "w-k:drawn"}
	if got := tabIDsOf(latest(t, r)); !equal(got, want) {
		t.Fatalf("tab bar = %v, want %v", got, want)
	}
}

func TestTheTabBarLeavesOutClosedWorkspaces(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	closed := placed(workspace("w-x", "closed"), 1)
	closed.Closed = true

	// Act.
	r.SetRegistry(registry(closed, placed(workspace("w-a", "a"), 2)))

	// Assert.
	if got := tabIDsOf(latest(t, r)); !equal(got, []string{"w-a:drawn"}) {
		t.Fatalf("tab bar = %v, want only the open workspace", got)
	}
}

func TestTheTabBarWithholdsAHiddenWorkspace(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	hidden := placed(hiddenWorkspace("w-h", "hidden"), 1)

	// Act.
	r.SetRegistry(registry(hidden))

	// Assert.
	if got := tabIDsOf(latest(t, r)); !equal(got, []string{"w-h:hidden"}) {
		t.Fatalf("tab bar = %v, want the hidden tab withheld", got)
	}
}

func TestTheTabBarWithholdsAWorkspaceOfACollapsedRepository(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(placed(workspace("w-a", "a"), 1))
	reg.Repositories[0].Folded = true

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if got := tabIDsOf(latest(t, r)); !equal(got, []string{"w-a:collapsed"}) {
		t.Fatalf("tab bar = %v, want the collapsed tab withheld", got)
	}
}

func TestTheTabBarDrawsAMergedWorkspaceEvenWhenHidden(t *testing.T) {
	// Arrange: a merged workspace files under recently merged whatever its
	// hidden flag, so its tab is drawn.
	r, _ := newResolver(t)
	merged := placed(workspace("w-m", "merged"), 1)
	merged.MergedAt = at(0)
	merged = hiddenAt(merged, 0)

	// Act.
	r.SetRegistry(registry(merged))

	// Assert.
	if got := tabIDsOf(latest(t, r)); !equal(got, []string{"w-m:drawn"}) {
		t.Fatalf("tab bar = %v, want the merged tab drawn", got)
	}
}

func TestTheTabBarWithholdsAMergedWorkspaceOfACollapsedRepository(t *testing.T) {
	// Arrange: its merged group follows the repository's fold, so its tab does.
	r, _ := newResolver(t)
	merged := placed(workspace("w-m", "merged"), 1)
	merged.MergedAt = at(0)
	reg := registry(merged)
	reg.Repositories[0].Folded = true

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if got := tabIDsOf(latest(t, r)); !equal(got, []string{"w-m:collapsed"}) {
		t.Fatalf("tab bar = %v, want the merged tab withheld", got)
	}
}

// tabsRegistry is one registry holding every kind of tab: drawn, hidden,
// collapsed, closed and hidden-merge, placed out of slice order.
func tabsRegistry() sidebar.Registry {
	folded := wsm.Repository{ID: "repo-2", Dir: "/repos/two", Name: "beta", DefaultBranch: "main", Folded: true}
	collapsed := placed(workspace("w-c", "collapsed"), 2)
	collapsed.Repo = folded.ID
	closed := placed(workspace("w-x", "closed"), 4)
	closed.Closed = true
	mergeHidden := placed(workspace("w-q", "merge"), 5)
	mergeHidden.MergeOf = &wsm.MergeOrigin{}
	reg := registry(placed(workspace("w-d", "drawn"), 3), collapsed, closed, mergeHidden, placed(hiddenWorkspace("w-h", "hidden"), 1))
	reg.Repositories = append(reg.Repositories, folded)
	return reg
}

func TestTabs(t *testing.T) {
	// Arrange
	reg := tabsRegistry()

	// Act
	got := sidebar.Tabs(reg)

	// Assert: registry order, closed and hidden-merge workspaces have no tab.
	want := []sidebar.Tab{
		{Workspace: reg.Workspaces[4], Withheld: wsm.WithheldHidden},
		{Workspace: reg.Workspaces[1], Withheld: wsm.WithheldCollapsed},
		{Workspace: reg.Workspaces[0], Withheld: wsm.Drawn},
	}
	if len(got) != len(want) {
		t.Fatalf("Tabs = %+v, want %+v", got, want)
	}
	for i := range want {
		if got[i].Workspace.ID != want[i].Workspace.ID || got[i].Withheld != want[i].Withheld {
			t.Fatalf("Tabs[%d] = (%q, %v), want (%q, %v)", i, got[i].Workspace.ID, got[i].Withheld, want[i].Workspace.ID, want[i].Withheld)
		}
	}
}

func TestTheTabBarIsDrawnFromTabs(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	reg := tabsRegistry()
	var want []string
	for _, tab := range sidebar.Tabs(reg) {
		want = append(want, string(tab.Workspace.ID)+":"+tab.Withheld.String())
	}

	// Act
	r.SetRegistry(reg)

	// Assert
	if got := tabIDsOf(latest(t, r)); !equal(got, want) {
		t.Fatalf("tab bar = %v, want Tabs %v", got, want)
	}
}

func TestSortHiddenNewestFirst(t *testing.T) {
	// Arrange: hide order disagrees with place.
	in := []wsm.Workspace{
		hiddenAt(placed(workspace("w1", "first"), 1), 0),
		hiddenAt(placed(workspace("w2", "second"), 2), 2*time.Hour),
		hiddenAt(placed(workspace("w3", "third"), 3), time.Hour),
	}

	// Act.
	got := sidebar.SortHiddenNewestFirst(in)

	// Assert.
	var names []string
	for _, ws := range got {
		names = append(names, ws.Name)
	}
	if !equal(names, []string{"second", "third", "first"}) {
		t.Fatalf("order = %v, want the latest hide first", names)
	}
}

func TestSortHiddenNewestFirstRefusesARowWithNoHideInstant(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a hidden row with no hide instant was sorted, not refused")
		}
	}()

	// Act.
	sidebar.SortHiddenNewestFirst([]wsm.Workspace{workspace("w1", "unstamped")})
}

func TestTheHiddenSectionListsTheLatestHideFirst(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(
		hiddenAt(placed(workspace("w1", "early"), 1), 0),
		hiddenAt(placed(workspace("w2", "late"), 2), time.Hour),
	))

	// Assert.
	got := rowNames(latest(t, r).GetHidden().GetRows().GetRows())
	if !equal(got, []string{"late", "early"}) {
		t.Fatalf("hidden rows = %v, want the latest hide first", got)
	}
}
