package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"slices"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"

	"google.golang.org/protobuf/proto"
)

func TestRepoSectionCarriesTheRepositoryJoinKey(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	key := latest(t, r).GetRepository().GetSections()[0].GetKey().GetRepository()
	if key.GetId() != string(repo.ID) || key.GetDir() != repo.Dir {
		t.Fatalf("repo key = %v, want the registry's identity and dir", key)
	}
}

func TestRepoSectionDrawsEvenWithNoRows(t *testing.T) {
	// Arrange: a registered repository whose only workspace merged.
	r, _ := newResolver(t)
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act.
	r.SetRegistry(registry(merged))

	// Assert: a section that vanished would make the grouping flicker.
	sections := latest(t, r).GetRepository().GetSections()
	if len(sections) != 1 || len(innerRows(rosterwalk.UnsectionedRows(sections[0]))) != 0 {
		t.Fatal("a repository section with no rows did not draw")
	}
}

// TestRepoSectionDrawsForARepositoryWithNoWorkspaceAtAll is the
// just-registered repository: RegisterRepository mints a repository row on its
// own, with nothing under it, and the section is the ONLY thing that says the
// registration worked. It is a different case from the section whose workspaces
// merged -- that one had rows once -- so it gets its own test.
func TestRepoSectionDrawsForARepositoryWithNoWorkspaceAtAll(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry())

	// Assert.
	sections := latest(t, r).GetRepository().GetSections()
	if len(sections) != 1 {
		t.Fatalf("the repository view carried %d sections, want the registered repository's own", len(sections))
	}
	if got := sections[0].GetKey().GetRepository().GetId(); got != string(repo.ID) {
		t.Fatalf("section key = %q, want the registered repository %q", got, repo.ID)
	}
	if rows := innerRows(rosterwalk.UnsectionedRows(sections[0])); len(rows) != 0 {
		t.Fatalf("the section drew %d rows, want none", len(rows))
	}
}

func TestRecentlyMergedHoistsOutOfTheRepositoryGrouping(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act.
	r.SetRegistry(registry(merged, workspace("w-live", "live")))

	// Assert.
	got := rowNames(repoRows(t, latest(t, r)))
	if !equal(got, []string{"live"}) {
		t.Fatalf("repo rows = %v, want the merged workspace hoisted out", got)
	}
}

func TestRecentlyMergedCarriesTheMergedRow(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act.
	r.SetRegistry(registry(merged))

	// Assert.
	got := rowNames(rosterwalk.MergedRows(latest(t, r)))
	if !equal(got, []string{"merged"}) {
		t.Fatalf("merged rows = %v, want the merged workspace", got)
	}
}

func TestRecentlyMergedComposesItsHeading(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry())

	// Assert.
	got := latest(t, r).GetRecentlyMerged().GetHeader().GetLabel().GetText()
	if got != "Recently Merged" {
		t.Fatalf("heading = %q, want the section's one label", got)
	}
}

func TestRosterRecordsAWorkspaceNamingAnUnregisteredRepository(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	stray := workspace("w-stray", "stray")
	stray.Repo = ids.RepoID("repo-missing")

	// Act.
	r.SetRegistry(registry(stray))

	// Assert.
	if !hasError(surfaces.Records(), "daemon.sidebar.repository_view") {
		t.Fatal("a workspace naming an unregistered repository was not recorded")
	}
}

// dirsInRoster walks the roster exactly as the sidecar's log forwarder does --
// the repository sections, the task sections and the recently-merged section,
// each row and its children -- and answers every workspace directory it can
// resolve a ref for.
func dirsInRoster(roster *frontendv1.WorkspaceRoster) map[string]bool {
	out := map[string]bool{}
	for _, row := range rosterwalk.AllRows(roster) {
		if ref := row.GetWorkspace().GetWorkspace(); ref.GetId() != "" && ref.GetDir() != "" {
			out[ref.GetDir()] = true
		}
	}
	return out
}

// TestEveryRegisteredWorkspaceIsResolvableFromTheRoster pins the guarantee the
// LOG PLANE rests on. The sidecar resolves a record's workspace by walking the
// delivered roster, and a registered workspace the roster does not name is one
// whose file-scoped records fall back to the global sink with
// `forward_undelivered`. So the roster's completeness is not a rendering
// nicety: every registry row must be findable there, whatever state it is in.
func TestEveryRegisteredWorkspaceIsResolvableFromTheRoster(t *testing.T) {
	task := wsm.Task{ID: ids.TaskID("task-1"), Title: "the task", CreatedAt: epoch}
	assign := func(ws wsm.Workspace) wsm.Workspace {
		id := task.ID
		ws.Task = &id
		return ws
	}
	close := func(ws wsm.Workspace) wsm.Workspace {
		ws.Closed = true
		return ws
	}
	merge := func(ws wsm.Workspace) wsm.Workspace {
		at := epoch
		ws.MergedAt = &at
		ws.Closed = true
		return ws
	}

	tests := []struct {
		name string
		ws   wsm.Workspace
	}{
		{name: "an ordinary open workspace", ws: workspace("w-open", "open")},
		{name: "a workspace assigned to no task", ws: workspace("w-unassigned", "unassigned")},
		{name: "a workspace assigned to a task", ws: assign(workspace("w-assigned", "assigned"))},
		{name: "a closed workspace", ws: close(workspace("w-closed", "closed"))},
		{name: "a merged workspace", ws: merge(workspace("w-merged", "merged"))},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(tt.ws)
			reg.Tasks = []wsm.Task{task}

			// Act.
			r.SetRegistry(reg)

			// Assert.
			if !dirsInRoster(latest(t, r))[tt.ws.Dir] {
				t.Fatalf("the roster names no ref for %q; its records cannot be forwarded file-scoped", tt.ws.Dir)
			}
		})
	}
}

// The remediation the roster's assertion names is the one the state store's
// own boot check names. A person who meets the violation in either log is told
// the same thing to do about it.
func TestTheRosterAssertionNamesTheSameRemedyAsTheStore(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	stray := workspace("w-stray", "stray")
	stray.Repo = ids.RepoID("repo-missing")

	// Act.
	r.SetRegistry(registry(stray))

	// Assert.
	const want = "re-register the workspace's directory, which mints its repository row, or forget the workspace"
	for _, rec := range surfaces.Records() {
		if rec.Operation != "daemon.sidebar.repository_view" || rec.Level != "error" {
			continue
		}
		if rec.Context["remediation"] != want {
			t.Fatalf("remediation = %v, want %q", rec.Context["remediation"], want)
		}
		return
	}
	t.Fatal("the roster recorded no repository-invariant assertion")
}

// With the invariant held, the repo grouping DRAWS EVERY WORKSPACE and records
// nothing. It is the positive half of the assertion above: the drop the
// assertion reports is a defect, so a good registry must never take one.
func TestTheRepoGroupingDrawsEveryWorkspaceOfAGoodRegistry(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	one, two, three := workspace("w-1", "one"), workspace("w-2", "two"), workspace("w-3", "three")

	// Act.
	r.SetRegistry(registry(one, two, three))

	// Assert.
	got := rowNames(repoRows(t, latest(t, r)))
	// The order is the roster's own; the subject is that nothing is MISSING.
	if !equal(got, []string{"one", "two", "three"}) {
		t.Fatalf("repo grouping rows = %v, want every workspace", got)
	}
	if hasError(surfaces.Records(), "daemon.sidebar.repository_view") {
		t.Fatal("a good registry recorded a repository-invariant assertion")
	}
}

func TestRepositorySectionStatesItsFold(t *testing.T) {
	tests := []struct {
		name          string
		folded        bool
		wantCollapsed bool
	}{
		{"an expanded repository", false, false},
		{"a collapsed repository", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(workspace("w1", "one"))
			reg.Repositories[0].Folded = tt.folded

			// Act.
			r.SetRegistry(reg)

			// Assert: the arm is always set, and it is the recorded fold.
			section := latest(t, r).GetRepository().GetSections()[0]
			if (section.GetCollapsed() != nil) != tt.wantCollapsed || (section.GetExpanded() != nil) == tt.wantCollapsed {
				t.Fatalf("fold = %v, want collapsed=%v", section.GetFold(), tt.wantCollapsed)
			}
		})
	}
}

func TestMergedSectionStatesItsFold(t *testing.T) {
	tests := []struct {
		name          string
		folded        bool
		wantCollapsed bool
	}{
		{"an expanded band", false, false},
		{"a collapsed band", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(workspace("w1", "one"))
			reg.View.MergedFolded = tt.folded

			// Act.
			r.SetRegistry(reg)

			// Assert: the arm is always set, and it is the recorded fold.
			section := latest(t, r).GetRecentlyMerged()
			if (section.GetCollapsed() != nil) != tt.wantCollapsed || (section.GetExpanded() != nil) == tt.wantCollapsed {
				t.Fatalf("fold = %v, want collapsed=%v", section.GetFold(), tt.wantCollapsed)
			}
		})
	}
}

func TestRepoSectionCountsEveryRowItsRegionCarries(t *testing.T) {
	// Arrange: two unrelated workspaces.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	got := latest(t, r).GetRepository().GetSections()[0].GetHeader().GetCount().GetWorkspaces()
	if got != 2 {
		t.Fatalf("count = %d, want 2", got)
	}
}

func TestRepoSectionCountIncludesANestedFamilyRow(t *testing.T) {
	// Arrange: a child nested under its recorded parent.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	child := workspace("w-child", "child")
	child.Parent = &parent.ID

	// Act.
	r.SetRegistry(registry(parent, child))

	// Assert: one top-level row, two workspaces.
	section := latest(t, r).GetRepository().GetSections()[0]
	if top := len(rosterwalk.UnsectionedRows(section)); top != 1 {
		t.Fatalf("top-level rows = %d, want the child nested under its parent", top)
	}
	got := section.GetHeader().GetCount().GetWorkspaces()
	if got != 2 {
		t.Fatalf("count = %d, want 2 (the nested child counted)", got)
	}
}

func TestRepoSectionCountLeavesOutAClosedRow(t *testing.T) {
	// Arrange: one live workspace and one closed one, which rides the wire but
	// no client draws.
	r, _ := newResolver(t)
	closed := workspace("w-closed", "closed")
	closed.Closed = true

	// Act.
	r.SetRegistry(registry(workspace("w-live", "live"), closed))

	// Assert: the count is the rows the unfolded section shows.
	got := latest(t, r).GetRepository().GetSections()[0].GetHeader().GetCount().GetWorkspaces()
	if got != 1 {
		t.Fatalf("count = %d, want 1 (the closed row is not drawn)", got)
	}
}

func TestRepoSectionCountKeepsALiveChildOfAClosedParent(t *testing.T) {
	// Arrange: a live child nested under a closed parent, which the client
	// drops while hoisting the child into its place.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	parent.Closed = true
	child := workspace("w-child", "child")
	child.Parent = &parent.ID

	// Act.
	r.SetRegistry(registry(parent, child))

	// Assert.
	got := latest(t, r).GetRepository().GetSections()[0].GetHeader().GetCount().GetWorkspaces()
	if got != 1 {
		t.Fatalf("count = %d, want 1 (the hoisted live child)", got)
	}
}

func TestAnEmptyRepoSectionStatesACountOfZero(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry())

	// Assert: always set, even at zero.
	count := latest(t, r).GetRepository().GetSections()[0].GetHeader().GetCount()
	if count == nil || count.GetWorkspaces() != 0 {
		t.Fatalf("count = %v, want set to 0", count)
	}
}

func TestRecentlyMergedCountsItsRows(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act.
	r.SetRegistry(registry(merged, workspace("w-live", "live")))

	// Assert.
	got := latest(t, r).GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces()
	if got != 1 {
		t.Fatalf("count = %d, want 1", got)
	}
}

func TestRecentlyMergedCountsEveryMergedRowThoughEachIsClosed(t *testing.T) {
	// Arrange: two merges, whose rows are closed by design and are exactly
	// what the band draws.
	r, _ := newResolver(t)
	first := workspace("w-a", "a")
	first.MergedAt = at(0)
	first.Closed = true
	second := workspace("w-b", "b")
	second.MergedAt = at(1)
	second.Closed = true

	// Act.
	r.SetRegistry(registry(first, second))

	// Assert.
	got := latest(t, r).GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces()
	if got != 2 {
		t.Fatalf("count = %d, want 2", got)
	}
}

// hiddenWorkspace is a registered workspace the user hid at the epoch.
func hiddenWorkspace(id, name string) wsm.Workspace {
	return hiddenAt(workspace(id, name), 0)
}

// hiddenAt is WS hidden D after the epoch: hidden and its instant, together,
// as wsm records them.
func hiddenAt(ws wsm.Workspace, d time.Duration) wsm.Workspace {
	ws.Hidden = true
	ws.HiddenAt = at(d)
	return ws
}

func TestAHiddenWorkspaceHoistsOutOfTheRepositorySections(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(hiddenWorkspace("w-hidden", "hidden"), workspace("w-live", "live"))

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if got := rowNames(repoRows(t, latest(t, r))); !equal(got, []string{"live"}) {
		t.Fatalf("repo rows = %v, want the hidden workspace hoisted out", got)
	}
}

func TestTheHiddenSectionCarriesTheHiddenRow(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(hiddenWorkspace("w-hidden", "hidden"), workspace("w-live", "live")))

	// Assert.
	section := latest(t, r).GetHidden()
	if got := rowNames(topHidden(latest(t, r))); !equal(got, []string{"hidden"}) {
		t.Fatalf("hidden rows = %v, want the one hidden workspace", got)
	}
	if got := section.GetHeader().GetLabel().GetText(); got != sidebar.HiddenSectionLabel {
		t.Fatalf("hidden label = %q, want %q", got, sidebar.HiddenSectionLabel)
	}
	if got := section.GetHeader().GetCount().GetWorkspaces(); got != 1 {
		t.Fatalf("hidden count = %d, want 1", got)
	}
}

func TestTheHiddenSectionIsOnTheWireWithNothingHidden(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w-live", "live")))

	// Assert.
	section := latest(t, r).GetHidden()
	if section == nil || section.GetHeader() == nil || section.GetFold() == nil {
		t.Fatalf("hidden section = %v, want it whole with no rows", section)
	}
	if len(section.GetHiddenRows().GetRows()) != 0 || section.GetHeader().GetCount().GetWorkspaces() != 0 {
		t.Fatalf("hidden section = %v, want no rows and a zero count", section)
	}
}

func TestAClosedHiddenWorkspaceIsInNoSection(t *testing.T) {
	// Arrange: a closed workspace is listed nowhere but recently merged.
	r, _ := newResolver(t)
	closed := hiddenWorkspace("w-closed", "closed")
	closed.Closed = true

	// Act.
	r.SetRegistry(registry(closed, hiddenWorkspace("w-open", "open")))

	// Assert.
	section := latest(t, r).GetHidden()
	if got := rowNames(topHidden(latest(t, r))); !equal(got, []string{"open"}) {
		t.Fatalf("hidden rows = %v, want only the open one", got)
	}
	if got := section.GetHeader().GetCount().GetWorkspaces(); got != 1 {
		t.Fatalf("hidden count = %d, want 1", got)
	}
}

func TestAMergedHiddenWorkspaceFilesUnderRecentlyMerged(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := hiddenWorkspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true

	// Act.
	r.SetRegistry(registry(merged))

	// Assert.
	roster := latest(t, r)
	if len(topHidden(roster)) != 0 {
		t.Fatal("a merged workspace was filed under hidden")
	}
	if got := rowNames(rosterwalk.MergedRows(roster)); !equal(got, []string{"merged"}) {
		t.Fatalf("merged rows = %v, want the merged workspace", got)
	}
}

func TestHiddenSectionStatesItsFold(t *testing.T) {
	tests := []struct {
		name          string
		folded        bool
		wantCollapsed bool
	}{
		{"an expanded section", false, false},
		{"a collapsed section", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(hiddenWorkspace("w1", "one"))
			reg.View.HiddenFolded = tt.folded

			// Act.
			r.SetRegistry(reg)

			// Assert: the arm is always set, and it is the recorded fold.
			section := latest(t, r).GetHidden()
			if (section.GetCollapsed() != nil) != tt.wantCollapsed || (section.GetExpanded() != nil) == tt.wantCollapsed {
				t.Fatalf("fold = %v, want collapsed=%v", section.GetFold(), tt.wantCollapsed)
			}
		})
	}
}

// mergedIn is a workspace of REPOSITORY merged at offset D.
func mergedIn(id string, repository wsm.Repository, d time.Duration) wsm.Workspace {
	ws := workspace(id, id)
	ws.Repo = repository.ID
	ws.MergedAt = at(d)
	ws.Closed = true
	return ws
}

func TestRecentlyMergedGroupsByRepositoryInTheRepositoryGroupingsOrder(t *testing.T) {
	// Arrange: beta is listed first, alpha's place is first.
	r, _ := newResolver(t)
	reg := twoRepoRegistry(mergedIn("b1", otherRepo, 0), mergedIn("a1", repo, 0))
	reg.Repositories = []wsm.Repository{otherRepo, repo}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	groups := latest(t, r).GetRecentlyMerged().GetGroups()
	var got []string
	for _, g := range groups {
		got = append(got, g.GetHeader().GetLabel().GetText()+"="+g.GetKey().GetRepository().GetId())
	}
	want := []string{"alpha=" + string(repo.ID), "beta=" + string(otherRepo.ID)}
	if !equal(got, want) {
		t.Fatalf("groups = %v, want %v", got, want)
	}
}

func TestRecentlyMergedGroupKeepsTheRepositoryDir(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(mergedIn("a1", repo, 0)))

	// Assert: the key is the repository section's own join key.
	group := latest(t, r).GetRecentlyMerged().GetGroups()[0]
	section := latest(t, r).GetRepository().GetSections()[0]
	if !proto.Equal(group.GetKey(), section.GetKey()) {
		t.Fatalf("group key = %v, want the repository section's %v", group.GetKey(), section.GetKey())
	}
}

func TestRecentlyMergedGroupOrdersItsRowsByMergeTime(t *testing.T) {
	// Arrange: the later merge holds the later place.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(placed(mergedIn("older", repo, 0), 1), placed(mergedIn("newer", repo, time.Minute), 2)))

	// Assert: the latest merge first (owner request, 2026-10-13).
	got := rowNames(rosterwalk.MergedRows(latest(t, r)))
	if !equal(got, []string{"newer", "older"}) {
		t.Fatalf("group rows = %v, want the latest merge first", got)
	}
}

func TestRecentlyMergedLeavesARepositoryWithNoMergeWithoutAGroup(t *testing.T) {
	// Arrange: beta has only a live workspace.
	r, _ := newResolver(t)
	live := workspace("b-live", "b-live")
	live.Repo = otherRepo.ID
	reg := twoRepoRegistry(live, mergedIn("a1", repo, 0))

	// Act.
	r.SetRegistry(reg)

	// Assert.
	groups := latest(t, r).GetRecentlyMerged().GetGroups()
	if len(groups) != 1 || groups[0].GetKey().GetRepository().GetId() != string(repo.ID) {
		t.Fatalf("groups = %v, want only alpha's", groups)
	}
}

func TestRecentlyMergedGroupFollowsItsRepositorysFold(t *testing.T) {
	tests := []struct {
		name          string
		folded        bool
		wantCollapsed bool
	}{
		{"an expanded repository's group is shown", false, false},
		{"a collapsed repository's group is not", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(mergedIn("a1", repo, 0))
			reg.Repositories[0].Folded = tt.folded

			// Act.
			r.SetRegistry(reg)

			// Assert: the arm is always set, and it is the repository's fold.
			group := latest(t, r).GetRecentlyMerged().GetGroups()[0]
			if (group.GetRepositoryCollapsed() != nil) != tt.wantCollapsed || (group.GetShown() != nil) == tt.wantCollapsed {
				t.Fatalf("drawn = %v, want repository_collapsed=%v", group.GetDrawn(), tt.wantCollapsed)
			}
		})
	}
}

func TestRecentlyMergedKeepsACollapsedRepositorysRowsOnTheWire(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry(mergedIn("a1", repo, 0))
	reg.Repositories[0].Folded = true

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if got := rowNames(rosterwalk.MergedRows(latest(t, r))); !equal(got, []string{"a1"}) {
		t.Fatalf("merged rows = %v, want the collapsed repository's merge still carried", got)
	}
}

func TestRecentlyMergedCountLeavesOutACollapsedRepositorysRows(t *testing.T) {
	// Arrange: beta is collapsed and has two merges; alpha has one.
	r, _ := newResolver(t)
	reg := twoRepoRegistry(mergedIn("b1", otherRepo, 0), mergedIn("b2", otherRepo, 0), mergedIn("a1", repo, 0))
	reg.Repositories[1].Folded = true

	// Act.
	r.SetRegistry(reg)

	// Assert.
	if got := latest(t, r).GetRecentlyMerged().GetHeader().GetCount().GetWorkspaces(); got != 1 {
		t.Fatalf("count = %d, want only the expanded repository's 1", got)
	}
}

func TestRecentlyMergedRecordsAMergeNamingAnUnregisteredRepository(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	stray := mergedIn("stray", wsm.Repository{ID: ids.RepoID("repo-missing")}, 0)

	// Act.
	r.SetRegistry(registry(stray, mergedIn("a1", repo, 0)))

	// Assert: recorded once with its diagnosis, and the band keeps the rest.
	var found []dlog.Record
	for _, rec := range surfaces.Records() {
		if rec.Level == "error" && rec.Operation == "daemon.sidebar.merged_section" {
			found = append(found, rec)
		}
	}
	if len(found) != 1 || found[0].Context["repo_id"] != "repo-missing" || found[0].Context["workspaces"] != 1 {
		t.Fatalf("records = %v, want one naming repo-missing and its 1 workspace", found)
	}
	if got := rowNames(rosterwalk.MergedRows(latest(t, r))); !equal(got, []string{"a1"}) {
		t.Fatalf("merged rows = %v, want the registered repository's merge", got)
	}
}

func TestRepoSectionNeverMeasuredIsPending(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry())

	// Assert.
	divergence := latest(t, r).GetRepository().GetSections()[0].GetDivergence()
	if divergence.GetPending() == nil {
		t.Fatalf("divergence = %v, want the pending arm", divergence)
	}
}

func TestRepoSectionDrawsItsDivergenceMeasure(t *testing.T) {
	tests := []struct {
		name    string
		measure repodivergence.Measure
		want    *frontendv1.RosterRepoDivergence
	}{
		{
			name:    "counted",
			measure: repodivergence.Counted{Branch: "master", Upstream: "origin/master", Ahead: 3, Behind: 9},
			want: &frontendv1.RosterRepoDivergence{State: &frontendv1.RosterRepoDivergence_Counted{
				Counted: &frontendv1.RosterRepoDivergenceCounted{Branch: "master", Upstream: "origin/master", Ahead: 3, Behind: 9}}},
		},
		{
			name:    "no upstream",
			measure: repodivergence.NoUpstream{Branch: "main"},
			want: &frontendv1.RosterRepoDivergence{State: &frontendv1.RosterRepoDivergence_NoUpstream{
				NoUpstream: &frontendv1.RosterRepoDivergenceNoUpstream{Branch: "main"}}},
		},
		{
			name:    "failed",
			measure: repodivergence.Failed{Reason: "fetching origin failed: offline"},
			want: &frontendv1.RosterRepoDivergence{State: &frontendv1.RosterRepoDivergence_Failed{
				Failed: &frontendv1.RosterRepoDivergenceFailed{Reason: "fetching origin failed: offline"}}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(registry())

			// Act.
			r.SetRepoDivergence(repo.ID, tc.measure)

			// Assert: the measure is pushed on its own, not on the next registry.
			got := latest(t, r).GetRepository().GetSections()[0].GetDivergence()
			if !proto.Equal(got, tc.want) {
				t.Fatalf("divergence = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestRepoDivergenceMeasuredBeforeTheRegistryIsDrawnOnceItArrives(t *testing.T) {
	// Arrange: the startup sweep can finish before the first registry.
	r, _ := newResolver(t)
	r.SetRepoDivergence(repo.ID, repodivergence.NoUpstream{Branch: "main"})

	// Act.
	r.SetRegistry(registry())

	// Assert.
	if got := latest(t, r).GetRepository().GetSections()[0].GetDivergence().GetNoUpstream().GetBranch(); got != "main" {
		t.Fatalf("no-upstream branch = %q, want the measure taken before the registry", got)
	}
}

func TestRepoDivergenceRefusesAMissingMeasure(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(registry())
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("a nil measure was accepted; it is a caller's broken invariant")
		}
	}()

	// Act.
	r.SetRepoDivergence(repo.ID, nil)
}

func TestTheHiddenSectionIsLabelledHidden(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(hiddenWorkspace("w-hidden", "hidden")))

	// Assert.
	if got := latest(t, r).GetHidden().GetHeader().GetLabel().GetText(); got != "Hidden" {
		t.Fatalf("hidden label = %q, want %q", got, "Hidden")
	}
}

func TestAHiddenRowCountsFromItsHide(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(hiddenAt(workspace("w1", "one"), time.Hour)))

	// Assert.
	got := topHidden(latest(t, r))[0].GetWhen().GetHidden()
	if got == nil || got.GetAtMs() != at(time.Hour).UnixMilli() {
		t.Fatalf("when = %v, want the hide instant", topHidden(latest(t, r))[0].GetWhen())
	}
}

func TestAMergedHiddenRowCountsFromItsMerge(t *testing.T) {
	// Arrange: merged wins over hidden, as it files under Recently Merged.
	r, _ := newResolver(t)
	ws := hiddenAt(workspace("w1", "one"), time.Hour)
	ws.MergedAt = at(2 * time.Hour)
	ws.Closed = true

	// Act.
	r.SetRegistry(registry(ws))

	// Assert.
	rows := rosterwalk.MergedRows(latest(t, r))
	if len(rows) != 1 || rows[0].GetWhen().GetMerged() == nil {
		t.Fatalf("merged rows = %v, want one counting from its merge", rows)
	}
}

func TestTheOpenSectionIsLabelledOpen(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(registry(workspace("w1", "one")))

	// Assert.
	if got := latest(t, r).GetOpen().GetLabel().GetText(); got != "Open" {
		t.Fatalf("open label = %q, want %q", got, "Open")
	}
}

func TestOpenSectionStatesItsFold(t *testing.T) {
	tests := []struct {
		name          string
		folded        bool
		wantCollapsed bool
	}{
		{"an expanded section", false, false},
		{"a collapsed section", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(workspace("w1", "one"))
			reg.View.OpenFolded = tt.folded

			// Act.
			r.SetRegistry(reg)

			// Assert: the arm is always set, and it is the recorded fold.
			section := latest(t, r).GetOpen()
			if (section.GetCollapsed() != nil) != tt.wantCollapsed || (section.GetExpanded() != nil) == tt.wantCollapsed {
				t.Fatalf("fold = %v, want collapsed=%v", section.GetFold(), tt.wantCollapsed)
			}
		})
	}
}

func TestTheOpenCountCountsTheRowsTheOpenSectionLists(t *testing.T) {
	// Arrange: a family of two, a workspace in another repository, a closed
	// one, a hidden one and a merged one.
	r, _ := newResolver(t)
	parent := workspace("w-parent", "parent")
	child := workspace("w-child", "child")
	child.Parent = &parent.ID
	other := workspace("w-other", "other")
	other.Repo = otherRepo.ID
	closed := workspace("w-closed", "closed")
	closed.Closed = true
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Closed = true
	reg := twoRepoRegistry(parent, child, other, closed, hiddenWorkspace("w-hidden", "hidden"), merged)

	// Act.
	r.SetRegistry(reg)

	// Assert: the nested child counts; the closed, hidden and merged do not.
	if got := latest(t, r).GetRepository().GetOpenCount().GetWorkspaces(); got != 3 {
		t.Fatalf("open count = %d, want 3", got)
	}
}

func TestTheOpenCountIsTheSumOfTheSectionCounts(t *testing.T) {
	// Arrange: a folded repository section still counts toward the open count.
	r, _ := newResolver(t)
	reg := twoRepoRegistry(workspace("w1", "one"), func() wsm.Workspace {
		ws := workspace("w2", "two")
		ws.Repo = otherRepo.ID
		return ws
	}())
	reg.Repositories[1].Folded = true

	// Act.
	r.SetRegistry(reg)

	// Assert.
	view := latest(t, r).GetRepository()
	var sum uint32
	for _, section := range view.GetSections() {
		sum += section.GetHeader().GetCount().GetWorkspaces()
	}
	if got := view.GetOpenCount().GetWorkspaces(); got != sum || got != 2 {
		t.Fatalf("open count = %d, want the sections' sum %d (2)", got, sum)
	}
}

// collapseArms is SECTION's own collapse arm, then each subsection's, as
// "collapsible" or "holds_current" ("collapsed" for a collapsed one).
func collapseArms(section *frontendv1.RosterRepoSection) []string {
	var out []string
	switch {
	case section.GetCollapsed() != nil:
		out = append(out, "collapsed")
	case section.GetExpanded().GetHoldsCurrent() != nil:
		out = append(out, "holds_current")
	case section.GetExpanded().GetCollapsible() != nil:
		out = append(out, "collapsible")
	default:
		out = append(out, "unset")
	}
	for _, sub := range rosterwalk.Subsections(section) {
		switch {
		case sub.GetCollapsed() != nil:
			out = append(out, "collapsed")
		case sub.GetExpanded().GetHoldsCurrent() != nil:
			out = append(out, "holds_current")
		case sub.GetExpanded().GetCollapsible() != nil:
			out = append(out, "collapsible")
		default:
			out = append(out, "unset")
		}
	}
	return out
}

func TestAnExpandedSectionSaysWhetherItHoldsTheCurrentWorkspace(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(reg *sidebar.Registry)
		// want is the repository's arm, then the empty subsection's, then later's.
		want []string
	}{
		{
			name:    "nothing is current",
			arrange: func(*sidebar.Registry) {},
			want:    []string{"collapsible", "collapsible", "collapsible"},
		},
		{
			name:    "the current workspace sits in no subsection",
			arrange: func(reg *sidebar.Registry) { current := ids.WorkspaceID("w2"); reg.Current = &current },
			want:    []string{"holds_current", "collapsible", "collapsible"},
		},
		{
			name:    "the current workspace sits in a subsection",
			arrange: func(reg *sidebar.Registry) { current := ids.WorkspaceID("w1"); reg.Current = &current },
			want:    []string{"holds_current", "collapsible", "holds_current"},
		},
		{
			name: "a collapsed section states no collapse arm",
			arrange: func(reg *sidebar.Registry) {
				current := ids.WorkspaceID("w2")
				reg.Current = &current
				reg.Repositories[0].Folded = true
			},
			want: []string{"collapsed", "collapsible", "collapsible"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			r, _ := newResolver(t)
			reg := arrangedRegistry()
			tt.arrange(&reg)

			// Act
			r.SetRegistry(reg)

			// Assert
			if got := collapseArms(latest(t, r).GetRepository().GetSections()[0]); !equal(got, tt.want) {
				t.Fatalf("collapse arms = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestASectionsCollapseArmFollowsASwitch(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	reg := arrangedRegistry()
	current := ids.WorkspaceID("w2")
	reg.Current = &current
	r.SetRegistry(reg)

	// Act
	r.SetSelected("w1")

	// Assert
	if got := collapseArms(latest(t, r).GetRepository().GetSections()[0]); !equal(got, []string{"holds_current", "collapsible", "holds_current"}) {
		t.Fatalf("collapse arms = %v, want later to hold the switched-to workspace", got)
	}
}

// queueView is a registry showing the merge queues, carrying WS across the
// fixture repository and otherRepo.
func queueView(ws ...wsm.Workspace) sidebar.Registry {
	reg := twoRepoRegistry(ws...)
	reg.View.OpenShowsMergeQueues = true
	return reg
}

// inLine is a line of sync merges, each running in and credited to its own
// workspace, front first; the front is admitted.
func inLine(ws ...ids.WorkspaceID) []sidebar.QueueEntry {
	out := make([]sidebar.QueueEntry, 0, len(ws))
	for i, w := range ws {
		out = append(out, sidebar.QueueEntry{Workspace: w, Credited: w, Admitted: i == 0})
	}
	return out
}

// queueSubjectIDs reads the workspace ids a section's merge queue draws: its
// head's, then each waiting merge's.
func queueSubjectIDs(section *frontendv1.RosterRepoSection) []string {
	queue := section.GetMergeQueue()
	var out []string
	if head := queue.GetHead(); head != nil {
		out = append(out, head.GetSubject().GetWorkspace().GetId())
	}
	for _, waiting := range queue.GetWaiting() {
		out = append(out, waiting.GetSubject().GetWorkspace().GetId())
	}
	return out
}

// firstQueue reads the first repository section's merge queue.
func firstQueue(t *testing.T, r sidebar.Resolver) *frontendv1.RosterRepoMergeQueue {
	t.Helper()
	return latest(t, r).GetRepository().GetSections()[0].GetMergeQueue()
}

func TestOpenSectionStatesWhatItsRepositorySectionsShow(t *testing.T) {
	tests := []struct {
		name            string
		mergeQueues     bool
		wantMergeQueues bool
	}{
		{"the workspaces by default", false, false},
		{"the merge queues once asked", true, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry(workspace("w1", "one"))
			reg.View.OpenShowsMergeQueues = tt.mergeQueues

			// Act.
			r.SetRegistry(reg)

			// Assert: the arm is always set, and it is the recorded view.
			section := latest(t, r).GetOpen()
			if (section.GetMergeQueues() != nil) != tt.wantMergeQueues || (section.GetWorkspaces() != nil) == tt.wantMergeQueues {
				t.Fatalf("shows = %v, want merge queues=%v", section.GetShows(), tt.wantMergeQueues)
			}
		})
	}
}

func TestOpenSectionMarksAPausedMergeQueue(t *testing.T) {
	// queueStep states one repository's queue; a case's steps apply in order.
	type queueStep struct {
		repo  wsm.RepoKey
		queue sidebar.MergeQueue
	}
	running := sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1"}, Line: []ids.WorkspaceID{"w1"}}
	tests := []struct {
		name       string
		steps      []queueStep
		wantPaused bool
	}{
		{"no queue stated", nil, false},
		{"a running queue", []queueStep{{theRepo, running}}, false},
		{"a paused queue with a merge on it", []queueStep{{theRepo, sidebar.MergeQueue{Paused: true, Workspaces: running.Workspaces, Line: running.Line}}}, true},
		{"a paused queue with no merge on it", []queueStep{{theRepo, sidebar.MergeQueue{Paused: true}}}, true},
		{"one paused queue among running ones", []queueStep{{theRepo, running}, {wsm.RepoKey("/repos/two/.git"), sidebar.MergeQueue{Paused: true}}}, true},
		{"a pause since lifted", []queueStep{{theRepo, sidebar.MergeQueue{Paused: true}}, {theRepo, sidebar.MergeQueue{}}}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			for _, step := range tt.steps {
				r.SetMergeQueue(step.repo, step.queue)
			}

			// Act.
			r.SetRegistry(registry(workspace("w1", "one")))

			// Assert.
			if got := latest(t, r).GetOpen().GetMergeQueuePaused() != nil; got != tt.wantPaused {
				t.Fatalf("merge_queue_paused present = %v, want %v", got, tt.wantPaused)
			}
		})
	}
}

func TestEveryRepositorySectionShowsItsWorkspacesByDefault(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	other := workspace("w2", "two")
	other.Repo = otherRepo.ID
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1"}, Line: inLine("w1")})

	// Act.
	r.SetRegistry(twoRepoRegistry(workspace("w1", "one"), other))

	// Assert.
	for _, section := range latest(t, r).GetRepository().GetSections() {
		if section.GetWorkspaces() == nil || section.GetMergeQueue() != nil {
			t.Fatalf("section %s shows %v, want its workspaces", section.GetKey().GetRepository().GetId(), section.GetShows())
		}
	}
}

func TestARepositorySectionShowingItsMergeQueueDrawsTheFrontAsItsHeadAndTheRestWaiting(t *testing.T) {
	// Arrange: three of the fixture repository's workspaces in line, the
	// running one first, and one of otherRepo's on its own queue.
	r, _ := newResolver(t)
	other := workspace("w-other", "other")
	other.Repo = otherRepo.ID
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{
		Workspaces: []ids.WorkspaceID{"w3", "w1", "w2"},
		Line:       inLine("w3", "w1", "w2"),
	})
	r.SetMergeQueue(wsm.RepoKey("/repos/two/.git"), sidebar.MergeQueue{
		Workspaces: []ids.WorkspaceID{"w-other"},
		Line:       inLine("w-other"),
	})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two"), workspace("w3", "three"), workspace("w-idle", "idle"), other))

	// Assert.
	sections := latest(t, r).GetRepository().GetSections()
	if got := sections[0].GetMergeQueue().GetHead().GetSubject().GetName(); got != "three" {
		t.Fatalf("first section's head = %q, want the front, three", got)
	}
	if got, want := queueSubjectIDs(sections[0]), []string{"w3", "w1", "w2"}; !slices.Equal(got, want) {
		t.Fatalf("first section's queue = %v, want %v", got, want)
	}
	if got, want := queueSubjectIDs(sections[1]), []string{"w-other"}; !slices.Equal(got, want) {
		t.Fatalf("second section's queue = %v, want %v", got, want)
	}
}

func TestARepositorySectionShowingItsMergeQueueCountsTheMergesInLine(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1", "w2"}, Line: inLine("w1", "w2")})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two"), workspace("w3", "three")))

	// Assert: the header counts the two merges; the open count still counts
	// the three open workspaces.
	roster := latest(t, r)
	if got := roster.GetRepository().GetSections()[0].GetHeader().GetCount().GetWorkspaces(); got != 2 {
		t.Fatalf("header count = %d, want the 2 merges in line", got)
	}
	if got := roster.GetRepository().GetOpenCount().GetWorkspaces(); got != 3 {
		t.Fatalf("open count = %d, want the 3 open workspaces", got)
	}
}

func TestARepositorySectionShowingItsMergeQueueKeepsItsEntries(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	reg := registry(workspace("w1", "one"), workspace("w2", "two"))
	reg.View.OpenShowsMergeQueues = true

	// Act.
	r.SetRegistry(reg)

	// Assert: the tab bar still walks the entries.
	if got := rowNames(repoRows(t, latest(t, r))); !slices.Equal(got, []string{"one", "two"}) {
		t.Fatalf("entries = %v, want both open workspaces", got)
	}
}

func TestAnEmptyMergeQueueDrawsNothing(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert: the arm is set, with no head and nothing waiting, and the
	// header counts none.
	section := latest(t, r).GetRepository().GetSections()[0]
	queue := section.GetMergeQueue()
	if queue == nil || queue.GetHead() != nil || len(queue.GetWaiting()) != 0 {
		t.Fatalf("shows = %v, want an empty merge queue", section.GetShows())
	}
	if got := section.GetHeader().GetCount().GetWorkspaces(); got != 0 {
		t.Fatalf("header count = %d, want 0", got)
	}
}

func TestAnAsyncMergeIsDrawnAsItsRequesterNeverItsMergeWorkspace(t *testing.T) {
	// Arrange: an async merge's hidden merge workspace at the front, and
	// another waiting behind it.
	r, _ := newResolver(t)
	mergeWS := workspace("w-merge", "land-one")
	mergeWS.MergeOf = &wsm.MergeOrigin{Requester: "w1"}
	waitWS := workspace("w-merge2", "land-two")
	waitWS.MergeOf = &wsm.MergeOrigin{Requester: "w2"}
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{
		Workspaces: []ids.WorkspaceID{"w-merge", "w-merge2"},
		Line: []sidebar.QueueEntry{
			{Workspace: "w-merge", Credited: "w1", Async: true, Admitted: true},
			{Workspace: "w-merge2", Credited: "w2", Async: true},
		},
		AsyncRequesters: []ids.WorkspaceID{"w1", "w2"},
	})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two"), mergeWS, waitWS))

	// Assert.
	queue := firstQueue(t, r)
	head := queue.GetHead().GetSubject()
	if head.GetWorkspace().GetId() != "w1" || head.GetName() != "one" || head.GetMode().GetAsync() == nil {
		t.Fatalf("head = %v, want the requester one, async", head)
	}
	waiting := queue.GetWaiting()[0].GetSubject()
	if waiting.GetWorkspace().GetId() != "w2" || waiting.GetName() != "two" || waiting.GetMode().GetAsync() == nil {
		t.Fatalf("waiting = %v, want the requester two, async", waiting)
	}
}

func TestASyncMergeIsDrawnAsItsOwnWorkspace(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1"}, Line: inLine("w1")})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert.
	if head := firstQueue(t, r).GetHead().GetSubject(); head.GetName() != "one" || head.GetMode().GetSync() == nil {
		t.Fatalf("head = %v, want one, sync", head)
	}
}

func TestEveryMergeInTheQueueCarriesTheBubbleItReveals(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	line := inLine("w1", "w2")
	line[0].Reveal = &frontendv1.FeedId{Value: "bubble-1"}
	line[1].Reveal = &frontendv1.FeedId{Value: "bubble-2"}
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1", "w2"}, Line: line})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	queue := firstQueue(t, r)
	if got := queue.GetHead().GetSubject().GetReveal().GetValue(); got != "bubble-1" {
		t.Fatalf("head reveal = %q, want bubble-1", got)
	}
	if got := queue.GetWaiting()[0].GetSubject().GetReveal().GetValue(); got != "bubble-2" {
		t.Fatalf("waiting reveal = %q, want bubble-2", got)
	}
}

func TestTheHeadStatesItsStanding(t *testing.T) {
	queued := time.UnixMilli(7_000)
	tests := []struct {
		name     string
		admitted bool
		facts    *footer.MergeFacts
		want     string
		wantStep *string
	}{
		{name: "a front not yet admitted waits", want: "waiting"},
		{name: "an admitted front with no step yet merges with no step", admitted: true, want: "merging"},
		{name: "a rebasing front states its replay", admitted: true, facts: &footer.MergeFacts{State: "merging", Step: footer.StepRebasing, Replayed: 3, Total: 7}, want: "merging", wantStep: ptr("rebasing 3/7")},
		{name: "a fixing front states its attempt", admitted: true, facts: &footer.MergeFacts{State: "merging", Step: footer.StepFixing, Attempt: 2, MaxAttempts: 3}, want: "merging", wantStep: ptr("fixing attempt 2/3")},
		{name: "a testing front states its step", admitted: true, facts: &footer.MergeFacts{State: "merging", Step: footer.StepTesting}, want: "merging", wantStep: ptr("testing")},
		{name: "facts of no merge in flight state no step", admitted: true, facts: &footer.MergeFacts{State: "failed", Step: footer.StepTesting}, want: "merging"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetRegistry(queueView(workspace("w1", "one")))
			if tt.facts != nil {
				r.SetMerge("w1", *tt.facts)
			}

			// Act.
			r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1"},
				Line: []sidebar.QueueEntry{{Workspace: "w1", Credited: "w1", Admitted: tt.admitted, QueuedAt: queued}}})

			// Assert.
			head := firstQueue(t, r).GetHead()
			switch tt.want {
			case "waiting":
				if head.GetWaiting().GetQueuedAtMs() != queued.UnixMilli() {
					t.Fatalf("head = %v, want waiting since %d", head, queued.UnixMilli())
				}
			case "merging":
				merging := head.GetMerging()
				if merging == nil {
					t.Fatalf("head = %v, want merging", head)
				}
				if (merging.Step == nil) != (tt.wantStep == nil) || (tt.wantStep != nil && merging.GetStep() != *tt.wantStep) {
					t.Fatalf("step = %v, want %v", merging.Step, tt.wantStep)
				}
			}
		})
	}
}

func TestTheHeadCarriesItsStages(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	line := inLine("w1")
	line[0].Stages = []*frontendv1.RosterMergeQueueStage{{Label: "queue", Round: 1,
		State: &frontendv1.RosterMergeQueueStage_Live{Live: &frontendv1.RosterMergeQueueStageLive{StartedAtMs: 5}}}}
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1"}, Line: line})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert.
	if got := firstQueue(t, r).GetHead().GetStages(); len(got) != 1 || !proto.Equal(got[0], line[0].Stages[0]) {
		t.Fatalf("stages = %v, want the front's", got)
	}
}

func TestAWaitingMergeCarriesWhenAndWhereItWasPutInLine(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	queued := time.UnixMilli(9_000)
	line := inLine("w1", "w2")
	line[1].QueuedAt = queued
	line[1].PlacedAtFront = true
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1", "w2"}, Line: line})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	waiting := firstQueue(t, r).GetWaiting()[0]
	if waiting.GetQueuedAtMs() != queued.UnixMilli() || !waiting.GetPlacedAtFront() {
		t.Fatalf("waiting = %v, want queued at %d, placed at the front", waiting, queued.UnixMilli())
	}
}

func TestAPausedQueueSaysSo(t *testing.T) {
	tests := []struct {
		name   string
		paused bool
	}{
		{"a paused queue", true},
		{"a running queue", false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			r.SetMergeQueue(theRepo, sidebar.MergeQueue{Paused: tt.paused, Workspaces: []ids.WorkspaceID{"w1"}, Line: inLine("w1")})

			// Act.
			r.SetRegistry(queueView(workspace("w1", "one")))

			// Assert.
			if got := firstQueue(t, r).GetPaused() != nil; got != tt.paused {
				t.Fatalf("paused = %v, want %v", got, tt.paused)
			}
		})
	}
}

func TestAFailedMergeIsTheHeadWhileTheLineIsEmpty(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Failed: &sidebar.FailedMerge{
		Entry:   sidebar.QueueEntry{Workspace: "w1", Credited: "w1", Admitted: true, Reveal: &frontendv1.FeedId{Value: "bubble-1"}},
		Summary: "the tests failed",
	}})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert.
	section := latest(t, r).GetRepository().GetSections()[0]
	head := section.GetMergeQueue().GetHead()
	if head.GetFailed().GetSummary() != "the tests failed" || head.GetSubject().GetName() != "one" || head.GetSubject().GetReveal().GetValue() != "bubble-1" {
		t.Fatalf("head = %v, want one's failure with its bubble", head)
	}
	if got := section.GetHeader().GetCount().GetWorkspaces(); got != 0 {
		t.Fatalf("header count = %d, want no merge in line", got)
	}
}

func TestAFailedMergeIsNotTheHeadWhileAMergeIsInLine(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{
		Workspaces: []ids.WorkspaceID{"w2"},
		Line:       inLine("w2"),
		Failed:     &sidebar.FailedMerge{Entry: sidebar.QueueEntry{Workspace: "w1", Credited: "w1"}, Summary: "it failed"},
	})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	if got := queueSubjectIDs(latest(t, r).GetRepository().GetSections()[0]); !slices.Equal(got, []string{"w2"}) {
		t.Fatalf("queue = %v, want the merge in line alone", got)
	}
}

func TestAFailedMergeWhoseWorkspaceLeftIsPlacedByItsCredit(t *testing.T) {
	// Arrange: the failed merge workspace is gone; its requester remains.
	r, _ := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Failed: &sidebar.FailedMerge{
		Entry: sidebar.QueueEntry{Workspace: "w-merge", Credited: "w1", Async: true}, Summary: "it failed"}})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert.
	if head := firstQueue(t, r).GetHead(); head.GetSubject().GetName() != "one" || head.GetFailed() == nil {
		t.Fatalf("head = %v, want one's failure", head)
	}
}

func TestAFailedMergeWhoseWorkspacesAllLeftIsRecordedAndNotDrawn(t *testing.T) {
	// Arrange.
	r, logs := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Failed: &sidebar.FailedMerge{
		Entry: sidebar.QueueEntry{Workspace: "w-merge", Credited: "w-gone", Async: true}, Summary: "it failed"}})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert.
	if head := firstQueue(t, r).GetHead(); head != nil {
		t.Fatalf("head = %v, want none", head)
	}
	if !hasRecord(logs.Records(), dlog.LevelInfo, "daemon.sidebar.queue_lines") {
		t.Fatal("the failed merge that cannot be placed was not recorded")
	}
}

func TestAMergeOnlyRequestedIsNotInTheQueueView(t *testing.T) {
	// Arrange: w2 has a row on the queue but is not in line yet.
	r, _ := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Workspaces: []ids.WorkspaceID{"w1", "w2"}, Line: inLine("w1")})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	if got := queueSubjectIDs(latest(t, r).GetRepository().GetSections()[0]); !slices.Equal(got, []string{"w1"}) {
		t.Fatalf("queue = %v, want only the merge in line", got)
	}
}

func TestAQueueWhoseLineAloneIsSetIsKept(t *testing.T) {
	// Arrange: a queue stated with a line and nothing else.
	r, _ := newResolver(t)
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Act.
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Line: inLine("w1")})

	// Assert.
	if got := queueSubjectIDs(latest(t, r).GetRepository().GetSections()[0]); !slices.Equal(got, []string{"w1"}) {
		t.Fatalf("queue = %v, want the line's merge", got)
	}
}

func TestAQueueWhoseFailedMergeAloneIsSetIsKept(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Act.
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Failed: &sidebar.FailedMerge{Entry: sidebar.QueueEntry{Workspace: "w1", Credited: "w1"}, Summary: "it failed"}})

	// Assert.
	if head := firstQueue(t, r).GetHead(); head.GetFailed() == nil {
		t.Fatalf("head = %v, want the failed merge", head)
	}
}

func TestAMergeInLineTheRegistryDoesNotCarryIsRecordedAndNotDrawn(t *testing.T) {
	// Arrange.
	r, logs := newResolver(t)
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Line: inLine("w-gone", "w1")})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one")))

	// Assert.
	if got := queueSubjectIDs(latest(t, r).GetRepository().GetSections()[0]); !slices.Equal(got, []string{"w1"}) {
		t.Fatalf("queue = %v, want the registered merge alone", got)
	}
	if !hasError(logs.Records(), "daemon.sidebar.queue_lines") {
		t.Fatal("the unregistered merge in line was not recorded at error")
	}
}

func TestAMergeCreditedToAWorkspaceTheRegistryDoesNotCarryIsRecordedAndNotDrawn(t *testing.T) {
	// Arrange: the front runs in w1 but is credited to a workspace gone.
	r, logs := newResolver(t)
	line := inLine("w1", "w2")
	line[0].Credited = "w-gone"
	r.SetMergeQueue(theRepo, sidebar.MergeQueue{Line: line})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))

	// Assert.
	if got := queueSubjectIDs(latest(t, r).GetRepository().GetSections()[0]); !slices.Equal(got, []string{"w2"}) {
		t.Fatalf("queue = %v, want the credited merge alone", got)
	}
	if !hasError(logs.Records(), "daemon.sidebar.merge_queue") {
		t.Fatal("the merge credited to an unregistered workspace was not recorded at error")
	}
}

func TestTwoQueuesOfOneRepositoryAreRecordedAndBothDrawn(t *testing.T) {
	// Arrange.
	r, logs := newResolver(t)
	r.SetMergeQueue(wsm.RepoKey("/a/.git"), sidebar.MergeQueue{Line: inLine("w1")})
	r.SetMergeQueue(wsm.RepoKey("/b/.git"), sidebar.MergeQueue{Line: inLine("w2")})

	// Act.
	r.SetRegistry(queueView(workspace("w1", "one"), workspace("w2", "two")))

	// Assert: both, in queue-key order.
	if got := queueSubjectIDs(latest(t, r).GetRepository().GetSections()[0]); !slices.Equal(got, []string{"w1", "w2"}) {
		t.Fatalf("queue = %v, want both queues' merges", got)
	}
	if !hasError(logs.Records(), "daemon.sidebar.queue_lines") {
		t.Fatal("two queues of one repository were not recorded at error")
	}
}

func TestTheOneShotSubsectionIsKeylessCountedAndNeverCollapses(t *testing.T) {
	// Arrange
	r, _ := newResolver(t)
	reg := registry(marked(placed(workspace("w1", "one"), 1)), marked(placed(workspace("w2", "two"), 2)))

	// Act
	r.SetRegistry(reg)

	// Assert
	subs := rosterwalk.Subsections(latest(t, r).GetRepository().GetSections()[0])
	if len(subs) != 1 {
		t.Fatalf("subsections = %d, want the one-shot subsection alone", len(subs))
	}
	sub := subs[0]
	if sub.GetOneShot() == nil || sub.GetNamed() != nil {
		t.Errorf("kind = %v, want the one-shot arm, with no key", sub.GetKind())
	}
	if got := sub.GetHeader().GetLabel().GetText(); got != sidebar.OneShotSubsectionTitle {
		t.Errorf("label = %q, want %q", got, sidebar.OneShotSubsectionTitle)
	}
	if got := sub.GetHeader().GetCount().GetWorkspaces(); got != 2 {
		t.Errorf("count = %d, want 2", got)
	}
	if sub.GetExpanded().GetNeverCollapses() == nil {
		t.Errorf("fold = %v, want expanded and never collapsing", sub.GetFold())
	}
}
