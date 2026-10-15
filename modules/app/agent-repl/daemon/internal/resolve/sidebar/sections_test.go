package sidebar_test

import (
	"agentrepl/protohelpers/rosterwalk"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/repodivergence"
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

	// Act.
	r.SetRegistry(registry(merged))

	// Assert: a section that vanished would make the grouping flicker.
	sections := latest(t, r).GetRepository().GetSections()
	if len(sections) != 1 || len(sections[0].GetRows().GetRows()) != 0 {
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
	if rows := sections[0].GetRows().GetRows(); len(rows) != 0 {
		t.Fatalf("the section drew %d rows, want none", len(rows))
	}
}

func TestRecentlyMergedHoistsOutOfTheRepositoryGrouping(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)

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
	if top := len(section.GetRows().GetRows()); top != 1 {
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
	second := workspace("w-b", "b")
	second.MergedAt = at(1)

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
	if got := rowNames(section.GetRows().GetRows()); !equal(got, []string{"hidden"}) {
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
	if len(section.GetRows().GetRows()) != 0 || section.GetHeader().GetCount().GetWorkspaces() != 0 {
		t.Fatalf("hidden section = %v, want no rows and a zero count", section)
	}
}

func TestTheHiddenSectionCountLeavesOutAClosedRow(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	closed := hiddenWorkspace("w-closed", "closed")
	closed.Closed = true

	// Act.
	r.SetRegistry(registry(closed, hiddenWorkspace("w-open", "open")))

	// Assert.
	section := latest(t, r).GetHidden()
	if len(section.GetRows().GetRows()) != 2 {
		t.Fatalf("hidden rows = %v, want both on the wire", rowNames(section.GetRows().GetRows()))
	}
	if got := section.GetHeader().GetCount().GetWorkspaces(); got != 1 {
		t.Fatalf("hidden count = %d, want 1 (the closed row is not drawn)", got)
	}
}

func TestAMergedHiddenWorkspaceFilesUnderRecentlyMerged(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	merged := hiddenWorkspace("w-merged", "merged")
	merged.MergedAt = at(0)

	// Act.
	r.SetRegistry(registry(merged))

	// Assert.
	roster := latest(t, r)
	if len(roster.GetHidden().GetRows().GetRows()) != 0 {
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
	return ws
}

func TestRecentlyMergedGroupsByRepositoryInTheRepositoryGroupingsOrder(t *testing.T) {
	// Arrange: beta is registered first, alpha sorts first by name.
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
	got := rowNames(latest(t, r).GetRecentlyMerged().GetGroups()[0].GetRows().GetRows())
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
	got := latest(t, r).GetHidden().GetRows().GetRows()[0].GetWhen().GetHidden()
	if got == nil || got.GetAtMs() != at(time.Hour).UnixMilli() {
		t.Fatalf("when = %v, want the hide instant", latest(t, r).GetHidden().GetRows().GetRows()[0].GetWhen())
	}
}

func TestAMergedHiddenRowCountsFromItsMerge(t *testing.T) {
	// Arrange: merged wins over hidden, as it files under Recently Merged.
	r, _ := newResolver(t)
	ws := hiddenAt(workspace("w1", "one"), time.Hour)
	ws.MergedAt = at(2 * time.Hour)

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
