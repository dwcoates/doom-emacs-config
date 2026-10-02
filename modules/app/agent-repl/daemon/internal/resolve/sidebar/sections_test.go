package sidebar_test

import (
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestRosterResolvesBothGroupings(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	task := wsm.Task{ID: ids.TaskID("task-1"), Title: "the task", CreatedAt: epoch}
	assigned := workspace("w-assigned", "assigned")
	taskID := task.ID
	assigned.Task = &taskID
	reg := registry(assigned)
	reg.Tasks = []wsm.Task{task}

	// Act.
	r.SetRegistry(reg)

	// Assert: which one is DRAWN is webview-local; both are resolved.
	roster := latest(t, r)
	if len(roster.GetRepository().GetSections()) != 1 {
		t.Fatal("the repository grouping was not resolved")
	}
	if len(roster.GetTask().GetSections()) != 1 {
		t.Fatal("the task grouping was not resolved")
	}
}

func TestTaskViewOmitsAnUnassignedWorkspace(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	task := wsm.Task{ID: ids.TaskID("task-1"), Title: "the task", CreatedAt: epoch}
	reg := registry(workspace("w-unassigned", "unassigned"))
	reg.Tasks = []wsm.Task{task}

	// Act.
	r.SetRegistry(reg)

	// Assert: there is no "no task" section — an unassigned workspace is no
	// answer to "what is each task's work".
	sections := latest(t, r).GetTask().GetSections()
	if len(sections) != 1 || len(sections[0].GetRows().GetRows()) != 0 {
		t.Fatal("an unassigned workspace reached the task view")
	}
}

func TestTaskSectionCarriesTheDoneCheck(t *testing.T) {
	tests := []struct {
		name string
		done bool
	}{
		{name: "open", done: false},
		{name: "done", done: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			r, _ := newResolver(t)
			reg := registry()
			reg.Tasks = []wsm.Task{{
				ID: ids.TaskID("task-1"), Title: "the task", Done: tc.done, CreatedAt: epoch,
			}}

			// Act.
			r.SetRegistry(reg)

			// Assert.
			sections := latest(t, r).GetTask().GetSections()
			if got := sections[0].GetHeader().GetDone().GetDone(); got != tc.done {
				t.Fatalf("done = %v, want %v", got, tc.done)
			}
		})
	}
}

func TestTaskSectionIsKeyedByIdNotTitle(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	reg := registry()
	reg.Tasks = []wsm.Task{{ID: ids.TaskID("task-7"), Title: "renamed later", CreatedAt: epoch}}

	// Act.
	r.SetRegistry(reg)

	// Assert: a task can be renamed without becoming a different task.
	sections := latest(t, r).GetTask().GetSections()
	if got := sections[0].GetKey().GetTaskId(); got != "task-7" {
		t.Fatalf("task key = %q, want the task id", got)
	}
}

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

func TestRecentlyMergedHoistsOutOfTheTaskGrouping(t *testing.T) {
	// Arrange.
	r, _ := newResolver(t)
	task := wsm.Task{ID: ids.TaskID("task-1"), Title: "the task", CreatedAt: epoch}
	taskID := task.ID
	merged := workspace("w-merged", "merged")
	merged.MergedAt = at(0)
	merged.Task = &taskID
	reg := registry(merged)
	reg.Tasks = []wsm.Task{task}

	// Act.
	r.SetRegistry(reg)

	// Assert.
	sections := latest(t, r).GetTask().GetSections()
	if len(sections[0].GetRows().GetRows()) != 0 {
		t.Fatal("a merged workspace stayed in the task grouping")
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
	got := rowNames(latest(t, r).GetRecentlyMerged().GetRows().GetRows())
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

func TestRosterRecordsAWorkspaceAssignedToAnUnregisteredTask(t *testing.T) {
	// Arrange.
	r, surfaces := newResolver(t)
	stray := workspace("w-stray", "stray")
	missing := ids.TaskID("task-missing")
	stray.Task = &missing

	// Act.
	r.SetRegistry(registry(stray))

	// Assert.
	if !hasError(surfaces.Records(), "daemon.sidebar.task_view") {
		t.Fatal("a workspace assigned to an unregistered task was not recorded")
	}
}

// dirsInRoster walks the roster exactly as the sidecar's log forwarder does --
// the repository sections, the task sections and the recently-merged section,
// each row and its children -- and answers every workspace directory it can
// resolve a ref for.
func dirsInRoster(roster *frontendv1.WorkspaceRoster) map[string]bool {
	out := map[string]bool{}
	var walk func([]*frontendv1.RosterRow)
	walk = func(rows []*frontendv1.RosterRow) {
		for _, row := range rows {
			if ref := row.GetWorkspace().GetWorkspace(); ref.GetId() != "" && ref.GetDir() != "" {
				out[ref.GetDir()] = true
			}
			walk(row.GetChildren())
		}
	}
	for _, section := range roster.GetRepository().GetSections() {
		walk(section.GetRows().GetRows())
	}
	for _, section := range roster.GetTask().GetSections() {
		walk(section.GetRows().GetRows())
	}
	walk(roster.GetRecentlyMerged().GetRows().GetRows())
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
	if !equal(got, []string{"one", "three", "two"}) {
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
