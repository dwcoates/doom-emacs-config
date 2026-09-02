package sidebar_test

import (
	"testing"

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
