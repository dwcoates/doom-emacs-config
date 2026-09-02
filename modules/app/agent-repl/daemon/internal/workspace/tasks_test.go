package workspace

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

func TestCreateTaskRecordsTheTask(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	task, err := f.verbs.CreateTask(context.Background(), "ship the daemon")

	// Assert.
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if task.Title != "ship the daemon" {
		t.Fatalf("task = %+v, want the supplied title", task)
	}
}

func TestCreateTaskRefusesABlankTitle(t *testing.T) {
	// Arrange: an untitled row in the task view names nothing.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.CreateTask(context.Background(), "   ")

	// Assert.
	asRefusal(t, err, ArmBlankTitle)
}

func TestCreateTaskSurfacesTheRecordFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.taskErr = errors.New("the database is locked")

	// Act.
	_, err := f.verbs.CreateTask(context.Background(), "title")

	// Assert.
	if err == nil {
		t.Fatal("CreateTask() = nil error, want the record failure surfaced")
	}
}

// arrangeTask records one task the update verbs can resolve.
func arrangeTask(f *fixture, id ids.TaskID, title string, done bool) {
	f.db.tasks = append(f.db.tasks, wsm.Task{ID: id, Title: title, Done: done})
}

func TestUpdateTaskRetitles(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	arrangeTask(f, "task-1", "original", false)
	title := "renamed"

	// Act.
	if err := f.verbs.UpdateTask(context.Background(), "task-1", wsm.TaskChange{Title: &title}); err != nil {
		t.Fatalf("UpdateTask: %v", err)
	}

	// Assert.
	change := f.db.taskChanges["task-1"]
	if change.Title == nil || *change.Title != "renamed" {
		t.Fatalf("recorded change = %+v, want the retitle", change)
	}
}

func TestUpdateTaskRefusesAChangeThatSaysNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.UpdateTask(context.Background(), "task-1", wsm.TaskChange{})

	// Assert.
	asRefusal(t, err, ArmBlankTitle)
}

func TestUpdateTaskRefusesABlankRetitle(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	blank := "  "

	// Act.
	err := f.verbs.UpdateTask(context.Background(), "task-1", wsm.TaskChange{Title: &blank})

	// Assert.
	asRefusal(t, err, ArmBlankTitle)
}

func TestUpdateTaskCompletesATask(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	arrangeTask(f, "task-1", "original", false)
	done := true

	// Act.
	if err := f.verbs.UpdateTask(context.Background(), "task-1", wsm.TaskChange{Done: &done}); err != nil {
		t.Fatalf("UpdateTask: %v", err)
	}

	// Assert.
	change := f.db.taskChanges["task-1"]
	if change.Done == nil || !*change.Done {
		t.Fatalf("recorded change = %+v, want the completion", change)
	}
}

func TestAssignTaskRecordsTheAssignment(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	arrangeTask(f, "task-1", "original", false)
	task := ids.TaskID("task-1")

	// Act.
	if err := f.verbs.AssignTask(context.Background(), "w1", &task); err != nil {
		t.Fatalf("AssignTask: %v", err)
	}

	// Assert.
	got := f.db.assignments["w1"]
	if got == nil || *got != "task-1" {
		t.Fatalf("recorded assignment = %v, want task-1", got)
	}
}

func TestAssignTaskUnassigns(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.AssignTask(context.Background(), "w1", nil); err != nil {
		t.Fatalf("AssignTask: %v", err)
	}

	// Assert.
	got, recorded := f.db.assignments["w1"]
	if !recorded || got != nil {
		t.Fatalf("recorded assignment = (%v, %v), want a recorded unassignment", got, recorded)
	}
}

func TestAssignTaskRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	task := ids.TaskID("task-1")

	// Act.
	err := f.verbs.AssignTask(context.Background(), "nope", &task)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

// TestUpdateTaskRefusesAnUnknownTask pins UpdateTaskError.unknown_task: a ref
// naming no task is an ANSWER, not a state-client error.
func TestUpdateTaskRefusesAnUnknownTask(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	done := true

	// Act.
	err := f.verbs.UpdateTask(context.Background(), "no-such-task", wsm.TaskChange{Done: &done})

	// Assert.
	asRefusal(t, err, ArmUnknownTask)
}

// TestUpdateTaskRefusesACompletionOfADoneTask pins UpdateTaskError.no_change:
// set_done on a task already done moves nothing.
func TestUpdateTaskRefusesACompletionOfADoneTask(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	arrangeTask(f, "task-1", "original", true)
	done := true

	// Act.
	err := f.verbs.UpdateTask(context.Background(), "task-1", wsm.TaskChange{Done: &done})

	// Assert.
	asRefusal(t, err, ArmNoChange)
}

// TestUpdateTaskRefusesARetitleToTheSameTitle pins the other half of
// no_change: a retitle to the title the task already carries.
func TestUpdateTaskRefusesARetitleToTheSameTitle(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	arrangeTask(f, "task-1", "original", false)
	title := "original"

	// Act.
	err := f.verbs.UpdateTask(context.Background(), "task-1", wsm.TaskChange{Title: &title})

	// Assert.
	asRefusal(t, err, ArmNoChange)
}

// TestAssignTaskRefusesAnUnknownTask pins
// AssignWorkspaceTaskError.unknown_task, which the foreign key would otherwise
// surface as a constraint failure.
func TestAssignTaskRefusesAnUnknownTask(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	task := ids.TaskID("no-such-task")

	// Act.
	err := f.verbs.AssignTask(context.Background(), "w1", &task)

	// Assert.
	asRefusal(t, err, ArmUnknownTask)
}

// TestAssignTaskUnassignsWithoutResolvingATask pins that clearing an
// assignment needs no task to exist: there is no ref to be unknown.
func TestAssignTaskUnassignsWithoutResolvingATask(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.AssignTask(context.Background(), "w1", nil); err != nil {
		t.Fatalf("AssignTask(nil): %v", err)
	}

	// Assert.
	if got, ok := f.db.assignments["w1"]; !ok || got != nil {
		t.Fatalf("recorded assignment = %v, want an unassignment", got)
	}
}
