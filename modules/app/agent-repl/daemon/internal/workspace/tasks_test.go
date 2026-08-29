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

func TestUpdateTaskRetitles(t *testing.T) {
	// Arrange.
	f := newFixture(t)
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
