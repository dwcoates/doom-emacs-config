package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestCreateTaskMintsAnIdentity(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	task, err := s.CreateTask(context.Background(), "write the report")

	// Assert
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if len(task.ID) != IDLength {
		t.Fatalf("task id = %q, want %d characters", task.ID, IDLength)
	}
	if task.Title != "write the report" || task.Done {
		t.Fatalf("task = %+v, want an open task with the given title", task)
	}
}

func TestCreateTaskRefusesAnEmptyTitle(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.CreateTask(context.Background(), "")

	// Assert
	if err == nil {
		t.Fatalf("CreateTask with no title succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.create_task", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestUpdateTaskRetitles(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	task, err := s.CreateTask(context.Background(), "old")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	title := "new"

	// Act
	if err := s.UpdateTask(context.Background(), task.ID, TaskChange{Title: &title}); err != nil {
		t.Fatalf("UpdateTask: %v", err)
	}

	// Assert
	tasks, err := s.Tasks(context.Background())
	if err != nil {
		t.Fatalf("Tasks: %v", err)
	}
	if tasks[0].Title != "new" {
		t.Fatalf("title = %q, want %q", tasks[0].Title, "new")
	}
}

func TestUpdateTaskMarksItDone(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	task, err := s.CreateTask(context.Background(), "finish")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	done := true

	// Act
	if err := s.UpdateTask(context.Background(), task.ID, TaskChange{Done: &done}); err != nil {
		t.Fatalf("UpdateTask: %v", err)
	}

	// Assert
	tasks, err := s.Tasks(context.Background())
	if err != nil {
		t.Fatalf("Tasks: %v", err)
	}
	if !tasks[0].Done {
		t.Fatalf("done = false after marking it done")
	}
}

func TestUpdateTaskRefusesAChangeThatSetsNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	task, err := s.CreateTask(context.Background(), "t")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	err = s.UpdateTask(context.Background(), task.ID, TaskChange{})

	// Assert
	if err == nil {
		t.Fatalf("UpdateTask with an empty change succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.update_task", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestUpdateTaskRefusesAnEmptyTitle(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	task, err := s.CreateTask(context.Background(), "t")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	title := ""

	// Act
	err = s.UpdateTask(context.Background(), task.ID, TaskChange{Title: &title})

	// Assert
	if err == nil {
		t.Fatalf("UpdateTask retitling to nothing succeeded")
	}
}

func TestUpdateTaskRefusesAnUnknownTask(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	done := true

	// Act
	err := s.UpdateTask(context.Background(), TaskID("absent"), TaskChange{Done: &done})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("UpdateTask = %v, want ErrNotFound", err)
	}
}

func TestTasksLoadsEveryRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.CreateTask(context.Background(), "one"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if _, err := s.CreateTask(context.Background(), "two"); err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	got, err := s.Tasks(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Tasks: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("loaded %d tasks, want 2", len(got))
	}
}

func TestAssignWorkspaceTaskBindsThem(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	task, err := s.CreateTask(context.Background(), "assigned")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}

	// Act
	if err := s.AssignWorkspaceTask(context.Background(), ws.ID, &task.ID); err != nil {
		t.Fatalf("AssignWorkspaceTask: %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Task == nil || *got.Task != task.ID {
		t.Fatalf("task = %v, want %q", got.Task, task.ID)
	}
}

func TestAssignWorkspaceTaskUnassigns(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	task, err := s.CreateTask(context.Background(), "assigned")
	if err != nil {
		t.Fatalf("CreateTask: %v", err)
	}
	if err := s.AssignWorkspaceTask(context.Background(), ws.ID, &task.ID); err != nil {
		t.Fatalf("AssignWorkspaceTask: %v", err)
	}

	// Act
	if err := s.AssignWorkspaceTask(context.Background(), ws.ID, nil); err != nil {
		t.Fatalf("AssignWorkspaceTask(nil): %v", err)
	}

	// Assert
	got, err := s.Workspace(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if got.Task != nil {
		t.Fatalf("task = %q after unassigning, want none", *got.Task)
	}
}

func TestAssignWorkspaceTaskRefusesAnUnknownTask(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	absent := TaskID("absent")

	// Act
	err := s.AssignWorkspaceTask(context.Background(), ws.ID, &absent)

	// Assert
	if err == nil {
		t.Fatalf("AssignWorkspaceTask with an unknown task succeeded")
	}
}

func TestAssignWorkspaceTaskRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.AssignWorkspaceTask(context.Background(), WorkspaceID("absent"), nil)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("AssignWorkspaceTask = %v, want ErrNotFound", err)
	}
}
