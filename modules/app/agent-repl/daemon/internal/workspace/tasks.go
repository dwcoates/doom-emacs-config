package workspace

import (
	"context"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// CreateTask records a new user task. A BLANK TITLE IS REFUSED: an untitled row
// in the roster's task view names nothing, and the user could never tell two of
// them apart.
func (v *verbs) CreateTask(ctx context.Context, title string) (wsm.Task, error) {
	log := v.deps.Log.Global()
	if strings.TrimSpace(title) == "" {
		return wsm.Task{}, refuse(log, "CreateTask", ArmBlankTitle, "a task must carry a title", false)
	}
	task, err := v.deps.DB.CreateTask(ctx, title)
	if err != nil {
		log.Error(opCreateTask, "could not record the task", dlog.Context{"cause": err.Error()})
		return wsm.Task{}, fmt.Errorf("create task: %w", err)
	}
	log.Debug(opCreateTask, "created a task", dlog.Context{"task": string(task.ID), "title": task.Title})
	v.republishRegistry(ctx, log, opCreateTask)
	return task, nil
}

// UpdateTask retitles, completes or reopens a task. A change that says nothing
// is refused, and so is a retitle to a blank title, for the same reason
// CreateTask refuses one.
func (v *verbs) UpdateTask(ctx context.Context, id ids.TaskID, change wsm.TaskChange) error {
	log := v.deps.Log.Global().With(dlog.Context{"task": string(id)})
	if change.Title == nil && change.Done == nil {
		return refuse(log, "UpdateTask", ArmBlankTitle, "a task update must change something", false)
	}
	if change.Title != nil && strings.TrimSpace(*change.Title) == "" {
		return refuse(log, "UpdateTask", ArmBlankTitle, "a task must carry a title", false)
	}
	// THE TASK IS RESOLVED BEFORE IT IS CHANGED. A ref naming no task and a
	// change the task already holds are both ANSWERS the contract spells
	// (UpdateTaskError.unknown_task, UpdateTaskError.no_change); left to the
	// state client they surface as a not-found error and a silent success.
	current, found, err := v.task(ctx, id)
	if err != nil {
		log.Error(opUpdateTask, "could not read the task", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("update task %q: %w", id, err)
	}
	if !found {
		return refuse(log, "UpdateTask", ArmUnknownTask,
			fmt.Sprintf("no task %q is recorded", id), true)
	}
	if !changesAnything(current, change) {
		return refuse(log, "UpdateTask", ArmNoChange,
			"the change asked for is what the task already holds", false)
	}
	if err := v.deps.DB.UpdateTask(ctx, id, change); err != nil {
		log.Error(opUpdateTask, "could not update the task", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("update task %q: %w", id, err)
	}
	log.Debug(opUpdateTask, "updated the task", dlog.Context{
		"retitled": change.Title != nil, "done_changed": change.Done != nil,
	})
	v.republishRegistry(ctx, log, opUpdateTask)
	return nil
}

// AssignTask assigns a workspace to a task, or unassigns it when task is nil.
func (v *verbs) AssignTask(ctx context.Context, ws ids.WorkspaceID, task *ids.TaskID) error {
	_, log, err := v.owned(ctx, "AssignWorkspaceTask", ws)
	if err != nil {
		return err
	}
	if task != nil {
		_, found, err := v.task(ctx, *task)
		if err != nil {
			log.Error(opAssignTask, "could not read the task", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("assign task on %q: %w", ws, err)
		}
		if !found {
			return refuse(log, "AssignWorkspaceTask", ArmUnknownTask,
				fmt.Sprintf("no task %q is recorded", *task), true)
		}
	}
	if err := v.deps.DB.AssignWorkspaceTask(ctx, ws, task); err != nil {
		log.Error(opAssignTask, "could not record the assignment", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("assign task on %q: %w", ws, err)
	}
	log.Debug(opAssignTask, "recorded the task assignment", dlog.Context{"unassigned": task == nil})
	v.republishRegistry(ctx, log, opAssignTask)
	return nil
}

// task answers one recorded task by id, reporting whether it exists.
func (v *verbs) task(ctx context.Context, id ids.TaskID) (wsm.Task, bool, error) {
	tasks, err := v.deps.DB.Tasks(ctx)
	if err != nil {
		return wsm.Task{}, false, err
	}
	for _, t := range tasks {
		if t.ID == id {
			return t, true, nil
		}
	}
	return wsm.Task{}, false, nil
}

// changesAnything reports whether a change would move the task at all. A
// retitle to the title it already carries and a set_done on a task already
// done are the same non-change, and the contract has one arm for both.
func changesAnything(current wsm.Task, change wsm.TaskChange) bool {
	if change.Title != nil && strings.TrimSpace(*change.Title) != current.Title {
		return true
	}
	if change.Done != nil && *change.Done != current.Done {
		return true
	}
	return false
}
