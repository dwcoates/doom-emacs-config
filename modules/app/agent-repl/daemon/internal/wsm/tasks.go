package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// CreateTask records a new user task.
func (s *store) CreateTask(ctx context.Context, title string) (Task, error) {
	const op = "daemon.wsm.create_task"
	if title == "" {
		err := errors.New("wsm: empty task title")
		s.log.Error(op, "refused a task with no title", withError(dlog.Context{}, err))
		return Task{}, err
	}
	task := Task{ID: NewTaskID(), Title: title, CreatedAt: time.Now().UTC()}
	err := s.write(ctx, op, dlog.Context{"task": string(task.ID), "title": title}, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO tasks (id, title, done, created_at) VALUES (?, ?, 0, ?)`,
			task.ID, task.Title, nanos(task.CreatedAt))
		return err
	})
	if err != nil {
		return Task{}, err
	}
	return task, nil
}

// UpdateTask applies one mutation to a task. A change that sets nothing is
// refused: a silent no-op write hides a caller's bug.
func (s *store) UpdateTask(ctx context.Context, id TaskID, change TaskChange) error {
	const op = "daemon.wsm.update_task"
	fields := dlog.Context{"task": string(id)}
	if change.Title == nil && change.Done == nil {
		err := errors.New("wsm: task change sets nothing")
		s.log.Error(op, "refused an empty task change", withError(fields, err))
		return err
	}
	if change.Title != nil {
		if *change.Title == "" {
			err := errors.New("wsm: empty task title")
			s.log.Error(op, "refused a task retitled to nothing", withError(fields, err))
			return err
		}
		fields["title"] = *change.Title
	}
	if change.Done != nil {
		fields["done"] = *change.Done
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		if change.Title != nil {
			res, err := tx.ExecContext(ctx, `UPDATE tasks SET title = ? WHERE id = ?`, *change.Title, id)
			if err != nil {
				return err
			}
			if err := requireOneRow(res, fmt.Sprintf("wsm: task %s", id)); err != nil {
				return err
			}
		}
		if change.Done != nil {
			res, err := tx.ExecContext(ctx, `UPDATE tasks SET done = ? WHERE id = ?`, *change.Done, id)
			if err != nil {
				return err
			}
			if err := requireOneRow(res, fmt.Sprintf("wsm: task %s", id)); err != nil {
				return err
			}
		}
		return nil
	})
}

// Tasks loads every task, all-or-nothing.
func (s *store) Tasks(ctx context.Context) ([]Task, error) {
	var out []Task
	err := s.read(ctx, "daemon.wsm.tasks", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT id, title, done, folded, created_at FROM tasks ORDER BY created_at, id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []Task
		for rows.Next() {
			var (
				task    Task
				created int64
			)
			if err := rows.Scan(&task.ID, &task.Title, &task.Done, &task.Folded, &created); err != nil {
				return err
			}
			task.CreatedAt = fromNanos(created)
			loaded = append(loaded, task)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// AssignWorkspaceTask assigns a workspace to a task, or unassigns it when task
// is nil. The foreign key refuses an unknown task rather than storing a
// dangling assignment.
func (s *store) AssignWorkspaceTask(ctx context.Context, id WorkspaceID, task *TaskID) error {
	fields := dlog.Context{"workspace": string(id)}
	var value any
	if task != nil {
		value = string(*task)
		fields["task"] = string(*task)
	}
	return s.setWorkspaceField(ctx, "daemon.wsm.assign_workspace_task", "task_id", id, value, fields)
}
