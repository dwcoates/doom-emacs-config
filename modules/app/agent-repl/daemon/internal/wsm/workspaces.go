package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"time"

	"claude-repld/internal/dlog"
)

// normalizeDir is the ONE spelling of a worktree directory this store keys on:
// absolute, symlinks resolved, cleaned (which trims the trailing slash and the
// "." and ".." elements). Registration is idempotent by it, so every spelling
// of one directory reaches the same row.
//
// A path that does not exist cannot have its symlinks resolved; the deepest
// existing ancestor is resolved instead and the remainder is appended, so a
// directory under a symlinked parent still normalizes the same way once it
// appears.
func normalizeDir(dir string) (string, error) {
	if dir == "" {
		return "", errors.New("wsm: empty workspace dir")
	}
	abs, err := filepath.Abs(dir)
	if err != nil {
		return "", fmt.Errorf("wsm: absolutize %q: %w", dir, err)
	}
	abs = filepath.Clean(abs)
	if resolved, err := filepath.EvalSymlinks(abs); err == nil {
		return filepath.Clean(resolved), nil
	}
	// Walk up to the deepest existing ancestor, resolve that, and re-join the
	// tail so the answer is stable once the leaf is created.
	rest := ""
	head := abs
	for {
		parent := filepath.Dir(head)
		if parent == head {
			return abs, nil
		}
		rest = filepath.Join(filepath.Base(head), rest)
		head = parent
		if resolved, err := filepath.EvalSymlinks(head); err == nil {
			return filepath.Clean(filepath.Join(resolved, rest)), nil
		}
	}
}

// workspaceColumns is the one select list every workspace read shares, so a
// column added to the row can never be decoded by only some of them.
const workspaceColumns = `id, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, priority, task_id, last_selected_at, merged_at, created_at`

// scanWorkspace decodes one workspace row all-or-nothing: an out-of-range
// priority is a decode failure, never a silently substituted default.
func scanWorkspace(row interface{ Scan(...any) error }) (Workspace, error) {
	var (
		ws       Workspace
		parent   sql.NullString
		priority sql.NullInt64
		task     sql.NullString
		selected sql.NullInt64
		merged   sql.NullInt64
		created  int64
	)
	if err := row.Scan(&ws.ID, &ws.Repo, &ws.Dir, &ws.Name, &ws.Branch, &ws.ParentBranch, &parent, &ws.Closed, &ws.Attention, &priority, &task, &selected, &merged, &created); err != nil {
		return Workspace{}, err
	}
	if parent.Valid {
		id := WorkspaceID(parent.String)
		ws.Parent = &id
	}
	if priority.Valid {
		p := Priority(priority.Int64)
		if !p.valid() {
			return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "priority", Err: fmt.Errorf("unknown priority %d", priority.Int64)}
		}
		ws.Priority = &p
	}
	if task.Valid {
		id := TaskID(task.String)
		ws.Task = &id
	}
	ws.LastSelectedAt = optTime(selected)
	ws.MergedAt = optTime(merged)
	ws.CreatedAt = fromNanos(created)
	return ws, nil
}

// RegisterWorkspace records a workspace, idempotent by normalized dir. It mints
// the WorkspaceID and, on first sight of the repository, the RepoID. The bool
// reports whether the record was created.
func (s *store) RegisterWorkspace(ctx context.Context, dir string, facts RegisterFacts) (Workspace, bool, error) {
	const op = "daemon.wsm.register_workspace"
	normalized, err := normalizeDir(dir)
	if err != nil {
		s.log.Error(op, "refused a workspace dir that cannot be normalized", withError(dlog.Context{"dir": dir}, err))
		return Workspace{}, false, err
	}
	fields := dlog.Context{"dir": normalized, "requested_dir": dir}

	var (
		out     Workspace
		created bool
	)
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		existing, err := scanWorkspace(tx.QueryRowContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces WHERE dir = ?`, normalized))
		switch {
		case err == nil:
			out, created = existing, false
			return nil
		case !errors.Is(err, sql.ErrNoRows):
			return err
		}
		repoDir, err := normalizeDir(facts.RepoDir)
		if err != nil {
			return err
		}
		repo, err := ensureRepo(ctx, tx, repoDir, facts.DefaultBranch)
		if err != nil {
			return err
		}
		name := facts.Name
		if name == "" {
			name = filepath.Base(normalized)
		}
		out = Workspace{
			ID:           NewWorkspaceID(),
			Repo:         repo,
			Dir:          normalized,
			Name:         name,
			Branch:       facts.Branch,
			ParentBranch: facts.ParentBranch,
			Parent:       facts.Parent,
			CreatedAt:    time.Now().UTC(),
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, priority, task_id, is_current, last_selected_at, merged_at, serving_instance, created_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, 0, 0, NULL, NULL, 0, NULL, NULL, NULL, ?)`,
			out.ID, out.Repo, out.Dir, out.Name, out.Branch, out.ParentBranch, nullWorkspace(out.Parent), nanos(out.CreatedAt))
		if err != nil {
			return err
		}
		created = true
		return nil
	})
	if err != nil {
		return Workspace{}, false, err
	}
	return out, created, nil
}

// ensureRepo returns the repository for a canonicalized common dir, minting one
// on first sight. The display name is the dir's base name; the default branch
// is the announcing caller's, which is the only party that reads it off git.
//
// An EMPTY defaultBranch leaves a recorded one alone: "the caller did not look
// it up" is not "the repository has no default branch", and overwriting a known
// value with a blank would silently unset the merge target every later
// announcement depends on.
func ensureRepo(ctx context.Context, tx *sql.Tx, repoDir, defaultBranch string) (RepoID, error) {
	var id RepoID
	err := tx.QueryRowContext(ctx, `SELECT id FROM repositories WHERE dir = ?`, repoDir).Scan(&id)
	switch {
	case err == nil:
		if defaultBranch != "" {
			if _, err := tx.ExecContext(ctx, `UPDATE repositories SET default_branch = ? WHERE id = ?`, defaultBranch, id); err != nil {
				return "", err
			}
		}
		return id, nil
	case !errors.Is(err, sql.ErrNoRows):
		return "", err
	}
	id = NewRepoID()
	if _, err := tx.ExecContext(ctx, `INSERT INTO repositories (id, dir, name, default_branch) VALUES (?, ?, ?, ?)`, id, repoDir, filepath.Base(repoDir), defaultBranch); err != nil {
		return "", err
	}
	return id, nil
}

// Workspace loads one workspace by id.
func (s *store) Workspace(ctx context.Context, id WorkspaceID) (Workspace, error) {
	var out Workspace
	err := s.read(ctx, "daemon.wsm.workspace", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		ws, err := scanWorkspace(s.db().QueryRowContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces WHERE id = ?`, id))
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		out = ws
		return err
	})
	return out, err
}

// WorkspaceByDir loads one workspace by its worktree directory, in any spelling.
func (s *store) WorkspaceByDir(ctx context.Context, dir string) (Workspace, error) {
	const op = "daemon.wsm.workspace_by_dir"
	normalized, err := normalizeDir(dir)
	if err != nil {
		s.log.Error(op, "refused a workspace dir that cannot be normalized", withError(dlog.Context{"dir": dir}, err))
		return Workspace{}, err
	}
	var out Workspace
	err = s.read(ctx, op, dlog.Context{"dir": normalized}, func(ctx context.Context) error {
		ws, err := scanWorkspace(s.db().QueryRowContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces WHERE dir = ?`, normalized))
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace at %q: %w", normalized, ErrNotFound)
		}
		out = ws
		return err
	})
	return out, err
}

// ListWorkspaces loads every workspace, all-or-nothing.
func (s *store) ListWorkspaces(ctx context.Context) ([]Workspace, error) {
	var out []Workspace
	err := s.read(ctx, "daemon.wsm.list_workspaces", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces ORDER BY created_at, id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []Workspace
		for rows.Next() {
			ws, err := scanWorkspace(rows)
			if err != nil {
				return err
			}
			loaded = append(loaded, ws)
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

// ListRepositories loads every repository, all-or-nothing.
func (s *store) ListRepositories(ctx context.Context) ([]Repository, error) {
	var out []Repository
	err := s.read(ctx, "daemon.wsm.list_repositories", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT id, dir, name, default_branch FROM repositories ORDER BY dir`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []Repository
		for rows.Next() {
			var repo Repository
			if err := rows.Scan(&repo.ID, &repo.Dir, &repo.Name, &repo.DefaultBranch); err != nil {
				return err
			}
			loaded = append(loaded, repo)
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

// setWorkspaceField is the one write path for a single workspace column: it
// refuses an unknown workspace rather than silently affecting no rows.
func (s *store) setWorkspaceField(ctx context.Context, op, column string, id WorkspaceID, value any, fields dlog.Context) error {
	fields["workspace"] = string(id)
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET `+column+` = ? WHERE id = ?`, value, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}

// requireOneRow turns "no rows affected" into a refusal. A write that matched
// nothing is an unknown id, never a success.
func requireOneRow(res sql.Result, what string) error {
	n, err := res.RowsAffected()
	if err != nil {
		return err
	}
	if n == 0 {
		return fmt.Errorf("%s: %w", what, ErrNotFound)
	}
	return nil
}

// SetClosed records whether a workspace's editor state is torn down.
func (s *store) SetClosed(ctx context.Context, id WorkspaceID, closed bool) error {
	return s.setWorkspaceField(ctx, "daemon.wsm.set_closed", "closed", id, closed, dlog.Context{"closed": closed})
}

// SetAttention sets or clears the roster's attention marker.
func (s *store) SetAttention(ctx context.Context, id WorkspaceID, on bool) error {
	return s.setWorkspaceField(ctx, "daemon.wsm.set_attention", "attention", id, on, dlog.Context{"attention": on})
}

// SetMergedAt records that the workspace's merge landed.
func (s *store) SetMergedAt(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.setWorkspaceField(ctx, "daemon.wsm.set_merged_at", "merged_at", id, nanos(at), dlog.Context{"merged_at": at})
}

// SetPriority sets or clears a workspace's roster priority. An undeclared
// priority is refused rather than stored for a later read to trip over.
func (s *store) SetPriority(ctx context.Context, id WorkspaceID, p *Priority) error {
	const op = "daemon.wsm.set_priority"
	fields := dlog.Context{"workspace": string(id)}
	if p == nil {
		fields["priority"] = nil
		return s.setWorkspaceField(ctx, op, "priority", id, nil, fields)
	}
	if !p.valid() {
		err := fmt.Errorf("wsm: unknown priority %d", int(*p))
		s.log.Error(op, "refused an undeclared priority", withError(fields, err))
		return err
	}
	fields["priority"] = int(*p)
	return s.setWorkspaceField(ctx, op, "priority", id, int(*p), fields)
}

// SetCurrent records the user's selection of a workspace at an instant. At most
// one workspace is current, which the unique index makes structural: the clear
// and the set are one transaction.
func (s *store) SetCurrent(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.write(ctx, "daemon.wsm.set_current", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET is_current = 0 WHERE is_current = 1`); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET is_current = 1, last_selected_at = ? WHERE id = ?`, nanos(at), id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}

// Current reports the currently selected workspace, nil when none is.
func (s *store) Current(ctx context.Context) (*WorkspaceID, error) {
	var out *WorkspaceID
	err := s.read(ctx, "daemon.wsm.current", dlog.Context{}, func(ctx context.Context) error {
		var id WorkspaceID
		err := s.db().QueryRowContext(ctx, `SELECT id FROM workspaces WHERE is_current = 1`).Scan(&id)
		if errors.Is(err, sql.ErrNoRows) {
			out = nil
			return nil
		}
		if err != nil {
			return err
		}
		out = &id
		return nil
	})
	return out, err
}

// Forget deletes a workspace's every record — the nuke's durable half. The
// dependent rows cascade; the creation job, which predates registration and so
// carries no reference, is deleted here in the same transaction.
func (s *store) Forget(ctx context.Context, id WorkspaceID) error {
	return s.write(ctx, "daemon.wsm.forget", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `DELETE FROM creation_jobs WHERE workspace_id = ?`, id); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM workspaces WHERE id = ?`, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}
