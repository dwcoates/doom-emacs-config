package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"time"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/dlog"
	"claude-repld/internal/tempdirs"
)

// normalizeDir is the ONE spelling of a worktree directory this store keys on:
// dirpath.Canonical's — absolute, cleaned, symlinks resolved, and every
// existing component in its on-disk case. Registration is idempotent by it, so
// every spelling of one directory, case included, reaches the same row.
func normalizeDir(dir string) (string, error) {
	if dir == "" {
		return "", errors.New("wsm: empty workspace dir")
	}
	normalized, err := dirpath.Canonical(dir)
	if err != nil {
		return "", fmt.Errorf("wsm: normalize %q: %w", dir, err)
	}
	return normalized, nil
}

// workspaceColumns is the one select list every workspace read shares, so a
// column added to the row can never be decoded by only some of them.
const workspaceColumns = `id, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, priority, task_id, last_selected_at, last_activity_at, merged_at, spawned_shim_pid, created_at, result_end, result_read`

// scanWorkspace decodes one workspace row all-or-nothing: an out-of-range
// priority is a decode failure, never a silently substituted default.
func scanWorkspace(row interface{ Scan(...any) error }) (Workspace, error) {
	var (
		ws       Workspace
		parent   sql.NullString
		priority sql.NullInt64
		task     sql.NullString
		selected sql.NullInt64
		activity sql.NullInt64
		merged   sql.NullInt64
		spawned  sql.NullInt64
		created  int64
		end      sql.NullString
		read     bool
	)
	if err := row.Scan(&ws.ID, &ws.Repo, &ws.Dir, &ws.Name, &ws.Branch, &ws.ParentBranch, &parent, &ws.Closed, &ws.Attention, &priority, &task, &selected, &activity, &merged, &spawned, &created, &end, &read); err != nil {
		return Workspace{}, err
	}
	if end.Valid {
		result := TurnResultEnd(end.String)
		if !result.valid() {
			return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "result_end", Err: fmt.Errorf("unknown turn-end arm %q", end.String)}
		}
		ws.Result = &TurnResult{End: result, Read: read}
	}
	if spawned.Valid {
		// A NON-POSITIVE RECORDED PID IS A CORRUPT ROW, never a silently
		// dropped one: the whole point of the value is that a successor probes
		// the process it names, and kill(0) on a non-positive pid addresses
		// a process GROUP or every process this daemon may signal.
		if spawned.Int64 <= 0 {
			return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "spawned_shim_pid", Err: errors.New("a recorded spawned shim pid is positive")}
		}
		pid := int(spawned.Int64)
		ws.SpawnedShimPID = &pid
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
	ws.LastActivityAt = optTime(activity)
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
	// BOTH DIRECTORIES ARE CHECKED BEFORE ANYTHING IS READ OR WRITTEN: the
	// workspace's own, and the repository it would mint on first sight. A
	// workspace row already standing for a temporary directory is refused too
	// rather than handed back -- the boot retires such rows, and an
	// announcement must not keep one alive.
	if err := s.refuseTemporary(op, normalized); err != nil {
		return Workspace{}, false, err
	}
	if facts.RepoDir != "" {
		if err := s.refuseTemporary(op, facts.RepoDir); err != nil {
			return Workspace{}, false, err
		}
	}

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

// RegisterRepository records a repository on its own, with NO workspace under
// it, idempotent by normalized dir. The bool reports whether the record was
// minted here.
//
// It is ensureRepo reached directly. The repository row was previously
// reachable only from inside RegisterWorkspace's transaction, so a repository
// existed only once somebody worked in it; this is the same mint, in its own
// transaction, for a repository that has no workspace yet. Nothing else about
// the row differs -- an empty defaultBranch leaves a recorded one alone here
// exactly as it does there.
func (s *store) RegisterRepository(ctx context.Context, dir, defaultBranch string) (Repository, bool, error) {
	const op = "daemon.wsm.register_repository"
	normalized, err := normalizeDir(dir)
	if err != nil {
		s.log.Error(op, "refused a repository dir that cannot be normalized", withError(dlog.Context{"dir": dir}, err))
		return Repository{}, false, err
	}
	fields := dlog.Context{"dir": normalized, "requested_dir": dir}
	if err := s.refuseTemporary(op, normalized); err != nil {
		return Repository{}, false, err
	}

	var (
		out     Repository
		created bool
	)
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var existing RepoID
		err := tx.QueryRowContext(ctx, `SELECT id FROM repositories WHERE dir = ?`, normalized).Scan(&existing)
		switch {
		case err == nil:
			created = false
		case errors.Is(err, sql.ErrNoRows):
			created = true
		default:
			return err
		}
		id, err := ensureRepo(ctx, tx, normalized, defaultBranch)
		if err != nil {
			return err
		}
		// READ THE ROW BACK rather than composing it here: the name and the
		// default branch a mint records are ensureRepo's to decide, and a
		// second spelling of them in this function is the drift that makes an
		// answer disagree with the registry it just wrote.
		var scanErr error
		out, scanErr = scanRepository(tx.QueryRowContext(ctx, `SELECT `+repositoryColumns+` FROM repositories WHERE id = ?`, id))
		return scanErr
	})
	if err != nil {
		return Repository{}, false, err
	}
	return out, created, nil
}

// RefuseTemporary is refuseTemporary for a verb that asks before it registers.
func (s *store) RefuseTemporary(dir string) error {
	return s.refuseTemporary("daemon.wsm.refuse_temporary", dir)
}

// refuseTemporary is the registry's ONE temporary-directory check, run by both
// writes that mint a row (RegisterWorkspace, RegisterRepository) before they
// touch the database: a directory inside a temporary root never becomes a
// repository or a workspace (owner ruling, 2026-10-06).
//
// The refusal is an ANSWER the verb above reports at INFO through its typed
// refusal, so here it is recorded at DEBUG; the answer surfaces as a
// *tempdirs.InsideError the caller reads with tempdirs.AsInside. A dir the
// guard cannot canonicalize is a fault and is recorded at ERROR.
func (s *store) refuseTemporary(op, dir string) error {
	err := s.temporary.Check(dir)
	if err == nil {
		return nil
	}
	if inside, ok := tempdirs.AsInside(err); ok {
		s.log.Debug(op, "refused a directory inside a temporary root", dlog.Context{
			"dir": inside.Dir, "temporary_root": inside.Root,
		})
		return fmt.Errorf("wsm: %w", err)
	}
	s.log.Error(op, "could not judge whether the directory is temporary", withError(dlog.Context{"dir": dir}, err))
	return fmt.Errorf("wsm: judge %q: %w", dir, err)
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

// repositoryColumns is every column a Repository is read from, in
// scanRepository's order: the ONE column list both reads share.
const repositoryColumns = `id, dir, name, default_branch, folded`

// scanRepository reads one Repository from a row of repositoryColumns.
func scanRepository(row interface{ Scan(...any) error }) (Repository, error) {
	var repo Repository
	err := row.Scan(&repo.ID, &repo.Dir, &repo.Name, &repo.DefaultBranch, &repo.Folded)
	return repo, err
}

// SetRepositoryFolded records whether a repository's roster section is
// collapsed. An unknown repository is refused (ErrNotFound), never a write
// that touched nothing.
func (s *store) SetRepositoryFolded(ctx context.Context, id RepoID, folded bool) error {
	return s.write(ctx, "daemon.wsm.set_repository_folded", dlog.Context{"repo_id": string(id), "folded": folded},
		func(ctx context.Context, tx *sql.Tx) error {
			res, err := tx.ExecContext(ctx, `UPDATE repositories SET folded = ? WHERE id = ?`, folded, id)
			if err != nil {
				return err
			}
			return requireOneRow(res, fmt.Sprintf("wsm: repository %s", id))
		})
}

// ListRepositories loads every repository, all-or-nothing.
func (s *store) ListRepositories(ctx context.Context) ([]Repository, error) {
	var out []Repository
	err := s.read(ctx, "daemon.wsm.list_repositories", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT `+repositoryColumns+` FROM repositories ORDER BY dir`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []Repository
		for rows.Next() {
			repo, err := scanRepository(rows)
			if err != nil {
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
//
// A CLOSED WORKSPACE IS SERVED BY NO DAEMON. Closing releases the serving
// ownership and clears the recorded spawned shim pid IN THE SAME UPDATE that
// sets closed, so no close path (kill, close, teardown, merge terminal, boot's
// missing-directory close) can leave a closed row a later handover transfers.
// This is the one helper every close goes through; reopening touches only
// the flag, because the bring-up claims serving itself.
func (s *store) SetClosed(ctx context.Context, id WorkspaceID, closed bool) error {
	const op = "daemon.wsm.set_closed"
	fields := dlog.Context{"closed": closed}
	if !closed {
		return s.setWorkspaceField(ctx, op, "closed", id, false, fields)
	}
	fields["workspace"] = string(id)
	var released sql.NullString
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		err := tx.QueryRowContext(ctx, `SELECT serving_instance FROM workspaces WHERE id = ?`, id).Scan(&released)
		if errors.Is(err, sql.ErrNoRows) {
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		}
		if err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET closed = 1, serving_instance = NULL, spawned_shim_pid = NULL WHERE id = ?`, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
	if err != nil {
		return err
	}
	if released.Valid && released.String != "" {
		s.log.Info(op, "the close released the workspace's serving ownership",
			dlog.Context{"workspace": string(id), "instance": released.String})
	}
	return nil
}

// SetSpawnedShimPID records, or clears with nil, the pid of a shim a daemon
// SPAWNED for this workspace.
//
// IT IS WRITTEN AT THE FORK, not at a started session, and that is the whole
// point of it. A shim's socket is bound by Node several tens of milliseconds
// after the fork returns, and its two conversation locks are taken later still
// (inside StartSession), so for that whole window the spawn leaves NO KERNEL
// TRACE: a successor daemon booting into it reads "lock free, socket absent",
// concludes no shim survives, and spawns a SECOND shim onto one session
// socket, which the shim itself then refuses at bind and dies. Measured
// 2026-09-13: shim spawned at T+0, daemon killed at T+60ms, successor booted
// at T+65ms and spawned its own at T+80ms, the first bound at T+110ms and the
// second died at T+190ms. This column is that missing trace.
//
// A NON-POSITIVE PID IS REFUSED at the write, the same way Session.ShimPID is:
// the value exists to be handed to kill(pid, 0), where 0 and negatives address
// process groups rather than the one process meant.
func (s *store) SetSpawnedShimPID(ctx context.Context, id WorkspaceID, pid *int) error {
	const op = "daemon.wsm.set_spawned_shim_pid"
	fields := dlog.Context{"workspace": string(id)}
	var stored any
	if pid != nil {
		if *pid <= 0 {
			err := fmt.Errorf("wsm: workspace %s: a recorded spawned shim pid is positive, got %d", id, *pid)
			s.log.Error(op, "refused a non-positive spawned shim pid", withError(fields, err))
			return err
		}
		fields["shim_pid"] = *pid
		stored = int64(*pid)
	}
	return s.setWorkspaceField(ctx, op, "spawned_shim_pid", id, stored, fields)
}

// SetAttention sets or clears the roster's attention marker.
func (s *store) SetAttention(ctx context.Context, id WorkspaceID, on bool) error {
	return s.setWorkspaceField(ctx, "daemon.wsm.set_attention", "attention", id, on, dlog.Context{"attention": on})
}

// SetResult records, or clears with nil, the roster's last turn result. An
// undeclared turn-end arm is refused rather than stored for a later read to
// trip over.
func (s *store) SetResult(ctx context.Context, id WorkspaceID, result *TurnResult) error {
	const op = "daemon.wsm.set_result"
	fields := dlog.Context{"workspace": string(id)}
	var (
		end  any
		read bool
	)
	if result != nil {
		if !result.End.valid() {
			err := fmt.Errorf("wsm: unknown turn-end arm %q", result.End)
			s.log.Error(op, "refused an undeclared turn-end arm", withError(fields, err))
			return err
		}
		end, read = string(result.End), result.Read
		fields["end"], fields["read"] = string(result.End), result.Read
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET result_end = ?, result_read = ? WHERE id = ?`, end, read, id)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
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

// deleteRepositoryRows deletes a repository's own records: its row, and the
// merge queue's per-repository row. It is the ONE removal of a repository
// record; Forget reaches it when a repository's last workspace goes, and
// RetireRepository when a repository's directory is gone.
func deleteRepositoryRows(ctx context.Context, tx *sql.Tx, repo RepoID, repoDir string) error {
	if _, err := tx.ExecContext(ctx, `DELETE FROM repositories WHERE id = ?`, repo); err != nil {
		return err
	}
	// The merge queue's per-repository row is keyed by the repository's own
	// dir rather than by its id, so the cascade cannot reach it. A pause flag
	// for a repository no workspace is registered under names the same path
	// the repository row just stopped naming.
	if _, err := tx.ExecContext(ctx, `DELETE FROM merge_queue_repos WHERE repo_key = ?`, repoDir); err != nil {
		return err
	}
	return nil
}

// Forget deletes a workspace's every record — the nuke's durable half, and the
// whole of the forget verb. The dependent rows cascade; the creation job, which
// predates registration and so carries no reference, is deleted here in the
// same transaction.
//
// THE REPOSITORY GOES TOO when the forgotten workspace was the last one
// registered under it. Registering a directory mints BOTH records and nothing
// else ever deleted the repository one, so every register-then-clean-up left a
// repository row naming a path that may no longer exist — and a row naming a
// deleted path breaks workspace and sink resolution elsewhere. A repository
// still referenced by another workspace is left exactly as it was.
//
// The whole deletion is ONE transaction: a Forget either removes every record
// or removes none, so a failure mid-way cannot leave a workspace whose
// repository is gone or a repository whose workspaces are.
func (s *store) Forget(ctx context.Context, id WorkspaceID) (ForgetReport, error) {
	var out ForgetReport
	err := s.write(ctx, "daemon.wsm.forget", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		out = ForgetReport{}
		var (
			repo    RepoID
			repoDir string
		)
		err := tx.QueryRowContext(ctx,
			`SELECT repositories.id, repositories.dir
			   FROM workspaces JOIN repositories ON repositories.id = workspaces.repo_id
			  WHERE workspaces.id = ?`, id).Scan(&repo, &repoDir)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return fmt.Errorf("wsm: workspace %s: %w", id, ErrNotFound)
		case err != nil:
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM creation_jobs WHERE workspace_id = ?`, id); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM workspaces WHERE id = ?`, id)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
			return err
		}
		var remaining int
		if err := tx.QueryRowContext(ctx, `SELECT count(*) FROM workspaces WHERE repo_id = ?`, repo).Scan(&remaining); err != nil {
			return err
		}
		if remaining > 0 {
			return nil
		}
		if err := deleteRepositoryRows(ctx, tx, repo, repoDir); err != nil {
			return err
		}
		out.Repository = repo
		out.RepositoryDir = repoDir
		return nil
	})
	if err != nil {
		return ForgetReport{}, err
	}
	return out, nil
}
