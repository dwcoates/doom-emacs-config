package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"time"

	"agent-repld/internal/dirpath"
	"agent-repld/internal/dlog"
	"agent-repld/internal/tempdirs"
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
//
// The last three columns are DERIVED, not stored on the row: whether the
// workspace's creation job asked for the one-shot form, whether a merge
// queue entry stands for it -- its own, or that of a merge workspace forked
// for its detached merge (`merge_of` names it), which is where its merge
// stands once handed off -- and when its last turn ended. Every read selects
// FROM workspaces unaliased, so the correlated subqueries name
// `workspaces.id`.
//
// THE LAST TURN END IS READ OFF THE TURNS THEMSELVES rather than stamped on
// the row, so it cannot drift from the closes it summarizes: CloseTurn's one
// write of closed_at is the only edge, and turns_by_workspace keeps the MAX an
// index range.
const workspaceColumns = `id, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, priority, task_id, last_selected_at, last_activity_at, merged_at, spawned_shim_pid, created_at, result_end, result_read, hidden, hidden_at, merge_of, merge_request_id, merge_fork_head, merge_compact_after, merge_close_on_landing, merge_cleanup_worktree, merge_cleanup_dir, landed_head, landed_at, commit_count, place, merge_revealed, cache_lapses_at, cache_lapse_gated_tokens, closure_at, closure_by, status_changed_at, is_current, ` + rootSubsectionColumn + `, ` +
	`EXISTS (SELECT 1 FROM creation_jobs WHERE creation_jobs.workspace_id = workspaces.id AND creation_jobs.one_shot = 1), ` +
	`EXISTS (SELECT 1 FROM merge_queue WHERE merge_queue.workspace_id = workspaces.id ` +
	`OR merge_queue.workspace_id IN (SELECT merge_ws.id FROM workspaces AS merge_ws WHERE merge_ws.merge_of = workspaces.id)), ` +
	`(SELECT MAX(turns.closed_at) FROM turns WHERE turns.workspace_id = workspaces.id)`

// scanWorkspace decodes one workspace row all-or-nothing: an out-of-range
// priority is a decode failure, never a silently substituted default.
func scanWorkspace(row interface{ Scan(...any) error }) (Workspace, error) {
	var (
		ws            Workspace
		parent        sql.NullString
		priority      sql.NullInt64
		task          sql.NullString
		selected      sql.NullInt64
		activity      sql.NullInt64
		merged        sql.NullInt64
		spawned       sql.NullInt64
		created       int64
		end           sql.NullString
		read          bool
		mergeOf       sql.NullString
		request       sql.NullString
		forkHead      sql.NullString
		compact       bool
		closes        sql.NullString
		cleanup       bool
		cleanDir      sql.NullString
		landHead      sql.NullString
		landAt        sql.NullInt64
		commits       sql.NullInt64
		ended         sql.NullInt64
		hiddenAt      sql.NullInt64
		lapsesAt      sql.NullInt64
		gated         sql.NullInt64
		closedAt      sql.NullInt64
		closedBy      sql.NullString
		statusChanged sql.NullInt64
		subsection    sql.NullString
	)
	if err := row.Scan(&ws.ID, &ws.Repo, &ws.Dir, &ws.Name, &ws.Branch, &ws.ParentBranch, &parent, &ws.Closed, &ws.Attention, &priority, &task, &selected, &activity, &merged, &spawned, &created, &end, &read, &ws.Hidden, &hiddenAt, &mergeOf, &request, &forkHead, &compact, &closes, &cleanup, &cleanDir, &landHead, &landAt, &commits, &ws.Place, &ws.MergeRevealed, &lapsesAt, &gated, &closedAt, &closedBy, &statusChanged, &ws.Current, &subsection, &ws.OneShot, &ws.MergePending, &ended); err != nil {
		return Workspace{}, err
	}
	if mergeOf.Valid != request.Valid {
		return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "merge_of", Err: errors.New("a merge workspace records both its requester and its request")}
	}
	if mergeOf.Valid {
		ws.MergeOf = &MergeOrigin{Requester: WorkspaceID(mergeOf.String), Request: DetachedMergeID(request.String), ForkHead: forkHead.String, CompactAfter: compact,
			CloseOnLanding: WorkspaceID(closes.String), CleanupWorktree: cleanup, CleanupDir: cleanDir.String}
	} else if closes.Valid || cleanup || cleanDir.Valid {
		return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "merge_close_on_landing", Err: errors.New("only a merge workspace carries an async merge's landing effects")}
	} else if forkHead.Valid {
		return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "merge_fork_head", Err: errors.New("only a merge workspace records a fork head")}
	} else if compact {
		return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "merge_compact_after", Err: errors.New("only a merge workspace compacts its requester after it lands")}
	}
	landed, err := decodeLanding(ws.ID, landHead, landAt)
	if err != nil {
		return Workspace{}, err
	}
	ws.Landed = landed
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
	ws.Subsection = nullSubsection(subsection)
	ws.LastSelectedAt = optTime(selected)
	ws.LastActivityAt = optTime(activity)
	ws.LastTurnEndedAt = optTime(ended)
	ws.StatusChangedAt = optTime(statusChanged)
	ws.MergedAt = optTime(merged)
	ws.CreatedAt = fromNanos(created)
	// HIDDEN AND ITS INSTANT STAND AND FALL TOGETHER (SetHidden writes both in
	// one statement, and layout 43 stamped every row already hidden), so a
	// row with one and not the other is corrupt.
	if ws.Hidden != hiddenAt.Valid {
		return Workspace{}, &DecodeError{Table: "workspaces", Row: string(ws.ID), Field: "hidden_at", Err: errors.New("a workspace records when it was hidden exactly while it is hidden")}
	}
	ws.HiddenAt = optTime(hiddenAt)
	lapse, err := decodeCacheLapse(ws, lapsesAt, gated)
	if err != nil {
		return Workspace{}, err
	}
	ws.CacheLapse = lapse
	closure, err := decodeClosure(ws, closedAt, closedBy)
	if err != nil {
		return Workspace{}, err
	}
	ws.Closure = closure
	count, err := decodeCommitCount(ws.ID, commits)
	if err != nil {
		return Workspace{}, err
	}
	ws.CommitCount = count
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
		var priority any
		if facts.Priority != nil {
			if !facts.Priority.valid() {
				return fmt.Errorf("wsm: unknown priority %d", int(*facts.Priority))
			}
			priority = int(*facts.Priority)
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
			Priority:     facts.Priority,
			CreatedAt:    time.Now().UTC(),
			MergeOf:      facts.MergeOf,
		}
		var mergeOf, request, forkHead, closeOnLanding, cleanupDir any
		compactAfter, cleanupWorktree := false, false
		if out.MergeOf != nil {
			compactAfter = out.MergeOf.CompactAfter
			cleanupWorktree = out.MergeOf.CleanupWorktree
			closeOnLanding = nullableText(string(out.MergeOf.CloseOnLanding))
			cleanupDir = nullableText(out.MergeOf.CleanupDir)
			mergeOf, request = string(out.MergeOf.Requester), string(out.MergeOf.Request)
			if out.MergeOf.ForkHead != "" {
				forkHead = out.MergeOf.ForkHead
			}
		}
		out.Place, err = landingPlace(ctx, tx, out.Priority, out.ID)
		if err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO workspaces (id, repo_id, dir, name, branch, parent_branch, parent_id, closed, attention, priority, task_id, is_current, last_selected_at, merged_at, serving_instance, created_at, merge_of, merge_request_id, merge_fork_head, merge_compact_after, merge_close_on_landing, merge_cleanup_worktree, merge_cleanup_dir, place)
			 VALUES (?, ?, ?, ?, ?, ?, ?, 0, 0, ?, NULL, 0, NULL, NULL, NULL, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			out.ID, out.Repo, out.Dir, out.Name, out.Branch, out.ParentBranch, nullWorkspace(out.Parent), priority, nanos(out.CreatedAt), mergeOf, request, forkHead, compactAfter, closeOnLanding, cleanupWorktree, cleanupDir, out.Place)
		if err != nil {
			return err
		}
		// A CHILD SITS WHEREVER ITS PARENT DOES (arrange.go): the record
		// answered reads its subsection the way every read does.
		var sub sql.NullString
		if err := tx.QueryRowContext(ctx, `SELECT `+rootSubsectionColumn+` FROM workspaces WHERE id = ?`, out.ID).Scan(&sub); err != nil {
			return err
		}
		out.Subsection = nullSubsection(sub)
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
	// A NEW REPOSITORY LANDS AFTER EVERY OTHER ONE (arrange.go).
	if _, err := tx.ExecContext(ctx, `INSERT INTO repositories (id, dir, name, default_branch, place) VALUES (?, ?, ?, ?, (SELECT COALESCE(MAX(place), 0) + 1 FROM repositories))`, id, repoDir, filepath.Base(repoDir), defaultBranch); err != nil {
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
		loaded, err := listWorkspaces(ctx, s.db())
		out = loaded
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// listWorkspaces loads every workspace through Q, all-or-nothing.
func listWorkspaces(ctx context.Context, q querier) ([]Workspace, error) {
	return listRows(ctx, q, `SELECT `+workspaceColumns+` FROM workspaces ORDER BY created_at, id`, scanWorkspace)
}

// repositoryColumns is every column a Repository is read from, in
// scanRepository's order: the ONE column list both reads share.
const repositoryColumns = `id, dir, name, default_branch, folded, place`

// scanRepository reads one Repository from a row of repositoryColumns.
func scanRepository(row interface{ Scan(...any) error }) (Repository, error) {
	var repo Repository
	err := row.Scan(&repo.ID, &repo.Dir, &repo.Name, &repo.DefaultBranch, &repo.Folded, &repo.Place)
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
		loaded, err := listRepositories(ctx, s.db())
		out = loaded
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// listRepositories loads every repository through Q, in their stored order,
// all-or-nothing.
func listRepositories(ctx context.Context, q querier) ([]Repository, error) {
	return listRows(ctx, q, `SELECT `+repositoryColumns+` FROM repositories ORDER BY place`, scanRepository)
}

// setWorkspaceField is the one write path for a single workspace column: it
// refuses an unknown workspace rather than silently affecting no rows.
func (s *store) setWorkspaceField(ctx context.Context, op, column string, id WorkspaceID, value any, fields dlog.Context) error {
	return s.setWorkspaceColumns(ctx, op, id, fields, workspaceColumn{name: column, value: value})
}

// workspaceColumn is one column setWorkspaceColumns assigns, and its value.
type workspaceColumn struct {
	name  string
	value any
}

// setWorkspaceColumns assigns COLUMNS on one workspace's row in ONE statement,
// so they land together or not at all. It is the one write path for a plain
// update of a workspace's own columns, and it refuses an unknown workspace
// rather than silently affecting no rows.
func (s *store) setWorkspaceColumns(ctx context.Context, op string, id WorkspaceID, fields dlog.Context, columns ...workspaceColumn) error {
	fields["workspace"] = string(id)
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		return updateWorkspaceColumns(ctx, tx, id, columns...)
	})
}

// updateWorkspaceColumns is setWorkspaceColumns's statement, inside a
// transaction the caller already holds: for a write whose columns depend on a
// read of the same row (Reopen), so the read and the update cannot interleave
// with another write.
func updateWorkspaceColumns(ctx context.Context, tx *sql.Tx, id WorkspaceID, columns ...workspaceColumn) error {
	assignments := make([]string, len(columns))
	args := make([]any, 0, len(columns)+1)
	for i, column := range columns {
		assignments[i] = column.name + " = ?"
		args = append(args, column.value)
	}
	args = append(args, id)
	res, err := tx.ExecContext(ctx, `UPDATE workspaces SET `+strings.Join(assignments, ", ")+` WHERE id = ?`, args...)
	if err != nil {
		return err
	}
	return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
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

// MarkClosed records that a workspace's editor state is torn down, and, in the
// same statement, WHEN and BY WHOM (frontend.v1.RosterRowClosure).
//
// A CLOSED WORKSPACE IS SERVED BY NO DAEMON. Closing releases the serving
// ownership and clears the recorded spawned shim pid IN THE SAME UPDATE that
// sets closed, so no close path (kill, close, teardown, merge terminal, boot's
// missing-directory close) can leave a closed row a later handover transfers.
// This is the one helper every close goes through.
//
// THE FIRST CLOSE IS THE ONE RECORDED: a row already closed keeps its record
// (a kill marks the row closed before its teardown and again after it, and a
// boot re-closes a row to release its serving ownership), except that a USER
// close takes over a record the user did not make -- the user killing a
// workspace the daemon had closed is the user's kill.
func (s *store) MarkClosed(ctx context.Context, id WorkspaceID, closure Closure) error {
	const op = "daemon.wsm.mark_closed"
	fields := dlog.Context{"workspace": string(id), "closure_at": closure.At, "closure_by": string(closure.By)}
	if !closure.By.valid() {
		err := fmt.Errorf("wsm: workspace %s: unknown close cause %q", id, closure.By)
		s.log.Error(op, "refused a close with no declared cause", withError(fields, err))
		return err
	}
	var (
		released      sql.NullString
		subscriptions int64
	)
	err := s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		err := scanWorkspaceRow(ctx, tx, id, "serving_instance", &released)
		if err != nil {
			return err
		}
		// `restamp` is evaluated against the row as it stood BEFORE this
		// update: SQLite reads every right-hand side off the old row.
		const restamp = `(closed = 0 OR (? = 'user' AND closure_by IS NOT 'user'))`
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET closed = 1, serving_instance = NULL, spawned_shim_pid = NULL, `+
			`closure_at = CASE WHEN `+restamp+` THEN ? ELSE closure_at END, `+
			`closure_by = CASE WHEN `+restamp+` THEN ? ELSE closure_by END WHERE id = ?`,
			string(closure.By), nanos(closure.At), string(closure.By), string(closure.By), id)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
			return err
		}
		// A CLOSE ENDS THE WORKSPACE'S MERGE QUEUE SUBSCRIPTIONS in the same
		// transaction, so no close path can leave one standing
		// (mergesubscriptions.go).
		subscriptions, err = endSubscriptionsInTx(ctx, tx, id)
		return err
	})
	if err != nil {
		return err
	}
	if released.Valid && released.String != "" {
		s.log.Info(op, "the close released the workspace's serving ownership",
			dlog.Context{"workspace": string(id), "instance": released.String})
	}
	if subscriptions > 0 {
		s.log.Info(op, "the close ended the workspace's merge queue subscriptions",
			dlog.Context{"workspace": string(id), "subscriptions": subscriptions})
	}
	return nil
}

// Reopen records that a workspace's editor state stands again, and clears its
// close record in the same statement.
//
// A RE-OPEN MOVES NOTHING: the workspace returns to the place it held in the
// registry when it was closed (place.go). Re-opening touches only the flag,
// the record and a merge's settlement, because the bring-up claims serving
// itself.
//
// AN OPEN WORKSPACE IS NEVER MERGED. Re-opening a merged workspace is the user
// taking it back to revise what it landed, so the same statement retires the
// merge's settlement: merged_at (the row leaves Recently Merged for its
// groupings), the count the merge landed (commit_count goes back to "never
// counted", and the next turn close recounts it live), the detached merge's
// landing (landed_head, landed_at), which would otherwise have the merge
// workspace sweep retire it again, and, for a merge workspace, its hiding: a
// merge workspace the user opened is revealed, never drawn nowhere. A row that
// is not merged keeps every one of those columns as it stands.
//
// A REOPENED WORKSPACE KEEPS ITS CACHE WARM AGAIN, so the same statement
// clears the projected cache lapse its close left (cachelapse.Keeper), unless
// the row is still hidden, whose projection is the hide's.
func (s *store) Reopen(ctx context.Context, id WorkspaceID) error {
	fields := dlog.Context{"workspace": string(id)}
	return s.write(ctx, "daemon.wsm.reopen", fields, func(ctx context.Context, tx *sql.Tx) error {
		var merged sql.NullInt64
		var mergeOf sql.NullString
		var hidden bool
		if err := scanWorkspaceRow(ctx, tx, id, "merged_at, merge_of, hidden", &merged, &mergeOf, &hidden); err != nil {
			return err
		}
		fields["was_merged"] = merged.Valid
		columns := []workspaceColumn{
			{name: "closed", value: false},
			{name: "closure_at", value: nil},
			{name: "closure_by", value: nil},
		}
		if !hidden {
			columns = append(columns,
				workspaceColumn{name: "cache_lapses_at", value: nil},
				workspaceColumn{name: "cache_lapse_gated_tokens", value: nil})
		}
		if merged.Valid {
			columns = append(columns,
				workspaceColumn{name: "merged_at", value: nil},
				workspaceColumn{name: "commit_count", value: nil},
				workspaceColumn{name: "landed_head", value: nil},
				workspaceColumn{name: "landed_at", value: nil})
			if mergeOf.Valid {
				columns = append(columns, workspaceColumn{name: "merge_revealed", value: true})
			}
		}
		return updateWorkspaceColumns(ctx, tx, id, columns...)
	})
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
	return s.setWorkspaceColumns(ctx, op, id, fields,
		workspaceColumn{name: "result_end", value: end},
		workspaceColumn{name: "result_read", value: read})
}

// SetMergedAt records that the workspace's merge landed, and, when COMMITS is
// set, the commit count the merge brought in, in the same statement: from this
// write on, SetLiveCommitCount leaves the count alone. A nil COMMITS keeps the
// count the workspace's turns last recorded.
func (s *store) SetMergedAt(ctx context.Context, id WorkspaceID, at time.Time, commits *uint32) error {
	const op = "daemon.wsm.set_merged_at"
	fields := dlog.Context{"workspace": string(id), "merged_at": at}
	if commits == nil {
		fields["commit_count"] = nil
		return s.setWorkspaceField(ctx, op, "merged_at", id, nanos(at), fields)
	}
	fields["commit_count"] = *commits
	return s.setWorkspaceColumns(ctx, op, id, fields,
		workspaceColumn{name: "merged_at", value: nanos(at)},
		workspaceColumn{name: "commit_count", value: int64(*commits)})
}

// SetPriority sets or clears a workspace's priority. SETTING one moves the
// workspace to second-to-last among the open workspaces of that priority, in
// the same transaction (place.go); CLEARING one moves nothing. An undeclared
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
	fields["workspace"] = string(id)
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET priority = ? WHERE id = ?`, int(*p), id)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
			return err
		}
		at, err := reland(ctx, tx, id, p)
		fields["place"] = at
		return err
	})
}

// SetHidden records whether a workspace sits in the roster's hidden section,
// and, in the same statement, WHEN it was hidden: AT while it is hidden, none
// once it is shown again. The same statement CLEARS the projected cache lapse
// either way: a shown workspace keeps its cache warm again, and a fresh hide
// waits for the shim's own projection rather than reading an old one. Hiding a workspace already hidden re-stamps it, so
// the instant is always the latest hide. An unknown workspace is refused
// (ErrNotFound), never a write that touched nothing.
func (s *store) SetHidden(ctx context.Context, id WorkspaceID, hidden bool, at time.Time) error {
	var hiddenAt any
	fields := dlog.Context{"hidden": hidden}
	if hidden {
		hiddenAt = nanos(at)
		fields["hidden_at"] = at
	}
	return s.setWorkspaceColumns(ctx, "daemon.wsm.set_hidden", id, fields,
		workspaceColumn{name: "hidden", value: hidden},
		workspaceColumn{name: "hidden_at", value: hiddenAt},
		workspaceColumn{name: "cache_lapses_at", value: nil},
		workspaceColumn{name: "cache_lapse_gated_tokens", value: nil})
}

// SetCurrent records the user's selection of a workspace at an instant. At most
// one workspace is current, which the unique index makes structural: the clear
// and the set are one transaction.
//
// SELECTING A WORKSPACE CLEARS ITS PROJECTED CACHE LAPSE, as showing it does
// (SetHidden): the hidden workspace the user looks at keeps its cache warm
// (Workspace.LetsCacheLapse), and once they move off it the lapse is the
// shim's fresh projection, never the one that stood before the selection. A
// CLOSED workspace keeps its lapse: it lapses whether or not it is selected.
func (s *store) SetCurrent(ctx context.Context, id WorkspaceID, at time.Time) error {
	return s.write(ctx, "daemon.wsm.set_current", dlog.Context{"workspace": string(id), "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET is_current = 0 WHERE is_current = 1`); err != nil {
			return err
		}
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET is_current = 1, last_selected_at = ?, cache_lapses_at = CASE WHEN closed = 1 THEN cache_lapses_at END, cache_lapse_gated_tokens = CASE WHEN closed = 1 THEN cache_lapse_gated_tokens END WHERE id = ?`, nanos(at), id)
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
