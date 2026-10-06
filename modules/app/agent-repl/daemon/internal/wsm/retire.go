package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"

	"claude-repld/internal/dlog"
)

// RetireRepository deletes a repository's record and every workspace record
// registered under it, in ONE transaction: either all of them go or none do.
//
// It is the boot's answer to a repository whose directory is gone
// (internal/boot retireGoneRepositories): nothing can be restored or worked in
// there, yet the row drew in the sidebar and every worktree reap logged it.
// The workspaces go the way Forget takes one -- their creation jobs by hand,
// every dependent row by the schema's cascade -- and the repository goes
// through deleteRepositoryRows, the same removal Forget's last-workspace tail
// uses.
//
// IT REFUSES rather than discards. An OPEN workspace is one a user can still
// act on, and a held prompt, a lease or a merge-queue entry is undelivered
// intent or unfinished work; each answers ErrRepositoryInUse, naming what was
// found, and writes nothing.
func (s *store) RetireRepository(ctx context.Context, id RepoID) (RetireReport, error) {
	var out RetireReport
	err := s.write(ctx, "daemon.wsm.retire_repository", dlog.Context{"repo": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		out = RetireReport{}
		var repoDir string
		err := tx.QueryRowContext(ctx, `SELECT dir FROM repositories WHERE id = ?`, id).Scan(&repoDir)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return fmt.Errorf("wsm: repository %s: %w", id, ErrNotFound)
		case err != nil:
			return err
		}
		for _, check := range []struct {
			what  string
			query string
			args  []any
		}{
			{"open workspaces", `SELECT count(*) FROM workspaces WHERE repo_id = ? AND closed = 0`, []any{id}},
			{"undelivered held prompts", `SELECT count(*) FROM held_prompts JOIN workspaces ON workspaces.id = held_prompts.workspace_id
			   WHERE workspaces.repo_id = ? AND held_prompts.tombstone_kind IS NULL`, []any{id}},
			{"leases", `SELECT count(*) FROM leases JOIN workspaces ON workspaces.id = leases.workspace_id WHERE workspaces.repo_id = ?`, []any{id}},
			{"merge-queue entries", `SELECT count(*) FROM merge_queue WHERE repo_key = ?
			   OR workspace_id IN (SELECT id FROM workspaces WHERE repo_id = ?)`, []any{repoDir, id}},
		} {
			var n int
			if err := tx.QueryRowContext(ctx, check.query, check.args...).Scan(&n); err != nil {
				return err
			}
			if n > 0 {
				return fmt.Errorf("wsm: repository %s has %d %s: %w", id, n, check.what, ErrRepositoryInUse)
			}
		}
		rows, err := tx.QueryContext(ctx, `SELECT id FROM workspaces WHERE repo_id = ? ORDER BY id`, id)
		if err != nil {
			return err
		}
		var workspaces []WorkspaceID
		for rows.Next() {
			var ws WorkspaceID
			if err := rows.Scan(&ws); err != nil {
				rows.Close()
				return err
			}
			workspaces = append(workspaces, ws)
		}
		if err := rows.Close(); err != nil {
			return err
		}
		if err := rows.Err(); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM creation_jobs WHERE workspace_id IN (SELECT id FROM workspaces WHERE repo_id = ?)`, id); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `DELETE FROM workspaces WHERE repo_id = ?`, id); err != nil {
			return err
		}
		if err := deleteRepositoryRows(ctx, tx, id, repoDir); err != nil {
			return err
		}
		out.RepositoryDir = repoDir
		out.Workspaces = workspaces
		return nil
	})
	if err != nil {
		return RetireReport{}, err
	}
	return out, nil
}
