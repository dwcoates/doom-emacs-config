package wsm

import (
	"context"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
)

// A WORKSPACE WHOSE `repo_id` NAMES NO REPOSITORY ROW IS AN INVARIANT
// VIOLATION, and it is impossible to create through this package: the
// `workspaces.repo_id` column is declared `NOT NULL REFERENCES
// repositories(id)` (schema.go), every handle Open hands out carries
// `_pragma=foreign_keys(1)` (open.go) so SQLite refuses the write at the
// engine, and the one INSERT — RegisterWorkspace — mints the repository row
// through ensureRepo FIRST, inside the same transaction as the workspace row.
//
// What no schema can prevent is a row that was ALREADY there. Foreign keys are
// a per-connection pragma, so any handle opened without it — the `sqlite3` CLI,
// which defaults the pragma OFF, a file restored from a backup taken through
// one, a repair someone ran by hand — can leave a workspace behind whose
// repository is gone. Such a row reads back perfectly and then disappears out
// of the roster's repository grouping, which is exactly how one was found (the
// daemon log sweep, 2026-09-13).
//
// So the open REPORTS it, once, at ERROR, naming every row and the remedy. It
// does not delete anything: the workspace state is the user's data and is
// carried forward, never thrown away (see migrate.go), and a row nobody can see
// is still a row someone can re-register. It does not refuse the open either —
// a daemon that will not boot serves nothing at all, which is strictly worse
// than a daemon that boots and says what is wrong.

// orphanWorkspace is one workspace row whose repository is not registered. It
// is the shape the boot-time report names each violation with.
type orphanWorkspace struct {
	// ID is the workspace's daemon-minted identity.
	ID WorkspaceID
	// Dir is the workspace's worktree directory, which is what a person
	// re-registers to heal the row.
	Dir string
	// Repo is the repository identity the row names and the registry does not
	// carry.
	Repo RepoID
}

// orphanRemediation is the one spelling of what to do about a violating row.
// The sidebar's own assertion quotes the same sentence, so the log a person
// reads says the same thing wherever they meet the violation.
const orphanRemediation = "re-register the workspace's directory, which mints its repository row, or forget the workspace"

// workspacesWithUnregisteredRepository loads every workspace row whose repo_id
// matches no repository, all-or-nothing. It is the invariant's own query: the
// boot check reads it, and a test can assert it directly.
func (s *store) workspacesWithUnregisteredRepository(ctx context.Context) ([]orphanWorkspace, error) {
	var out []orphanWorkspace
	err := s.read(ctx, "daemon.wsm.repository_invariant", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `
			SELECT workspaces.id, workspaces.dir, workspaces.repo_id
			  FROM workspaces
			  LEFT JOIN repositories ON repositories.id = workspaces.repo_id
			 WHERE repositories.id IS NULL
			 ORDER BY workspaces.created_at, workspaces.id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []orphanWorkspace
		for rows.Next() {
			var orphan orphanWorkspace
			if err := rows.Scan(&orphan.ID, &orphan.Dir, &orphan.Repo); err != nil {
				return err
			}
			loaded = append(loaded, orphan)
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

// checkRepositoryInvariant is the boot-time check. A clean file records
// nothing; a file carrying violations records ONE error naming all of them and
// the remedy, and the open proceeds.
//
// The QUERY's own failure is not softened: a read this store cannot perform is
// a broken file, and it fails the open the way every other all-or-nothing read
// in this package does.
func (s *store) checkRepositoryInvariant(ctx context.Context) error {
	const op = "daemon.wsm.open"
	orphans, err := s.workspacesWithUnregisteredRepository(ctx)
	if err != nil {
		s.log.Error(op, "could not check whether every workspace's repository is registered", dlog.Context{
			"path": s.path, "error": err.Error(),
		})
		return fmt.Errorf("wsm: check the repository invariant in %q: %w", s.path, err)
	}
	if len(orphans) == 0 {
		s.log.Debug(op, "every registered workspace names a registered repository", dlog.Context{"path": s.path})
		return nil
	}
	named := make([]string, 0, len(orphans))
	for _, orphan := range orphans {
		named = append(named, fmt.Sprintf("%s@%s->%s", orphan.ID, orphan.Dir, orphan.Repo))
	}
	s.log.Error(op, "workspaces name a repository the registry does not carry", dlog.Context{
		"path":                s.path,
		"workspaces":          len(orphans),
		"rows":                strings.Join(named, ","),
		"invariant_violation": "workspace.Repo names no registered repository",
		"remediation":         orphanRemediation,
	})
	return nil
}
