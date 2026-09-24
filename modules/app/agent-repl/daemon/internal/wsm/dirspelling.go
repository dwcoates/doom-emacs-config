package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
)

// A WORKSPACE ROW WHOSE `dir` IS NOT THE DIRECTORY'S ON-DISK SPELLING is left
// over from before normalizeDir read the case back off the disk. Registration
// used to key on the spelling it was handed, so a case-insensitive volume let
// one worktree be registered twice — 223c4ec7b27f4f5a at `.../ChessCom/...`
// and b58a6f10703e493f at `.../chesscom/...`, 2026-09-15 — and every lookup
// now answers the on-disk spelling, so a row stored under another one is a
// row no lookup by directory can reach.
//
// So the open RECONCILES each such row, once per boot, the way the
// repository invariant beside it is reported (repoinvariant.go):
//
//   - a CLOSED row that is the only one for its directory is RESPELLED to the
//     on-disk spelling (INFO). Nothing else keys on the spelling of a closed
//     row: its shim, whose kernel lock is derived from the directory, is gone.
//   - a CLOSED row whose directory another row already holds is a
//     CASE-DUPLICATE, and it is RETIRED — forgotten — when it carries nothing
//     a user could lose: no turn, no prompt still held, no session still
//     live, no lease, no merge, no ported prompt (INFO). Merging two
//     workspaces' state instead would pick one vendor conversation over the
//     other, which is not a call the daemon can make.
//   - anything else — an OPEN row, or a duplicate carrying any of the above —
//     is REPORTED once, at ERROR, naming every row and the remedy, and left
//     exactly as it is: it is the user's data.
//
// A row whose directory cannot be canonicalized at all is reported in the same
// record. The open proceeds in every case: a daemon that will not boot serves
// nothing, which is strictly worse than one that says what is wrong.

// dirSpellingRemediation is the one spelling of what to do about a row the
// open could not reconcile itself.
const dirSpellingRemediation = "close the workspace and restart the daemon to respell it, or forget the duplicate once nothing in it is needed"

// spelledRow is one workspace row as the reconciliation reads it.
type spelledRow struct {
	id     WorkspaceID
	dir    string
	closed bool
}

// reconcileDirSpellings is the boot-time reconciliation described above.
//
// The READ's own failure is not softened: a registry this store cannot read is
// a broken file, and it fails the open the way every other all-or-nothing read
// in this package does. So does a respell or a retire that fails to commit.
func (s *store) reconcileDirSpellings(ctx context.Context) error {
	const op = "daemon.wsm.open"
	rows, err := s.spelledRows(ctx)
	if err != nil {
		s.log.Error(op, "could not read the workspace registry to reconcile its directory spellings", dlog.Context{
			"path": s.path, "error": err.Error(),
		})
		return fmt.Errorf("wsm: read the workspace registry in %q: %w", s.path, err)
	}
	holders := make(map[string]WorkspaceID, len(rows))
	for _, row := range rows {
		holders[row.dir] = row.id
	}
	var unreconciled []string
	for _, row := range rows {
		canonical, err := s.canonicalDir(row.dir)
		if err != nil {
			unreconciled = append(unreconciled, fmt.Sprintf("%s@%s (cannot canonicalize: %v)", row.id, row.dir, err))
			continue
		}
		if canonical == row.dir {
			continue
		}
		holder, taken := holders[canonical]
		switch {
		case !row.closed:
			unreconciled = append(unreconciled, fmt.Sprintf("%s@%s (open; on disk %s)", row.id, row.dir, canonical))
		case !taken:
			if err := s.respell(ctx, row, canonical); err != nil {
				return err
			}
			delete(holders, row.dir)
			holders[canonical] = row.id
		default:
			holding, err := s.stateHeldBy(ctx, row.id)
			if err != nil {
				s.log.Error(op, "could not read what a case-duplicate workspace holds", dlog.Context{
					"path": s.path, "workspace": string(row.id), "error": err.Error(),
				})
				return fmt.Errorf("wsm: read what workspace %s holds: %w", row.id, err)
			}
			if len(holding) > 0 {
				unreconciled = append(unreconciled, fmt.Sprintf("%s@%s (duplicate of %s; holds %s)",
					row.id, row.dir, holder, strings.Join(holding, "+")))
				continue
			}
			if _, err := s.Forget(ctx, row.id); err != nil {
				s.log.Error(op, "could not retire a case-duplicate workspace", dlog.Context{
					"path": s.path, "workspace": string(row.id), "error": err.Error(),
				})
				return fmt.Errorf("wsm: retire case-duplicate workspace %s: %w", row.id, err)
			}
			delete(holders, row.dir)
			s.log.Info(op, "retired a closed workspace registered under another spelling of a registered directory; it held nothing", dlog.Context{
				"path": s.path, "workspace": string(row.id), "dir": row.dir,
				"canonical_dir": canonical, "kept_workspace": string(holder),
			})
		}
	}
	if len(unreconciled) == 0 {
		s.log.Debug(op, "every registered workspace is keyed by its directory's on-disk spelling", dlog.Context{"path": s.path})
		return nil
	}
	s.log.Error(op, "workspaces are registered under a spelling that is not their directory's on-disk one", dlog.Context{
		"path":                s.path,
		"workspaces":          len(unreconciled),
		"rows":                strings.Join(unreconciled, ","),
		"invariant_violation": "workspace.Dir is its directory's on-disk spelling",
		"remediation":         dirSpellingRemediation,
	})
	return nil
}

// spelledRows loads every workspace row's identity, directory and closed flag,
// oldest first, all-or-nothing.
func (s *store) spelledRows(ctx context.Context) ([]spelledRow, error) {
	var out []spelledRow
	err := s.read(ctx, "daemon.wsm.dir_spelling", dlog.Context{}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx, `SELECT id, dir, closed FROM workspaces ORDER BY created_at, id`)
		if err != nil {
			return err
		}
		defer rows.Close()
		var loaded []spelledRow
		for rows.Next() {
			var row spelledRow
			if err := rows.Scan(&row.id, &row.dir, &row.closed); err != nil {
				return err
			}
			loaded = append(loaded, row)
		}
		if err := rows.Err(); err != nil {
			return err
		}
		out = loaded
		return nil
	})
	return out, err
}

// respell rewrites one closed row's directory to its on-disk spelling.
func (s *store) respell(ctx context.Context, row spelledRow, canonical string) error {
	const op = "daemon.wsm.open"
	fields := dlog.Context{"path": s.path, "workspace": string(row.id), "dir": row.dir, "canonical_dir": canonical}
	err := s.write(ctx, "daemon.wsm.dir_spelling", fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET dir = ? WHERE id = ? AND dir = ?`, canonical, row.id, row.dir)
		if err != nil {
			return err
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", row.id))
	})
	if err != nil {
		s.log.Error(op, "could not respell a closed workspace to its directory's on-disk spelling", withError(fields, err))
		return fmt.Errorf("wsm: respell workspace %s: %w", row.id, err)
	}
	s.log.Info(op, "respelled a closed workspace to its directory's on-disk spelling", fields)
	return nil
}

// heldStateQueries name, per kind of state, the one query that counts a
// workspace's rows of it. A case-duplicate holding ANY of these is never
// retired by the open: each is something a user could lose.
var heldStateQueries = []struct {
	kind  string
	query string
}{
	{kind: "turns", query: `SELECT count(*) FROM turns WHERE workspace_id = ?`},
	{kind: "held_prompts", query: `SELECT count(*) FROM held_prompts WHERE workspace_id = ? AND tombstone_kind IS NULL`},
	{kind: "live_session", query: `SELECT count(*) FROM sessions WHERE workspace_id = ? AND terminal_kind IS NULL`},
	{kind: "leases", query: `SELECT count(*) FROM leases WHERE workspace_id = ?`},
	{kind: "merge_ledger", query: `SELECT count(*) FROM merge_ledger WHERE workspace_id = ?`},
	{kind: "merge_queue", query: `SELECT count(*) FROM merge_queue WHERE workspace_id = ?`},
	{kind: "ported_prompts", query: `SELECT count(*) FROM ported_prompts WHERE workspace_id = ?`},
}

// stateHeldBy names every kind of state one workspace still holds.
func (s *store) stateHeldBy(ctx context.Context, id WorkspaceID) ([]string, error) {
	var out []string
	err := s.read(ctx, "daemon.wsm.dir_spelling", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var held []string
		for _, q := range heldStateQueries {
			var n int
			if err := s.db().QueryRowContext(ctx, q.query, id).Scan(&n); err != nil {
				return fmt.Errorf("count %s: %w", q.kind, err)
			}
			if n > 0 {
				held = append(held, q.kind)
			}
		}
		out = held
		return nil
	})
	return out, err
}
