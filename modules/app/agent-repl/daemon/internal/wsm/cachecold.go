package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// cacheColdDDL is the layout-90 addition: cache_cold_turn_at, the turn clock
// (the workspace's last turn end, in nanoseconds) at which its prompt cache was
// last KNOWN COLD; NULL when it never was, which every existing row is.
//
// IT IS WHAT KEEPS A COLD CACHE FROM BEING BEATEN (owner ruling, 2026-10-29:
// passing the cold gate by any answer makes no LLM request of its own). A
// cleared or compacted conversation's shim anchor carries no cache lifetime, so
// the shim never refuses a keep-alive as lapsed, and the cache clock still
// reads the pre-gate turn, so the beat was overdue the moment the answer's
// session came up and went out at once (2026-10-29, doom and
// agent-repl-theme-update). No keep-alive is owed while the last turn end has
// not moved past this mark (Workspace.CacheKnownCold); the next real turn's
// close moves it, and the beats resume. It is on the row, not in the keeper's
// memory, so a restart keeps it. IT IS ADDITIVE: the previous build's writes
// name no such column. The fresh-file schema appends this same text
// (schema.go).
const cacheColdDDL = `
ALTER TABLE workspaces ADD COLUMN cache_cold_turn_at INTEGER;
`

// MarkCacheCold records workspace ID's prompt cache as cold at its current
// turn clock, and answers the turn clock it recorded (nil when no turn of the
// workspace has closed, so no beat is owed anyway).
//
// THE TURN CLOCK IS READ IN THE STATEMENT THAT WRITES IT, so no turn's close
// can fall between a read and the mark and be marked cold with it; the
// mark is read back in the same transaction.
func (s *store) MarkCacheCold(ctx context.Context, id WorkspaceID) (*time.Time, error) {
	var marked *time.Time
	err := s.write(ctx, "daemon.wsm.mark_cache_cold", dlog.Context{"workspace": string(id)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE workspaces
			SET cache_cold_turn_at = (SELECT MAX(turns.closed_at) FROM turns WHERE turns.workspace_id = workspaces.id)
			WHERE id = ?`, id)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id)); err != nil {
			return err
		}
		var at sql.NullInt64
		if err := scanWorkspaceRow(ctx, tx, id, `cache_cold_turn_at`, &at); err != nil {
			return err
		}
		marked = optTime(at)
		return nil
	})
	return marked, err
}

// CacheKnownCold reports whether the workspace's prompt cache is known cold:
// marked cold (MarkCacheCold) and no turn has closed since. A workspace no
// turn of which has closed has no cache to keep either way, and is not marked.
func (w Workspace) CacheKnownCold() bool {
	return w.CacheColdTurnAt != nil && w.LastTurnEndedAt != nil && !w.LastTurnEndedAt.After(*w.CacheColdTurnAt)
}
