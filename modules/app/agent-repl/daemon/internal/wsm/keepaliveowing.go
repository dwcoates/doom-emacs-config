package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

// keepaliveOwingDDL is the layout-162 addition: whether the workspace owes
// keep-alives, as the keeper last decided it (cachelapse.Keeper).
// keepalive_owed_none_at is when it began owing none, NULL while it owes
// them; keepalive_owed_from is when it last began owing them again, NULL
// when it never stopped. Both are nanoseconds, NULL on every existing row.
//
// THEY ARE ON THE ROW, NEVER IN THE KEEPER'S MEMORY, because a keep-alive due
// while the workspace owed none is no miss of the keeper's, and the daemon
// that decided it owed none is seldom the one that judges the lapse: a
// workspace withheld at 19:55 under one daemon, its beat due at 20:32, was
// judged missed at 00:13 by a successor two handovers on (2026-11-17). IT IS
// ADDITIVE: the previous build's writes name neither column. The fresh-file
// schema appends this same text (schema.go).
const keepaliveOwingDDL = `
ALTER TABLE workspaces ADD COLUMN keepalive_owed_none_at INTEGER;
ALTER TABLE workspaces ADD COLUMN keepalive_owed_from INTEGER;
`

// SetKeepaliveOwing records the keeper's decision that workspace ID owes
// keep-alives (OWING) or owes none, made at AT. Owing none keeps the earliest
// instant of a standing decision; owing again stamps keepalive_owed_from only
// when none was owed, so a workspace that owed them all along keeps no stamp.
// Each is one statement, so no other write falls between its read and write.
func (s *store) SetKeepaliveOwing(ctx context.Context, id WorkspaceID, owing bool, at time.Time) error {
	return s.write(ctx, "daemon.wsm.set_keepalive_owing", dlog.Context{"workspace": string(id), "owing": owing, "at": at}, func(ctx context.Context, tx *sql.Tx) error {
		statement := `UPDATE workspaces SET keepalive_owed_none_at = COALESCE(keepalive_owed_none_at, ?) WHERE id = ?`
		if owing {
			statement = `UPDATE workspaces SET
				keepalive_owed_from = CASE WHEN keepalive_owed_none_at IS NULL THEN keepalive_owed_from ELSE ? END,
				keepalive_owed_none_at = NULL
				WHERE id = ?`
		}
		res, err := tx.ExecContext(ctx, statement, nanos(at), id)
		if err != nil {
			return fmt.Errorf("wsm: record whether workspace %s owes keep-alives: %w", id, err)
		}
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
}
