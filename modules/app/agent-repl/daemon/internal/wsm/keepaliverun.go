package wsm

import (
	"context"
	"database/sql"
	"fmt"
)

// keepaliveRunDDL is the layout-152 addition: the workspace's KEEP-ALIVE RUN,
// how many keep-alives the daemon has sent it since its last turn closed.
// keepalive_run_turn_at is the turn clock (the workspace's last turn end, in
// nanoseconds) the run counts from, and keepalive_run_beats how many
// keep-alives were sent while it stood; NULL and 0 on every existing row.
//
// IT IS WHAT CAPS A RUN AT ITS BREAK-EVEN (cachelapse.KeepaliveRunCap): after
// that many keep-alives with no turn between them, the rent spent keeping the
// cache warm equals the one cold rewrite it protects against. The count is on
// the row, never in the keeper's memory, because daemon restarts are
// frequent and a run outlives many of them. IT IS ADDITIVE: the previous
// build's writes name neither column. The fresh-file schema appends this same
// text (schema.go).
const keepaliveRunDDL = `
ALTER TABLE workspaces ADD COLUMN keepalive_run_turn_at INTEGER;
ALTER TABLE workspaces ADD COLUMN keepalive_run_beats INTEGER NOT NULL DEFAULT 0;
`

// lastTurnEndedColumn is a workspace row's turn clock: when its most recent
// turn closed, NULL when none has.
const lastTurnEndedColumn = `(SELECT MAX(turns.closed_at) FROM turns WHERE turns.workspace_id = workspaces.id)`

// keepaliveRunColumn derives Workspace.KeepaliveRunBeats: the run's count
// while the turn clock it counts from is still the workspace's, and 0 once a
// turn has closed since. A RUN ENDS WITH NO WRITE OF ITS OWN: the turn's close
// moves the clock, and the run it counted from no longer reads.
const keepaliveRunColumn = `CASE WHEN keepalive_run_turn_at IS ` + lastTurnEndedColumn + ` THEN keepalive_run_beats ELSE 0 END`

// countKeepalive counts one sent keep-alive into workspace ID's run, inside a
// transaction the caller holds: the run goes on while its turn clock stands,
// and starts over at one under the clock a turn moved. THE TURN CLOCK IS READ
// IN THE STATEMENT THAT WRITES IT (SQLite evaluates every assignment against
// the row as it stood), so no turn's close can fall between a read and the
// count. It is THE ONE WRITE of the run (TestTheKeepaliveRunHasOneWrite).
func countKeepalive(ctx context.Context, tx *sql.Tx, id WorkspaceID) error {
	res, err := tx.ExecContext(ctx, `UPDATE workspaces SET
		keepalive_run_beats = CASE WHEN keepalive_run_turn_at IS `+lastTurnEndedColumn+` THEN keepalive_run_beats + 1 ELSE 1 END,
		keepalive_run_turn_at = `+lastTurnEndedColumn+`
		WHERE id = ?`, id)
	if err != nil {
		return fmt.Errorf("wsm: count a keep-alive into the run of workspace %s: %w", id, err)
	}
	return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
}
