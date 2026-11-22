package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"strings"

	"agent-repld/internal/dlog"
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
	// A NEW RUN STARTS WITH NOTHING SPENT AND NO END RECORDED
	// (keepaliveRunSpendDDL): the columns are reset in the same statement
	// that starts it.
	res, err := tx.ExecContext(ctx, `UPDATE workspaces SET
		keepalive_run_beats = CASE WHEN keepalive_run_turn_at IS `+lastTurnEndedColumn+` THEN keepalive_run_beats + 1 ELSE 1 END,
		keepalive_run_ended = CASE WHEN keepalive_run_turn_at IS `+lastTurnEndedColumn+` THEN keepalive_run_ended ELSE 0 END,
		`+resetRunSpend+`,
		keepalive_run_turn_at = `+lastTurnEndedColumn+`
		WHERE id = ?`, id)
	if err != nil {
		return fmt.Errorf("wsm: count a keep-alive into the run of workspace %s: %w", id, err)
	}
	return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
}

// keepaliveRunSpendDDL is the layout-191 addition: what the workspace's
// keep-alive run SPENT, by token kind, and whether its END was recorded
// (EndKeepaliveRun). Zero on every existing row. The run's columns stay on
// the row after a turn moves the clock (only the derived beats read 0), so
// a run's end is read whenever it is recorded, and the ended flag records it
// once. IT IS ADDITIVE: the previous build's writes name none of them. The
// fresh-file schema appends this same text (schema.go).
const keepaliveRunSpendDDL = `
ALTER TABLE workspaces ADD COLUMN keepalive_run_uncached INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN keepalive_run_cache_read INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN keepalive_run_write_5m INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN keepalive_run_write_1h INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN keepalive_run_output INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN keepalive_run_ended INTEGER NOT NULL DEFAULT 0;
`

// runSpendColumns are the run's spend columns, in KeepaliveRunTokens order.
var runSpendColumns = []string{"keepalive_run_uncached", "keepalive_run_cache_read", "keepalive_run_write_5m", "keepalive_run_write_1h", "keepalive_run_output"}

// resetRunSpend zeroes the run's spend when countKeepalive starts a new run.
var resetRunSpend = func() string {
	out := ""
	for i, c := range runSpendColumns {
		if i > 0 {
			out += ", "
		}
		out += c + " = CASE WHEN keepalive_run_turn_at IS " + lastTurnEndedColumn + " THEN " + c + " ELSE 0 END"
	}
	return out
}()

// KeepaliveRunTokens is what a keep-alive run spent, by token kind.
type KeepaliveRunTokens struct {
	Uncached, CacheRead, Write5m, Write1h, Output uint64
}

func (t KeepaliveRunTokens) values() []any {
	return []any{t.Uncached, t.CacheRead, t.Write5m, t.Write1h, t.Output}
}

// KeepaliveRunEnd is how a keep-alive run ended.
type KeepaliveRunEnd string

// The ends of a keep-alive run.
const (
	// KeepaliveRunResumed is a run a turn ended: the workspace worked again.
	KeepaliveRunResumed KeepaliveRunEnd = "resumed"
	// KeepaliveRunCapped is a run its break-even ended (cachelapse.KeepaliveRunCap).
	KeepaliveRunCapped KeepaliveRunEnd = "run_cap"
	// KeepaliveRunClosed is a run whose workspace closed with no turn after it.
	KeepaliveRunClosed KeepaliveRunEnd = "closed"
)

// KeepaliveRunEnding is the moment EndKeepaliveRun is asked at.
type KeepaliveRunEnding int

// The moments a run's end is asked at.
const (
	// RunEndingBeat is a keep-alive about to be counted: a run a turn ended
	// since is recorded before the beat starts a new one.
	RunEndingBeat KeepaliveRunEnding = iota + 1
	// RunEndingTurnClosed is a turn's close recorded.
	RunEndingTurnClosed
	// RunEndingCap is the run reaching its break-even.
	RunEndingCap
	// RunEndingClosure is the workspace closing or reopening.
	RunEndingClosure
)

// KeepaliveRun is one ended keep-alive run.
type KeepaliveRun struct {
	Beats int
	Spent KeepaliveRunTokens
	End   KeepaliveRunEnd
}

// EndKeepaliveRun records the end of workspace ID's keep-alive run, asked at
// ENDING, when it has ended and no end was recorded for it: a run a turn
// moved the clock past ended RESUMED whenever it is asked; otherwise the cap
// ends it at RunEndingCap, and the workspace's close at RunEndingClosure
// when the row is closed. It answers the run and true when it recorded an
// end, and false when there is none to record (no run, one still standing,
// or one already recorded). The read and the mark are one transaction, so a
// run's end is recorded once.
func (s *store) EndKeepaliveRun(ctx context.Context, id WorkspaceID, ending KeepaliveRunEnding) (KeepaliveRun, bool, error) {
	var run KeepaliveRun
	var recorded bool
	err := s.write(ctx, "daemon.wsm.end_keepalive_run", dlog.Context{"workspace": string(id), "ending": int(ending)}, func(ctx context.Context, tx *sql.Tx) error {
		var stands, ended, closed bool
		if err := scanWorkspaceRow(ctx, tx, id, `keepalive_run_beats, keepalive_run_turn_at IS `+lastTurnEndedColumn+`, keepalive_run_ended, closed, `+strings.Join(runSpendColumns, ", "),
			&run.Beats, &stands, &ended, &closed, &run.Spent.Uncached, &run.Spent.CacheRead, &run.Spent.Write5m, &run.Spent.Write1h, &run.Spent.Output); err != nil {
			return err
		}
		if run.Beats == 0 || ended {
			return nil
		}
		switch {
		case !stands:
			run.End = KeepaliveRunResumed
		case ending == RunEndingCap:
			run.End = KeepaliveRunCapped
		case ending == RunEndingClosure && closed:
			run.End = KeepaliveRunClosed
		default:
			return nil
		}
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET keepalive_run_ended = 1 WHERE id = ?`, id)
		if err != nil {
			return err
		}
		recorded = true
		return requireOneRow(res, fmt.Sprintf("wsm: workspace %s", id))
	})
	if err != nil || !recorded {
		return KeepaliveRun{}, false, err
	}
	return run, true, nil
}

// AddKeepaliveRunSpend adds SPENT to workspace ID's keep-alive run while the
// run stands: spend answered after a turn ended the run belongs to no run,
// and is counted only in the spend series. It answers whether a standing run
// took it.
func (s *store) AddKeepaliveRunSpend(ctx context.Context, id WorkspaceID, spent KeepaliveRunTokens) (bool, error) {
	var took bool
	err := s.write(ctx, "daemon.wsm.add_keepalive_run_spend", dlog.Context{"workspace": string(id), "spent": spent}, func(ctx context.Context, tx *sql.Tx) error {
		set := make([]string, len(runSpendColumns))
		for i, c := range runSpendColumns {
			set[i] = c + " = " + c + " + ?"
		}
		args := append(spent.values(), id)
		res, err := tx.ExecContext(ctx, `UPDATE workspaces SET `+strings.Join(set, ", ")+`
			WHERE id = ? AND keepalive_run_beats > 0 AND keepalive_run_turn_at IS `+lastTurnEndedColumn, args...)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		took = n == 1
		return err
	})
	return took, err
}
