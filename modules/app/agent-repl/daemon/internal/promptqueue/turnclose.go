package promptqueue

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE ONE DOOR A TURN CLOSES THROUGH. The queue owns the turn rows, and every
// close of one — a vendor terminal, a shim that died on its own, an adoption
// or a boot finding a turn nobody saw end, a teardown, a displaced turn put
// back — comes through the three functions in this file, which close the
// durable row AND tell the feed (feed.Resolver.OnTurnClosed) in one step.
// Neither happens without the other, so every turn that ends has an ending
// row: the feed draws one from the close unless the turn's own terminal
// already did.
//
// THE FEED IS TOLD EVEN WHEN THE DURABLE WRITE FAILS. The turn has ended
// either way, and a feed left showing it running is the defect this door
// exists to make impossible; the failed write is recorded at ERROR and handed
// back. doorguard_test.go fails any other production site that closes a turn.

// closeTurn closes one turn's row with HOW and draws its ending.
func (q *queue) closeTurn(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, how wsm.TurnClose, log dlog.Logger) error {
	at := q.deps.Now()
	err := q.deps.DB.CloseTurn(ctx, turn, at, how)
	if err != nil {
		log.Error(opTurnEnded, "could not stamp the turn's close; its ending is drawn regardless", dlog.Context{
			"turn": string(turn), "close": closeName(how), "cause": err.Error(),
		})
		err = fmt.Errorf("close turn %q on %q: %w", turn, ws, err)
	}
	q.deps.Feed.OnTurnClosed(ws, turn, wsm.RecordedClose{How: how, At: at})
	q.retireCutIf(ws, turn, func() (dlog.Logger, bool) { return log, true })
	return err
}

// CloseOrphans closes, in one transaction, every turn of a workspace that has
// no terminal, as orphaned, and draws each one's ending. See the Queue
// interface.
func (q *queue) CloseOrphans(ctx context.Context, ws ids.WorkspaceID, at time.Time) (wsm.OrphanReport, error) {
	report, err := q.deps.DB.CloseOrphans(ctx, ws, at)
	if err != nil {
		// NOTHING WAS CLOSED: the transaction is whole or nothing, so no
		// ending is owed and none is drawn. The caller records the failure.
		return wsm.OrphanReport{}, fmt.Errorf("close the orphaned turns of %q: %w", ws, err)
	}
	for _, turn := range report.Turns {
		q.deps.Feed.OnTurnClosed(ws, turn, wsm.RecordedClose{How: wsm.CloseOrphaned, At: report.At})
		q.retireCutIf(ws, turn, q.workspaceLog(ctx, ws))
	}
	return report, nil
}

// ClaimDisplacedTurn takes a displaced turn's mark exclusively and, when the
// turn was still open, closes it as orphaned and draws that ending. See the
// Queue interface.
func (q *queue) ClaimDisplacedTurn(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (bool, error) {
	at := q.deps.Now()
	claim, err := q.deps.DB.ClaimDisplacedTurn(ctx, turn, at)
	if err != nil {
		return false, fmt.Errorf("claim the displaced turn %q on %q: %w", turn, ws, err)
	}
	if claim.Closed {
		q.deps.Feed.OnTurnClosed(ws, turn, wsm.RecordedClose{How: wsm.CloseOrphaned, At: at})
		q.retireCutIf(ws, turn, q.workspaceLog(ctx, ws))
	}
	return claim.Claimed, nil
}
