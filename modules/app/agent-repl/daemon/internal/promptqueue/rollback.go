package promptqueue

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/holdfold"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

const opRollBack = "daemon.promptqueue.roll_back"

// HeldSince answers the held prompts of a workspace queued at or after SINCE,
// in queue order: the prompts a rollback to a prompt sent at SINCE drops. A
// held session act (a queued model or permission-mode change) is not a prompt
// and is never dropped by a rollback.
func (q *queue) HeldSince(ctx context.Context, ws ids.WorkspaceID, since time.Time) ([]ids.TurnID, error) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	return heldSince(standing, since), nil
}

func heldSince(standing []wsm.HeldPrompt, since time.Time) []ids.TurnID {
	var out []ids.TurnID
	for _, held := range standing {
		if holdfold.SessionAct(held) || held.QueuedAt.Before(since) {
			continue
		}
		out = append(out, held.Turn)
	}
	return out
}

// RollBack runs PERFORM — the vendor-side rollback — while it OWNS THE
// WORKSPACE'S TURN SEQUENCE: under the delivery lock, so no prompt is
// delivered while the conversation is being cut. Only when PERFORM succeeds
// are the held prompts DROP retired, so a refused rollback leaves the queue as
// it was. Every prompt in DROP must still be standing: the caller planned
// against the queue, and a hold that vanished means the plan no longer
// describes it (ErrHoldsChanged, nothing performed).
func (q *queue) RollBack(ctx context.Context, ws ids.WorkspaceID, since time.Time, drop []ids.TurnID, perform func(context.Context) error) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"drop": len(drop)})

	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opRollBack, "the rollback was refused: the holds could not be read", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	if now := heldSince(standing, since); !sameTurns(now, drop) {
		log.Info(opRollBack, "the rollback was refused: the held prompts changed since it was planned",
			dlog.Context{"planned": len(drop), "standing": len(now)})
		return ErrHoldsChanged
	}
	if err := perform(ctx); err != nil {
		log.Info(opRollBack, "the rollback was not performed; the held prompts stand", dlog.Context{"cause": err.Error()})
		return err
	}
	if len(drop) > 0 {
		// ALL OR NOTHING: the vendor conversation is already cut, so a partial
		// drop would leave some queued prompts gone and others standing.
		if err := q.deps.DB.TombstoneHeldPrompts(ctx, drop, wsm.Tombstone{Kind: tombstoneRolledBack, At: q.deps.Now()}); err != nil {
			log.Error(opRollBack, "the held prompts the rollback dropped could not be retired; they stay queued",
				dlog.Context{"cause": err.Error()})
			return fmt.Errorf("drop holds on %q: %w", ws, err)
		}
	}
	for _, turn := range drop {
		q.clearHeadIf(ws, turn)
		q.retireEditIf(ctx, ws, turn, tombstoneRolledBack, log)
	}
	log.Info(opRollBack, "the rollback was performed and the held prompts queued since were dropped", nil)
	return q.pushTray(ctx, ws, log)
}

func sameTurns(a, b []ids.TurnID) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
