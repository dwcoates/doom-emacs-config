package promptqueue

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/wsm"
)

const (
	opRollBack     = "daemon.promptqueue.roll_back"
	opRollBackHeld = "daemon.promptqueue.roll_back_held"
)

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
// describes it (ErrHoldsChanged, nothing performed). A shim call standing on
// the workspace is waited out first (lockDeliveryIdle).
func (q *queue) RollBack(ctx context.Context, ws ids.WorkspaceID, since time.Time, drop []ids.TurnID, perform func(context.Context) error) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"drop": len(drop)})

	// A STANDING CALL IS WAITED OUT, never run beside: a stop that lands while
	// its turn's StartTurn is in flight rolls that turn back once the shim has
	// it, rather than reaching the shim ahead of the start.
	d, err := q.lockDeliveryIdle(ctx, ws, log, "rollback")
	if err != nil {
		return fmt.Errorf("roll back %q: %w", ws, err)
	}
	defer d.unlock()

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
	// THE SHIM REWINDS WITH THE DELIVERY LOCK RELEASED (call.go): the call
	// claims every hold it drops, and no delivery runs beside it.
	d.outside(shimCall{what: "rollback", holds: drop}, log, func() { err = perform(ctx) })
	if err != nil {
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
		q.retireEditIf(ctx, d, turn, tombstoneRolledBack, log)
	}
	log.Info(opRollBack, "the rollback was performed and the held prompts queued since were dropped", nil)
	return q.pushTray(ctx, ws, log)
}

// RollBackHeld is the stop key taking back a prompt the user sent while a turn
// runs, before anything answered it (agentrepl.v1.InterruptTurn
// .roll_back_unanswered): a HELD prompt is rolled back just as a sent one is,
// so long as nothing a reader sees as the agent answering has been drawn in
// the root feed since it was queued (feed.Resolver.AnsweredSince). The
// running turn is left running: the prompt being taken back is not its.
//
// THE PROMPT IS THE NEWEST STANDING ONE THE USER SENT. A held session act is
// not a prompt, and a prompt the daemon, a program or another workspace's
// agent submitted is not the user's to take back, so neither is ever chosen.
//
// THE WHOLE DECISION IS TAKEN UNDER THE DELIVERY LOCK, the lock a turn end
// pops the head under: the hold cannot be delivered between being judged and
// being retired, and a hold a delivery call already claims is no longer
// merely held, so it is left to that delivery. ok is false, and nothing
// changes, when no turn runs, no such prompt stands, or output followed it.
func (q *queue) RollBackHeld(ctx context.Context, ws ids.WorkspaceID) (wsm.HeldPrompt, bool, error) {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return wsm.HeldPrompt{}, false, err
	}
	d := q.lockDelivery(ws)
	defer d.unlock()

	running, ok := q.turnInFlight(ws)
	if !ok {
		log.Debug(opRollBackHeld, "no turn runs, so no held prompt is taken back", nil)
		return wsm.HeldPrompt{}, false, nil
	}
	log = log.With(dlog.Context{"running": string(running)})
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opRollBackHeld, "the held prompts could not be read; none was taken back", dlog.Context{"cause": err.Error()})
		return wsm.HeldPrompt{}, false, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	held, ok := newestUserHeld(standing)
	if !ok {
		log.Debug(opRollBackHeld, "no prompt the user sent stands held", dlog.Context{"holds": len(standing)})
		return wsm.HeldPrompt{}, false, nil
	}
	log = log.With(dlog.Context{"turn": string(held.Turn), "queued_at": held.QueuedAt})
	if q.deps.Feed.AnsweredSince(ws, held.QueuedAt) {
		log.Info(opRollBackHeld, "agent output was drawn after the held prompt was queued, so it stays held", nil)
		return wsm.HeldPrompt{}, false, nil
	}
	if q.claimedByCall(ws, held.Turn) {
		log.Info(opRollBackHeld, "the held prompt is being delivered to the shim, so it is not taken back", nil)
		return wsm.HeldPrompt{}, false, nil
	}
	if err := q.dropLocked(ctx, d, held, tombstoneRolledBack, opRollBackHeld, log); err != nil {
		return wsm.HeldPrompt{}, false, err
	}
	log.Info(opRollBackHeld, "nothing answered the held prompt since it was queued, so it was taken back", nil)
	return held, true, nil
}

// newestUserHeld answers the newest of STANDING, in queue order, that is a
// prompt the user sent.
func newestUserHeld(standing []wsm.HeldPrompt) (wsm.HeldPrompt, bool) {
	for i := len(standing) - 1; i >= 0; i-- {
		held := standing[i]
		if !holdfold.SessionAct(held) && promptorigin.UserSent(promptorigin.Named(held.Origin)) {
			return held, true
		}
	}
	return wsm.HeldPrompt{}, false
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
