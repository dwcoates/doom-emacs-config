package promptqueue

import (
	"context"
	"sort"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// A SUBMISSION WHILE PROMPTS ARE HELD AND NOTHING RUNS IS THE USER'S "TRY NOW"
// (owner ruling, 2026-10-06). Held prompts must never sit stuck with nothing
// running, even when no vendor event ends the block that holds them, so:
//
//  1. the NEW prompt is ALWAYS held, queued behind the prompts already held,
//     whatever happens next (on the after-reconnect hold, unclassified);
//  2. the OLDEST held prompt is delivered now, past a standing vendor block;
//     the prompts behind it are released and classified against the turn it
//     started by the existing release path (releaseReconnectHoldsLocked) once
//     the vendor serves;
//  3. a delivery the vendor refuses puts the oldest back on the hold in its
//     place, and the block stands as it did.
//
// A turn in flight leaves the submission on its ordinary path, and so does a
// workspace with nothing held.

// submitBehindHeld runs the try-now when it applies, reporting whether it
// answered the submission. The caller holds the delivery lock (d), has
// established that the session is up, and passes main-thread prompts only.
func (q *queue) submitBehindHeld(ctx context.Context, d *delivery, sub Submission, log dlog.Logger) (Disposition, bool, error) {
	if running, ok := q.runningTurn(d.ws); ok {
		log.Debug(opSubmit, "a turn is in flight; the submission takes the ordinary path, not a try-now", dlog.Context{"running_turn": string(running)})
		return Disposition{}, false, nil
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, d.ws)
	if err != nil {
		log.Error(opSubmit, "could not read the held prompts a submission queues behind", dlog.Context{"cause": err.Error()})
		return Disposition{}, true, err
	}
	// THE PROMPTS A TRY-NOW CAN MOVE are the ones waiting on the vendor or on
	// nothing at all: the after-reconnect hold and the free entries. One held
	// by another condition (a drain, a build refresh, a merge) waits on that
	// condition, which a submission does not end.
	held := make([]wsm.HeldPrompt, 0, len(standing))
	for _, h := range standing {
		if h.Tombstone == nil && (h.Hold == nil || *h.Hold == wsm.HoldReconnect) {
			held = append(held, h)
		}
	}
	if len(held) == 0 {
		return Disposition{}, false, nil
	}
	log.Info(opSubmit, "prompts are held and nothing runs; the submission is held behind them and the oldest is tried now",
		dlog.Context{"held": len(held)})
	disposition, err := q.hold(ctx, sub, "", &leaseHold{kind: wsm.HoldReconnect}, log)
	if err != nil {
		return Disposition{}, true, err
	}
	q.tryOldestHeld(ctx, d, held, log)
	return disposition, true, nil
}

// tryOldestHeld delivers the oldest of HELD now, past a standing vendor block,
// and releases what waits behind it once the vendor serves. A refused delivery
// puts it back on the after-reconnect hold, in its place. The caller holds the
// delivery lock (d).
func (q *queue) tryOldestHeld(ctx context.Context, d *delivery, held []wsm.HeldPrompt, log dlog.Logger) {
	sort.SliceStable(held, func(i, j int) bool { return held[i].QueuedAt.Before(held[j].QueuedAt) })
	oldest := held[0]
	fields := dlog.Context{"oldest_turn": string(oldest.Turn)}
	// AN EDIT WITHHOLDS ITS PROMPT AND EVERYTHING AFTER IT, and a call in
	// flight owns the prompt it claims.
	switch {
	case q.withheldByEdit(d.ws, oldest):
		log.Info(opSubmit, "the oldest held prompt is being edited; nothing is tried now", fields)
		return
	case q.claimedByCall(d.ws, oldest.Turn):
		log.Info(opSubmit, "the oldest held prompt is already being delivered; nothing more is tried now", fields)
		return
	}
	if oldest.Hold != nil {
		if err := q.deps.DB.UpdateHeldPromptHold(ctx, oldest.Turn, nil, ""); err != nil {
			log.Error(opSubmit, "could not lift the after-reconnect hold of the prompt tried now", merged(fields, dlog.Context{"cause": err.Error()}))
			return
		}
		oldest.Hold = nil
	}
	log.Info(opSubmit, "the oldest held prompt is tried now", fields)
	sent, err := q.deliverHeld(ctx, d, oldest, log)
	if err != nil || !sent {
		q.rehold(ctx, d, oldest, err, log)
		return
	}
	log.Info(opSubmit, "the prompt tried now was delivered; what waits behind it is released when the vendor serves", fields)
	q.releaseReconnectHoldsLocked(ctx, d, log)
}

// rehold puts a prompt the try-now could not deliver back on the
// after-reconnect hold, in its place (its QueuedAt is unchanged), and takes
// down the row its acceptance mirrored. A prompt the shim already re-held
// (holdForReconnect) or that is no longer standing needs nothing more.
func (q *queue) rehold(ctx context.Context, d *delivery, h wsm.HeldPrompt, cause error, log dlog.Logger) {
	fields := dlog.Context{"oldest_turn": string(h.Turn)}
	if cause != nil {
		fields["cause"] = cause.Error()
	}
	now, err := q.standingHold(ctx, d.ws, h.Turn)
	if err != nil || now.Tombstone != nil {
		log.Info(opSubmit, "the prompt tried now is no longer standing; nothing is re-held", fields)
		return
	}
	if now.Hold != nil && *now.Hold == wsm.HoldReconnect {
		log.Info(opSubmit, "the vendor did not take the prompt tried now; it is back on the after-reconnect hold in its place", fields)
		return
	}
	kind := wsm.HoldReconnect
	if err := q.deps.DB.UpdateHeldPromptHold(ctx, h.Turn, &kind, ""); err != nil {
		log.Error(opSubmit, "could not put the refused prompt back on the after-reconnect hold", merged(fields, dlog.Context{"store_cause": err.Error()}))
		return
	}
	q.deps.Feed.OnPromptRetired(d.ws, &conversationv1.AgentPrompt{Id: &conversationv1.TurnId{Value: string(h.Turn)}})
	log.Info(opSubmit, "the vendor refused the prompt tried now; it is back on the after-reconnect hold in its place", fields)
	// A tray that could not be republished is recorded at ERROR by pushTray;
	// the hold itself is durable either way.
	_ = q.pushTray(ctx, d.ws, log)
}
