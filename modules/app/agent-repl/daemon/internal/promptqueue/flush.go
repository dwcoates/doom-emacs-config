package promptqueue

import (
	"context"
	"fmt"
	"sort"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// A FLUSH SENDS EVERY HELD PROMPT TO THE SHIM AT ONCE (owner ruling,
// 2026-10-12). An in-place merge the agent's turn asks for runs in the
// workspace's own session once that turn ends, and what the user sent before
// then is almost always meant to be handled before the merge, not after it.
// So the request flushes the queue: each held prompt, in order, is sent to
// join the running turn exactly as an after_tool_call verdict sends one
// (join.go), nothing interrupted, and the vendor takes them in as it queues
// prompts of its own -- folded at the running turn's next tool boundary, or
// run together as the next turn once it ends. The merge waits for that turn
// too (merge.run.awaitFree), so the prompts are answered before the merge
// drives the session.
//
// WITH NO TURN RUNNING, the first is delivered as a turn of its own and the
// rest join it.
//
// THE QUEUE'S ORDER IS KEPT. The flush stops at the first prompt that cannot
// join -- one a daemon hold keeps (a lease, a reconnect, the cold gate), one
// an edit withholds, a session act, a prompt to a subagent -- and that prompt
// and everything behind it wait as they would have.

// opFlush is the operation a flush's records are filed under.
const opFlush = "daemon.promptqueue.flush"

// FlushHeld sends every held prompt of WS to the shim at once (see above).
// It answers an error only when the queue's own state could not be read; a
// prompt that cannot go is left held, with the reason recorded.
func (q *queue) FlushHeld(ctx context.Context, ws ids.WorkspaceID) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	d := q.lockDelivery(ws)
	defer d.unlock()

	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		log.Error(opFlush, "could not read the held prompts to flush", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("flush the held prompts of %q: read the holds: %w", ws, err)
	}
	queued := make([]wsm.HeldPrompt, 0, len(standing))
	for _, h := range standing {
		if h.Tombstone == nil {
			queued = append(queued, h)
		}
	}
	if len(queued) == 0 {
		log.Debug(opFlush, "nothing is held; there is nothing to flush", nil)
		return nil
	}
	sort.SliceStable(queued, func(i, j int) bool { return queued[i].QueuedAt.Before(queued[j].QueuedAt) })

	why, kept, err := q.flushKept(ctx, ws, log)
	if err != nil {
		return err
	}
	if kept {
		log.Info(opFlush, "the held prompts are not flushed; they wait as they would have", dlog.Context{
			"why": why, "held": len(queued),
		})
		return nil
	}

	sent, deferred := 0, 0
	for i, held := range queued {
		if why, ok := q.flushable(ws, held); !ok {
			log.Info(opFlush, "a held prompt cannot be flushed; it and every prompt behind it wait as they would have", dlog.Context{
				"turn": string(held.Turn), "why": why, "sent": sent, "deferred": deferred, "left": len(queued) - i,
			})
			return nil
		}
		// A VERDICT STILL BEING JUDGED ABOUT THIS PROMPT IS SUPERSEDED: the
		// prompt is on its way to the shim, and a verdict settling after it
		// must not interject or re-hold it.
		q.supersede(ws, held.Turn)
		sub := submissionOf(held)
		running, ok := q.flushTarget(ws)
		if !ok {
			delivered, err := q.deliverHeld(ctx, d, held, log)
			if err != nil || !delivered {
				log.Info(opFlush, "the first held prompt could not open a turn for the rest to join; they wait as they would have", dlog.Context{
					"turn": string(held.Turn), "sent": sent, "left": len(queued) - i,
				})
				return nil
			}
			sent++
			continue
		}
		switch q.joinLocked(ctx, d, sub, running, joinFlushed, log) {
		case joinSent:
			sent++
		case joinDeferred:
			deferred++
		case joinGone:
		case joinWaiting:
			log.Info(opFlush, "a held prompt could not join the running turn; it and every prompt behind it wait as they would have", dlog.Context{
				"turn": string(held.Turn), "running_turn": string(running), "sent": sent, "deferred": deferred, "left": len(queued) - i,
			})
			return nil
		}
	}
	log.Info(opFlush, "flushed every held prompt to the shim; nothing was interrupted", dlog.Context{
		"sent": sent, "deferred": deferred,
	})
	return nil
}

// flushKept answers why nothing may be flushed now, if something keeps the
// whole queue: a holding lease, a bounce draining the workspace, a session
// act running, an interjection under way, or a shim call that opens no turn.
// The caller holds the delivery lock. An unreadable lease is an error,
// recorded here.
func (q *queue) flushKept(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) (string, bool, error) {
	lease, held, err := q.deps.DB.Lease(ctx, ws)
	if err != nil {
		log.Error(opFlush, "could not read the occupancy lease; nothing is flushed", dlog.Context{"cause": err.Error()})
		return "", false, fmt.Errorf("flush the held prompts of %q: read the lease: %w", ws, err)
	}
	if held && lease.Policy == wsm.PolicyHold {
		return "a " + holderName(lease.Holder) + " lease holds the workspace's prompts", true, nil
	}
	if q.isDraining(ws) {
		return "the workspace is draining for a bounce, so the prompts go to the new shim", true, nil
	}
	if cut, ok := q.runningCut(ws); ok {
		return "a session act (" + cut.command.String() + ") is the running turn", true, nil
	}
	q.mu.Lock()
	interrupting := q.stateLocked(ws).interrupting
	q.mu.Unlock()
	if interrupting {
		return "an interjection is interrupting the running turn, and goes first", true, nil
	}
	if call, ok := q.standingCall(ws); ok && !call.opensTurn {
		return "a shim call (" + call.what + ") that opens no turn is in flight", true, nil
	}
	return "", false, nil
}

// flushable answers why HELD cannot be flushed, if it cannot.
func (q *queue) flushable(ws ids.WorkspaceID, held wsm.HeldPrompt) (string, bool) {
	switch {
	case held.Hold != nil:
		return "a daemon hold (" + held.Hold.String() + ") keeps it", false
	case held.Act != nil:
		return "it is a session act, which runs as a turn of its own", false
	case held.Target != nil:
		return "it is addressed to a subagent", false
	case q.withheldByEdit(ws, held):
		return "it is being edited, or is queued after one that is", false
	}
	return "", true
}

// flushTarget answers the turn a flushed prompt joins: the turn in flight, or
// the one a standing shim call is opening. None when no turn runs.
func (q *queue) flushTarget(ws ids.WorkspaceID) (ids.TurnID, bool) {
	if call, ok := q.standingCall(ws); ok && call.opensTurn {
		return call.turn, true
	}
	return q.turnInFlight(ws)
}

// supersede discards every verdict still being judged about TURN's content.
func (q *queue) supersede(ws ids.WorkspaceID, turn ids.TurnID) {
	state := q.state(ws)
	state.verdicts.Lock()
	defer state.verdicts.Unlock()
	state.bumpEpochLocked(turn)
}
