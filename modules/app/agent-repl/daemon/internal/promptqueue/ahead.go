package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/holdfold"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// A PROMPT IS JUDGED ONLY AGAINST THE ITEM IMMEDIATELY AHEAD OF IT (owner rule,
// 2026-09-30). The queue is first in, first out across prompts and session
// acts alike, so what a new prompt could interrupt is the one thing in front
// of it:
//
//   - a held session act (a context cut, a model or permission-mode change):
//     never interrupted by a prompt behind it, so the prompt is not
//     classified and waits;
//   - a held prompt still QUEUED: classified against it, and an interrupt
//     verdict COALESCES the two -- nothing has started, so the new prompt is
//     folded into the queued one rather than interrupting anything;
//   - nothing held: the running turn, judged as before.

// aheadKind names what stands immediately ahead of a prompt.
type aheadKind int

const (
	// aheadRunning: nothing is held ahead, so the running turn is.
	aheadRunning aheadKind = iota
	// aheadAct: a held session act is.
	aheadAct
	// aheadQueued: a held prompt, still queued, is.
	aheadQueued
)

// itemAhead answers what stands immediately ahead of the held prompt TURN: the
// standing hold just before it in queue order, or, when TURN is not held yet,
// the last standing hold. aheadRunning when no hold stands ahead of it.
func (q *queue) itemAhead(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID) (aheadKind, wsm.HeldPrompt, error) {
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return aheadRunning, wsm.HeldPrompt{}, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	var prev *wsm.HeldPrompt
	for i := range standing {
		if standing[i].Turn == turn {
			break
		}
		prev = &standing[i]
	}
	if prev == nil {
		return aheadRunning, wsm.HeldPrompt{}, nil
	}
	if holdfold.SessionAct(*prev) {
		return aheadAct, *prev, nil
	}
	return aheadQueued, *prev, nil
}

// behindActVerdict is the verdict a prompt held behind a session act earns.
func (q *queue) behindActVerdict(ahead wsm.HeldPrompt) wsm.Classification {
	return wsm.Classification{
		Arm:    wsm.ArmHoldForTurnEnd,
		Reason: "the prompt is queued behind a session act, which no prompt interrupts: it waits and is never classified",
		At:     q.deps.Now(),
	}
}

// behindActNeverClassified records, at INFO, a prompt kept from the
// classifier by the session act ahead of it.
func behindActNeverClassified(log dlog.Logger, held ids.TurnID, ahead wsm.HeldPrompt) {
	log.Info(opClassify, "the prompt is queued behind a session act; it waits for it and is never classified", dlog.Context{
		"held_turn": string(held), "ahead_turn": string(ahead.Turn), "ahead": saidText(ahead.Said),
	})
}

// judgeQueued asks the classifier about SUB against the queued prompt AHEAD,
// then settles the verdict. Asynchronous, like every judgement.
func (q *queue) judgeQueued(ctx context.Context, sub Submission, ahead wsm.HeldPrompt, epoch uint64, log dlog.Logger) {
	verdict, err := q.deps.Judge.Judge(ctx, saidText(ahead.Said), saidText(sub.Said))
	if err != nil {
		log.Error(opClassify, "the classifier failed on a prompt behind a queued prompt; it waits its turn", dlog.Context{
			"turn": string(sub.Turn), "ahead_turn": string(ahead.Turn), "cause": err.Error(),
		})
		q.settleQueued(ctx, sub, ahead, epoch, wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the classifier could not decide, so the prompt waits its turn",
			At:     q.deps.Now(),
		}, classifier.RouteQueue, log)
		return
	}
	q.settleQueued(ctx, sub, ahead, epoch, wsm.Classification{
		Arm: routeArm(verdict.Route), Reason: verdict.Reason, At: q.deps.Now(),
	}, verdict.Route, log)
}

// settleQueued settles a verdict reached against a queued prompt.
//
// IT TAKES THE DELIVERY LOCK, THEN THE VERDICT LOCK, the one order the queue
// takes them in. A turn end pops under the delivery lock, so the prompt ahead
// cannot be delivered between this reading it as queued and this folding the
// new prompt into it: the merge either lands before the pop reads it, or the
// pop has already delivered it and this sees it running.
//
// A verdict that would not wait -- after_tool_call or interrupt -- against the
// prompt ahead:
//   - still queued: COALESCES the two, since nothing ahead has started and
//     there is no running work to join or to interrupt;
//   - now the running turn (delivered while the verdict was reached): joins
//     it or interrupts it, as the same verdict against a running turn does;
//   - gone (dropped): the prompt waits its turn.
func (q *queue) settleQueued(ctx context.Context, sub Submission, ahead wsm.HeldPrompt, epoch uint64, c wsm.Classification, route classifier.Route, log dlog.Logger) {
	if q.settleQueuedLocked(ctx, sub, ahead, epoch, c, route, log) {
		// THE INTERRUPT IS SENT OFF THE LOCKS: the running turn's end takes
		// the delivery lock to pop what is held.
		if !q.interject(ctx, sub, ahead.Turn, log) {
			q.reportHeld(ctx, sub, log)
		}
	}
}

// settleQueuedLocked is settleQueued under the delivery and verdict locks. It
// reports whether the prompt ahead is now running and must be interrupted.
func (q *queue) settleQueuedLocked(ctx context.Context, sub Submission, ahead wsm.HeldPrompt, epoch uint64, c wsm.Classification, route classifier.Route, log dlog.Logger) bool {
	d := q.lockDelivery(sub.WS)
	defer d.unlock()
	state := d.state
	state.verdicts.Lock()
	if state.verdictStaleLocked(sub.Turn, epoch, c, log) {
		state.verdicts.Unlock()
		return false
	}
	if route == classifier.RouteQueue {
		q.record(ctx, sub, c, log)
		state.verdicts.Unlock()
		q.reportHeld(ctx, sub, log)
		return false
	}
	current, found, err := q.deps.DB.HeldPromptByTurn(ctx, ahead.Turn)
	if err != nil {
		state.verdicts.Unlock()
		log.Error(opClassify, "could not read the prompt ahead to coalesce into; the prompt waits its turn", dlog.Context{
			"turn": string(sub.Turn), "ahead_turn": string(ahead.Turn), "cause": err.Error(),
		})
		q.recordWaiting(ctx, sub, "the prompt ahead could not be read, so the prompt waits its turn", log)
		return false
	}
	// THE PROMPT AHEAD MAY BE ON ITS WAY TO THE SHIM: a call in flight claims
	// it (call.go). Folding into it now would change words already sent, so
	// it is read as started, as it is the moment its call settles.
	claimed := found && current.Tombstone == nil && q.claimedByCall(sub.WS, ahead.Turn)
	switch {
	case found && current.Tombstone == nil && !claimed:
		err := q.coalesce(ctx, sub, current, log)
		state.verdicts.Unlock()
		if err != nil {
			q.recordWaiting(ctx, sub, "the prompt could not be folded into the one ahead, so it waits its turn", log)
		}
	case claimed || q.isRunning(sub.WS, ahead.Turn):
		q.record(ctx, sub, c, log)
		state.verdicts.Unlock()
		log.Info(opClassify, "the prompt ahead started while the verdict was reached; the prompt is routed against it as it runs", dlog.Context{
			"turn": string(sub.Turn), "ahead_turn": string(ahead.Turn), "route": route.String(),
		})
		if route == classifier.RouteAfterToolCall {
			// THE DELIVERY LOCK IS ALREADY HELD, which is what the join needs.
			q.joinLocked(ctx, d, sub, ahead.Turn, log)
			return false
		}
		return true
	default:
		state.verdicts.Unlock()
		log.Info(opClassify, "the prompt ahead is gone; the prompt waits its turn", dlog.Context{
			"turn": string(sub.Turn), "ahead_turn": string(ahead.Turn),
		})
		q.recordWaiting(ctx, sub, "the prompt it would have interrupted is gone, so it waits its turn", log)
	}
	return false
}

// recordWaiting stamps hold_for_turn_end with REASON and reports the place.
func (q *queue) recordWaiting(ctx context.Context, sub Submission, reason string, log dlog.Logger) {
	q.record(ctx, sub, wsm.Classification{Arm: wsm.ArmHoldForTurnEnd, Reason: reason, At: q.deps.Now()}, log)
	q.reportHeld(ctx, sub, log)
}

// isRunning reports whether TURN is the session's running turn.
func (q *queue) isRunning(ws ids.WorkspaceID, turn ids.TurnID) bool {
	watcher, ok := q.deps.Watcher(ws)
	if !ok {
		return false
	}
	running := watcher.TurnInFlight()
	return running != nil && *running == turn
}

// coalesce folds SUB into the queued prompt INTO: INTO keeps its place and
// takes SUB's content after its own, marked coalesced, and SUB leaves the
// tray as its own entry. The caller holds the delivery and verdict locks.
//
// THE MERGE AND THE RETIREMENT ARE ONE TRANSACTION (CoalesceHeldPrompts), so a
// failure leaves both entries exactly as they were. The queued prompt's own
// verdict stands: it was reached against the running turn, not about SUB.
func (q *queue) coalesce(ctx context.Context, sub Submission, into wsm.HeldPrompt, log dlog.Logger) error {
	if err := q.deps.DB.CoalesceHeldPrompts(ctx, wsm.Coalescence{
		Into:    into.Turn,
		From:    sub.Turn,
		Said:    mergedSaid(into.Said, sub.Said),
		Retired: wsm.Tombstone{Kind: tombstoneCoalesced, At: q.deps.Now()},
	}); err != nil {
		log.Error(opClassify, "could not fold the prompt into the queued prompt ahead; both entries stand as they were", dlog.Context{
			"turn": string(sub.Turn), "into_turn": string(into.Turn), "cause": err.Error(),
		})
		return err
	}
	log.Info(opClassify, "the prompt was ruled to interrupt a prompt that had not started; it is folded into it", dlog.Context{
		"turn": string(sub.Turn), "into_turn": string(into.Turn),
	})
	q.deps.Footer.OnSubmission(sub.WS, footer.Submission{Stage: footer.StageCoalesced})
	return q.pushTray(ctx, sub.WS, log)
}

// mergedSaid is INTO's content followed by FROM's, blocks in order, so an
// image either carried is kept.
func mergedSaid(into, from *conversationv1.UserSaid) *conversationv1.UserSaid {
	blocks := append([]*conversationv1.UserContentBlock{}, into.GetContent().GetBlocks()...)
	blocks = append(blocks, from.GetContent().GetBlocks()...)
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}
