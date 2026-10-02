package promptqueue

import (
	"context"
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// A PROMPT THAT ADDS TO THE RUNNING WORK JOINS THE RUNNING TURN (owner ruling,
// 2026-09-30). The classifier's after_tool_call verdict sends the prompt to
// the shim at once with StartTurnRequest.join_running_turn: nothing is
// interrupted, and the vendor takes the prompt in after the running turn's
// current tool call. The vendor decides its fate, and the session watcher
// reads it:
//
//   - FOLDED into the running turn: its prompt row names that turn, and its
//     own turn ends unrun (wsm.CloseFolded);
//   - NOT FOLDED, because the running turn ended with no tool boundary left:
//     it runs as the next turn, standing in flight the moment the running
//     turn ends, so the turn end pops nothing into it.
//
// A FOLDED PROMPT IS ANSWERED ONLY IF THE TURN IT JOINED IS (owner ruling,
// 2026-10-01). The vendor can fold a prompt into a turn in the same instant
// that turn fails — a turn that ran out of API retries does exactly that — and
// the prompt is then in the transcript but never answered. So the queue keeps
// every prompt folded into a running turn until that turn ends, and when it
// ends FAILED it resubmits each as its own turn (resubmitFolded). A turn that
// completes answered them; one the user stopped, or an interjection
// superseded, runs no resubmission: the stop is the user ending the work, and
// an interjecting prompt runs next with the folded text already in its
// transcript.
//
// ONE PROMPT JOINS AT A TIME. While one waits, a prompt behind it is never
// classified: it waits its turn, as it would behind a session act.

// opJoin is the operation a join's records are filed under.
const opJoin = "daemon.promptqueue.join"

// joiningPrompt is the prompt sent to join the running turn.
type joiningPrompt struct {
	// turn is the prompt's own turn.
	turn ids.TurnID
	// into is the turn it was sent to join.
	into ids.TurnID
	// prompt is its text, the turn fact the footer and the roster take when
	// it runs as its own turn.
	prompt string
	// said and origin are the prompt as it was submitted, kept so a prompt
	// folded into a turn that then fails can be resubmitted as it was sent.
	said   *conversationv1.UserSaid
	origin conversationv1.PromptOrigin
}

// joining answers the prompt waiting to join the running turn, if one does.
func (q *queue) joining(ws ids.WorkspaceID) (joiningPrompt, bool) {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	if !ok || state.joining == nil {
		return joiningPrompt{}, false
	}
	return *state.joining, true
}

// behindJoinVerdict is the verdict a prompt earns while another waits to join
// the running turn: it waits its turn and is never classified.
func (q *queue) behindJoinVerdict() wsm.Classification {
	return wsm.Classification{
		Arm:    wsm.ArmHoldForTurnEnd,
		Reason: "another prompt is joining the running turn; this one waits its turn and is never classified",
		At:     q.deps.Now(),
	}
}

// behindJoinNeverClassified records, at INFO, a prompt kept from the
// classifier by the prompt joining the running turn ahead of it.
func behindJoinNeverClassified(log dlog.Logger, held ids.TurnID, joining joiningPrompt) {
	log.Info(opClassify, "another prompt is joining the running turn; the prompt waits its turn and is never classified", dlog.Context{
		"held_turn": string(held), "joining_turn": string(joining.turn), "running_turn": string(joining.into),
	})
}

// settleJoin settles an after_tool_call verdict against the running turn.
//
// IT TAKES THE DELIVERY LOCK, THEN THE VERDICT LOCK, the one order the queue
// takes them in, and joins under the delivery lock: a turn end pops under it,
// so the running turn's end cannot deliver this prompt a second time while it
// is being sent to join that turn.
func (q *queue) settleJoin(ctx context.Context, sub Submission, running ids.TurnID, epoch uint64, c wsm.Classification, log dlog.Logger) {
	d := q.lockDelivery(sub.WS)
	defer d.unlock()
	state := d.state
	state.verdicts.Lock()
	if state.verdictStaleLocked(sub.Turn, epoch, c, log) {
		state.verdicts.Unlock()
		return
	}
	q.record(ctx, sub, c, log)
	state.verdicts.Unlock()
	q.joinLocked(ctx, d, sub, running, log)
}

// joinLocked sends SUB to join RUNNING. The caller holds the delivery lock
// (d), which is released for the call itself (call.go). Every way the join
// cannot go leaves the prompt held for the running turn's end, with the reason
// stamped on its verdict.
func (q *queue) joinLocked(ctx context.Context, d *delivery, sub Submission, running ids.TurnID, log dlog.Logger) {
	log = log.With(dlog.Context{"turn": string(sub.Turn), "running_turn": string(running)})
	held, err := q.standingHold(ctx, sub.WS, sub.Turn)
	switch {
	case errors.Is(err, ErrAlreadyDelivered), errors.Is(err, ErrNoSuchHold):
		log.Info(opJoin, "the prompt no longer stands; there is nothing to send to join the running turn", dlog.Context{"cause": err.Error()})
		return
	case err != nil:
		log.Error(opJoin, "could not read the prompt's hold; it waits for the running turn to end", dlog.Context{"cause": err.Error()})
		q.recordWaiting(ctx, sub, "the prompt's hold could not be read, so it waits for the running turn to end", log)
		return
	}
	if reason, blocked := q.joinBlocked(sub.WS, held, running); blocked {
		log.Info(opJoin, "the prompt cannot join the running turn; it waits for it to end", dlog.Context{"why": reason})
		q.recordWaiting(ctx, sub, reason, log)
		return
	}
	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		log.Info(opJoin, "the workspace has no session to send the prompt to; it waits for the running turn to end", nil)
		q.recordWaiting(ctx, sub, "the session was not there to take the prompt into the running turn, so it waits for it to end", log)
		return
	}
	watcher, ok := q.deps.Watcher(sub.WS)
	if !ok {
		log.Info(opJoin, "the workspace has no session watcher; the prompt waits for the running turn to end", nil)
		q.recordWaiting(ctx, sub, "the session was not there to take the prompt into the running turn, so it waits for it to end", log)
		return
	}
	if !watcher.OnTurnJoining(sub.WS, sub.Turn) {
		log.Info(opJoin, "the running turn is one the vendor started, which is never joined; the prompt waits for it to end", nil)
		q.recordWaiting(ctx, sub, "the running turn is one the agent started on its own, which is never joined, so the prompt waits for it to end", log)
		return
	}
	text := saidText(sub.Said)
	if err := q.recordTurn(ctx, wsm.Turn{
		ID: sub.Turn, Workspace: sub.WS, Text: text, Origin: sub.Origin.String(), StartedAt: q.deps.Now(),
	}, log); err != nil {
		watcher.OnTurnOpenFailed(sub.WS, sub.Turn)
		log.Error(opJoin, "could not record the turn before sending the prompt to join the running turn; it waits for it to end", dlog.Context{"cause": err.Error()})
		q.recordWaiting(ctx, sub, "the prompt's turn could not be recorded, so it waits for the running turn to end", log)
		return
	}
	q.mirrorAccepted(sub.WS, sub.Turn, sub.Said, sub.Origin)
	var success *shimv1.StartTurnSuccess
	d.outside(shimCall{what: "join", turn: sub.Turn, holds: []ids.TurnID{sub.Turn}}, log, func() {
		success, err = sender.JoinRunningTurn(ctx, sub.Turn, sub.Said, sub.Origin)
	})
	if err != nil {
		watcher.OnTurnOpenFailed(sub.WS, sub.Turn)
		log.Error(opJoin, "the shim refused the prompt sent to join the running turn; it waits for it to end", dlog.Context{"cause": err.Error()})
		q.recordWaiting(ctx, sub, "the session would not take the prompt into the running turn, so it waits for it to end", log)
		return
	}
	q.setJoining(sub.WS, joiningPrompt{turn: sub.Turn, into: running, prompt: text, said: sub.Said, origin: sub.Origin})
	q.deps.Footer.OnSubmission(sub.WS, footer.Submission{Prompt: text, Stage: footer.StageAfterToolCall})
	handOver(sub.WS, success, watcher)
	q.touchEngagement(ctx, sub.WS, log)
	log.Info(opJoin, "sent the prompt to join the running turn after its current tool call; nothing was interrupted", nil)
	// A retirement that fails is recorded at ERROR where it failed.
	_ = q.retireDelivered(ctx, sub.WS, sub.Turn, log)
}

// joinBlocked answers why HELD cannot join RUNNING now, if something keeps
// it: the running turn ended before the prompt was sent (it then waits for the
// pop, in order), another prompt already joins it, the workspace drains for a
// bounce, or an edit withholds the prompt.
func (q *queue) joinBlocked(ws ids.WorkspaceID, held wsm.HeldPrompt, running ids.TurnID) (string, bool) {
	// ONE SHIM CALL AT A TIME (call.go): a delivery in flight is ahead of
	// the join, and its settle may leave no turn to join.
	if call, ok := q.standingCall(ws); ok {
		return "a delivery to the shim (" + call.what + ") is in flight, so the prompt waits for the running turn to end", true
	}
	if !q.isRunning(ws, running) {
		return "the turn it was to join ended before it was sent, so it waits its turn", true
	}
	if other, ok := q.joining(ws); ok {
		return "another prompt (" + string(other.turn) + ") is already joining the running turn, so this one waits for it to end", true
	}
	if q.isDraining(ws) {
		return "the workspace is draining for a bounce, so the prompt waits for the new session", true
	}
	if q.withheldByEdit(ws, held) {
		return "the prompt is being edited, so it waits for the running turn to end", true
	}
	return "", false
}

// setJoining records the prompt sent to join the running turn.
func (q *queue) setJoining(ws ids.WorkspaceID, joined joiningPrompt) {
	q.mu.Lock()
	defer q.mu.Unlock()
	q.stateLocked(ws).joining = &joined
}

// endJoiningLocked clears the joining prompt when TURN is its own turn, and
// answers it; q.mu is held.
func (state *wsState) endJoiningLocked(turn ids.TurnID) (joiningPrompt, bool) {
	if state.joining == nil || state.joining.turn != turn {
		return joiningPrompt{}, false
	}
	joined := *state.joining
	state.joining = nil
	return joined, true
}

// onFolded closes the turn of a prompt the vendor folded into the running
// turn. It ran no turn of its own, so nothing about the running turn changes:
// no interrupt clears, no session act retires, nothing is popped, and no
// banner is raised.
func (q *queue) onFolded(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, log dlog.Logger) {
	q.mu.Lock()
	state := q.stateLocked(ws)
	joined, wasJoining := state.endJoiningLocked(turn)
	if wasJoining {
		// KEPT UNTIL THE TURN IT JOINED ENDS: answered if that turn completes,
		// resubmitted if it fails (resubmitFolded).
		if state.folded == nil {
			state.folded = map[ids.TurnID][]joiningPrompt{}
		}
		state.folded[joined.into] = append(state.folded[joined.into], joined)
	}
	q.mu.Unlock()
	if !wasJoining {
		log.Error(opJoin, "a turn closed as folded that was not the prompt joining the running turn", dlog.Context{
			"invariant_violation": "a folded close for a turn the queue never sent to join",
		})
	}
	_ = q.closeTurn(ctx, ws, turn, wsm.CloseFolded, log)
	log.Info(opJoin, "the vendor folded the prompt into the running turn; its own turn closed unrun", dlog.Context{
		"folded_into": string(joined.into),
	})
}

// onJoinedTurnStood gives the footer and the roster the turn fact of a joining
// prompt that now runs as its own turn: the turn it was sent to join ended
// without folding it in.
func (q *queue) onJoinedTurnStood(ws ids.WorkspaceID, joined joiningPrompt, log dlog.Logger) {
	started := &footer.TurnStarted{At: q.deps.Now(), Act: footer.ActPrompt, Prompt: joined.prompt}
	q.deps.Footer.SetTurn(ws, started)
	q.deps.Sidebar.SetTurn(ws, started)
	q.deps.Sidebar.AckTurn(ws)
	log.Info(opJoin, "the turn the prompt was sent to join ended without folding it in; it runs as its own turn", dlog.Context{
		"joining_turn": string(joined.turn), "joined_turn": string(joined.into),
	})
}

// takeFoldedLocked removes and answers the prompts folded into TURN; q.mu is
// held. Every end of a turn takes them, so none outlives the turn it joined.
func (state *wsState) takeFoldedLocked(turn ids.TurnID) []joiningPrompt {
	folded := state.folded[turn]
	delete(state.folded, turn)
	return folded
}

// resubmitFolded resubmits, each as its own turn and in the order they were
// folded, the prompts the vendor folded into a turn that then FAILED: they are
// in the transcript, but the turn that took them never answered them. It runs
// after the failed turn's end has released the delivery lock, so each goes
// through Submit — delivered at once into a free session, or held, DEFERRED,
// behind whatever the turn end delivered first.
func (q *queue) resubmitFolded(ws ids.WorkspaceID, failed ids.TurnID, folded []joiningPrompt, log dlog.Logger) {
	for _, prompt := range folded {
		fields := dlog.Context{"failed_turn": string(failed), "folded_turn": string(prompt.turn)}
		disposition, err := q.Submit(context.Background(), Submission{
			WS: ws, Turn: wsm.NewTurnID(), Said: prompt.said, Origin: prompt.origin,
			// DEFERRED: it runs as its own turn and never interrupts one the
			// failed turn's end delivered ahead of it.
			Delivery: wsm.DeliveryDeferred,
		})
		if err != nil {
			fields["cause"] = err.Error()
			log.Error(opJoin, "could not resubmit a prompt folded into a turn that failed; it stays unanswered", fields)
			continue
		}
		fields["delivered"] = disposition.Delivered
		log.Info(opJoin, "resubmitted a prompt folded into a turn that failed, so it is answered as its own turn", fields)
	}
}
