package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// hold parks a submission. It records the durable hold FIRST — the tray is a
// view of the store, so a hold the user can see is a hold that survives — then
// pushes the tray, then judges.
//
// running is the turn in flight when a running turn is what parks the prompt,
// and empty when a lease is. lease is the daemon-side condition, nil when none
// applies.
func (q *queue) hold(ctx context.Context, sub Submission, running ids.TurnID, lease *leaseHold, log dlog.Logger) (Disposition, error) {
	held := wsm.HeldPrompt{
		Workspace: sub.WS,
		Turn:      sub.Turn,
		Said:      sub.Said,
		Origin:    sub.Origin.String(),
		Target:    sub.Target,
		QueuedAt:  q.deps.Now(),
	}
	if lease != nil {
		kind := lease.kind
		held.Hold = &kind
		held.ScheduleID = lease.scheduleID
	}
	// AN UNINTERRUPTIBLE RUNNING TURN IS DECIDED HERE, before the entry is
	// ever recorded: a context cut cannot be interrupted, so no classifier
	// runs and none will. Stamping `classifying` first would push a verdict
	// the daemon already knows is wrong and then replace it, and a tray that
	// shows a decision being made when none is being made is a lie the client
	// has to un-draw.
	uninterruptible := conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED
	if running != "" {
		uninterruptible = q.state(sub.WS).uninterruptible
		if uninterruptible != conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
			held.Classification = &wsm.Classification{
				Arm:     wsm.ArmUninterruptibleTurn,
				Reason:  "the running turn is a context cut and cannot be interrupted",
				Command: uninterruptible,
				At:      q.deps.Now(),
			}
		} else {
			held.Classification = &wsm.Classification{Arm: wsm.ArmClassifying, At: q.deps.Now()}
		}
	}

	if err := q.deps.DB.PutHeldPrompt(ctx, held); err != nil {
		log.Error(opHold, "could not record the hold", dlog.Context{"cause": err.Error()})
		return Disposition{}, fmt.Errorf("hold prompt %q on %q: %w", sub.Turn, sub.WS, err)
	}
	if err := q.pushTray(ctx, sub.WS, log); err != nil {
		return Disposition{}, err
	}
	log.Info(opHold, "the prompt is held", dlog.Context{
		"lease_hold": lease != nil, "running_turn": string(running),
	})

	disposition := Disposition{Held: held.Hold, Classification: held.Classification}
	if running == "" {
		return disposition, nil
	}
	if uninterruptible != conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
		log.Debug(opClassify, "the running turn is a context cut; no classifier runs", dlog.Context{
			"command": uninterruptible.String(),
		})
		return disposition, nil
	}

	// THE VERDICT IS ASYNCHRONOUS. The tray's `classifying` arm exists exactly
	// so the submission is answered now and the judge's round trip does not sit
	// inside the rpc.
	q.classifying.Add(1)
	go func() {
		defer q.classifying.Done()
		q.judge(context.WithoutCancel(ctx), sub, running, log)
	}()
	return disposition, nil
}

// judge asks the classifier about one held prompt and records what it said. An
// error is NOT a verdict: it is stamped as classification_error rather than
// guessed either way.
func (q *queue) judge(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) {
	// AN UNINTERRUPTIBLE RUNNING TURN is decided before the model is asked: a
	// context cut cannot be interrupted, so there is nothing to judge.
	if command := q.state(sub.WS).uninterruptible; command != conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
		q.record(ctx, sub, wsm.Classification{
			Arm:     wsm.ArmUninterruptibleTurn,
			Reason:  "the running turn is a context cut and cannot be interrupted",
			Command: command,
			At:      q.deps.Now(),
		}, log)
		return
	}

	// THE QUEUE'S RUNNING TURN IS THE AUTHORITY on whether the session is
	// busy: the watcher reported it in flight, and that turn's end is what
	// drains the tray. A store that cannot show it as open is a DISAGREEMENT
	// between the two — a defect, logged loudly — but it is never the
	// prompt's problem. Delivering now would start a turn into a session the
	// watcher says is busy; holding for the running turn's end keeps the
	// prompt moving on the one event that is certain to come, so that is the
	// verdict. What the model would have compared against is missing, so the
	// model is not asked.
	runningText, found, storeOpen, err := q.runningText(ctx, sub.WS, running)
	if err != nil {
		log.Error(opClassify, "could not read the open turns to judge against; the prompt waits for the running turn to end",
			dlog.Context{"running_turn": string(running), "cause": err.Error()})
		q.record(ctx, sub, wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the running turn could not be read, so the prompt waits for it to end",
			At:     q.deps.Now(),
		}, log)
		return
	}
	if !found {
		log.Error(opClassify, "the queue's running turn is not open in the store; the prompt waits for the running turn to end",
			dlog.Context{"running_turn": string(running), "store_open_turns": storeOpen})
		q.record(ctx, sub, wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the running turn has no open record to compare against, so the prompt waits for it to end",
			At:     q.deps.Now(),
		}, log)
		return
	}

	verdict, err := q.deps.Judge.Judge(ctx, runningText, saidText(sub.Said))
	if err != nil {
		log.Error(opClassify, "the classifier failed; the prompt keeps waiting",
			dlog.Context{"cause": err.Error()})
		q.record(ctx, sub, wsm.Classification{
			Arm: wsm.ArmClassificationError, Reason: err.Error(), At: q.deps.Now(),
		}, log)
		return
	}
	if !verdict.Interject {
		log.Debug(opClassify, "the prompt waits for the running turn to end",
			dlog.Context{"reason": verdict.Reason})
		q.record(ctx, sub, wsm.Classification{
			Arm: wsm.ArmHoldForTurnEnd, Reason: verdict.Reason, At: q.deps.Now(),
		}, log)
		return
	}

	q.record(ctx, sub, wsm.Classification{
		Arm: wsm.ArmInterject, Reason: verdict.Reason, At: q.deps.Now(),
	}, log)
	q.interject(ctx, sub, running, log)
}

// runningText reads the running turn's text, which is half of what the judge
// compares. found is false when the store holds no OPEN record of the running
// turn, and storeOpen then names the turns it does hold open, as the evidence
// of the disagreement.
func (q *queue) runningText(ctx context.Context, ws ids.WorkspaceID, running ids.TurnID) (text string, found bool, storeOpen []string, err error) {
	open, err := q.deps.DB.OpenTurns(ctx, ws)
	if err != nil {
		return "", false, nil, fmt.Errorf("read the open turns on %q: %w", ws, err)
	}
	storeOpen = make([]string, 0, len(open))
	for _, turn := range open {
		if turn.ID == running {
			return turn.Text, true, nil, nil
		}
		storeOpen = append(storeOpen, string(turn.ID))
	}
	return "", false, storeOpen, nil
}

// record stamps a verdict on the held prompt and re-pushes the tray.
func (q *queue) record(ctx context.Context, sub Submission, c wsm.Classification, log dlog.Logger) {
	if err := q.deps.DB.UpdateHeldPromptClassification(ctx, sub.Turn, c); err != nil {
		log.Error(opClassify, "could not record the verdict", dlog.Context{
			"turn": string(sub.Turn), "cause": err.Error(),
		})
		return
	}
	log.Debug(opClassify, "recorded the verdict", dlog.Context{
		"turn": string(sub.Turn), "arm": armName(c.Arm), "reason": c.Reason,
	})
	if err := q.pushTray(ctx, sub.WS, log); err != nil {
		log.Error(opClassify, "the verdict was recorded but the tray was not republished",
			dlog.Context{"cause": err.Error()})
	}
}

// interject runs the interject re-spec: the interrupting prompt moves to the
// SEMANTIC HEAD before teardown begins, the footer's waiting-interrupting fires
// the MOMENT the interrupt registers, and delivery waits for the turn's REAL
// end. A failed interrupt strips the jump and stamps the classification error.
func (q *queue) interject(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) {
	head := sub.Turn
	q.mu.Lock()
	state, ok := q.states[sub.WS]
	if !ok {
		state = &wsState{}
		q.states[sub.WS] = state
	}
	state.head = &head
	state.interrupting = true
	q.mu.Unlock()
	q.deps.Footer.SetInterrupting(sub.WS, true)
	log.Info(opInterject, "the prompt jumped the queue and the interrupt was registered",
		dlog.Context{"turn": string(sub.Turn), "interrupted_turn": string(running)})

	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		q.stripJump(ctx, sub, "the workspace lost its session before the interrupt could be sent", log)
		return
	}
	if err := sender.KillTurn(ctx, running, false); err != nil {
		q.stripJump(ctx, sub, err.Error(), log)
		return
	}
	log.Debug(opInterject, "the interrupt was sent; delivery waits for the turn's real end", nil)
}

// stripJump undoes a failed interjection: the queue jump goes, the footer's
// interrupting status clears, and the prompt is stamped classification_error.
func (q *queue) stripJump(ctx context.Context, sub Submission, cause string, log dlog.Logger) {
	q.mu.Lock()
	if state, ok := q.states[sub.WS]; ok {
		state.head = nil
		state.interrupting = false
	}
	q.mu.Unlock()
	q.deps.Footer.SetInterrupting(sub.WS, false)
	log.Error(opInterject, "the interrupt failed; the queue jump is stripped",
		dlog.Context{"turn": string(sub.Turn), "cause": cause})
	q.record(ctx, sub, wsm.Classification{
		Arm: wsm.ArmClassificationError, Reason: cause, At: q.deps.Now(),
	}, log)
}

// armName renders a classification arm for a log record.
func armName(a wsm.ClassificationArm) string {
	switch a {
	case wsm.ArmClassifying:
		return "classifying"
	case wsm.ArmInterject:
		return "interject"
	case wsm.ArmHoldForTurnEnd:
		return "hold_for_turn_end"
	case wsm.ArmUninterruptibleTurn:
		return "uninterruptible_turn"
	case wsm.ArmClassificationError:
		return "classification_error"
	default:
		return fmt.Sprintf("arm(%d)", a)
	}
}
