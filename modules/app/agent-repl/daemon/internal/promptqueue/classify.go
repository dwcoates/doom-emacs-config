package promptqueue

import (
	"context"
	"errors"
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
		log.Info(opClassify, "the running turn is a context cut; the prompt is stamped uninterruptible and no classifier runs", dlog.Context{
			"command": uninterruptible.String(),
		})
		return disposition, nil
	}

	// THE VERDICT IS ASYNCHRONOUS. The tray's `classifying` arm exists exactly
	// so the submission is answered now and the judge's round trip does not sit
	// inside the rpc.
	epoch := q.contentEpoch(sub.WS, sub.Turn)
	q.classifying.Add(1)
	go func() {
		defer q.classifying.Done()
		q.judge(context.WithoutCancel(ctx), sub, running, epoch, log)
	}()
	return disposition, nil
}

// classifyHeld re-enters a STANDING hold into the classifier path against the
// running turn — an edit's commit is its caller. It is the same decision hold
// takes for a fresh submission: a context cut is stamped uninterruptible and
// judged by nobody, anything else is stamped classifying and judged
// asynchronously, and the verdict's own mechanics (an interject included) are
// the ordinary ones.
func (q *queue) classifyHeld(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) {
	if command := q.state(sub.WS).uninterruptible; command != conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
		log.Info(opClassify, "the running turn is a context cut; the prompt is stamped uninterruptible and no classifier runs", dlog.Context{
			"command": command.String(),
		})
		q.record(ctx, sub, wsm.Classification{
			Arm:     wsm.ArmUninterruptibleTurn,
			Reason:  "the running turn is a context cut and cannot be interrupted",
			Command: command,
			At:      q.deps.Now(),
		}, log)
		return
	}
	q.record(ctx, sub, wsm.Classification{Arm: wsm.ArmClassifying, At: q.deps.Now()}, log)
	epoch := q.contentEpoch(sub.WS, sub.Turn)
	q.classifying.Add(1)
	go func() {
		defer q.classifying.Done()
		q.judge(context.WithoutCancel(ctx), sub, running, epoch, log)
	}()
}

// judge asks the classifier about one held prompt and records what it said.
// NO FAILURE ON THE WAY IS EVER STAMPED classification_error: the tray draws
// that arm as "unclassified", which is a failure leaking into the UI rather
// than a state. Each failure is resolved to the prompt's true state — held
// for the running turn's end — and logged where the daemon can see it.
func (q *queue) judge(ctx context.Context, sub Submission, running ids.TurnID, epoch uint64, log dlog.Logger) {
	c, interject := q.verdictFor(ctx, sub, running, log)
	q.settle(ctx, sub, running, epoch, c, interject, log)
}

// verdictFor reaches the verdict judge settles: the classification to record,
// and whether it interjects.
func (q *queue) verdictFor(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) (wsm.Classification, bool) {
	// AN UNINTERRUPTIBLE RUNNING TURN is decided before the model is asked: a
	// context cut cannot be interrupted, so there is nothing to judge.
	if command := q.state(sub.WS).uninterruptible; command != conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED {
		return wsm.Classification{
			Arm:     wsm.ArmUninterruptibleTurn,
			Reason:  "the running turn is a context cut and cannot be interrupted",
			Command: command,
			At:      q.deps.Now(),
		}, false
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
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the running turn could not be read, so the prompt waits for it to end",
			At:     q.deps.Now(),
		}, false
	}
	if !found {
		log.Error(opClassify, "the queue's running turn is not open in the store; the prompt waits for the running turn to end",
			dlog.Context{"running_turn": string(running), "store_open_turns": storeOpen})
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the running turn has no open record to compare against, so the prompt waits for it to end",
			At:     q.deps.Now(),
		}, false
	}

	// A FAILED CLASSIFIER IS NOT A VERDICT, but the prompt still has a true
	// state: it is waiting for the running turn, and holding for that turn's
	// end is the verdict that can never interrupt work wrongly. The failure is
	// the daemon's to see, on disk, at error; the user sees a held prompt that
	// delivers when the turn ends. It is not retried: the retry would sit
	// between the prompt and its verdict, and the safe verdict loses nothing
	// but an interjection.
	verdict, err := q.deps.Judge.Judge(ctx, runningText, saidText(sub.Said))
	if err != nil {
		log.Error(opClassify, "the classifier failed; the prompt waits for the running turn to end",
			dlog.Context{"running_turn": string(running), "cause": err.Error()})
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the classifier could not decide, so the prompt waits for the running turn to end",
			At:     q.deps.Now(),
		}, false
	}
	if !verdict.Interject {
		log.Debug(opClassify, "the prompt waits for the running turn to end",
			dlog.Context{"reason": verdict.Reason})
		return wsm.Classification{
			Arm: wsm.ArmHoldForTurnEnd, Reason: verdict.Reason, At: q.deps.Now(),
		}, false
	}

	return wsm.Classification{
		Arm: wsm.ArmInterject, Reason: verdict.Reason, At: q.deps.Now(),
	}, true
}

// settle records a verdict and, on an interject, runs the interjection — but
// only while the content it judged still stands. An edit's commit replacing
// the content bumps the turn's epoch under the same verdict lock, so a verdict
// about the replaced words is discarded rather than stamped on the new ones.
func (q *queue) settle(ctx context.Context, sub Submission, running ids.TurnID, epoch uint64, c wsm.Classification, interject bool, log dlog.Logger) {
	state := q.state(sub.WS)
	state.verdicts.Lock()
	defer state.verdicts.Unlock()
	if now := state.epochs[sub.Turn]; now != epoch {
		log.Info(opClassify, "the verdict is about content an edit has since replaced; it is discarded", dlog.Context{
			"turn": string(sub.Turn), "arm": armName(c.Arm), "judged_epoch": epoch, "content_epoch": now,
		})
		return
	}
	q.record(ctx, sub, c, log)
	if interject {
		q.interject(ctx, sub, running, log)
	}
}

// contentEpoch answers how many times a held prompt's content was replaced.
func (q *queue) contentEpoch(ws ids.WorkspaceID, turn ids.TurnID) uint64 {
	state := q.state(ws)
	state.verdicts.Lock()
	defer state.verdicts.Unlock()
	return state.epochs[turn]
}

// replaceContent replaces a held prompt's content and bumps its epoch in one
// step under the verdict lock, so no verdict about the old content can settle
// after it. The caller holds the delivery lock.
func (q *queue) replaceContent(ctx context.Context, ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid) error {
	state := q.state(ws)
	state.verdicts.Lock()
	defer state.verdicts.Unlock()
	if err := q.deps.DB.ReplaceHeldPromptSaid(ctx, turn, said); err != nil {
		return err
	}
	if state.epochs == nil {
		state.epochs = map[ids.TurnID]uint64{}
	}
	state.epochs[turn]++
	return nil
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
	// EVERY VERDICT IS ON DISK AT INFO: why a prompt interrupted or waited is
	// the first question a stuck tray raises, and a debug record answers it
	// nowhere the daemon keeps.
	log.Info(opClassify, "recorded the verdict", dlog.Context{
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
// end. A refused interrupt strips the jump and returns the prompt to held.
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
		q.stripJump(ctx, sub, running, errors.New("the workspace lost its session before the interrupt could be sent"), log)
		return
	}
	if err := sender.KillTurn(ctx, running, false); err != nil {
		q.stripJump(ctx, sub, running, err, log)
		return
	}
	log.Debug(opInterject, "the interrupt was sent; delivery waits for the turn's real end", nil)
}

// stripJump undoes a refused interjection: the queue jump goes, the footer's
// interrupting status clears, and the prompt goes BACK TO HELD — it delivers
// at the running turn's end, exactly as a holding verdict would have. A
// refused interrupt is not a failed classification: the verdict was reached,
// the session declined to act on it, and the prompt's true state is waiting.
//
// The interjection's kill is UNFORCED, so the shim interrupts the synchronous
// turn only and never refuses because the turn spawned detached work: that
// work runs on. The refusals that remain are narrated at the level their
// nature earns:
//   - `no_turn_open` is the turn having ended on its own before the interrupt
//     landed, an expected race, recorded at info;
//   - `live` is a refusal the shim no longer produces, so one arriving is a
//     contract breach, recorded at warn under its own message;
//   - any other refusal, or a missing session, is unexpected but costs the
//     prompt nothing, recorded at warn.
func (q *queue) stripJump(ctx context.Context, sub Submission, running ids.TurnID, cause error, log dlog.Logger) {
	q.mu.Lock()
	if state, ok := q.states[sub.WS]; ok {
		state.head = nil
		state.interrupting = false
	}
	q.mu.Unlock()
	q.deps.Footer.SetInterrupting(sub.WS, false)
	fields := dlog.Context{"turn": string(sub.Turn), "interrupted_turn": string(running), "cause": cause.Error()}
	var ended interface{ KillFoundNoTurnOpen() bool }
	var live interface{ KillRefusedLive() bool }
	switch {
	case errors.As(cause, &ended) && ended.KillFoundNoTurnOpen():
		log.Info(opInterject, "the running turn had already ended when the interrupt landed; the jump is stripped and the prompt waits for the turn's end", fields)
	case errors.As(cause, &live) && live.KillRefusedLive():
		log.Warn(opInterject, "the shim refused an unforced interrupt as live, which its contract no longer produces; the jump is stripped and the prompt waits for the turn to end", fields)
	default:
		log.Warn(opInterject, "the interrupt was refused; the jump is stripped and the prompt waits for the turn to end", fields)
	}
	q.record(ctx, sub, wsm.Classification{
		Arm:    wsm.ArmHoldForTurnEnd,
		Reason: "the running turn could not be interrupted, so the prompt waits for it to end",
		At:     q.deps.Now(),
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
