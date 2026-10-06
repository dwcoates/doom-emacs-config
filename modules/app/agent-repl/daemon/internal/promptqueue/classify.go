package promptqueue

import (
	"context"
	"errors"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
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
		Delivery:  sub.Delivery,
		Act:       sub.Act,
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
	//
	// A PROMPT THE CLASSIFIER IS NEVER ASKED ABOUT -- a session act, a
	// deferred prompt -- is decided here too, for the same reason
	// (neverJudged).
	var cut runningCut
	underCut := false
	var unjudged func(dlog.Logger, ids.TurnID)
	ahead := aheadRunning
	var aheadHeld wsm.HeldPrompt
	if running != "" {
		if cut, underCut = q.runningCut(sub.WS); underCut {
			held.Classification = &wsm.Classification{
				Arm:     wsm.ArmUninterruptibleTurn,
				Reason:  "the running turn is a context cut and cannot be interrupted",
				Command: cut.command,
				At:      q.deps.Now(),
			}
		} else if verdict, why, ok := q.neverJudged(sub); ok {
			unjudged = why
			held.Classification = &verdict
		} else if verdict, why, ok := q.judgedAgainst(ctx, sub, &ahead, &aheadHeld, log); ok {
			unjudged = why
			held.Classification = &verdict
		} else {
			held.Classification = &wsm.Classification{Arm: wsm.ArmClassifying, At: q.deps.Now()}
		}
	}

	if err := q.deps.DB.PutHeldPrompt(ctx, held); err != nil {
		// A merge hold the release outran is the arbitration answering, and
		// the submission path that asked for it states the outcome
		// (applyLeasePolicy).
		if !errors.Is(err, wsm.ErrMergeLeaseGone) {
			log.Error(opHold, "could not record the hold", dlog.Context{"cause": err.Error()})
		}
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
		q.reportHeld(ctx, sub, log)
		return disposition, nil
	}
	if underCut {
		keptBehindSessionAct(log, opClassify, cut, sub.Turn,
			"the running turn is a context cut; the prompt is stamped uninterruptible and no classifier runs")
		q.reportHeld(ctx, sub, log)
		return disposition, nil
	}
	if unjudged != nil {
		unjudged(log, running)
		q.reportHeld(ctx, sub, log)
		return disposition, nil
	}
	q.deps.Footer.OnSubmission(sub.WS, footer.Submission{Stage: footer.StageClassifying})

	// THE VERDICT IS ASYNCHRONOUS. The tray's `classifying` arm exists exactly
	// so the submission is answered now and the judge's round trip does not sit
	// inside the rpc.
	q.startJudge(ctx, sub, running, ahead, aheadHeld, log)
	return disposition, nil
}

// judgedAgainst decides the verdict of a prompt whose item ahead makes
// classification needless: a held session act ahead, a prompt joining the
// running turn ahead, or a queue that could not be read. ok is false when the prompt is to be judged; *ahead and
// *aheadHeld then name what it is judged against.
func (q *queue) judgedAgainst(ctx context.Context, sub Submission, ahead *aheadKind, aheadHeld *wsm.HeldPrompt, log dlog.Logger) (wsm.Classification, func(dlog.Logger, ids.TurnID), bool) {
	kind, prev, err := q.itemAhead(ctx, sub.WS, sub.Turn)
	if err != nil {
		log.Error(opClassify, "could not read what is queued ahead of the prompt; it waits its turn and is not classified",
			dlog.Context{"turn": string(sub.Turn), "cause": err.Error()})
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "what is queued ahead of the prompt could not be read, so it waits its turn",
			At:     q.deps.Now(),
		}, func(dlog.Logger, ids.TurnID) {}, true
	}
	*ahead, *aheadHeld = kind, prev
	if kind == aheadAct {
		return q.behindActVerdict(prev), func(log dlog.Logger, _ ids.TurnID) { behindActNeverClassified(log, sub.Turn, prev) }, true
	}
	if joining, ok := q.joining(sub.WS); ok && kind == aheadRunning {
		return q.behindJoinVerdict(), func(log dlog.Logger, _ ids.TurnID) { behindJoinNeverClassified(log, sub.Turn, joining) }, true
	}
	return wsm.Classification{}, nil, false
}

// startJudge runs the classifier asynchronously against the item ahead: the
// queued prompt ahead when one is, the running turn otherwise.
func (q *queue) startJudge(ctx context.Context, sub Submission, running ids.TurnID, ahead aheadKind, aheadHeld wsm.HeldPrompt, log dlog.Logger) {
	epoch := q.contentEpoch(sub.WS, sub.Turn)
	q.classifying.Add(1)
	go func() {
		defer q.classifying.Done()
		if ahead == aheadQueued {
			q.judgeQueued(context.WithoutCancel(ctx), sub, aheadHeld, epoch, log)
			return
		}
		q.judge(context.WithoutCancel(ctx), sub, running, epoch, log)
	}()
}

// classifyHeld re-enters a STANDING hold into the classifier path against the
// running turn — an edit's commit is its caller. It is the same decision hold
// takes for a fresh submission: a context cut is stamped uninterruptible and
// judged by nobody, anything else is stamped classifying and judged
// asynchronously, and the verdict's own mechanics (an interject included) are
// the ordinary ones.
func (q *queue) classifyHeld(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) {
	if cut, ok := q.runningCut(sub.WS); ok {
		keptBehindSessionAct(log, opClassify, cut, sub.Turn,
			"the running turn is a context cut; the prompt is stamped uninterruptible and no classifier runs")
		q.record(ctx, sub, wsm.Classification{
			Arm:     wsm.ArmUninterruptibleTurn,
			Reason:  "the running turn is a context cut and cannot be interrupted",
			Command: cut.command,
			At:      q.deps.Now(),
		}, log)
		return
	}
	if verdict, why, ok := q.neverJudged(sub); ok {
		why(log, running)
		q.record(ctx, sub, verdict, log)
		return
	}
	ahead := aheadRunning
	var aheadHeld wsm.HeldPrompt
	if verdict, why, ok := q.judgedAgainst(ctx, sub, &ahead, &aheadHeld, log); ok {
		why(log, running)
		q.record(ctx, sub, verdict, log)
		return
	}
	q.record(ctx, sub, wsm.Classification{Arm: wsm.ArmClassifying, At: q.deps.Now()}, log)
	q.startJudge(ctx, sub, running, ahead, aheadHeld, log)
}

// sessionActVerdict answers the verdict a held prompt earns when its own text
// IS a session act — /compact, /compact <text>, or /clear, as
// sessioncommand.ContextCut reads them. Such a prompt is QUEUED AND NEVER
// CLASSIFIED (owner ruling, 2026-09-27): it waits for the running turn's end
// and is then delivered as the context cut it is (deliver routes it to
// runContextCut). The routing classifier is never asked, so it can never earn
// an interject.
func (q *queue) sessionActVerdict(sub Submission) (wsm.Classification, conversationv1.SessionCommand, bool) {
	command, _, ok := contextCutOf(sub)
	if !ok {
		return wsm.Classification{}, conversationv1.SessionCommand_SESSION_COMMAND_UNSPECIFIED, false
	}
	_, literal := contextCutCommand(actKindOf(command))
	return wsm.Classification{
		Arm:    wsm.ArmHoldForTurnEnd,
		Reason: "the prompt is " + literal + ", a session act: it is queued behind the running turn and never classified",
		At:     q.deps.Now(),
	}, command, true
}

// neverJudged answers the verdict a prompt earns when the classifier is NEVER
// asked about it, and the INFO record that says why: a session act (queued
// behind the running turn and delivered as the act it is) or a deferred prompt
// (its own turn after the running one). ok is false for a prompt the
// classifier judges. It is the ONE reading hold, classifyHeld and verdictFor
// share, so a kind of prompt kept from the model is kept from it on every
// path at once.
func (q *queue) neverJudged(sub Submission) (verdict wsm.Classification, why func(log dlog.Logger, running ids.TurnID), ok bool) {
	if verdict, command, isAct := q.sessionActVerdict(sub); isAct {
		return verdict, func(log dlog.Logger, running ids.TurnID) { neverClassified(log, sub.Turn, command, running) }, true
	}
	if sub.Act != nil {
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the entry is a " + sub.Act.Kind + " change, a session act: it is queued behind what is ahead of it and never classified",
			At:     q.deps.Now(),
		}, func(log dlog.Logger, running ids.TurnID) { heldActNeverClassified(log, sub.Turn, sub.Act, running) }, true
	}
	if verdict, isDeferred := q.deferredVerdict(sub); isDeferred {
		return verdict, func(log dlog.Logger, running ids.TurnID) { deferredNeverClassified(log, sub.Turn, running) }, true
	}
	return wsm.Classification{}, nil, false
}

// deferredVerdict answers the verdict a DEFERRED prompt earns while a turn
// runs: hold for that turn's end. It is QUEUED AND NEVER CLASSIFIED, like a
// session act, because what the user asked for is "after this turn, as its
// own turn", and a classifier's interject is exactly what that excludes. A
// turn end delivers held prompts one at a time, so it runs as its own turn.
func (q *queue) deferredVerdict(sub Submission) (wsm.Classification, bool) {
	if sub.Delivery != wsm.DeliveryDeferred {
		return wsm.Classification{}, false
	}
	return wsm.Classification{
		Arm:    wsm.ArmHoldForTurnEnd,
		Reason: "the prompt was deferred: it runs as its own turn after the running one, and is never classified",
		At:     q.deps.Now(),
	}, true
}

// deferredNeverClassified records, at INFO, a deferred prompt kept from the
// classifier.
func deferredNeverClassified(log dlog.Logger, held, running ids.TurnID) {
	log.Info(opClassify, "the prompt was deferred; it waits for the running turn to end and is never classified", dlog.Context{
		"held_turn": string(held), "running_turn": string(running),
	})
}

// heldActNeverClassified records, at INFO, a held model or permission-mode
// change kept from the classifier.
func heldActNeverClassified(log dlog.Logger, held ids.TurnID, act *wsm.HeldAct, running ids.TurnID) {
	log.Info(opClassify, "the entry is a session act; it is queued behind the running turn and never classified", dlog.Context{
		"held_turn": string(held), "session_act": act.Kind, "value": act.Value, "running_turn": string(running),
	})
}

// neverClassified records, at INFO, a session act kept from the classifier.
func neverClassified(log dlog.Logger, held ids.TurnID, command conversationv1.SessionCommand, running ids.TurnID) {
	log.Info(opClassify, "the prompt is a session act; it is queued behind the running turn and never classified", dlog.Context{
		"held_turn": string(held), "session_act": command.String(), "running_turn": string(running),
	})
}

// judge asks the classifier about one held prompt and records what it said.
// NO FAILURE ON THE WAY IS EVER STAMPED classification_error: the tray draws
// that arm as "unclassified", which is a failure leaking into the UI rather
// than a state. Each failure is resolved to the prompt's true state — held
// for the running turn's end — and logged where the daemon can see it.
func (q *queue) judge(ctx context.Context, sub Submission, running ids.TurnID, epoch uint64, log dlog.Logger) {
	c, route := q.verdictFor(ctx, sub, running, log)
	q.settle(ctx, sub, running, epoch, c, route, log)
}

// verdictFor reaches the verdict judge settles: the classification to record,
// and where it routes the prompt.
func (q *queue) verdictFor(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) (wsm.Classification, classifier.Route) {
	// AN UNINTERRUPTIBLE RUNNING TURN is decided before the model is asked: a
	// context cut cannot be interrupted, so there is nothing to judge.
	if cut, ok := q.runningCut(sub.WS); ok {
		keptBehindSessionAct(log, opClassify, cut, sub.Turn,
			"a context cut began while the prompt waited for its verdict; it is stamped uninterruptible and the model is not asked")
		return wsm.Classification{
			Arm:     wsm.ArmUninterruptibleTurn,
			Reason:  "the running turn is a context cut and cannot be interrupted",
			Command: cut.command,
			At:      q.deps.Now(),
		}, classifier.RouteQueue
	}

	// A PROMPT THAT IS ITSELF A SESSION ACT, OR IS DEFERRED, never reaches
	// the model. hold and classifyHeld decide it before any judge starts;
	// this is the classifier call site's own refusal, so no path to Judge can
	// carry one.
	if verdict, why, ok := q.neverJudged(sub); ok {
		why(log, running)
		return verdict, classifier.RouteQueue
	}

	// A RUNNING TURN WAITING OUT A FAILED API CALL is decided before the model
	// is asked (owner ruling, 2026-10-01). The turn cannot reach a tool
	// boundary until the vendor's next scheduled attempt answers, so joining
	// it would hold the prompt for that whole wait, and the classifier is
	// itself an API call that fails the same way. The prompt interrupts the
	// wait and runs as its own turn, which calls the API at once.
	if q.deps.Footer.RetryStanding(sub.WS) {
		log.Info(opClassify, "the running turn is waiting out a failed API call; the prompt interrupts it and the model is not asked",
			dlog.Context{"running_turn": string(running), "held_turn": string(sub.Turn)})
		return wsm.Classification{
			Arm:    routeArm(classifier.RouteInterrupt),
			Reason: "the running turn is waiting out a failed API call, so the prompt interrupts the wait and runs at once",
			At:     q.deps.Now(),
		}, classifier.RouteInterrupt
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
		}, classifier.RouteQueue
	}
	if !found && !q.isRunning(sub.WS, running) {
		// THE RUNNING TURN ENDED WHILE THE PROMPT WAITED FOR ITS VERDICT. The
		// judge runs off the submission's goroutine, against the turn that ran
		// when the prompt was held; its end closed the store's row first, and
		// the watcher no longer runs it. That is the ordinary race of a short
		// turn (a vendor-started one, often), not a disagreement: the verdict
		// is the same safe one, and the turn's end is what delivers the
		// prompt (integration of the suite run 2026-10-03,
		// TestKeepAliveAnswerAfterVendorTurnNeverServed).
		log.Info(opClassify, "the running turn ended while the prompt waited for its verdict; the prompt waits for the turn's end to deliver it",
			dlog.Context{"running_turn": string(running), "store_open_turns": storeOpen})
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the running turn ended while the prompt waited for its verdict, so it is delivered at that end",
			At:     q.deps.Now(),
		}, classifier.RouteQueue
	}
	if !found {
		log.Error(opClassify, "the queue's running turn is not open in the store; the prompt waits for the running turn to end",
			dlog.Context{"running_turn": string(running), "store_open_turns": storeOpen})
		return wsm.Classification{
			Arm:    wsm.ArmHoldForTurnEnd,
			Reason: "the running turn has no open record to compare against, so the prompt waits for it to end",
			At:     q.deps.Now(),
		}, classifier.RouteQueue
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
		}, classifier.RouteQueue
	}
	log.Debug(opClassify, "the classifier routed the prompt", dlog.Context{
		"route": verdict.Route.String(), "reason": verdict.Reason,
	})
	return wsm.Classification{Arm: routeArm(verdict.Route), Reason: verdict.Reason, At: q.deps.Now()}, verdict.Route
}

// settle records a verdict and, on an interject, runs the interjection — but
// only while the content it judged still stands. An edit's commit replacing
// the content bumps the turn's epoch under the same verdict lock, so a verdict
// about the replaced words is discarded rather than stamped on the new ones. A
// move's seal bumps it too (handoff.go): a verdict answering after the
// workspace was sealed for another daemon is that daemon's to reach.
func (q *queue) settle(ctx context.Context, sub Submission, running ids.TurnID, epoch uint64, c wsm.Classification, route classifier.Route, log dlog.Logger) {
	if route == classifier.RouteAfterToolCall {
		q.settleJoin(ctx, sub, running, epoch, c, log)
		return
	}
	interject := route == classifier.RouteInterrupt
	state := q.state(sub.WS)
	state.verdicts.Lock()
	if state.verdictStaleLocked(sub.Turn, epoch, c, log) {
		state.verdicts.Unlock()
		return
	}
	q.record(ctx, sub, c, log)
	// THE INTERJECTION IS REGISTERED UNDER THE VERDICT LOCK and its interrupt
	// is SENT OFF IT: the verdict is settled at the epoch it was judged at,
	// and no queue lock is held across a call to the shim (call.go). An edit
	// that commits after the registration finds the jump already standing, as
	// it would have found it after the interrupt was answered.
	// A verdict kept from interjecting by a running session act is re-stamped
	// and recorded by registerInterjection itself.
	registered := interject && q.registerInterjection(ctx, sub, running, log)
	state.verdicts.Unlock()
	if registered {
		q.sendInterrupt(ctx, sub, running, log)
		return
	}
	q.reportHeld(ctx, sub, log)
}

// reportHeld tells the footer where a held prompt stands in the queue: its
// 1-based place among the standing holds and how many stand. A prompt no
// longer standing (delivered or dropped since) reports nothing.
func (q *queue) reportHeld(ctx context.Context, sub Submission, log dlog.Logger) {
	standing, err := q.deps.DB.HeldPrompts(ctx, sub.WS)
	if err != nil {
		log.Error(opHold, "could not read the holds to report the prompt's place in the queue",
			dlog.Context{"turn": string(sub.Turn), "cause": err.Error()})
		return
	}
	position, queued := queuePlace(standing, sub.Turn)
	if position == 0 {
		log.Debug(opHold, "the prompt no longer stands; its place in the queue is not reported",
			dlog.Context{"turn": string(sub.Turn)})
		return
	}
	q.deps.Footer.OnSubmission(sub.WS, footer.Submission{
		Stage: footer.StageHeld, Position: position, Queued: queued,
	})
}

// queuePlace answers TURN's 1-based place among the standing holds, which the
// store answers in queue order, and how many stand; position 0 when TURN is
// not standing.
func queuePlace(holds []wsm.HeldPrompt, turn ids.TurnID) (position, queued uint32) {
	for i, h := range holds {
		if h.Turn == turn {
			position = uint32(i + 1)
		}
	}
	return position, uint32(len(holds))
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
	state.bumpEpochLocked(turn)
	return nil
}

// bumpEpochLocked records that TURN's content was replaced, so a verdict
// judged about the content before it settles as a discard. The caller holds
// the verdict lock.
func (state *wsState) bumpEpochLocked(turn ids.TurnID) {
	if state.epochs == nil {
		state.epochs = map[ids.TurnID]uint64{}
	}
	state.epochs[turn]++
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
		"turn": string(sub.Turn), "arm": c.Arm.String(), "reason": c.Reason,
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
//
// A RUNNING SESSION ACT CANNOT BE INTERJECTED, AND THE REFUSAL IS HERE, on the
// one path every interjection takes (a verdict's and a release's alike), not
// at any caller. The running-cut record is read under the same q.mu hold that
// installs the head, so no interjection can register against a /clear or a
// /compact the queue knows is running: the prompt keeps its place, is stamped
// uninterruptible_turn, and is delivered in order once the act ends. A user's
// explicit interrupt is not an interjection and never comes through here.
//
// It reports whether the interrupt was registered.
func (q *queue) interject(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) bool {
	if !q.registerInterjection(ctx, sub, running, log) {
		return false
	}
	q.sendInterrupt(ctx, sub, running, log)
	return true
}

// registerInterjection is the decision half of interject: the prompt moves to
// the semantic head and the footer's interrupting fires, or a running session
// act refuses it. It makes no call to the shim, so a caller holding a queue
// lock sends the interrupt (sendInterrupt) only once it has released it. It
// reports whether the interjection was registered.
func (q *queue) registerInterjection(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) bool {
	head := sub.Turn
	q.mu.Lock()
	state, ok := q.states[sub.WS]
	if !ok {
		state = &wsState{}
		q.states[sub.WS] = state
	}
	if state.cut != nil {
		cut := *state.cut
		q.mu.Unlock()
		keptBehindSessionAct(log, opInterject, cut, sub.Turn,
			"a session act is running; the held prompt is kept from interjecting and waits for the act to end")
		q.record(ctx, sub, wsm.Classification{
			Arm:     wsm.ArmUninterruptibleTurn,
			Reason:  "the running turn is a context cut and cannot be interrupted",
			Command: cut.command,
			At:      q.deps.Now(),
		}, log)
		return false
	}
	state.head = &head
	state.interrupting = true
	q.mu.Unlock()
	q.deps.Footer.SetInterrupting(sub.WS, true)
	q.deps.Footer.OnSubmission(sub.WS, footer.Submission{Stage: footer.StageInterjecting})
	log.Info(opInterject, "the prompt jumped the queue and the interrupt was registered",
		dlog.Context{"turn": string(sub.Turn), "interrupted_turn": string(running)})
	return true
}

// sendInterrupt is the call half of interject: the interrupt goes to the
// shim, and a refusal strips the jump. The caller holds no queue lock.
func (q *queue) sendInterrupt(ctx context.Context, sub Submission, running ids.TurnID, log dlog.Logger) {
	sender, ok := q.deps.Client(sub.WS)
	if !ok {
		q.stripJump(ctx, sub, running, errors.New("the workspace lost its session before the interrupt could be sent"), log)
		return
	}
	// THE STOP IS THE PROMPT'S SIDE EFFECT, NOT AN ACT OF ITS OWN, and the
	// record says so: the feed draws no interruption bubble for it, because
	// the superseding prompt is the whole account of the stop.
	if err := sender.KillTurn(ctx, running, false, interjectionCommand()); err != nil {
		q.stripJump(ctx, sub, running, err, log)
		return
	}
	log.Debug(opInterject, "the interrupt was sent; delivery waits for the turn's real end", nil)
}

// keptBehindSessionAct records, at INFO, a held prompt a running session act
// kept from interjecting: the act's turn, the act, and the held prompt's turn.
func keptBehindSessionAct(log dlog.Logger, op string, cut runningCut, held ids.TurnID, message string) {
	log.Info(op, message, dlog.Context{
		"session_act_turn": string(cut.turn),
		"session_act":      cut.command.String(),
		"held_turn":        string(held),
	})
}

// interjectionCommand is an interjection's HOW: the person sent a prompt that
// was judged to supersede the running turn.
func interjectionCommand() *conversationv1.AgentInterruptedByUser {
	return &conversationv1.AgentInterruptedByUser{
		Command: &conversationv1.AgentInterruptedByUser_Interjection{
			Interjection: &conversationv1.AgentInterruptedByUserInterjection{},
		},
	}
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

// routeArm is the verdict arm a classifier route records.
func routeArm(route classifier.Route) wsm.ClassificationArm {
	switch route {
	case classifier.RouteQueue:
		return wsm.ArmHoldForTurnEnd
	case classifier.RouteAfterToolCall:
		return wsm.ArmAfterToolCall
	case classifier.RouteInterrupt:
		return wsm.ArmInterject
	default:
		panic(fmt.Sprintf("promptqueue: classifier route %d has no verdict arm", int(route)))
	}
}

// verdictStaleLocked reports whether a verdict judged at EPOCH is about
// content an edit has since replaced or a move has since superseded, and
// records the discard at INFO when it is. The caller holds the verdict lock,
// under which every epoch bump happens.
func (state *wsState) verdictStaleLocked(turn ids.TurnID, epoch uint64, c wsm.Classification, log dlog.Logger) bool {
	now := state.epochs[turn]
	if now == epoch {
		return false
	}
	log.Info(opClassify, "the verdict is about content an edit has since replaced or a move has since superseded; it is discarded", dlog.Context{
		"turn": string(turn), "arm": c.Arm.String(), "judged_epoch": epoch, "content_epoch": now,
	})
	return true
}
