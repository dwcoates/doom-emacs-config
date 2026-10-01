package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/holdfold"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// SubmitSessionAct sends a session act down the ONE delivery path, so it cannot
// overtake a prompt the user already queued: a model change that jumped the
// queue would run the queued prompt on a model the user did not choose for it.
//
// AN ACT THAT MUST WAIT IS A HELD ENTRY, EXACTLY AS A PROMPT IS (owner rule,
// 2026-09-30): durable, on the tray, in first-in-first-out order with the
// prompts around it, and never classified. A context cut is held as the prompt
// whose text is the command (deliver runs it as the cut); a model or
// permission-mode change is held with its act (wsm.HeldAct). It used to wait
// in process memory instead: invisible on the tray, lost to a restart, and
// run AHEAD of every prompt held before it (2026-09-30: a /compact typed during
// a turn never appeared as queued).
//
// THE DECISION IS TAKEN UNDER THE DELIVERY LOCK, the lock a turn end pops
// under, so an act can neither start beside a turn that begins in the instant
// after the check nor be missed by a turn end that pops in that instant.
func (q *queue) SubmitSessionAct(ctx context.Context, ws ids.WorkspaceID, act Act) error {
	log, err := q.logger(ctx, ws)
	if err != nil {
		return err
	}
	log = log.With(dlog.Context{"act": act.Kind, "value": act.Value})

	switch act.Kind {
	case ActClear, ActCompact, ActSetModel, ActSetPermissionMode:
	default:
		log.Error(opAct, "the act names a kind the one delivery path does not carry", nil)
		return fmt.Errorf("session act %q on %q is not a kind the queue carries", act.Kind, ws)
	}

	if act.Turn != "" {
		if watcher, ok := q.deps.Watcher(ws); ok {
			if running := watcher.TurnInFlight(); running != nil && *running == act.Turn {
				q.logRepeatedStart(log, "session_act", act.Turn)
				return nil
			}
		}
	}

	drain := &q.state(ws).drain
	drain.Lock()
	defer drain.Unlock()

	running, ahead, err := q.whatIsAhead(ctx, ws)
	if err != nil {
		log.Error(opAct, "could not tell whether anything is ahead of the act",
			dlog.Context{"cause": err.Error()})
		return err
	}
	if !ahead {
		return q.runAct(ctx, ws, act, log)
	}
	if q.sealedForMove(ws) {
		// THE MOVE HAS SEALED WHAT IT CARRIES: an act held here now would be
		// run by no daemon. It is refused so the caller asks the daemon the
		// workspace moves to.
		refusalLevel(ctx, log, log.Info)(opAct, "the workspace's move has sealed what it carries; the act is refused so it is asked of the daemon the workspace moves to", nil)
		return fmt.Errorf("session act %q on %q: %w", act.Kind, ws, bounce.ErrMovedAway)
	}
	sub := heldActSubmission(ws, act)
	if _, err := q.hold(ctx, sub, running, nil, log); err != nil {
		return err
	}
	log.Info(opAct, "the act is held behind the work already in the path, in order with the prompts around it",
		dlog.Context{"held_turn": string(sub.Turn), "running_turn": string(running)})
	return nil
}

// heldActSubmission is the held entry an act that must wait becomes: a
// context cut as the prompt whose text is the command, any other act as an
// entry carrying the act and shown as its slash-command form.
func heldActSubmission(ws ids.WorkspaceID, act Act) Submission {
	turn := act.Turn
	if turn == "" {
		turn = wsm.NewTurnID()
	}
	origin := act.Origin
	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT
	}
	sub := Submission{WS: ws, Turn: turn, Origin: origin}
	switch act.Kind {
	case ActClear, ActCompact:
		_, literal := contextCutCommand(act.Kind)
		sub.Said = saidOf(joinArg(literal, act.Value))
	case ActSetModel:
		sub.Act = &wsm.HeldAct{Kind: wsm.ActModel, Value: act.Value}
		sub.Said = saidOf(joinArg("/model", act.Value))
	case ActSetPermissionMode:
		sub.Act = &wsm.HeldAct{Kind: wsm.ActPermissionMode, Value: act.Value}
		sub.Said = saidOf("permission mode: " + act.Value)
	}
	return sub
}

// actOfHeld is the act a held entry carrying one runs as.
func actOfHeld(sub Submission) Act {
	kind := ActSetModel
	if sub.Act.Kind == wsm.ActPermissionMode {
		kind = ActSetPermissionMode
	}
	return Act{Kind: kind, Value: sub.Act.Value, Turn: sub.Turn, Origin: sub.Origin}
}

// joinArg is a command literal with its argument, when it has one.
func joinArg(literal, arg string) string {
	if arg == "" {
		return literal
	}
	return literal + " " + arg
}

// saidOf is a text-only submission.
func saidOf(text string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}
}

// whatIsAhead reports whether anything stands ahead of a new act -- a bounce
// draining, a turn running (or a context cut recorded as the running turn),
// or a held entry -- and the running turn when one is. The caller holds the
// delivery lock.
func (q *queue) whatIsAhead(ctx context.Context, ws ids.WorkspaceID) (ids.TurnID, bool, error) {
	var running ids.TurnID
	if watcher, ok := q.deps.Watcher(ws); ok && watcher.TurnInFlight() != nil {
		running = *watcher.TurnInFlight()
	} else if cut, ok := q.runningCut(ws); ok {
		running = cut.turn
	}
	// A BOUNCE IS AHEAD OF EVERYTHING: the act applies to the new shim.
	if q.isDraining(ws) || running != "" {
		return running, true, nil
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return "", false, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	return "", len(standing) > 0, nil
}

// sealedForMove reports whether a move has sealed what the workspace's queue
// carries.
func (q *queue) sealedForMove(ws ids.WorkspaceID) bool {
	q.mu.Lock()
	defer q.mu.Unlock()
	state, ok := q.states[ws]
	return ok && state.bounce != nil && state.bounce.sealed
}

// runAct performs one act against the shim now.
func (q *queue) runAct(ctx context.Context, ws ids.WorkspaceID, act Act, log dlog.Logger) error {
	sender, ok := q.deps.Client(ws)
	if !ok {
		refusalLevel(ctx, log, log.Warn)(opAct, "the workspace has no session to act on", nil)
		return ErrNoSession
	}

	switch act.Kind {
	case ActSetModel:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case ActSetModel"})
		if err := sender.SetModel(ctx, act.Value); err != nil {
			log.Error(opAct, "the shim refused the model change", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("set model on %q: %w", ws, err)
		}
	case ActSetPermissionMode:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case ActSetPermissionMode"})
		if err := sender.SetPermissionMode(ctx, act.Value); err != nil {
			log.Error(opAct, "the shim refused the permission-mode change", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("set permission mode on %q: %w", ws, err)
		}
	case ActClear, ActCompact:
		log.Debug("daemon.promptqueue.disposition_decision", "selected a prompt disposition branch", dlog.Context{"function": "queue", "branch": "case ActClear, ActCompact"})
		if err := q.runContextCut(ctx, ws, act, sender, log); err != nil {
			return err
		}
	}
	log.Info(opAct, "the session act was delivered", nil)
	return nil
}

// runContextCut delivers /clear or /compact as a turn — the vendor's own CLI
// answers the command — and marks the running turn UNINTERRUPTIBLE, which is
// the classification a prompt arriving behind it earns.
func (q *queue) runContextCut(ctx context.Context, ws ids.WorkspaceID, act Act, sender Sender, log dlog.Logger) error {
	command, literal := contextCutCommand(act.Kind)
	text := literal
	if act.Value != "" {
		text = literal + " " + act.Value
	}
	origin := act.Origin
	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT
	}
	said := &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{{
			Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
		}},
	}}

	turn := act.Turn
	if turn == "" {
		turn = wsm.NewTurnID()
	}
	record := wsm.Turn{ID: turn, Workspace: ws, Text: text, Origin: origin.String(), StartedAt: q.deps.Now()}
	if err := q.recordTurn(ctx, record); err != nil {
		log.Error(opAct, "could not record the context cut's turn", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("record the context cut on %q: %w", ws, err)
	}

	q.mu.Lock()
	state, ok := q.states[ws]
	if !ok {
		state = &wsState{}
		q.states[ws] = state
	}
	state.cut = &runningCut{turn: turn, command: command}
	q.mu.Unlock()

	// THE FOOTER IS TOLD WHAT THE TURN CARRIES, BEFORE THE TURN EXISTS. Nothing
	// on the shim's streams states it — the first frame of a turn is an
	// activity, by which time the status is already past `submitting` — so a
	// /clear or a compaction draws as thinking·submitting rather than as
	// clearing or compacting unless the daemon says which it is.
	started := &footer.TurnStarted{At: q.deps.Now(), Act: footerAct(command), Prompt: text}
	q.deps.Footer.SetTurn(ws, started)
	q.deps.Sidebar.SetTurn(ws, started)

	// A CONTEXT CUT IS REFLECTED IN THE FEED THE INSTANT IT IS ACCEPTED, before
	// the shim is asked. A /clear draws its cleared divider now — the red bar and
	// the cleared feed appear immediately, and the shim's later ContextCut
	// confirms that SAME divider with its "context cleared" subtext. Neither
	// directive draws a user-prompt bubble: they are directives, not
	// conversational prompts, and their only visible outcome is the bar.
	switch command {
	case conversationv1.SessionCommand_SESSION_COMMAND_CLEAR:
		q.deps.Feed.OnClearReceived(ws, turn)
	case conversationv1.SessionCommand_SESSION_COMMAND_COMPACT:
		q.deps.Feed.OnCompactReceived(ws, turn)
	}

	// The watcher learns the cut's turn before the shim does, for the reason
	// deliverToSession states: a terminal can beat StartTurn's response back.
	watcher, watching := q.deps.Watcher(ws)
	if watching {
		watcher.OnTurnOpening(ws, turn)
	}
	success, err := sender.StartTurn(ctx, turn, said, origin)
	if err != nil {
		if watching {
			watcher.OnTurnOpenFailed(ws, turn)
		}
		// The optimistic divider promised a cut the shim refused; retire it so the
		// feed recovers to exactly what it showed before.
		q.deps.Feed.OnContextCutAborted(ws, turn)
		q.deps.Footer.SetTurn(ws, nil)
		q.deps.Sidebar.SetTurn(ws, nil)
		q.retireCut(ws)
		log.Error(opAct, "the shim refused the context cut", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("deliver the context cut on %q: %w", ws, err)
	}
	q.deps.Sidebar.AckTurn(ws)
	// The cut IS a turn, so the watcher is handed it like any other. It earns
	// NO mirrored user-prompt row: a recognized command earns no user message,
	// and the cut's visible outcome is the separation row the feed resolver
	// draws at EXECUTION time.
	if watching {
		handOver(ws, success, watcher)
	}
	log.Debug(opAct, "the context cut is running and the turn is uninterruptible",
		dlog.Context{"turn": string(turn), "command": command.String()})
	return nil
}

// footerAct names the footer's spelling of a context cut, so the strip draws
// `clearing` or `compacting` rather than the ordinary submit.
func footerAct(command conversationv1.SessionCommand) footer.SessionAct {
	switch command {
	case conversationv1.SessionCommand_SESSION_COMMAND_CLEAR:
		return footer.ActClear
	case conversationv1.SessionCommand_SESSION_COMMAND_COMPACT:
		return footer.ActCompact
	default:
		return footer.ActPrompt
	}
}

// retireCut releases the running-cut record a context cut set.
func (q *queue) retireCut(ws ids.WorkspaceID) {
	q.mu.Lock()
	defer q.mu.Unlock()
	if state, ok := q.states[ws]; ok {
		state.cut = nil
	}
}

// retireCutIf retires the running-cut record when TURN is the cut's own turn,
// and records the retirement on LOG. Every close of a turn row comes through
// turnclose.go's door, and the door calls this, so the record's lifetime is
// its turn's: a /compact whose turn was closed as an orphan by a teardown or a
// boot reconciliation, never reaching OnTurnEnded, cannot leave the queue
// holding every later prompt behind an act that is no longer running.
//
// LOG is resolved only when a cut is retired: the door's orphan closes run
// at teardown and boot, where resolving a workspace for nothing is a record
// with nothing to say.
func (q *queue) retireCutIf(ws ids.WorkspaceID, turn ids.TurnID, log func() (dlog.Logger, bool)) {
	q.mu.Lock()
	state, ok := q.states[ws]
	if !ok || state.cut == nil || state.cut.turn != turn {
		q.mu.Unlock()
		return
	}
	cut := *state.cut
	state.cut = nil
	q.mu.Unlock()
	logger, ok := log()
	if !ok {
		return
	}
	logger.Info(opAct, "the session act's turn closed; the queue no longer holds prompts behind it", dlog.Context{
		"session_act_turn": string(cut.turn), "session_act": cut.command.String(),
	})
}

// workspaceLog answers a lazy resolution of a workspace's logger for
// retireCutIf. A workspace that cannot be resolved is recorded at ERROR by
// q.logger itself.
func (q *queue) workspaceLog(ctx context.Context, ws ids.WorkspaceID) func() (dlog.Logger, bool) {
	return func() (dlog.Logger, bool) {
		log, err := q.logger(ctx, ws)
		return log, err == nil
	}
}

// contextCutOf reports whether a session-addressed submission's text IS a
// context cut, and which, with its argument. A bubble-addressed prompt goes to
// a subagent's own composer and is never a session act.
func contextCutOf(sub Submission) (conversationv1.SessionCommand, string, bool) {
	return holdfold.ContextCut(sub.Target, sub.Said)
}

// actKindOf names the act kind a context-cut command is carried as.
func actKindOf(command conversationv1.SessionCommand) string {
	if command == conversationv1.SessionCommand_SESSION_COMMAND_COMPACT {
		return ActCompact
	}
	return ActClear
}

// contextCutCommand names the session command an act kind is, and the literal
// the vendor's CLI answers.
func contextCutCommand(kind string) (conversationv1.SessionCommand, string) {
	if kind == ActCompact {
		return conversationv1.SessionCommand_SESSION_COMMAND_COMPACT, "/compact"
	}
	return conversationv1.SessionCommand_SESSION_COMMAND_CLEAR, "/clear"
}
