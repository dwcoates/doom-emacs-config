package promptqueue

import (
	"context"
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/wsm"
)

// SubmitSessionAct sends a session act down the ONE delivery path, so it cannot
// overtake a prompt the user already queued: a model change that jumped the
// queue would run the queued prompt on a model the user did not choose for it.
//
// An act arriving while a turn runs or a prompt waits is QUEUED behind them and
// drained at the next turn end, in submission order.
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

	queued, err := q.somethingIsAhead(ctx, ws)
	if err != nil {
		log.Error(opAct, "could not tell whether anything is ahead of the act",
			dlog.Context{"cause": err.Error()})
		return err
	}
	if queued {
		state := q.state(ws)
		q.mu.Lock()
		state.acts = append(state.acts, act)
		depth := len(state.acts)
		q.mu.Unlock()
		log.Info(opAct, "the act is queued behind the work already in the path",
			dlog.Context{"queued_acts": depth})
		return nil
	}
	return q.runAct(ctx, ws, act, log)
}

// somethingIsAhead reports whether a turn is running or a prompt is standing in
// the path, which is what an act must queue behind.
func (q *queue) somethingIsAhead(ctx context.Context, ws ids.WorkspaceID) (bool, error) {
	// A BOUNCE IS AHEAD OF EVERYTHING: the act applies to the new shim, and
	// the bounce's finish drains the acts before any prompt.
	if q.isDraining(ws) {
		return true, nil
	}
	if watcher, ok := q.deps.Watcher(ws); ok && watcher.TurnInFlight() != nil {
		return true, nil
	}
	standing, err := q.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return false, fmt.Errorf("read the holds for %q: %w", ws, err)
	}
	return len(standing) > 0, nil
}

// runAct performs one act against the shim now.
func (q *queue) runAct(ctx context.Context, ws ids.WorkspaceID, act Act, log dlog.Logger) error {
	sender, ok := q.deps.Client(ws)
	if !ok {
		log.Warn(opAct, "the workspace has no session to act on", nil)
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
	if err := q.deps.DB.PutTurn(ctx, record); err != nil {
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
	started := &footer.TurnStarted{At: q.deps.Now(), Act: footerAct(command)}
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
		if agent := success.GetPrompt().GetAgent(); agent.GetValue() != "" {
			watcher.SetMainAgent(agent)
		}
		watcher.OnTurnOpened(ws, success.GetPrompt(), success.GetPage())
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

// contextCutCommand names the session command an act kind is, and the literal
// the vendor's CLI answers.
func contextCutCommand(kind string) (conversationv1.SessionCommand, string) {
	if kind == ActCompact {
		return conversationv1.SessionCommand_SESSION_COMMAND_COMPACT, "/compact"
	}
	return conversationv1.SessionCommand_SESSION_COMMAND_CLEAR, "/clear"
}

// drainActs runs every act queued behind the path, in submission order. It is
// called at a turn end, BEFORE the next prompt is popped, so an act the user
// issued while a turn ran applies to the prompt that follows it.
func (q *queue) drainActs(ctx context.Context, ws ids.WorkspaceID, log dlog.Logger) {
	q.mu.Lock()
	state, ok := q.states[ws]
	if !ok || len(state.acts) == 0 {
		q.mu.Unlock()
		return
	}
	pending := state.acts
	state.acts = nil
	q.mu.Unlock()

	for _, act := range pending {
		if err := q.runAct(ctx, ws, act, log.With(dlog.Context{"act": act.Kind, "value": act.Value})); err != nil {
			log.Error(opAct, "a queued session act was not delivered at the turn's end",
				dlog.Context{"act": act.Kind, "cause": err.Error()})
		}
	}
}
