package feed

import (
	"errors"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// The sink is the routing table: one frame in, one family function, one row.
// Nothing here decides what a row LOOKS like — that is each family's business
// — and nothing in a family decides where its row goes.

// OnPrompt draws a prompt one agent addressed to another. It appears on the
// SENDER's feed as the outgoing send and on the RECIPIENT's as the delivered
// prompt: one kind, both ends, differing only in the composed address line.
func (r *resolver) OnPrompt(ws ids.WorkspaceID, agent *conversationv1.AgentId, prompt *conversationv1.AgentPrompt, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawAgentPrompt(s, agent, prompt)
}

// OnPeerMessage draws a message another Claude session sent into this
// conversation as the abbreviated peer bubble on the recipient's feed.
func (r *resolver) OnPeerMessage(ws ids.WorkspaceID, peer *conversationv1.PeerMessage, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawPeerMessage(s, peer)
}

// OnActivity draws one unit of a turn's synchronous progress.
func (r *resolver) OnActivity(ws ids.WorkspaceID, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawActivity(s, agent, act, nil)
}

// drawActivity routes one activity to its family. turn, when set, is the turn
// the row belongs to (history replay knows it; a live frame learns it from the
// session's in-flight turn).
func (r *resolver) drawActivity(s *wsState, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, turn *conversationv1.TurnId) {
	log := r.logger(s.id)
	at, ok := r.place(s, agent)
	if !ok {
		return
	}
	unit := act.GetActivityId().GetValue()
	if unit == "" {
		log.Error("daemon.feed.activity_without_identity",
			"an activity arrived with no unit identity; nothing can be upserted for it",
			dlog.Context{"agent": agent.GetValue()})
		return
	}

	// EVERY activity is filed under its API response before anything is drawn:
	// the unit stating a response's usage is frequently not the unit that
	// draws the stamp. When this filing recorded a usage figure, re-stamp the
	// turn's already-drawn bubbles so a bubble drawn before this later API
	// response grows to the turn's new total — every bubble of a turn shows the
	// same turn total. The current activity's own row is drawn below with the
	// up-to-date turn stamp, so it needs no re-stamp here.
	if turnKey, recorded := s.fileAPIResponse(unit, act.GetUsage()); recorded {
		r.restampTurnBubbles(s, turnKey)
	}

	var (
		row *frontendv1.FeedRow
		err error
	)
	switch item := act.GetItem().(type) {
	case *conversationv1.AgentActivity_Response:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Response"})
		row, err = r.drawResponse(s, at, agent, act, item.Response)
	case *conversationv1.AgentActivity_Thinking:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Thinking"})
		row, err = r.drawThinking(s, at, act, item.Thinking)
	case *conversationv1.AgentActivity_Read:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Read"})
		row, err = r.drawRead(s, at, act, item.Read)
	case *conversationv1.AgentActivity_Write:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Write"})
		row, err = r.drawWrite(s, at, act, item.Write)
	case *conversationv1.AgentActivity_Edit:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Edit"})
		row, err = r.drawEdit(s, at, act, item.Edit)
	case *conversationv1.AgentActivity_Grep:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Grep"})
		row, err = r.drawGrep(s, at, act, item.Grep)
	case *conversationv1.AgentActivity_Glob:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Glob"})
		row, err = r.drawGlob(s, at, act, item.Glob)
	case *conversationv1.AgentActivity_Bash:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Bash"})
		row, err = r.drawBash(s, at, act, item.Bash)
	case *conversationv1.AgentActivity_WebFetch:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_WebFetch"})
		row, err = r.drawWebFetch(s, at, act, item.WebFetch)
	case *conversationv1.AgentActivity_WebSearch:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_WebSearch"})
		row, err = r.drawWebSearch(s, at, act, item.WebSearch)
	case *conversationv1.AgentActivity_SkillUse:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_SkillUse"})
		row, err = r.drawSkill(s, at, act, item.SkillUse)
	case *conversationv1.AgentActivity_Subagent:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Subagent"})
		row, err = r.drawSubagent(s, at, act, item.Subagent, false)
		// The bubble is upserted where it LIVES, which a later frame carried
		// on another book does not move (see drawSubagent).
		if state, ok := s.subagents[unit]; ok && state.feed.feed != (feedid.Feed{}) {
			at = state.feed
		}
	case *conversationv1.AgentActivity_Hook:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Hook"})
		row, err = r.drawHook(s, at, act, item.Hook)
	case *conversationv1.AgentActivity_Artifact:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Artifact"})
		row, err = r.drawArtifact(s, at, act, item.Artifact)
	case *conversationv1.AgentActivity_PlanMode:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_PlanMode"})
		row, err = r.drawPlan(s, at, agent, act, item.PlanMode)
	case *conversationv1.AgentActivity_ReportFindings:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_ReportFindings"})
		row, err = r.drawFindings(s, at, act, item.ReportFindings)
	case *conversationv1.AgentActivity_Worktree:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Worktree"})
		row, err = r.drawWorktree(s, at, act, item.Worktree)
	case *conversationv1.AgentActivity_Monitor:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_Monitor"})
		row, err = r.drawMonitor(s, at, act, item.Monitor)
	case *conversationv1.AgentActivity_SendMessage:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawActivity", "branch": "case *conversationv1.AgentActivity_SendMessage"})
		// A send IS an agent-addressed prompt, drawn on the SENDER's feed with
		// the same component the recipient's delivered prompt is drawn with.
		row, err = r.drawSendMessage(s, at, act, item.SendMessage)
	default:
		// An unmodeled tool is NOT a failure and NEVER a feed row: its home is
		// the topbar's warning dropdown. Every other kind that draws nowhere
		// (task acts, wakeups, cron, notifications, injected context) answers
		// the same way. THINKING and MONITORS draw their own rows above, so
		// they are no longer in this list.
		//
		// THE KIND IS RECORDED, because a detachment naming this unit has to
		// be able to tell "nothing has drawn it YET" from "nothing will ever
		// draw it", so an announcement naming it is retired rather than held
		// until the turn's terminal reports it as work detached from a unit
		// the resolver never drew.
		s.markUndrawable(unit)
		r.retireDetachment(s, unit)
		err = errNotARow
	}

	if errors.Is(err, errNotARow) {
		log.Debug("daemon.feed.activity_draws_nothing",
			"an activity kind draws no feed row",
			dlog.Context{"unit": unit, "agent": agent.GetValue()})
		return
	}
	if err != nil {
		log.Error("daemon.feed.activity_undrawable",
			"an activity could not be resolved into a row",
			dlog.Context{"unit": unit, "agent": agent.GetValue(), "cause": err.Error()})
		return
	}
	if row == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "row == nil"})
		return
	}
	r.stampTurn(s, row, turn)
	log.Debug("daemon.feed.activity",
		"an activity row was upserted",
		dlog.Context{"unit": unit, "agent": agent.GetValue(), "row": row.GetId().GetValue()})
	r.upsert(s, at, row, true)
	r.recordCarrier(s, unit, agent, at)
	r.applyHeldDetachment(s, unit)
}

// recordCarrier remembers, at a unit's first drawn row, WHOSE call it is and
// WHERE its row stands: a detachment from the unit belongs to that agent, and
// its head is drawn at that row.
func (r *resolver) recordCarrier(s *wsState, unit string, agent *conversationv1.AgentId, at placement) {
	if u, ok := s.units[unit]; ok && u.carrier == "" {
		u.carrier = agent.GetValue()
		u.at = at
	}
	if state, ok := s.subagents[unit]; ok && state.carrier == "" {
		state.carrier = agent.GetValue()
	}
}

// stampTurn puts the turn a row belongs to on it. A separation belongs to no
// turn and is deliberately left unstamped.
//
// A ROW'S TURN IS NEVER UNLEARNED. A family recomposes its row from scratch on
// every frame, so the value handed here is a FRESH object even for a unit that
// was already drawn and already stamped; and a store replay re-draws a unit
// long after its turn closed, with no turn of its own and no turn in flight.
// Taking the already-published row's turn as the standing answer is what keeps
// a replay from erasing the stamp the live frame earned.
func (r *resolver) stampTurn(s *wsState, row *frontendv1.FeedRow, turn *conversationv1.TurnId) {
	if row.GetTurn() != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "row.GetTurn() != nil"})
		return
	}
	if turn != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "turn != nil"})
		row.Turn = turn
		return
	}
	if prior := r.publishedTurn(s, row.GetId().GetValue()); prior != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "prior := r.publishedTurn(s, row.GetId().GetValue()); prior != nil"})
		row.Turn = prior
		return
	}
	if s.turnStamp != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.turnStamp != nil"})
		row.Turn = &conversationv1.TurnId{Value: string(*s.turnStamp)}
	}
}

// publishedTurn answers the turn already published for a row id, or nil when
// the row has never been published or was published unstamped.
func (r *resolver) publishedTurn(s *wsState, id string) *conversationv1.TurnId {
	if id == "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "id == \"\""})
		return nil
	}
	for _, f := range s.feeds {
		if existing, ok := f.rows[id]; ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "existing, ok := f.rows[id]; ok"})
			return existing.GetTurn()
		}
	}
	return nil
}

// OnQuestion draws the agent blocking on a choice.
func (r *resolver) OnQuestion(ws ids.WorkspaceID, agent *conversationv1.AgentId, q *conversationv1.AgentQuestion, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawQuestion(s, agent, q)
}

// OnPermission draws the agent blocking on consent.
func (r *resolver) OnPermission(ws ids.WorkspaceID, agent *conversationv1.AgentId, p *conversationv1.AgentPermission, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawPermission(s, agent, p)
}

// OnContextCut draws the separation divider a context cut leaves.
func (r *resolver) OnContextCut(ws ids.WorkspaceID, agent *conversationv1.AgentId, cut *conversationv1.ContextCut, at *conversationv1.HistoryPointer, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawContextCut(s, agent, cut, at)
}

// OnApiError records a mid-turn vendor failure as EVIDENCE on the turn. It is
// never a terminal and never its own row: the turn's end is the frame-level
// failure arm and nothing else.
func (r *resolver) OnApiError(ws ids.WorkspaceID, agent *conversationv1.AgentId, failed *conversationv1.ApiRequestFailed, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.addEvidence(s, apiErrorEvidence(failed.GetMessage()))
	r.logger(ws).Warn("daemon.feed.api_error",
		"a mid-turn vendor request failure was recorded as the turn's evidence",
		dlog.Context{"agent": agent.GetValue(), "message": failed.GetMessage()})
}

// turnEvidenceLine is ONE line of a turn's evidence: the sentence the
// terminal's headline carries, plus what recorded it.
//
// The origin is kept because a mid-turn api failure and the terminal a turn
// died of can be THE SAME vendor failure arriving twice, and only the origin
// lets the terminal recognise it (see erroredOutcome).
type turnEvidenceLine struct {
	// text is the sentence, as the headline states it.
	text string
	// apiFailure says this line was recorded by OnApiError -- a MID-TURN
	// vendor request failure -- rather than by any other evidence source.
	apiFailure bool
	// apiMessage is that failure's own vendor sentence, which may be empty
	// when the vendor stated none.
	apiMessage string
}

// apiErrorEvidence words a mid-turn vendor failure as evidence.
//
// ONE SPELLING, and that is the point: the live sink and the history replay
// both record this line, and two copies of the sentence would be two contracts
// that could drift apart between a turn watched live and the same turn read
// back.
func apiErrorEvidence(message string) turnEvidenceLine {
	line := "a vendor request failed mid-turn and the turn went on"
	if message != "" {
		line = line + ": " + message
	}
	return turnEvidenceLine{text: line, apiFailure: true, apiMessage: message}
}

// addEvidence attaches a line to the turn in flight, if one is.
func (r *resolver) addEvidence(s *wsState, line turnEvidenceLine) {
	if s.turnInFlight == nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.turnInFlight == nil"})
		return
	}
	key := string(*s.turnInFlight)
	s.turnEvidence[key] = append(s.turnEvidence[key], line)
}

// OnAgentTerminal draws how one agent's stream ended.
func (r *resolver) OnAgentTerminal(ws ids.WorkspaceID, agent *conversationv1.AgentId, turn *ids.TurnID, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawTerminal(s, agent, turn, success, failure)
}

// OnMainAgent records the session's main agent: the one agent whose rows are
// the root feed's. It draws nothing.
func (r *resolver) OnMainAgent(ws ids.WorkspaceID, agent *conversationv1.AgentId) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	if s.mainAgent != "" && s.mainAgent != agent.GetValue() {
		r.logger(ws).Warn("daemon.feed.main_agent_renamed",
			"the session's main agent was renamed; the root feed now holds the new agent's rows",
			dlog.Context{"previous_agent": s.mainAgent, "agent": agent.GetValue()})
	} else {
		r.logger(ws).Debug("daemon.feed.main_agent",
			"the session's main agent was named for the root feed",
			dlog.Context{"agent": agent.GetValue()})
	}
	s.mainAgent = agent.GetValue()
}

// OnDetachedWork draws the bubble of work that left the stream.
func (r *resolver) OnDetachedWork(ws ids.WorkspaceID, agent *conversationv1.AgentId, work *conversationv1.AgentDetachedWork, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawDetachedWork(s, agent, work)
}

// OnLiveWorkChanged settles every detached shell, and every monitor's card,
// that left the live set without its own terminal having settled it.
func (r *resolver) OnLiveWorkChanged(ws ids.WorkspaceID, live sessionwatcher.LiveWorkSet) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.settleShellsLeftLive(s, live)
	r.settleMonitorsLeftLive(s, live)
}

// OnBash draws one detached shell's progress.
func (r *resolver) OnBash(ws ids.WorkspaceID, work *conversationv1.DetachedWorkId, bash *conversationv1.AgentBash, addr sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	r.drawDetachedShell(s, work, bash)
}

// OnTurnOpened records the turn the daemon just opened. IT DRAWS NOTHING: the
// prompt queue's own mirror draws the user_prompt row, and this is only the
// fact of which turn is running -- the fact drawQueryDied terminates against.
func (r *resolver) OnTurnOpened(ws ids.WorkspaceID, turn ids.TurnID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	running := turn
	s.turnInFlight = &running
	s.turnStamp = &running
	// A STANDING FINAL-ANSWER FAULT IS ABOUT THE TURN THAT ENDED, and the next
	// turn beginning is what retires it.
	r.turnStarted(s, running)
	r.logger(ws).Debug("daemon.feed.turn_opened",
		"the feed took the turn the daemon opened", dlog.Context{"turn": string(turn)})
}

// OnSessionUpdate reacts to the session-scoped facts that change rows.
func (r *resolver) OnSessionUpdate(ws ids.WorkspaceID, update *conversationv1.SessionUpdate) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)
	switch update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_QueryDied:
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "OnSessionUpdate", "branch": "case *conversationv1.SessionUpdate_QueryDied"})
		// The query died out from under the turn. A consumer with no stream
		// open still needs the turn's terminal, so the feed draws it here.
		r.drawQueryDied(s, update.GetQueryDied())
	default:
		log.Debug("daemon.feed.session_update_ignored",
			"a session update changes no feed row",
			dlog.Context{"arm": sessionUpdateArm(update)})
	}
}

// sessionUpdateArm names a session update's arm for a log record.
func sessionUpdateArm(update *conversationv1.SessionUpdate) string {
	switch update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_IdentityRotated:
		return "identity_rotated"
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died"
	case *conversationv1.SessionUpdate_ModelChanged:
		return "model_changed"
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode"
	case *conversationv1.SessionUpdate_McpServer:
		return "mcp_server"
	case *conversationv1.SessionUpdate_AccountUsage:
		return "account_usage"
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission_mode_changed"
	case *conversationv1.SessionUpdate_Diagnostics:
		return "diagnostics"
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage"
	case *conversationv1.SessionUpdate_Compacting:
		return "compacting"
	}
	return "unset"
}
