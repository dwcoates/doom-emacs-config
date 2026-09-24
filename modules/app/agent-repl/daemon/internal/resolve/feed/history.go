package feed

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// HISTORY REPLAY. Rows are built from replayed entries by THE SAME per-family
// functions a live frame goes through — one composition, so a replayed row and
// a live one cannot differ. What history replays is SETTLED frames: by the
// upsert rule a settled frame carries its start's facts, so no `start` is
// replayed and none is needed.

// OnHistoryPage replays a watch's opening catch-up page.
func (r *resolver) OnHistoryPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage, addr sessionwatcher.OutputAddress) {
	// A FORK'S PORTED CONVERSATION IS READ BEFORE THE LOCK IS TAKEN: it comes
	// from the daemon's durable record, and no read of a database belongs
	// inside the resolver's mutex.
	ported := r.portedPrompts(ws)

	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)

	r.replayPorted(s, ported)

	// The page is served NEWEST FIRST; the feed's order is oldest → newest, so
	// the replay walks it backwards. IT IS REPLAYED IN THE HISTORY PLANE, so
	// what the store carries stands below a fork's ported conversation and
	// above the rows this workspace draws live, whichever arrived first.
	entries := page.GetEntries()
	s.plane = planeHistory
	// THE PAGE STANDS IN NO TURN UNTIL IT DRAWS A PROMPT: see wsState.replayTurn.
	s.replayTurn = nil
	s.replayPromptDrawn = false
	s.replayAtFloor = page.GetFloor() != nil
	s.replayUnstamped = 0
	for i := len(entries) - 1; i >= 0; i-- {
		r.replayStamped(s, agent, entries[i])
	}
	s.replayTurn = nil
	s.plane = planeLive
	r.reportUnstampedReplay(s, agent, len(entries))

	// A REPLAY CARRIES NO START — "what history replays is SETTLED frames" —
	// so a spawn's settled frame waits for a start this page will never
	// deliver. The page's end is that delivery's own terminal: the bubble is
	// drawn from what the page did carry rather than held for a frame that is
	// not coming.
	r.retireHeldSpawns(s, "the history page ended")

	// WHETHER OLDER HISTORY REMAINS decides what a walk that reaches the
	// oldest replayed row may claim. `floor` means the replay reached the
	// oldest RETAINED entry, so the walk really is at the start; `more` means
	// what is on screen has older history behind it that this replay did not
	// deliver, and a walk that runs out must say so rather than claim a
	// beginning it never saw.
	if len(entries) == 0 && agent.GetValue() == "" {
		// AN EMPTY PAGE OF A WATCH THAT HAS NAMED NO AGENT YET draws nothing and
		// has nothing older behind it to mark: a fresh session's main watch
		// opens this way before its first row names the main agent. Placing it
		// would report an agent that simply has not spoken yet as unplaceable.
		log.Debug("daemon.feed.history_page_empty",
			"an empty opening page of a watch with no agent named yet was replayed; nothing to place",
			dlog.Context{"boundary": boundaryName(page)})
		return
	}
	at, placed := r.place(s, agent)
	if !placed {
		// place has reported the page's agent as unplaceable; its replay drew
		// nothing, so there is no feed to mark truncated either.
		return
	}
	f := r.feed(s, at.feed)
	switch boundary := page.GetBoundary().(type) {
	case *conversationv1.HistoryPage_Floor:
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "OnHistoryPage", "branch": "case *conversationv1.HistoryPage_Floor"})
		f.historyMore = nil
	case *conversationv1.HistoryPage_More:
		f.historyMore = &frontendv1.FailureHistoryReplayTruncated{
			Delivered: int64(len(entries)),
			Reason:    "the opening history page did not reach the oldest retained entry",
		}
		_ = boundary
	}

	log.Debug("daemon.feed.history_page",
		"a history page was replayed into the feed",
		dlog.Context{
			"agent": agent.GetValue(), "feed": f.key, "entries": len(entries),
			"more": f.historyMore != nil,
		})
}

// portedPrompts reads the conversation a fork carried over from its parent.
//
// A FAILED READ IS RECORDED, NEVER SWALLOWED, and the page still draws: the
// history the store carries is served either way, and refusing to draw it
// because the ported half could not be read would hide the whole conversation
// instead of half of it.
func (r *resolver) portedPrompts(ws ids.WorkspaceID) []PortedPrompt {
	if r.deps.PortedPrompts == nil {
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "r.deps.PortedPrompts == nil"})
		return nil
	}
	ported, err := r.deps.PortedPrompts(context.Background(), ws)
	if err != nil {
		r.logger(ws).Error("daemon.feed.ported_prompts_unreadable",
			"a fork's ported conversation could not be read; the page is drawn without the parent's questions",
			dlog.Context{"cause": err.Error()})
		return nil
	}
	return ported
}

// replayPorted draws a fork's ported conversation ONCE per workspace, in the
// plane that stands above everything else on the feed.
func (r *resolver) replayPorted(s *wsState, ported []PortedPrompt) {
	if s.portedDrawn || len(ported) == 0 {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "s.portedDrawn || len(ported) == 0"})
		return
	}
	s.portedDrawn = true
	s.plane = planePorted
	for _, prompt := range ported {
		r.drawPortedPrompt(s, prompt)
	}
	s.plane = planeLive
	r.logger(s.id).Debug("daemon.feed.ported_conversation",
		"a fork's ported parent conversation was drawn above the workspace's own rows",
		dlog.Context{"rows": len(ported)})
}

// replayStamped replays one entry under its own turn stamp (entryturn.go).
// A stamped entry is judged against the turns this feed has seen opened; an
// unstamped one that is not a prompt (a prompt names its own turn) is counted
// as attributed by position.
func (r *resolver) replayStamped(s *wsState, agent *conversationv1.AgentId, at *conversationv1.HistoryEntryAt) {
	entry := at.GetEntry()
	// A PROMPT OPENS ITS OWN TURN, so it is never judged against the turns
	// already open: drawing it is what makes its turn known.
	switch turn := at.GetTurn().GetValue(); {
	case entry.GetUserPrompt() != nil:
	case turn != "":
		r.replayStampKnown(s, agent, ids.TurnID(turn))
	default:
		s.replayUnstamped++
	}
	defer s.drawingEntry(at.GetTurn())()
	r.replayEntry(s, agent, entry, at.GetAt())
}

// reportUnstampedReplay says ONCE per replay that the page leaned on the
// positional fallback, at INFO: old data is expected, and the record is what
// tells a reader which attributions on screen are inferred rather than stated.
func (r *resolver) reportUnstampedReplay(s *wsState, agent *conversationv1.AgentId, entries int) {
	if s.replayUnstamped == 0 {
		return
	}
	r.logger(s.id).Info("daemon.feed.replay_unstamped",
		"replayed entries carried no turn id (pre-contract, or a turn no producer could name); they were attributed by position",
		dlog.Context{"agent": agent.GetValue(), "unstamped": s.replayUnstamped, "entries": entries})
	s.replayUnstamped = 0
}

// replayEntry routes one replayed entry to the family that draws it.
func (r *resolver) replayEntry(s *wsState, agent *conversationv1.AgentId, entry *conversationv1.HistoryEntry, at *conversationv1.HistoryPointer) {
	switch arm := entry.GetEntry().(type) {
	case *conversationv1.HistoryEntry_UserPrompt:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayEntry", "branch": "case *conversationv1.HistoryEntry_UserPrompt"})
		r.drawAgentPrompt(s, agent, arm.UserPrompt)
	case *conversationv1.HistoryEntry_AgentFrame:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayEntry", "branch": "case *conversationv1.HistoryEntry_AgentFrame"})
		r.replayFrame(s, arm.AgentFrame, at)
	case *conversationv1.HistoryEntry_PeerMessage:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayEntry", "branch": "case *conversationv1.HistoryEntry_PeerMessage"})
		r.drawPeerMessage(s, arm.PeerMessage)
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayEntry", "branch": "default"})
		r.logger(s.id).Warn("daemon.feed.history_entry_unset",
			"a replayed history entry carried no arm",
			dlog.Context{"agent": agent.GetValue()})
	}
}

// replayFrame routes one replayed agent frame. FRAMES ARE FLAT and the frame's
// own agent_id is the whole of their attribution, so the replay reads it here
// rather than inheriting the page's agent.
func (r *resolver) replayFrame(s *wsState, frame *conversationv1.AgentFrame, at *conversationv1.HistoryPointer) {
	agent := frame.GetAgentId()
	switch arm := frame.GetResult().(type) {
	case *conversationv1.AgentFrame_Update:
		switch update := arm.Update.GetUpdate().(type) {
		case *conversationv1.AgentUpdate_Activity:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentUpdate_Activity"})
			r.drawActivity(s, agent, update.Activity, nil)
		case *conversationv1.AgentUpdate_Question:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentUpdate_Question"})
			r.drawQuestion(s, agent, update.Question)
		case *conversationv1.AgentUpdate_Permission:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentUpdate_Permission"})
			r.drawPermission(s, agent, update.Permission)
		case *conversationv1.AgentUpdate_ContextCut:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentUpdate_ContextCut"})
			r.drawContextCut(s, agent, update.ContextCut, at)
		case *conversationv1.AgentUpdate_ContextBudgetWarning:
			// The vendor's own context-budget warning is a PAGE LINE with
			// nothing to draw in the feed: the footer's activity line is its
			// home, so a replay of it produces no row and no warning.
			r.logger(s.id).Debug("daemon.feed.context_budget_warning_draws_nothing",
				"a replayed context-budget warning draws no feed row",
				dlog.Context{"agent": agent.GetValue()})
		case *conversationv1.AgentUpdate_ApiError:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentUpdate_ApiError"})
			// Mid-turn evidence, replayed as evidence: it was never a terminal
			// and replaying it as one would invent a turn ending. The wording
			// is the live sink's own, so a replayed turn reads exactly as the
			// watched one did.
			r.addEvidence(s, apiErrorEvidence(update.ApiError.GetMessage()))
		default:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "default"})
			r.logger(s.id).Warn("daemon.feed.history_update_unset",
				"a replayed update carried no arm",
				dlog.Context{"agent": agent.GetValue()})
		}
	case *conversationv1.AgentFrame_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentFrame_Success"})
		r.replayTerminal(s, agent, arm.Success, nil)
	case *conversationv1.AgentFrame_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentFrame_Failure"})
		r.replayTerminal(s, agent, nil, arm.Failure)
	case *conversationv1.AgentFrame_DetachedWork:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentFrame_DetachedWork"})
		r.drawDetachedWork(s, agent, arm.DetachedWork)
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "default"})
		r.logger(s.id).Warn("daemon.feed.history_frame_unset",
			"a replayed frame carried no arm",
			dlog.Context{"agent": agent.GetValue()})
	}
}

// replayTerminal replays a terminal against the turn the replay is standing
// in — the turn of the last prompt THIS PAGE drew (wsState.replayTurn), never
// the live turn in flight.
//
// A TERMINAL THE PAGE DREW NO PROMPT FOR IS NOT CHARGED TO ANY TURN. It ends
// a turn whose prompt is older than the page (a page that opens mid-turn), or
// it is a subagent's stream ending on the subagent's own page; either way the
// turn it ended is not one this replay can name, and the live turn the queue
// has just opened is certainly not it. It is recorded and draws nothing, the
// same as a live terminal with no turn.
func (r *resolver) replayTerminal(s *wsState, agent *conversationv1.AgentId, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	if s.entryTurn != nil {
		r.replayStampedTerminal(s, agent, success, failure)
		return
	}
	turn := s.replayTurn
	if turn == nil {
		live := ""
		if s.turnInFlight != nil {
			live = string(*s.turnInFlight)
		}
		r.logger(s.id).Debug("daemon.feed.replayed_terminal_without_prompt",
			"a replayed terminal preceded every prompt on its page; it names no turn this replay drew and is charged to none",
			dlog.Context{"agent": agent.GetValue(), "turn_in_flight": live})
		return
	}
	s.replayTurn = nil
	r.drawTerminal(s, agent, turn, success, failure)
}

// replayStampedTerminal replays a terminal that NAMES its turn.
//
// ONLY THE MAIN AGENT'S TERMINAL ENDS A TURN. Every agent's entries carry the
// turn they were produced within, a subagent's included, and a subagent's
// stream ending is its bubble's business (drawTerminal with no turn).
//
// A TERMINAL AT THE PAGE'S HEAD, OF A TURN OLDER THAN THE PAGE, IS NOT DRAWN —
// the same as the unstamped path: its prompt is not on screen, and a stop
// notice above everything the page drew would stand for nothing the reader can
// see. Every other stamped terminal ends exactly the turn it names, and its
// prompt is the one it settles.
func (r *resolver) replayStampedTerminal(s *wsState, agent *conversationv1.AgentId, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	turn := *s.entryTurn
	// An unnamed main agent can only be the main watch's own unnamed page:
	// a child's book is watched only once its agent is known.
	if s.mainAgent != "" && agent.GetValue() != s.mainAgent {
		r.drawTerminal(s, agent, nil, success, failure)
		return
	}
	if s.predatesPage[turn] && !s.knownTurns[turn] {
		r.logger(s.id).Debug("daemon.feed.replayed_terminal_predates_page",
			"a replayed terminal ends a turn whose prompt is older than the page; it is charged to that turn and draws nothing",
			dlog.Context{"agent": agent.GetValue(), "turn": string(turn)})
		return
	}
	// THE POSITIONAL STANDING IS SPENT whenever a terminal lands, so an
	// unstamped terminal after this one is never charged to a turn that
	// already ended.
	if s.replayTurn != nil && *s.replayTurn == turn {
		s.replayTurn = nil
	}
	r.drawTerminal(s, agent, &turn, success, failure)
}

// boundaryName names a page's boundary arm for a record.
func boundaryName(page *conversationv1.HistoryPage) string {
	switch page.GetBoundary().(type) {
	case *conversationv1.HistoryPage_Floor:
		return "floor"
	case *conversationv1.HistoryPage_More:
		return "more"
	default:
		return "unset"
	}
}
