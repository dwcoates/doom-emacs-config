package feed

import (
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
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)

	// The page is served NEWEST FIRST; the feed's order is oldest → newest, so
	// the replay walks it backwards.
	entries := page.GetEntries()
	for i := len(entries) - 1; i >= 0; i-- {
		r.replayEntry(s, agent, entries[i].GetEntry(), entries[i].GetAt())
	}

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
	at := r.place(s, agent)
	f := r.feed(s, at.feed)
	switch boundary := page.GetBoundary().(type) {
	case *conversationv1.HistoryPage_Floor:
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

// replayEntry routes one replayed entry to the family that draws it.
func (r *resolver) replayEntry(s *wsState, agent *conversationv1.AgentId, entry *conversationv1.HistoryEntry, at *conversationv1.HistoryPointer) {
	switch arm := entry.GetEntry().(type) {
	case *conversationv1.HistoryEntry_UserPrompt:
		r.drawAgentPrompt(s, agent, arm.UserPrompt)
	case *conversationv1.HistoryEntry_AgentFrame:
		r.replayFrame(s, arm.AgentFrame, at)
	default:
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
			r.drawActivity(s, agent, update.Activity, nil)
		case *conversationv1.AgentUpdate_Question:
			r.drawQuestion(s, agent, update.Question)
		case *conversationv1.AgentUpdate_Permission:
			r.drawPermission(s, agent, update.Permission)
		case *conversationv1.AgentUpdate_ContextCut:
			r.drawContextCut(s, agent, update.ContextCut, at)
		case *conversationv1.AgentUpdate_ContextBudgetWarning:
			// The vendor's own context-budget warning is a PAGE LINE with
			// nothing to draw in the feed: the footer's activity line is its
			// home, so a replay of it produces no row and no warning.
			r.logger(s.id).Debug("daemon.feed.context_budget_warning_draws_nothing",
				"a replayed context-budget warning draws no feed row",
				dlog.Context{"agent": agent.GetValue()})
		case *conversationv1.AgentUpdate_ApiError:
			// Mid-turn evidence, replayed as evidence: it was never a terminal
			// and replaying it as one would invent a turn ending.
			line := "a vendor request failed mid-turn and the turn went on"
			if message := update.ApiError.GetMessage(); message != "" {
				line = line + ": " + message
			}
			r.addEvidence(s, line)
		default:
			r.logger(s.id).Warn("daemon.feed.history_update_unset",
				"a replayed update carried no arm",
				dlog.Context{"agent": agent.GetValue()})
		}
	case *conversationv1.AgentFrame_Success:
		r.replayTerminal(s, agent, arm.Success, nil)
	case *conversationv1.AgentFrame_Failure:
		r.replayTerminal(s, agent, nil, arm.Failure)
	case *conversationv1.AgentFrame_DetachedWork:
		r.drawDetachedWork(s, agent, arm.DetachedWork)
	default:
		r.logger(s.id).Warn("daemon.feed.history_frame_unset",
			"a replayed frame carried no arm",
			dlog.Context{"agent": agent.GetValue()})
	}
}

// replayTerminal replays a terminal against the turn the replay is standing
// in. A replayed terminal with no turn in flight belongs to a subagent's
// bubble, exactly as a live one does.
func (r *resolver) replayTerminal(s *wsState, agent *conversationv1.AgentId, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	r.drawTerminal(s, agent, s.turnInFlight, success, failure)
}
