package feed

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// HISTORY REPLAY. Rows are built from replayed entries by THE SAME per-family
// functions a live frame goes through — one composition, so a replayed row and
// a live one cannot differ. What history replays is SETTLED frames: by the
// upsert rule a settled frame carries its start's facts, so no `start` is
// replayed and none is needed.

// OnHistoryPage replays a watch's opening catch-up page. No watch replays
// history (feed paging on demand): the page is empty under tail_only, and
// under known_through carries only what was written after what the daemon
// holds, so it is drawn as the conversation catching up and never stands as a
// loaded page of its feed (book.go).
func (r *resolver) OnHistoryPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage) {
	r.replayPage(ws, agent, page, nil)
}

// replayPage draws one history page: a watch's catch-up (LOAD nil), or a page a
// reader's request loaded into its feed (book.go), whose entries a turn older
// than every loaded page are withheld until that turn's page is loaded.
func (r *resolver) replayPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage, load *pageLoad) {
	// A FORK'S PORTED CONVERSATION IS READ BEFORE THE LOCK IS TAKEN: it comes
	// from the daemon's durable record, and no read of a database belongs
	// inside the resolver's mutex.
	ported := r.portedPrompts(ws)
	closes := r.recordedCloses(ws, page)
	turns := entryTurns(page)
	addresses := r.recordedAddresses(ws, turns)
	r.learnLineage(ws, turns...)

	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)
	if load != nil && s != load.state {
		// A BIND RESET THE FEED while the page was being read: it is a page of
		// the conversation the workspace no longer runs, and drawing it would
		// put that conversation back.
		log.Info("daemon.feed.history_load_discarded",
			"a loaded history page arrived after the workspace's feed was reset; it was not drawn",
			dlog.Context{"agent": agent.GetValue(), "entries": len(page.GetEntries())})
		return
	}
	// NOTHING THE PAGE DRAWS IS PUBLISHED UNTIL THE PAGE IS WHOLLY PLACED: the
	// page's newest cut is established before any row is served, so no reader
	// is ever handed a row that cut goes on to withhold (holdPushes).
	defer r.holdPushes(s)()
	// A BUBBLE THE RECORD DRAWS IS JUDGED AGAINST THE LEVEL once the page is
	// placed, and before its pushes are released (deferred after holdPushes,
	// so it runs first): work the record shows running that no live set names
	// ran in an earlier vendor process (judgeHistoryBubbles).
	s.replaying = true
	defer func() {
		s.replaying = false
		r.judgeHistoryBubbles(s)
	}()

	// A FORK'S PORTED CONVERSATION IS PART OF WHAT ITS FIRST LOAD DRAWS: it
	// stands above the page's own rows, so the page's bound reaches it.
	s.load = load
	r.replayPorted(s, ported)

	// The page is served NEWEST FIRST; the feed's order is oldest → newest, so
	// the replay walks it backwards. IT IS REPLAYED IN THE HISTORY PLANE, so
	// what the store carries stands below a fork's ported conversation and
	// above the rows this workspace draws live, whichever arrived first.
	entries := page.GetEntries()
	s.plane = planeHistory
	s.load = load
	// A REPLAY'S OWN ROWS FOLLOW WHAT THE REPLAY DREW, never what an earlier
	// replay or the live plane left behind (order.go).
	clear(s.replayTail)
	// THE PAGE STANDS IN NO TURN UNTIL IT DRAWS A PROMPT: see wsState.replayTurn.
	s.replayTurn = nil
	s.replayPromptDrawn = false
	s.replayAtFloor = page.GetFloor() != nil
	s.replayUnstamped = 0
	s.replayCloses = closes
	for turn, addr := range addresses {
		s.addressTurn(turn, addr)
	}
	var withheld []*conversationv1.HistoryEntryAt
	for i := len(entries) - 1; i >= 0; i-- {
		if load != nil && r.withholds(s, entries[i]) {
			load.outcomes.note(entries[i], outcomeWithheld)
			withheld = append(withheld, entries[i])
			continue
		}
		r.replayLoadedEntry(s, agent, entries[i], load, outcomeDrew)
	}
	if load != nil {
		withheld = append(withheld, r.redrawPending(s, agent, load)...)
		load.book.pending = withheld
		if len(withheld) > 0 {
			log.Debug("daemon.feed.history_entries_withheld",
				"entries of turns whose prompts are on pages not yet loaded are withheld until those pages load",
				dlog.Context{"agent": agent.GetValue(), "feed": load.feedKey, "withheld": len(withheld)})
		}
	}
	// The page's last turn, when its terminal is not on the page and its
	// durable row is closed, ends here, still in the history plane.
	r.endReplayedTurn(s, "")
	s.replayCloses = nil
	s.replayTurn = nil
	s.plane = planeLive
	s.load = nil
	r.reportUnstampedReplay(s, agent, len(entries))

	// A REPLAY CARRIES NO START — "what history replays is SETTLED frames" —
	// so a spawn's settled frame waits for a start this page will never
	// deliver. The page's end is that delivery's own terminal: the bubble is
	// drawn from what the page did carry rather than held for a frame that is
	// not coming.
	r.retireHeldSpawns(s, "the history page ended")

	if load == nil {
		r.kickWaitingReaders(s, agent)
	}
	if len(entries) == 0 {
		// AN EMPTY PAGE DRAWS NOTHING: a tail_only watch opens on one, as does a
		// fresh session's main watch before its first row names the main
		// agent. Nothing on it is placed, so nothing is reported unplaceable.
		log.Debug("daemon.feed.history_page_empty",
			"an empty history page was replayed; nothing to place",
			dlog.Context{"agent": agent.GetValue(), "boundary": boundaryName(page), "loaded": load != nil})
		return
	}
	log.Debug("daemon.feed.history_page",
		"a history page was replayed into the feed",
		dlog.Context{
			"agent": agent.GetValue(), "entries": len(entries),
			"boundary": boundaryName(page), "loaded": load != nil,
		})
}

// replayLoadedEntry draws one page entry and, for a reader's load, tallies
// whether it drew a row in the load's feed: DREW names the outcome when it
// did, and outcomeNoRow when it did not.
func (r *resolver) replayLoadedEntry(s *wsState, agent *conversationv1.AgentId, at *conversationv1.HistoryEntryAt, load *pageLoad, drew entryOutcome) {
	if load == nil {
		r.replayPageEntry(s, agent, at)
		return
	}
	before := load.rows
	r.replayPageEntry(s, agent, at)
	if load.rows > before {
		load.outcomes.note(at, drew)
		return
	}
	load.outcomes.note(at, outcomeNoRow)
}

// replayPageEntry draws one page entry in the plane its lineage names.
//
// A FORK'S BOOK HOLDS ITS INHERITED PAST AROUND ITS OWN TURNS (the book orders
// by first insert, and the copy was ingested while the fork ran): each entry
// is drawn in the plane its lineage names.
func (r *resolver) replayPageEntry(s *wsState, agent *conversationv1.AgentId, at *conversationv1.HistoryEntryAt) {
	if classAgent, turn := entryClass(at, agent); r.inherits(s, classAgent, turn) {
		r.drawInherited(s, func() { r.replayStamped(s, agent, at) })
		return
	}
	r.replayStamped(s, agent, at)
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
	clear(s.replayTail)
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
	defer s.placingEntry(sessionwatcher.PlaceOf(at))()
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
		case *conversationv1.AgentUpdate_ApiError:
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "replayFrame", "branch": "case *conversationv1.AgentUpdate_ApiError"})
			// Mid-turn evidence, replayed as evidence: it was never a terminal
			// and replaying it as one would invent a turn ending. The wording
			// is the live sink's own, so a replayed turn reads exactly as the
			// watched one did.
			r.addEvidence(s, apiErrorEvidence(update.ApiError))
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

// A REPLAYED TURN IS DRAWN WHERE IT WAS DRAWN LIVE. The prompt queue records
// the address each turn draws at as it opens it (wsm.Turn.Address), and the
// replay reads those records into the very table the live draw consults
// (turnaddress.go), so a turn a merge started lands in its tab and every
// other turn on the root feed, on every replay as live.
//
// An entry of a turn the workspace never recorded (a fork's inherited past, a
// pre-contract entry, or a page whose addresses could not be read) is drawn on
// the root feed, as every unaddressed turn is.

// recordedAddresses reads the recorded output address of every turn a history
// page names, BEFORE the resolver's lock is taken. A FAILED READ IS RECORDED,
// NEVER SWALLOWED, and the page still draws, at the addresses the feed already
// holds: refusing the page would hide the whole conversation for want of where
// part of it goes.
func (r *resolver) recordedAddresses(ws ids.WorkspaceID, turns []ids.TurnID) map[ids.TurnID]*wsm.OutputAddress {
	if r.deps.TurnAddresses == nil || len(turns) == 0 {
		return nil
	}
	addresses, err := r.deps.TurnAddresses(context.Background(), ws, turns)
	if err != nil {
		r.logger(ws).Error("daemon.feed.turn_addresses_unreadable",
			"the replayed turns' recorded output addresses could not be read; the page is drawn at the addresses the feed already holds",
			dlog.Context{"turns": len(turns), "cause": err.Error()})
		return nil
	}
	return addresses
}
