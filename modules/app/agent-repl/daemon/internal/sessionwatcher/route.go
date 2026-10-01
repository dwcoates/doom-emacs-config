package sessionwatcher

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/contextcut"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// ---- the session's standing stream ----

// routeSessionUpdateLocked routes one SessionUpdate arm to the views that
// resolve from it. The split is per arm and stated once here: session identity
// and health are the topbar's, accounting and the rate-limit status are the
// footer's, and the session's death is everyone's.
func (w *watcher) routeSessionUpdateLocked(update *conversationv1.SessionUpdate) {
	switch u := update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_Diagnostics:
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the health reporter, the topbar and the roster", dlog.Context{
			"arm": sessionArm(update),
		})
		// THE RECORD IS WRITTEN BEFORE THE VIEW IS DRAWN, and that order is
		// the whole point of this arm. The topbar's warning strip and the
		// roster's degraded dot are drawn from the push itself, while
		// SessionHealth answers from the health reporter's recorded faults —
		// so a client that saw the warning and then asked SessionHealth was
		// told "healthy" for as long as the fault took to land, and a client
		// that saw the warning retracted could still be told "unhealthy".
		// MEASURED: a topbar republish at 17:36:05.586423 and the matching
		// `daemon.health.open_fault` at 17:36:05.588491, a 2.1ms window that
		// an integration run at -parallel 16 lost a test to. Recording first
		// closes it in BOTH directions: the published view is never ahead of
		// the health the daemon will answer with.
		w.sinks.Lifecycle.OnSessionDiagnostics(w.ws, u.Diagnostics)
		w.sinks.Topbar.OnSessionUpdate(w.ws, update)
		// THE ROSTER READS THE SAME PUSH. An open degraded window is what the
		// row's `degraded` arm is made of, and without this route that arm has
		// no producer at all: the dot would read `ready` for a session the
		// topbar is drawing as degraded, and the two surfaces would disagree
		// about one fact.
		w.sinks.Sidebar.OnSessionUpdate(w.ws, update)

	case *conversationv1.SessionUpdate_ContextUsage:
		// ONE READING, TWO DRAWINGS. The topbar's context chip states the
		// context held; the footer's tokens cell states how much the turn in
		// flight has grown it. Both take THIS update, so the two can never
		// read the window differently.
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the topbar and the footer", dlog.Context{
			"arm": sessionArm(update),
		})
		w.sinks.Topbar.OnSessionUpdate(w.ws, update)
		w.sinks.Footer.OnSessionUpdate(w.ws, update)

	case *conversationv1.SessionUpdate_McpServer:
		// THE FOOTER ANNOUNCES THE CHANGE. The topbar draws the server's
		// standing health; the footer's activity cell says, for its display
		// window, that it changed (footer-activity-tiers.md, the
		// session_change transient).
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the topbar and the footer", dlog.Context{
			"arm": sessionArm(update),
		})
		w.sinks.Topbar.OnSessionUpdate(w.ws, update)
		w.sinks.Footer.OnSessionUpdate(w.ws, update)

	case *conversationv1.SessionUpdate_FastMode,
		*conversationv1.SessionUpdate_IdentityRotated,
		*conversationv1.SessionUpdate_Title:
		// A ROTATION MOVES THE RESUME HANDLE: the id it names is what a later
		// resume must name, so the lifecycle sink records it once the lock is
		// let go.
		if rotated := update.GetIdentityRotated(); rotated != nil {
			w.pendingVendorSessionID = rotated.GetVendorSessionId()
		}
		// THE VENDOR'S ai-title RIDES HERE. It was previously unrouted and fell
		// to the default WARN, so the topbar never drew the vendor's own
		// conversation summary in place of the workspace name (owner ruling,
		// 2026-09-13). The topbar resolver already handles the `title` arm; the
		// gap was only that this route never delivered it. It ALSO drives the
		// title synthesizer's vendor-present skip: once the vendor states a
		// title, the daemon stops synthesizing its own.
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the topbar", dlog.Context{
			"arm": sessionArm(update),
		})
		w.sinks.Topbar.OnSessionUpdate(w.ws, update)
		// THE VENDOR STATING A TITLE STOPS OUR SYNTHESIS. Its ai-title always
		// wins, so once it exists the daemon spends no more on a title of its
		// own.
		if _, isTitle := update.GetUpdate().(*conversationv1.SessionUpdate_Title); isTitle && w.sinks.Title != nil {
			w.sinks.Title.OnVendorTitle(w.ws)
		}

	case *conversationv1.SessionUpdate_ModelChanged,
		*conversationv1.SessionUpdate_PermissionModeChanged:
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the topbar, the roster and the footer", dlog.Context{
			"arm": sessionArm(update),
		})
		w.sinks.Topbar.OnSessionUpdate(w.ws, update)
		w.sinks.Sidebar.OnSessionUpdate(w.ws, update)
		// The footer's session_change transient announces the change.
		w.sinks.Footer.OnSessionUpdate(w.ws, update)

	case *conversationv1.SessionUpdate_AccountUsage,
		*conversationv1.SessionUpdate_RateLimitStatus,
		*conversationv1.SessionUpdate_Compacting,
		// A COMPACTION'S PHASES are the footer's: its salient line while the
		// compaction runs, and the transient that announces how it concluded.
		*conversationv1.SessionUpdate_CompactionProgress:
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the footer", dlog.Context{
			"arm": sessionArm(update),
		})
		w.sinks.Footer.OnSessionUpdate(w.ws, update)

	case *conversationv1.SessionUpdate_QueryDied:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeSessionUpdateLocked", "branch": "case *conversationv1.SessionUpdate_QueryDied"})
		w.routeQueryDiedLocked(update)

	case *conversationv1.SessionUpdate_NetworkResumeWaits,
		*conversationv1.SessionUpdate_NetworkResumeOutcome:
		// VISIBILITY ONLY (owner ruling, 2026-09-28): a background subagent's
		// wait to be resumed after a network outage is the FOOTER's to draw —
		// its agents row, its chip glyph and the edges' transients — and no
		// other rule of this daemon reads it. It is deliberately NOT the
		// live-work ledger's: the waiting agent's run has ended, and a wait
		// counts toward no freeness, no close-quiet check and no drain.
		w.log.Debug("daemon.sessionwatcher.session_update", "session fact routed to the footer", dlog.Context{
			"arm": sessionArm(update),
		})
		w.sinks.Footer.OnSessionUpdate(w.ws, update)

	default:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeSessionUpdateLocked", "branch": "default"})
		w.log.Warn("daemon.sessionwatcher.session_update_unrouted", "a SessionUpdate arm has no route", dlog.Context{
			"arm": sessionArm(update),
		})
	}
}

// routeQueryDiedLocked handles the session's death: every view reflects it,
// and the daemon's own machinery is told, because an open turn will never get
// a terminal now and a lease holder waiting on freeness would wait forever.
func (w *watcher) routeQueryDiedLocked(update *conversationv1.SessionUpdate) {
	w.log.Error("daemon.sessionwatcher.query_died", "the session's query died", nil)
	w.sessionEnded = true
	// A TERMINAL HELD FOR A NAME THAT WILL NEVER COME. The query is dead, so
	// StartTurn's answer — the only thing that names the main agent — is not
	// arriving; the views take the terminal now rather than lose it.
	w.flushHeldTerminalLocked()

	w.sinks.Footer.OnSessionUpdate(w.ws, update)
	// THE FEED MAY NOT KNOW THE TURN YET. OnTurnOpening records the turn
	// BEFORE StartTurn is called, and a query that dies while that call is
	// still in flight beats Feed.OnTurnOpened -- the feed's only other source
	// for which turn is running. Handing it over here is what makes the
	// death's terminal reach the turn it killed; without it the push drew
	// nothing and the turn's ending waited on the shim's own query_died
	// terminal, which reaches the feed by the store in no fixed order.
	if w.turn != nil {
		w.sinks.Feed.OnTurnOpened(w.ws, *w.turn)
	}
	w.sinks.Feed.OnSessionUpdate(w.ws, update)
	w.sinks.Sidebar.OnSessionUpdate(w.ws, update)

	// EVERY TURN THE QUERY HELD DIES WITH IT: the one in flight, then each
	// one waiting behind it as it stands in flight in turn.
	for w.turn != nil {
		ended := *w.turn
		w.turnEndedLocked(ended, wsm.CloseFailed)
		if w.turn != nil {
			w.sinks.Feed.OnTurnOpened(w.ws, *w.turn)
		}
	}
	// NOTHING SURVIVES THE SESSION'S QUERY, so every live item concludes here.
	if changed := w.concludeAllLocked(concludedQueryDied); changed {
		w.publishLiveWorkLocked()
	}
	// THE DAEMON RESTARTS THE SESSION: nothing in the shim restarts a query
	// it lost, so the lifecycle sink is told once the lock is let go.
	w.pendingQueryDeath = true
}

// sessionArm names a SessionUpdate's set arm for a log record.
func sessionArm(update *conversationv1.SessionUpdate) string {
	switch update.GetUpdate().(type) {
	case *conversationv1.SessionUpdate_Diagnostics:
		return "diagnostics"
	case *conversationv1.SessionUpdate_ContextUsage:
		return "context_usage"
	case *conversationv1.SessionUpdate_ModelChanged:
		return "model_changed"
	case *conversationv1.SessionUpdate_PermissionModeChanged:
		return "permission_mode_changed"
	case *conversationv1.SessionUpdate_FastMode:
		return "fast_mode"
	case *conversationv1.SessionUpdate_McpServer:
		return "mcp_server"
	case *conversationv1.SessionUpdate_IdentityRotated:
		return "identity_rotated"
	case *conversationv1.SessionUpdate_AccountUsage:
		return "account_usage"
	case *conversationv1.SessionUpdate_RateLimitStatus:
		return "rate_limit_status"
	case *conversationv1.SessionUpdate_Compacting:
		return "compacting"
	case *conversationv1.SessionUpdate_Title:
		return "title"
	case *conversationv1.SessionUpdate_QueryDied:
		return "query_died"
	case *conversationv1.SessionUpdate_CompactionProgress:
		return "compaction_progress"
	case *conversationv1.SessionUpdate_NetworkResumeWaits:
		return "network_resume_waits"
	case *conversationv1.SessionUpdate_NetworkResumeOutcome:
		return "network_resume_outcome"
	default:
		return "unset"
	}
}

// ---- an agent's stream ----

// routeAgentResponseLocked routes one WatchAgent frame: the opening catch-up
// page, or one entry as written.
func (w *watcher) routeAgentResponseLocked(a *agentWatch, resp *shimv1.WatchAgentResponse) {
	if page := resp.GetPage(); page != nil {
		w.routeOpeningPageLocked(a, page, pageWatchOpened)
		return
	}
	if at := resp.GetEntry(); at != nil {
		w.routeEntryLocked(a, at)
		return
	}
	if at := resp.GetRetired(); at != nil {
		w.routeRetiredLocked(a, at)
		return
	}
	w.log.Warn("daemon.sessionwatcher.agent_frame_unrouted", "a WatchAgentResponse carried no frame", dlog.Context{
		"agent_id": a.id.GetValue(),
	})
}

// pageOrigin says which answer carried an opening page, because the two are
// not the same promise about the rows that follow.
type pageOrigin int

const (
	// pageWatchOpened is a watch's own first frame. The stream serves every
	// later row live and none of the page's rows again, so the page is the
	// ONLY sighting its rows get on this stream.
	pageWatchOpened pageOrigin = iota
	// pageTurnAccepted is the page StartTurn's answer carried. The main
	// watch is standing beside it and serves the same rows live, so nothing
	// on it is this watcher's to route or to count as served.
	pageTurnAccepted
)

// String names the origin for a log record.
func (o pageOrigin) String() string {
	if o == pageTurnAccepted {
		return "turn_accepted"
	}
	return "watch_opened"
}

// routeOpeningPageLocked hands an OPENING PAGE to the feed whole — a watch's
// own first frame, or the page StartTurnSuccess carried.
//
// THE PAGE IS NOT REPLAYED AS LIVE FRAMES. Its entries are NEWEST FIRST, so
// walking them would see a turn's terminal before its prompt and leave a
// finished turn recorded as in flight. What is read out of a page is the MAIN
// agent's identity, which an adopted session has no other source for until
// its next StartTurn, and the rows that CLOSE AN ACT (routePageClosingsLocked).
func (w *watcher) routeOpeningPageLocked(a *agentWatch, page *conversationv1.HistoryPage, origin pageOrigin) {
	w.knowPagePromptsLocked(page)
	if entries := page.GetEntries(); len(entries) > 0 {
		if ptr := entries[0].GetAt(); ptr != nil {
			w.adoptPagePointerLocked(a, ptr, origin)
		}
	}
	if a.id == nil {
		// THE MAIN WATCH'S OWN ROWS NAME THE MAIN AGENT for the views, before
		// the page that needs it is replayed. See nameMainForViewsLocked.
		if named := pageAgent(page); named != nil {
			w.nameMainForViewsLocked(named, "main_watch_page", false)
		}
		for _, entry := range page.GetEntries() {
			if prompt := entry.GetEntry().GetUserPrompt(); prompt != nil {
				w.adoptMainAgentLocked(prompt.GetAgent(), "history_page")
				// A PAGE OWES THE VIEWS NO TURN-OPEN EDGE — it is newest
				// first and opens nothing — so the naming is the whole
				// precondition and the held terminal goes now.
				w.releaseHeldTerminalLocked()
				break
			}
		}
	}
	w.log.Debug("daemon.sessionwatcher.history_page", "opening page routed to the feed", dlog.Context{
		"agent_id": a.id.GetValue(), "entries": len(page.GetEntries()),
		"origin": origin.String(), "catch_up": a.catchUp,
	})
	agent := w.watchAgentLocked(a)
	if agent == nil && a.id == nil {
		// UNNAMED BY StartTurn YET, but the main watch's page is the main
		// agent's by construction, and the views were just told who that is.
		agent = w.viewsMain
	}
	w.sinks.Feed.OnHistoryPage(w.ws, agent, feedPage(page, origin, a.catchUp))
	// A SPAWN ON THIS PAGE OWES ITS CHILD A WATCH. The page's own frames drew
	// the commission, but the created agent's conversation lives only on the
	// child's own book — so every spawn the page carries opens the same watch
	// a live spawn would, and the child's page replays into its sub-feed. See
	// watchSpawnedSubagentsOnPageLocked for the regression this repairs.
	w.watchSpawnedSubagentsOnPageLocked(page)
	// THE FOOTER SEES THE PAGE TOO, and for one reason only: a resumed
	// session's prior turns are facts no edge on this daemon's streams will
	// ever restate, so without the page the strip reports a rehydrated
	// conversation as one that has never run.
	w.sinks.Footer.OnHistoryPage(w.ws, agent, page)
	// AFTER the page is handed over whole, the rows on it that close an act
	// reach the views that act was standing in.
	w.routePageClosingsLocked(a, page, origin)
	a.paged = true
}

// knowPagePromptsLocked records every turn a page's PROMPTS open as a turn this
// watcher knows. Only a prompt opens a turn; a stamp on any other row is that
// row's claim, and learning a turn from it would excuse the very record that
// names a turn nobody opened.
func (w *watcher) knowPagePromptsLocked(page *conversationv1.HistoryPage) {
	for _, entry := range page.GetEntries() {
		if turn := entry.GetEntry().GetUserPrompt().GetId().GetValue(); turn != "" {
			w.knownTurns[ids.TurnID(turn)] = struct{}{}
		}
	}
}

// adoptPagePointerLocked records an opening page's newest pointer as the
// watch's high-water mark.
//
// STARTTURN'S PAGE NEVER MOVES A MARK THE MAIN WATCH ALREADY HOLDS. The main
// watch stands beside that page and serves the same rows live, and it may have
// served rows NEWER than the page's one (the turn's first frames can beat the
// answer back). Taking the page's pointer then would walk the mark backwards,
// and the next re-open would re-serve what the views already drew. A main
// watch that holds no mark yet — refused, or not yet paged — takes it, so its
// re-open catches up from the turn's own row rather than from nothing.
func (w *watcher) adoptPagePointerLocked(a *agentWatch, ptr *conversationv1.HistoryPointer, origin pageOrigin) {
	key := watchKey(a.id)
	if origin == pageTurnAccepted && w.known[key] != nil {
		return
	}
	w.known[key] = ptr
}

// feedPage is the page the FEED is handed. A page's boundary says whether
// older history remains BELOW it, and only a watch's first page is read from
// the top of the book: a catch-up page is bounded by known_through and
// StartTurn's page by its one-entry budget, so a `floor` on either means "the
// mark was reached" and a `more` means "the budget ran out" — neither is a
// statement about the conversation's oldest entry. Handed through, a catch-up
// `floor` cleared the replay-truncated marker the first page had set, and a
// turn page's `more` re-set it as a one-entry replay. So those two carry their
// entries and no boundary.
func feedPage(page *conversationv1.HistoryPage, origin pageOrigin, catchUp bool) *conversationv1.HistoryPage {
	if origin != pageTurnAccepted && !catchUp {
		return page
	}
	return &conversationv1.HistoryPage{Entries: page.GetEntries()}
}

// routePageClosingsLocked walks an opening page's rows that CLOSE AN ACT,
// OLDEST FIRST — the order they were written in — and gives each the one
// treatment its kind and the page's origin call for.
//
// A TERMINAL on any page is history: it is recorded as served, so a later
// re-serving of it on the live stream is a replay, and it is never routed.
//
// A CUT is routed by routePageCutLocked, because a cut the views never took
// leaves them standing in the act it ended: a watch re-opened after the
// vendor's `compacting` was seen live, whose catch-up page carried the cut,
// left the footer's compaction line and `compacting` state up until the
// turn's terminal took them down (daemon.footer.compaction_line_outlived_turn).
func (w *watcher) routePageClosingsLocked(a *agentWatch, page *conversationv1.HistoryPage, origin pageOrigin) {
	entries := page.GetEntries()
	for i := len(entries) - 1; i >= 0; i-- {
		at := entries[i]
		frame := at.GetEntry().GetAgentFrame()
		switch {
		case frame == nil:
			continue
		case isTerminalFrame(frame):
			w.closingReplayedLocked(a, frame, at.GetAt())
		case frame.GetUpdate().GetContextCut() != nil:
			w.routePageCutLocked(a, frame, at.GetAt(), PlaceOf(at), origin)
		}
	}
}

// routePageCutLocked decides what one cut on an opening page is, and routes
// it when it is an edge the views have not taken.
//
//   - On StartTurn's page it is left alone: the main watch standing beside
//     that page serves the same row live, and counting it served here would
//     make the live row read as a replay and never reach the views.
//   - A cut this watch was already served is a REPLAY, dropped whole.
//   - On a watch's FIRST page — a repaint — it is history, recorded as served
//     and never routed: it ended an act this daemon never saw begin, and
//     routing it would drop the context figure the topbar holds now and end
//     whatever turn the footer is drawing.
//   - On a CATCH-UP page it was written while no stream stood, so it is an
//     edge the views missed, and it is routed exactly as a live cut is — bar
//     the feed, which draws its divider from the page it was just handed.
func (w *watcher) routePageCutLocked(a *agentWatch, frame *conversationv1.AgentFrame, at *conversationv1.HistoryPointer, place *conversationv1.ConversationPlace, origin pageOrigin) {
	agent := frame.GetAgentId()
	ctx := dlog.Context{"agent_id": agent.GetValue(), "pointer": at.GetValue(), "origin": origin.String()}
	if origin == pageTurnAccepted {
		w.log.Debug("daemon.sessionwatcher.context_cut_on_turn_page", "a cut on StartTurn's page is left to the main watch, which serves the same row live", ctx)
		return
	}
	if w.closingReplayedLocked(a, frame, at) {
		return
	}
	if !a.catchUp {
		w.log.Debug("daemon.sessionwatcher.context_cut_history", "a cut on a watch's first page is history: recorded as served, never routed", ctx)
		return
	}
	w.routeContextCutLocked(agent, frame.GetUpdate().GetContextCut(), at, nil, place, cutCaughtUp)
}

// routeEntryLocked routes one live history entry.
func (w *watcher) routeEntryLocked(a *agentWatch, at *conversationv1.HistoryEntryAt) {
	if ptr := at.GetAt(); ptr != nil {
		w.known[watchKey(a.id)] = ptr
	}
	entry := at.GetEntry()
	if prompt := entry.GetUserPrompt(); prompt != nil {
		w.routePromptLocked(a, prompt, PlaceOf(at))
		return
	}
	if frame := entry.GetAgentFrame(); frame != nil {
		w.routeAgentFrameLocked(a, frame, at.GetAt(), at.GetTurn(), PlaceOf(at))
		return
	}
	if peer := entry.GetPeerMessage(); peer != nil {
		w.routePeerMessageLocked(a, peer, at.GetTurn(), PlaceOf(at))
		return
	}
	w.log.Warn("daemon.sessionwatcher.entry_unrouted", "a history entry carried no arm", dlog.Context{
		"agent_id": a.id.GetValue(),
	})
}

// routeRetiredLocked routes an entry the store RETIRED: a re-derivation of the
// vendor record behind it found the record no longer converts to it, so the
// feed removes whatever it drew for it. The frame carries the entry as last
// served, which is what names the rows to remove.
//
// ONLY THREE KINDS ARE RETIRABLE: a user prompt, a peer message, and an agent
// frame carrying an api_error update -- the page lines an older conversion
// could wrongly mint from a user-type record. Anything else on this arm is a
// producer breaking the contract; it is recorded at ERROR and nothing is drawn
// or undone.
//
// THE POINTER IS NOT ADOPTED AS THE WATCH'S MARK. A retired row keeps its OLD
// position (the store retires whatever row a re-read finds, anywhere in the
// book), so taking it would walk the high-water mark backwards and the next
// re-open would re-serve what the views already drew.
//
// TURN BOOKKEEPING IS LEFT ALONE. A retired prompt was recorded as a turn this
// watcher knows (knownTurns), and that set is what lets a later terminal
// naming the turn end nothing quietly rather than be reported as naming a turn
// the daemon never opened (terminalTurnLocked). Forgetting it would turn such a
// terminal into an ERROR against a healthy session. A prompt row never set the
// turn in flight, so there is nothing there to undo.
func (w *watcher) routeRetiredLocked(a *agentWatch, at *conversationv1.HistoryEntryAt) {
	entry := at.GetEntry()
	if prompt := entry.GetUserPrompt(); prompt != nil {
		w.log.Info("daemon.sessionwatcher.prompt_retired", "a retired prompt was routed to the feed for removal", dlog.Context{
			"agent_id": prompt.GetAgent().GetValue(), "turn_id": prompt.GetId().GetValue(), "pointer": at.GetAt().GetValue(),
		})
		w.sinks.Feed.OnPromptRetired(w.ws, prompt)
		return
	}
	if peer := entry.GetPeerMessage(); peer != nil {
		w.log.Info("daemon.sessionwatcher.peer_message_retired", "a retired peer message was routed to the feed for removal", dlog.Context{
			"agent_id": peer.GetAgent().GetValue(), "peer_id": peer.GetId(), "pointer": at.GetAt().GetValue(),
		})
		w.sinks.Feed.OnPeerMessageRetired(w.ws, peer)
		return
	}
	if failed := entry.GetAgentFrame().GetUpdate().GetApiError(); failed != nil {
		agent := entry.GetAgentFrame().GetAgentId()
		w.log.Info("daemon.sessionwatcher.api_error_retired", "a retired api error was routed to the feed for withdrawal", dlog.Context{
			"agent_id": agent.GetValue(), "turn_id": at.GetTurn().GetValue(), "pointer": at.GetAt().GetValue(),
		})
		w.sinks.Feed.OnApiErrorRetired(w.ws, agent, failed, at.GetTurn())
		return
	}
	w.log.Error("daemon.sessionwatcher.retired_unretirable", "a retired entry was of a kind the store never retires; nothing was removed", dlog.Context{
		"agent_id": a.id.GetValue(), "pointer": at.GetAt().GetValue(), "kind": entryKind(entry),
	})
}

// entryKind names a history entry's arm for a log record.
func entryKind(entry *conversationv1.HistoryEntry) string {
	switch {
	case entry.GetUserPrompt() != nil:
		return "user_prompt"
	case entry.GetPeerMessage() != nil:
		return "peer_message"
	case entry.GetAgentFrame().GetUpdate() != nil:
		return "agent_update"
	case entry.GetAgentFrame() != nil:
		return "agent_frame"
	default:
		return "unset"
	}
}

// routePromptLocked routes a prompt delivered to the watched agent. On the
// MAIN watch a live prompt names its recipient, which is the main agent.
//
// A PROMPT ROW OPENS NO TURN. The turn in flight is the queue's to state
// (OnTurnOpening, OnTurnOpened) or the session's facts' (a turn already
// running at a start or adoption) — see standTurnLocked. A row can be served
// again: when the store ends a standing watch, the shim re-opens the book and
// re-serves rows it already served, and a re-served prompt row once stood a
// finished turn back up in flight here while every other observer had closed it.
//
// THE ONE EXCEPTION IS A VENDOR-STARTED TURN. Its prompt row is the only
// statement anywhere that the turn opened -- no queue delivered it, and the
// shim writes the row as the turn's first, on the stream that then carries
// every row of the turn -- so it opens the turn here, once: only on the main
// watch, only live, and only for a turn this watcher has never had in hand, so
// a re-served row stands nothing back up. See adoptVendorTurnLocked.
func (w *watcher) routePromptLocked(a *agentWatch, prompt *conversationv1.AgentPrompt, place *conversationv1.ConversationPlace) {
	if a.id == nil {
		w.adoptMainAgentLocked(prompt.GetAgent(), "live_prompt")
		// The main agent is named, so a terminal held for the naming can be
		// replayed against the turn the queue stated.
		w.releaseHeldTerminalLocked()
	}
	if a.id == nil && prompt.GetOrigin() == conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED {
		w.adoptVendorTurnLocked(prompt)
	}
	// A FOLDED PROMPT'S ROW IS THE ONE STATEMENT THAT ITS JOIN LANDED: the
	// vendor took it into the turn it names, so its own turn ends unrun.
	if joined := prompt.GetFoldedInto().GetValue(); a.id == nil && joined != "" {
		w.foldJoinLocked(ids.TurnID(prompt.GetId().GetValue()), ids.TurnID(joined))
	}
	if turn := prompt.GetId().GetValue(); turn != "" {
		w.knownTurns[ids.TurnID(turn)] = struct{}{}
	}
	w.log.Debug("daemon.sessionwatcher.prompt", "prompt routed to the feed", dlog.Context{
		"agent_id": prompt.GetAgent().GetValue(), "turn_id": prompt.GetId().GetValue(),
	})
	w.sinks.Feed.OnPrompt(w.ws, prompt.GetAgent(), prompt, place)
}

// adoptVendorTurnLocked opens the turn a VENDOR_STARTED prompt row announces.
//
// Every observer a queued turn's open edge reaches is told the same here: the
// turn stands in flight (so a submission is held behind it and freeness waits
// on it), the footer and the feed take the turn-open edge, and the queue --
// told off the lock -- records the turn's durable row and the roster's turn
// fact, so the turn's terminal closes it through the one door and draws its
// ending, final answer included.
//
// A TURN ALREADY IN FLIGHT WAITS BEHIND IT (ruled 2026-09-28). The shim
// adopts a vendor-started turn BESIDE a turn whose send is still in the
// vendor's queue, delivers that send rather than refusing it, and matches each
// reply to its own send by id. So the adopted turn is the one running, and the
// turn it found in flight -- the daemon's own, opening or accepted, or an
// adopted turn such as the shim's network resume -- stands in flight again
// when it ends (turnEndedLocked).
func (w *watcher) adoptVendorTurnLocked(prompt *conversationv1.AgentPrompt) {
	turn := ids.TurnID(prompt.GetId().GetValue())
	if turn == "" {
		w.log.Error("daemon.sessionwatcher.turn_adopted_unidentified", "a vendor-started turn's prompt row named no turn; nothing was opened", dlog.Context{
			"agent_id": prompt.GetAgent().GetValue(),
		})
		return
	}
	if w.turnKnownLocked(turn) {
		w.log.Debug("daemon.sessionwatcher.turn_adopted_again", "a vendor-started turn's prompt row was served again; the turn was already taken up", dlog.Context{
			"turn_id": string(turn), "turn_in_flight": turnValue(w.turn),
		})
		return
	}
	ahead := w.turn
	aheadAdopted := w.adopted != nil && ahead != nil && *w.adopted == *ahead
	aheadAccepted := ahead != nil && w.acceptedLocked(*ahead)
	if !w.standTurnLocked(turn, "daemon.sessionwatcher.state_transition", "the vendor-started turn became the turn in flight") {
		return
	}
	adopted := turn
	w.adopted = &adopted
	behind := ""
	if ahead != nil && *ahead != turn {
		behind = string(*ahead)
		w.waiting = append(w.waiting, waitingTurn{turn: *ahead, accepted: aheadAccepted && !aheadAdopted, adopted: aheadAdopted})
	}
	w.sinks.Footer.OnTurnOpened(w.ws, turn)
	w.sinks.Feed.OnTurnOpened(w.ws, turn)
	w.pendingAdoptions = append(w.pendingAdoptions, turn)
	w.log.Info("daemon.sessionwatcher.turn_adopted", "the vendor started a turn on its own; it is tracked as the turn in flight", dlog.Context{
		"turn_id":  string(turn),
		"agent_id": prompt.GetAgent().GetValue(),
		"behind":   behind,
	})
}

// routePeerMessageLocked routes a message another Claude session sent into the
// watched agent's conversation. Unlike a live prompt it never opens a turn — it
// is not this agent's own work and drives no response of its own — so it is
// simply handed to the feed to draw as the peer bubble.
func (w *watcher) routePeerMessageLocked(a *agentWatch, peer *conversationv1.PeerMessage, turn *conversationv1.TurnId, place *conversationv1.ConversationPlace) {
	if a.id == nil {
		// A peer message names its recipient (the main agent for a session), so
		// on a not-yet-adopted watch it is the same first-sight of the main
		// agent a live prompt is.
		w.adoptMainAgentLocked(peer.GetAgent(), "live_peer_message")
	}
	w.log.Debug("daemon.sessionwatcher.peer_message", "peer message routed to the feed", dlog.Context{
		"agent_id": peer.GetAgent().GetValue(), "sender": peer.GetSender(), "peer_id": peer.GetId(),
	})
	w.sinks.Feed.OnPeerMessage(w.ws, peer, turn, place)
}

// routeAgentFrameLocked routes one AgentFrame by its arm. THE UNIT UPSERTED IS
// THE FRAME'S OWN agent_id, whichever stream carried it: frames are flat and
// nothing here reconstructs ancestry. `turn` is the entry's own turn stamp
// (HistoryEntryAt.turn), unset for an entry no producer stamped.
func (w *watcher) routeAgentFrameLocked(a *agentWatch, frame *conversationv1.AgentFrame, at *conversationv1.HistoryPointer, turn *conversationv1.TurnId, place *conversationv1.ConversationPlace) {
	agent := frame.GetAgentId()
	if a.id == nil {
		// EVERY FRAME ON THE MAIN WATCH IS THE MAIN AGENT'S, so it names the
		// root's owner for the views before it is routed to them.
		w.nameMainForViewsLocked(agent, "main_watch_frame", false)
	}

	if isClosingFrame(frame) && w.closingReplayedLocked(a, frame, at) {
		return
	}

	switch {
	case frame.GetUpdate() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeAgentFrameLocked", "branch": "case frame.GetUpdate() != nil"})
		w.routeUpdateLocked(agent, frame.GetUpdate(), at, turn, place)
	case frame.GetSuccess() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeAgentFrameLocked", "branch": "case frame.GetSuccess() != nil"})
		w.routeTerminalLocked(a, agent, turn, place, frame.GetSuccess(), nil)
	case frame.GetFailure() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeAgentFrameLocked", "branch": "case frame.GetFailure() != nil"})
		w.routeTerminalLocked(a, agent, turn, place, nil, frame.GetFailure())
	case frame.GetDetachedWork() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeAgentFrameLocked", "branch": "case frame.GetDetachedWork() != nil"})
		w.routeDetachedWorkLocked(agent, frame.GetDetachedWork(), turn, place)
	default:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeAgentFrameLocked", "branch": "default"})
		w.log.Warn("daemon.sessionwatcher.agent_frame_unrouted", "an AgentFrame carried no result arm", dlog.Context{
			"agent_id": agent.GetValue(),
		})
	}
}

// isTerminalFrame reports whether a frame is an agent's terminal.
func isTerminalFrame(frame *conversationv1.AgentFrame) bool {
	return frame.GetSuccess() != nil || frame.GetFailure() != nil
}

// isClosingFrame reports whether a frame CLOSES AN ACT: an agent's terminal
// ends its run, and a context cut ends the clear or compaction that produced
// it. These are the rows routed once per pointer (closingReplayedLocked).
func isClosingFrame(frame *conversationv1.AgentFrame) bool {
	return isTerminalFrame(frame) || frame.GetUpdate().GetContextCut() != nil
}

// closingKey is a closing row's identity on one watch.
func closingKey(a *agentWatch, at *conversationv1.HistoryPointer) string {
	return watchKey(a.id) + "\x00" + at.GetValue()
}

// closingReplayedLocked records a closing row as served and reports whether
// it had been served before — the ONE identity check every closing row passes
// through, live or on a page, so a row is routed at most once per pointer.
//
// A row with no pointer has no identity to compare, so it is never taken for
// a replay: it is routed rather than lost. The contract gives every entry a
// pointer, so its absence is recorded as the producer defect it is.
func (w *watcher) closingReplayedLocked(a *agentWatch, frame *conversationv1.AgentFrame, at *conversationv1.HistoryPointer) bool {
	agent := frame.GetAgentId()
	if at.GetValue() == "" {
		w.log.Error("daemon.sessionwatcher.closing_unaddressed", "a row closing an act carried no pointer, so a replay of it cannot be recognized", dlog.Context{
			"agent_id": agent.GetValue(), "closing": closingKind(frame),
		})
		return false
	}
	key := closingKey(a, at)
	if _, seen := w.seenClosings[key]; !seen {
		w.seenClosings[key] = struct{}{}
		return false
	}
	if isTerminalFrame(frame) {
		// A TERMINAL IS ROUTED ONCE. This row was already served on this
		// watch — live, or on an opening page — so it is the shim re-serving
		// its book after the store ended a standing watch, not an agent
		// ending. The frame names no turn, so routing it would charge it to
		// whichever turn is open NOW: that is how a store restart once ended
		// a live turn with the previous turn's terminal (2026-09-23 12:38:36).
		w.log.Info("daemon.sessionwatcher.terminal_replayed", "a terminal row already served was served again; it is dropped", dlog.Context{
			"agent_id": agent.GetValue(), "pointer": at.GetValue(), "turn_in_flight": turnValue(w.turn),
		})
		return true
	}
	// A CUT IS ROUTED ONCE, for the same reason: re-served, it would end
	// whatever compaction or turn the views are drawing NOW, and drop a
	// context figure read after it.
	w.log.Info("daemon.sessionwatcher.context_cut_replayed", "a cut row already served was served again; it is dropped", dlog.Context{
		"agent_id": agent.GetValue(), "pointer": at.GetValue(), "turn_in_flight": turnValue(w.turn),
	})
	return true
}

// closingKind names a closing row's kind for a log record.
func closingKind(frame *conversationv1.AgentFrame) string {
	if isTerminalFrame(frame) {
		return "terminal"
	}
	return "context_cut"
}

// routeUpdateLocked routes one AgentUpdate arm.
func (w *watcher) routeUpdateLocked(agent *conversationv1.AgentId, update *conversationv1.AgentUpdate, at *conversationv1.HistoryPointer, turn *conversationv1.TurnId, place *conversationv1.ConversationPlace) {
	switch {
	case update.GetActivity() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeUpdateLocked", "branch": "case update.GetActivity() != nil"})
		w.routeActivityLocked(agent, update.GetActivity(), turn, place)

	case update.GetQuestion() != nil:
		question := update.GetQuestion()
		w.log.Debug("daemon.sessionwatcher.question", "the agent is blocked on a choice", dlog.Context{
			"agent_id": agent.GetValue(), "question_id": question.GetId().GetValue(),
		})
		w.sinks.Feed.OnQuestion(w.ws, agent, question, turn, place)
		w.sinks.Footer.OnQuestion(w.ws, agent, question)
		w.notifyQuestionLocked(question)

	case update.GetPermission() != nil:
		permission := update.GetPermission()
		w.log.Debug("daemon.sessionwatcher.permission", "the agent is blocked on consent", dlog.Context{
			"agent_id": agent.GetValue(), "permission_id": permission.GetId().GetValue(),
		})
		w.sinks.Feed.OnPermission(w.ws, agent, permission, turn, place)
		w.sinks.Footer.OnPermission(w.ws, agent, permission)
		w.sinks.Sidebar.OnPermission(w.ws, agent, permission)
		w.notifyPermissionLocked(permission)

	case update.GetContextCut() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeUpdateLocked", "branch": "case update.GetContextCut() != nil"})
		w.routeContextCutLocked(agent, update.GetContextCut(), at, turn, place, cutLive)

	case update.GetApiError() != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeUpdateLocked", "branch": "case update.GetApiError() != nil"})
		w.log.Warn("daemon.sessionwatcher.api_error", "a vendor request failed mid-turn", dlog.Context{
			"agent_id": agent.GetValue(), "message": update.GetApiError().GetMessage(),
		})
		w.sinks.Feed.OnApiError(w.ws, agent, update.GetApiError(), turn, place)
		w.sinks.Footer.OnApiError(w.ws, agent, update.GetApiError())
		w.sinks.Sidebar.OnApiError(w.ws, agent, update.GetApiError())

	case update.GetContextBudgetWarning() != nil:
		// THE AGENT PLANE owns the budget warning: it is a transcript
		// attachment the sidecar produces, and the footer's activity line is
		// its only consumer.
		w.log.Debug("daemon.sessionwatcher.context_budget_warning", "the vendor warned the context window is filling", dlog.Context{
			"agent_id": agent.GetValue(),
		})
		w.sinks.Footer.OnContextBudgetWarning(w.ws, agent, update.GetContextBudgetWarning())

	default:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeUpdateLocked", "branch": "default"})
		w.log.Warn("daemon.sessionwatcher.update_unrouted", "an AgentUpdate arm has no route", dlog.Context{
			"agent_id": agent.GetValue(),
		})
	}
}

// cutSource says how a routed cut reached the watcher.
type cutSource int

const (
	// cutLive is a cut served as a live entry.
	cutLive cutSource = iota
	// cutCaughtUp is a cut on a catch-up page: written while no stream stood.
	cutCaughtUp
)

// String names the source for a log record.
func (s cutSource) String() string {
	if s == cutCaughtUp {
		return "caught_up"
	}
	return "live"
}

// routeContextCutLocked routes one context cut to every view that resolves
// from it. It is the ONE path a cut takes, whether it arrived live or on a
// catch-up page, so no view can be handed one and not the other.
//
// THE FEED IS THE ONE DIFFERENCE: a caught-up cut is on the page the feed was
// just handed whole, and it draws the divider from there.
func (w *watcher) routeContextCutLocked(agent *conversationv1.AgentId, cut *conversationv1.ContextCut, at *conversationv1.HistoryPointer, turn *conversationv1.TurnId, place *conversationv1.ConversationPlace, source cutSource) {
	ctx := dlog.Context{"agent_id": agent.GetValue(), "pointer": at.GetValue(), "source": source.String()}
	if source == cutCaughtUp {
		// INFO, NOT DEBUG: an act closed while no stream stood, and this is
		// the only record that its end reached the views at all.
		w.log.Info("daemon.sessionwatcher.context_cut", "a cut written while no stream stood was caught up; the footer and the topbar take it", ctx)
	} else {
		w.log.Debug("daemon.sessionwatcher.context_cut", "the conversation was cut; the footer clears its cut states", ctx)
		w.sinks.Feed.OnContextCut(w.ws, agent, cut, at, turn, place)
	}
	w.sinks.Footer.OnContextCut(w.ws, agent, cut)
	w.sinks.Topbar.OnContextCut(w.ws, agent, cut)
	// A CLEAR OR A COMPLETED COMPACTION moves the digest boundary, so the
	// synthesizer resets its hash and re-synthesizes on the next trigger. A
	// FAILED compaction cut nothing, so the digest is unchanged and the
	// synthesizer is left alone.
	if w.sinks.Title != nil && (cut.GetCleared() != nil || cut.GetCompacted() != nil) {
		w.sinks.Title.OnContextReset(w.ws)
	}
}

// routeActivityLocked routes one unit of a turn's synchronous progress, and
// records what the unit taught the watcher on the way through.
func (w *watcher) routeActivityLocked(agent *conversationv1.AgentId, act *conversationv1.AgentActivity, turn *conversationv1.TurnId, place *conversationv1.ConversationPlace) {
	w.recordActivityLocked(act)

	w.sinks.Feed.OnActivity(w.ws, agent, act, turn, place)
	w.sinks.Footer.OnActivity(w.ws, agent, act)
	// THE TOPBAR SEES EVERY ACTIVITY. It shows an unmodeled tool as a warning,
	// but it also accumulates the SESSION's token spend from the usage every
	// activity carries (internal/resolve/topbar's observeUsage), and a sink
	// handed only the unmodeled frames would count nothing at all.
	w.sinks.Topbar.OnActivity(w.ws, agent, act)
	if act.GetUnmodeled() != nil {
		w.log.Warn("daemon.sessionwatcher.unmodeled_activity", "an activity the schema does not model", dlog.Context{
			"agent_id": agent.GetValue(), "tool_name": unmodeledToolName(act.GetUnmodeled()),
		})
	}
	w.reapEndedMonitorLocked(act)
	w.routeDetachedSubagentLocked(agent, act)
	w.watchSpawnedSubagentLocked(act)
	w.notifyPushLocked(act)
}

// routeDetachedSubagentLocked routes a subagent frame that belongs to work
// which has ALREADY LEFT THE TURN, addressed by its handle.
//
// WHY THE HANDLE AND NOT THE STREAM. A detached run's frames reach this daemon
// on either book: the spawning agent's, when the producer settles the unit
// there (a task notification does exactly that), or the run's own, which the
// watcher opened at the announcement. The footer's chip counts LIVE runs, so it
// must retire one at its terminal WHEREVER the terminal arrived — and the only
// thing common to both deliveries is the work handle, which is why this routes
// by identity rather than by which stream carried the frame.
//
// The handle is recovered two ways, and both are facts this watcher already
// holds: the unit's own detachment (`facts[unit].work`, remembered when the
// announcement resolved), and the watch the frame arrived on (`entry.work`,
// set when the subagent's stream became detached work).
func (w *watcher) routeDetachedSubagentLocked(agent *conversationv1.AgentId, act *conversationv1.AgentActivity) {
	sub := act.GetSubagent()
	if sub == nil {
		return
	}
	work := w.detachedHandleForLocked(agent, act)
	if work == nil {
		return
	}
	w.log.Debug("daemon.sessionwatcher.detached_subagent_frame", "a detached subagent frame was routed by its handle", dlog.Context{
		"work_id": work.GetValue(), "agent_id": agent.GetValue(),
		"activity_id": act.GetActivityId().GetValue(),
	})
	w.sinks.Footer.OnSubagent(w.ws, work, sub)
	w.reapSettledDetachedSubagentLocked(work, sub)
}

// reapSettledDetachedSubagentLocked drops a detached subagent from the LIVE-WORK
// SET at its own terminal arm.
//
// THE UNIT'S TERMINAL IS THE ONLY SETTLE THE CONTRACT PROMISES. A detached run's
// own stream carries its response frames and no agent terminal: the producer
// settles the run as the SUBAGENT UNIT's terminal arm ("exactly one terminal
// arm ... on whichever stream is carrying the unit"), and the minting rule makes
// that frame retire the handle by equality — "a subagent's end retires the
// handle ... without any join table". So the live set, exactly like the footer's
// chip, retires the run HERE rather than waiting for a stream terminal that is
// never owed. Waiting for one left every settled detached subagent live for the
// rest of the session, which is what AwaitFree, turn liveness and the shutdown
// drain all read.
func (w *watcher) reapSettledDetachedSubagentLocked(work *conversationv1.DetachedWorkId, sub *conversationv1.AgentSubagent) {
	if sub.GetSuccess() == nil && sub.GetFailure() == nil {
		return
	}
	// BY THE HANDLE ALONE, which is what the ledger is keyed by: the run's
	// watch -- open, still opening, or never possible -- has no say in it.
	item, ok := w.liveHandleLocked(work.GetValue())
	if !ok || item.kind != kindSubagent {
		return
	}
	w.log.Debug("daemon.sessionwatcher.detached_subagent_settled", "a detached subagent settled at its own terminal", dlog.Context{
		"work_id": work.GetValue(), "agent_id": item.agent.GetValue(), "failed": sub.GetFailure() != nil,
	})
	if w.concludeLocked(work.GetValue(), concludedUnitTerminal) {
		w.publishLiveWorkLocked()
	}
}

// detachedHandleForLocked answers the handle a subagent frame belongs to, or
// nil when the run it names has not detached.
//
// A SPAWN MADE BY THE DETACHED RUN IS NOT THE RUN. A detached subagent's own
// stream also carries the subagents IT spawns, and their frames are subagent
// frames too; taking the stream's handle for them addressed a nested spawn's
// start and launch receipt to the PARENT run, which rewrote the parent's
// footer row with the child's identity and then retired it, so the parent
// came back as a minimal "subagent" row (daemon.footer.live_work_taken
// "added agent:<parent>" within seconds of each nested launch, 2026-09-23).
// A unit whose own start named a DIFFERENT agent than the stream's is such a
// spawn: it is addressed by its own handle once it detaches, and by nothing
// before.
func (w *watcher) detachedHandleForLocked(agent *conversationv1.AgentId, act *conversationv1.AgentActivity) *conversationv1.DetachedWorkId {
	unit := act.GetActivityId().GetValue()
	fact, known := w.facts[unit]
	if known && fact.work != nil {
		return fact.work
	}
	// THE UNIT IS ITS OWN HANDLE when nothing recorded the join: the contract
	// mints a subagent's handle from its spawn unit (DetachedWorkId.value ==
	// AgentActivityId.value), which is how a `created`-origin run -- a
	// re-adopted one, or one that named no agent to watch -- is retired by its
	// own terminal.
	if item, live := w.liveHandleLocked(unit); live && item.kind == kindSubagent {
		return item.work
	}
	entry, ok := w.agents[agent.GetValue()]
	if !ok || entry.work == nil {
		return nil
	}
	if known && fact.agent.GetValue() != "" && fact.agent.GetValue() != agent.GetValue() {
		w.log.Debug("daemon.sessionwatcher.nested_spawn_not_the_run",
			"a subagent frame on a detached run's stream names a spawn of that run, not the run itself; it is not addressed by the run's handle",
			dlog.Context{
				"agent_id": agent.GetValue(), "work_id": entry.work.GetValue(),
				"activity_id": unit, "spawned_agent": fact.agent.GetValue(),
			})
		return nil
	}
	return entry.work
}

// notifyPushLocked raises the host notification a PushNotification send earns.
//
// The agent reaching an ABSENT user is the attention marker's whole reason for
// existing (agent_activity.proto: "the LOCAL attention presentation ... is this
// system's own fan-out of the fact"), and nothing else in the daemon fans it
// out. The kind is agent_addressed: the agent addressed the user directly, and
// the pushed message IS the notification line.
//
// Only the START is a notification. The vendor's success and failure states
// report on a send already announced, and re-raising attention for them would
// mark the workspace twice for one message.
func (w *watcher) notifyPushLocked(act *conversationv1.AgentActivity) {
	push, ok := act.GetItem().(*conversationv1.AgentActivity_PushNotification)
	if !ok {
		return
	}
	start := push.PushNotification.GetStart()
	if start == nil {
		return
	}
	w.log.Debug("daemon.sessionwatcher.notify", "push notification raised", dlog.Context{
		"activity_id": act.GetActivityId().GetValue(),
	})
	w.sinks.Lifecycle.OnNotification(w.ws, HostNotification{
		Text: start.GetMessage(),
		At:   instantOf(start.GetStartedAt().GetAtMs()),
		Kind: NotificationAgentAddressed,
	})
}

// watchSpawnedSubagentLocked opens the WatchAgent stream a SYNC subagent's own
// work arrives on. A subagent's frames are addressed to the created agent and
// draw on that agent's sub-feed, and the only way they ever reach the daemon
// is a watch opened for it — so the spawn's start frame opens one eagerly,
// exactly as a detached announcement does. The watch is NOT live work: an
// in-turn subagent is the turn's own progress, and counting it would make the
// workspace unfree for the whole spawn.
func (w *watcher) watchSpawnedSubagentLocked(act *conversationv1.AgentActivity) {
	start := act.GetSubagent().GetStart()
	if start == nil {
		return
	}
	w.watchCreatedAgentLocked(start.GetCreatedAgentId(), act.GetActivityId().GetValue())
}

// watchCreatedAgentLocked opens the WatchAgent stream one spawn's created agent
// draws on, keyed by the created agent's id so the open is idempotent: a spawn
// already watched (a repeated live frame, a page that also carries the child's
// own book, a child re-adopted as live work) opens no second stream. It is the
// shared core of the two callers that name a created agent — the live spawn
// path and the opening-page replay.
func (w *watcher) watchCreatedAgentLocked(created *conversationv1.AgentId, activityID string) {
	if created.GetValue() == "" {
		w.log.Error("daemon.sessionwatcher.subagent_unaddressable", "a subagent spawn named no created agent to watch", dlog.Context{
			"activity_id": activityID,
		})
		return
	}
	if _, ok := w.agents[created.GetValue()]; ok {
		w.log.Debug("daemon.sessionwatcher.subagent_watch_repeat", "the spawned subagent is already watched", dlog.Context{
			"agent_id": created.GetValue(),
		})
		return
	}
	entry := &agentWatch{id: created}
	w.agents[created.GetValue()] = entry
	w.log.Debug("daemon.sessionwatcher.subagent_watch", "watching a spawned subagent's own stream", dlog.Context{
		"agent_id": created.GetValue(), "activity_id": activityID,
	})
	w.openAgentStreamLocked(entry)
}

// watchSpawnedSubagentsOnPageLocked opens a WatchAgent for every created child a
// spawn frame on an OPENING PAGE names.
//
// REGRESSION FIX (subagent conversation empty after resume). A subagent's own
// conversation reaches the feed ONLY through a WatchAgent opened for the created
// agent, and the live path opens one from every spawn frame
// (watchSpawnedSubagentLocked from routeActivityLocked, routeDetachedWorkLocked
// from an announcement). On RESUME neither fires: the opening page is walked by
// routeOpeningPageLocked rather than as live frames, so a subagent that finished
// in a prior session was neither live work nor re-watched and its book was never
// fetched — the expanded bubble showed only the parent's commission. The child's
// frames ARE in the store, so this is wiring, not new capture: for every spawn
// the page carries — the in-turn spawn activity, and the detached-work
// announcement's created arm — we open the same watch the live path would, and
// the child's own page replays into Feed{Agent: created}. Opening is idempotent
// (watchCreatedAgentLocked skips an already-watched agent), so this cannot
// double a watch the main watch or live-work adoption already holds.
//
// A SPAWN ABOVE THE PAGE'S NEWEST CUT OPENS NOTHING. The feed begins at that
// cut: the spawn's bubble is withheld, so the child's sub-feed can never be
// reached, and its book would be fetched for no reader. Live work that
// outlived the cut is adopted by the live-work path, not by this one.
func (w *watcher) watchSpawnedSubagentsOnPageLocked(page *conversationv1.HistoryPage) {
	entries := page.GetEntries()
	if bound := contextcut.NewestBoundOnPage(page); bound >= 0 {
		if skipped := len(entries) - bound - 1; skipped > 0 {
			w.log.Debug("daemon.sessionwatcher.page_spawns_above_cut", "a page's entries above its newest context cut open no subagent watches", dlog.Context{
				"entries_above_cut": skipped,
			})
		}
		entries = entries[:bound]
	}
	for _, at := range entries {
		frame := at.GetEntry().GetAgentFrame()
		if frame == nil {
			continue
		}
		switch arm := frame.GetResult().(type) {
		case *conversationv1.AgentFrame_Update:
			if act := arm.Update.GetActivity(); act != nil {
				w.watchSpawnedSubagentLocked(act)
			}
		case *conversationv1.AgentFrame_DetachedWork:
			if sub := arm.DetachedWork.GetCreated().GetWorkCreated().GetSubagent(); sub != nil {
				if start := sub.GetStart(); start != nil {
					w.watchCreatedAgentLocked(start.GetCreatedAgentId(), arm.DetachedWork.GetWork().GetValue())
				}
			}
		}
	}
}

// routeTerminalLocked routes how one agent's stream ended, and reaps the watch
// it was carried on. Exactly one of success and failure is set.
func (w *watcher) routeTerminalLocked(a *agentWatch, agent *conversationv1.AgentId, stamp *conversationv1.TurnId, place *conversationv1.ConversationPlace, success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) {
	// A TURN THE SHIM WAS ALREADY RUNNING WHEN THIS WATCHER ATTACHED has no
	// StartTurn answer coming to name the main agent: this watcher never sent
	// it one. Held, its terminal would wait for a name that never arrives and
	// the adopted turn would never end. The MAIN watch carries the main
	// agent's frames alone, so its terminal names the main agent itself.
	if a.id == nil && w.mainAgent == nil && w.turn != nil && *w.turn == w.factsTurn {
		w.adoptMainAgentLocked(agent, "adopted_turn_terminal")
	}
	if a.id == nil && w.mainAgent == nil {
		// THE MAIN AGENT HAS NOT BEEN NAMED YET, so this terminal cannot be
		// attributed to the turn. It arrived on the SHIM'S STREAM plane while
		// the naming rides StartTurn's answer, and the two planes carry no
		// ordering between them — so under load the end outruns the name.
		//
		// IT IS HELD, NOT DROPPED. Routing it unattributed would leave the
		// turn standing in flight with no edge left to end it: the footer
		// never comes back to idle and every freeness waiter hangs. Guessing
		// the attribution is equally wrong — it would drain the prompt queue
		// on a subagent's terminal. So the WHOLE terminal waits here, views
		// included, and is replayed in full the moment the name lands
		// (adoptMainAgentLocked) or the turn is known to be unattributable
		// (flushHeldTerminalLocked).
		//
		// DEBUG, NOT WARN: the hold is the design, and the race is an
		// ordinary consequence of two planes with no ordering between them.
		w.log.Debug("daemon.sessionwatcher.turn_end_withheld", "a terminal arrived before the main agent was named", dlog.Context{
			"agent_id": agent.GetValue(), "turn_id": turnValue(w.turn),
		})
		if w.held != nil {
			// ONE SLOT, and a second occupant means the main watch produced
			// two terminals with no naming in between. That is not a thing the
			// contract allows, and the first one would be silently lost.
			w.log.Error("daemon.sessionwatcher.turn_end_withheld_twice", "a second terminal arrived while one was already held", dlog.Context{
				"held_agent_id": w.held.agent.GetValue(), "agent_id": agent.GetValue(),
			})
		}
		w.held = &heldTerminal{agent: agent, stamp: stamp, place: place, success: success, failure: failure}
		return
	}

	isMain := w.isMainAgent(agent)

	var turn *ids.TurnID
	if isMain {
		turn = w.terminalTurnLocked(agent, stamp)
	}

	w.log.Debug("daemon.sessionwatcher.agent_terminal", "an agent's stream ended", dlog.Context{
		"agent_id": agent.GetValue(), "main": isMain, "failed": failure != nil,
	})
	w.sinks.Feed.OnAgentTerminal(w.ws, agent, turn, success, failure, place)
	w.sinks.Footer.OnAgentTerminal(w.ws, agent, turn, success, failure)
	w.sinks.Sidebar.OnAgentTerminal(w.ws, agent, turn, success, failure)

	switch {
	case isMain && turn != nil:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeTerminalLocked", "branch": "case isMain && turn != nil"})
		how := turnCloseOf(success, failure)
		w.log.Info("daemon.sessionwatcher.turn_ended", "the turn closed", dlog.Context{
			"turn_id": string(*turn), "close": int(how),
		})
		w.turnEndedLocked(*turn, how)
	}

	if w.retireAgentLocked(agent.GetValue(), concludedAgentTerminal) {
		w.publishLiveWorkLocked()
	}
}

// terminalTurnLocked answers WHICH TURN a main-agent terminal ends: the turn its
// own stamp names, never the turn that merely happens to be open.
//
//   - Stamped with the open turn: it ends that turn.
//   - Stamped with a turn this watcher knows but that is not open (one that
//     already ended, or was refused): it ends nothing that is open. DEBUG — a
//     second terminal for a finished turn is the producer's to explain, and the
//     open turn is not its business.
//   - Stamped with a turn this watcher never knew: it ends nothing. ERROR — a
//     record names a turn no queue opened and no page carried.
//   - UNSTAMPED (a producer from before the stamp, or one that could not name
//     the turn): today's positional answer, the turn in flight, recorded ONCE
//     per watcher at INFO.
func (w *watcher) terminalTurnLocked(agent *conversationv1.AgentId, stamp *conversationv1.TurnId) *ids.TurnID {
	if stamp.GetValue() == "" {
		if w.turn == nil {
			return nil
		}
		if !w.unstampedTerminalSeen {
			w.unstampedTerminalSeen = true
			w.log.Info("daemon.sessionwatcher.terminal_unstamped", "a turn terminal carried no turn id (pre-contract, or a producer that could not name it); it is charged to the turn in flight", dlog.Context{
				"agent_id": agent.GetValue(), "turn_id": string(*w.turn),
			})
		}
		open := *w.turn
		return &open
	}
	named := ids.TurnID(stamp.GetValue())
	if w.turn != nil && *w.turn == named {
		return &named
	}
	// A TURN WAITING BEHIND THE ADOPTED ONE IS STILL OPEN: its terminal ends it
	// where it waits, and the turn running ahead of it is left alone.
	for _, waiting := range w.waiting {
		if waiting.turn == named {
			w.log.Info("daemon.sessionwatcher.terminal_turn_waiting", "a terminal named a turn waiting behind the turn in flight; it ends that turn", dlog.Context{
				"agent_id": agent.GetValue(), "turn_id": string(named), "turn_in_flight": turnValue(w.turn),
			})
			return &named
		}
	}
	if w.turnKnownLocked(named) {
		w.log.Debug("daemon.sessionwatcher.terminal_turn_not_open", "a terminal named a turn that is not in flight; it ends nothing that is open", dlog.Context{
			"agent_id": agent.GetValue(), "turn_id": string(named), "turn_in_flight": turnValue(w.turn),
		})
		return nil
	}
	w.log.Error("daemon.sessionwatcher.terminal_turn_unknown", "a terminal named a turn this daemon never opened or replayed; it ends nothing", dlog.Context{
		"agent_id": agent.GetValue(), "turn_id": string(named), "turn_in_flight": turnValue(w.turn),
	})
	return nil
}

// turnKnownLocked reports whether this watcher has ever had the turn in hand:
// stood in flight, closed, or carried by a page or a routed prompt.
func (w *watcher) turnKnownLocked(turn ids.TurnID) bool {
	if _, seen := w.knownTurns[turn]; seen {
		return true
	}
	_, closed := w.closedTurns[turn]
	return closed
}

// heldTerminal is a main-watch terminal that arrived before the main agent was
// named. It is the whole terminal, so its replay is indistinguishable from the
// routing it would have had if the name had come first.
type heldTerminal struct {
	agent *conversationv1.AgentId
	stamp *conversationv1.TurnId
	// place is where the terminal's entry sits in its conversation, held
	// with it so the row it draws when released is ordered as the entry is.
	place   *conversationv1.ConversationPlace
	success *conversationv1.AgentSuccess
	failure *conversationv1.AgentFailure
}

// releaseHeldTerminalLocked replays a held terminal now that the main agent has
// a name. It is called from the ONE place the name is latched, so the replay
// cannot re-hold: w.mainAgent is non-nil by the time it runs.
func (w *watcher) releaseHeldTerminalLocked() {
	held := w.held
	if held == nil || w.mainAgent == nil {
		return
	}
	w.held = nil
	w.log.Debug("daemon.sessionwatcher.turn_end_released", "the held terminal was routed once the main agent was named", dlog.Context{
		"agent_id": held.agent.GetValue(), "turn_id": turnValue(w.turn),
	})
	w.routeTerminalLocked(w.mainWatchLocked(), held.agent, held.stamp, held.place, held.success, held.failure)
	// OFF THE CALLER'S GOROUTINE. See flushTurnEndsAsync: the naming arrives on
	// the prompt queue's own call, and the turn end goes back to that queue.
	w.flushTurnEndsAsync()
}

// flushHeldTerminalLocked routes a still-held terminal on the edges that settle
// the turn WITHOUT ever naming a main agent — the shim's refusal of the turn,
// and the session's own death. The attribution will never arrive, so the views
// take it unattributed rather than the terminal being lost.
func (w *watcher) flushHeldTerminalLocked() {
	held := w.held
	if held == nil {
		return
	}
	w.held = nil
	w.log.Info("daemon.sessionwatcher.turn_end_unattributed", "a held terminal was routed unattributed; the main agent was never named", dlog.Context{
		"agent_id": held.agent.GetValue(),
	})
	w.sinks.Feed.OnAgentTerminal(w.ws, held.agent, nil, held.success, held.failure, held.place)
	w.sinks.Footer.OnAgentTerminal(w.ws, held.agent, nil, held.success, held.failure)
	w.sinks.Sidebar.OnAgentTerminal(w.ws, held.agent, nil, held.success, held.failure)
	if w.retireAgentLocked(held.agent.GetValue(), concludedAgentTerminal) {
		w.publishLiveWorkLocked()
	}
}

// mainWatchLocked is the main agent's watch, which a replayed terminal is
// routed against. The watch is opened at bring-up; the empty stand-in keeps a
// replay from depending on that ordering.
func (w *watcher) mainWatchLocked() *agentWatch {
	if w.main != nil {
		return w.main
	}
	return &agentWatch{}
}

// turnCloseOf derives how a turn ended from the terminal arm that ended it.
// AgentSuccess.backgrounded closes the turn as COMPLETED: the stream ended
// because what was asked for happened, and the work's own stream carries it
// from there.
func turnCloseOf(success *conversationv1.AgentSuccess, failure *conversationv1.AgentFailure) TurnClose {
	if failure != nil {
		return wsm.CloseFailed
	}
	if success.GetInterrupted() != nil {
		return wsm.CloseKilled
	}
	return wsm.CloseCompleted
}

// turnValue renders a turn id for a log record.
func turnValue(turn *ids.TurnID) string {
	if turn == nil {
		return ""
	}
	return string(*turn)
}

// ---- detached work ----

// routeDetachedWorkLocked routes an announcement that work has left a stream:
// it ADMITS the item to the live-work ledger, and opens the watch it names.
// The announcement is what makes an item live, and only its conclusion ends it
// (livework.go); the watch draws its progress and has no say in its liveness.
//
// A MALFORMED ANNOUNCEMENT GOES NOWHERE. One that names no kind, or a `created`
// description that disagrees with its own kind, is refused and recorded at
// ERROR before any view takes it, so no view draws work the live set will never
// hold. A subagent that names NO AGENT is the one malformation that is still
// COUNTED (see admitUnaddressableLocked): the shim announced live work, and the
// daemon being unable to watch it does not make it any less live.
func (w *watcher) routeDetachedWorkLocked(announcer *conversationv1.AgentId, work *conversationv1.AgentDetachedWork, turn *conversationv1.TurnId, place *conversationv1.ConversationPlace) {
	kind, agent, ok := w.resolveDetachedLocked(work)
	if !ok {
		return
	}
	handle := work.GetWork()
	if kind == kindSubagent && agent.GetValue() == "" {
		w.admitUnaddressableLocked(handle)
		return
	}
	w.sinks.Feed.OnDetachedWork(w.ws, announcer, work, turn, place)
	w.sinks.Footer.OnDetachedWork(w.ws, announcer, work)
	w.sinks.Sidebar.OnDetachedWork(w.ws, announcer, work)

	w.log.Debug("daemon.sessionwatcher.detached_work", "work left its stream", dlog.Context{
		"work_id": handle.GetValue(), "kind": kind.String(), "agent_id": agent.GetValue(),
	})

	if _, retired := w.retiredWork[handle.GetValue()]; retired && handle.GetValue() != "" {
		// A RETIRED HANDLE STAYS RETIRED. The views above took the row (the
		// feed wants the output path an end-of-run upsert adds), but the work
		// is over and nothing here re-opens it. See watcher.retiredWork.
		w.log.Debug("daemon.sessionwatcher.detached_work_retired", "an announcement for work that already settled; it stays out of the live set", dlog.Context{
			"work_id": handle.GetValue(), "kind": kind.String(), "agent_id": agent.GetValue(),
		})
		return
	}

	switch kind {
	case kindSubagent:
		if entry, ok := w.agents[agent.GetValue()]; ok {
			// THE SYNC WATCH BECOMES THE DETACHED ONE. A spawn watched as the
			// turn's own progress carries NO handle and is not live work; the
			// announcement is what makes it live -- so returning here without
			// admitting it leaves the live set stating no agents, and an
			// interrupt addressed at all of them answering that nothing is
			// running.
			if entry.work == nil {
				entry.work = handle
				w.log.Debug("daemon.sessionwatcher.detached_work_promoted", "a watched subagent became detached work", dlog.Context{
					"agent_id": agent.GetValue(), "work_id": handle.GetValue(),
				})
				w.admitLocked(handle, kindSubagent, agent)
				w.publishLiveWorkLocked()
				// The promotion is not an excuse to leave a refused watch
				// dark: a spawn whose open the shim refused has no stream,
				// and this announcement is an occasion to open one.
				if entry.stream == nil && !w.inFlightLocked(entry.opening) {
					w.openAgentStreamLocked(entry)
				}
				return
			}
			// A REPEAT IS ALSO A RETRY, exactly as it is for a shell: the
			// shim refuses WatchAgent for a book it has not registered yet,
			// so an entry can be carrying no stream, and the repeated
			// announcement is the occasion to open one.
			if entry.stream == nil && !w.inFlightLocked(entry.opening) {
				w.log.Info("daemon.sessionwatcher.detached_work_reopen", "a repeated announcement re-opened a detached subagent's watch", dlog.Context{
					"agent_id": agent.GetValue(),
				})
				w.openAgentStreamLocked(entry)
				return
			}
			w.log.Debug("daemon.sessionwatcher.detached_work_repeat", "the subagent is already watched", dlog.Context{
				"agent_id": agent.GetValue(),
			})
			return
		}
		entry := &agentWatch{id: agent, work: handle}
		w.agents[agent.GetValue()] = entry
		w.admitLocked(handle, kindSubagent, agent)
		w.openAgentStreamLocked(entry)
		w.publishLiveWorkLocked()

	case kindBash:
		if handle.GetValue() == "" {
			w.log.Error("daemon.sessionwatcher.detached_shell_unaddressable", "a detached shell named no handle to watch", nil)
			return
		}
		if entry, ok := w.shells[handle.GetValue()]; ok {
			// A REPEAT IS ALSO A RETRY. The shim refuses WatchBash while its
			// store holds no rows for the handle yet, so an entry can be
			// carrying no stream; the repeated announcement is the occasion
			// to open one, and answering it "already watched" would leave the
			// shell dark for the rest of the session.
			if entry.stream == nil && !w.inFlightLocked(entry.opening) {
				w.log.Info("daemon.sessionwatcher.detached_work_reopen", "a repeated announcement re-opened a detached shell's watch", dlog.Context{
					"work_id": handle.GetValue(),
				})
				w.openShellStreamLocked(entry)
				return
			}
			w.log.Debug("daemon.sessionwatcher.detached_work_repeat", "the shell is already watched", dlog.Context{
				"work_id": handle.GetValue(),
			})
			return
		}
		// THE SHELL IS LIVE FROM THE ANNOUNCEMENT, and its open is made off
		// the lock; it is published here rather than at an install nobody can
		// order against the frames that follow. Whatever the open then does --
		// installs, is refused, or never answers -- the shell stays live until
		// the shim concludes it.
		entry := &shellWatch{work: handle}
		w.shells[handle.GetValue()] = entry
		w.admitLocked(handle, kindBash, nil)
		w.openShellStreamLocked(entry)
		w.publishLiveWorkLocked()

	case kindMonitor:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeDetachedWorkLocked", "branch": "case kindMonitor"})
		// NO STREAM: the contract gives a monitor none, so the ledger entry is
		// all it has until the monitor activity's own terminal concludes it.
		if w.admitLocked(handle, kindMonitor, nil) {
			w.publishLiveWorkLocked()
		}

	case kindWorkflow:
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "routeDetachedWorkLocked", "branch": "case kindWorkflow"})
		// A workflow is KICKED: no watch is ever opened for one, and the shim
		// client deliberately exposes no workflow verbs.
		w.log.Info("daemon.sessionwatcher.workflow_kicked", "a workflow run is not watched", dlog.Context{
			"work_id": handle.GetValue(),
		})

	default:
		// UNREACHABLE: resolveDetachedLocked refuses every announcement whose
		// kind it did not name. Reaching here is a defect in that resolution.
		w.log.Error("daemon.sessionwatcher.detached_kind_unrouted", "a resolved detached kind has no route; the announcement was not routed", dlog.Context{
			"work_id": handle.GetValue(), "kind": kind.String(),
		})
	}
}

// admitUnaddressableLocked counts a detached subagent whose announcement named
// NO AGENT. Nothing can be watched for it and no view is handed an
// announcement it cannot place, but it IS live work: leaving it out of the set
// let a lease holder judge the workspace free while it ran. It is admitted by
// its handle alone, and its spawn unit's own terminal -- which retires the
// handle by equality -- or a re-announcement concludes it like any other item.
// The malformation itself was recorded at ERROR by resolveDetachedLocked.
func (w *watcher) admitUnaddressableLocked(handle *conversationv1.DetachedWorkId) {
	if _, retired := w.retiredWork[handle.GetValue()]; retired {
		w.log.Debug("daemon.sessionwatcher.detached_work_retired", "an announcement for work that already settled; it stays out of the live set", dlog.Context{
			"work_id": handle.GetValue(), "kind": kindSubagent.String(),
		})
		return
	}
	if w.admitLocked(handle, kindSubagent, nil) {
		w.publishLiveWorkLocked()
	}
}

// resolveDetachedLocked answers what KIND of work an announcement names, and
// for a subagent the agent id its watch is addressed by — or refuses the
// announcement, recording why at ERROR.
//
// THE ANNOUNCEMENT'S OWN KIND IS THE AUTHORITY (AgentDetachedWork.kind, stated
// by the producer from the vendor's task record), never a fact about the unit
// the work detached from. That unit is not always of the work's kind: a
// subagent RESUMED BY `SendMessage` detaches from the send, and a unit this
// watcher never saw (a restored session) teaches it nothing at all. Reading
// the kind off the unit left both unrouted
// (daemon.sessionwatcher.detached_kind_unknown, 2026-09-27): the agent never
// reached the live set, and the footer never listed it.
//
// The unit-to-handle join is still recorded for a `detached` origin: the
// unit's own terminal is what retires the handle.
func (w *watcher) resolveDetachedLocked(work *conversationv1.AgentDetachedWork) (detachedKind, *conversationv1.AgentId, bool) {
	handle := work.GetWork().GetValue()
	kind, agent := announcedKind(work.GetKind())
	switch {
	case kind == kindUnknown:
		w.log.Error("daemon.sessionwatcher.detached_kind_unknown", "a detached announcement named no kind; it is refused, never guessed", dlog.Context{
			"work_id": handle, "origin": detachedOrigin(work),
		})
		return kindUnknown, nil, false
	case kind == kindSubagent && agent.GetValue() == "" && handle == "":
		w.log.Error("daemon.sessionwatcher.detached_subagent_unaddressable", "a detached subagent named neither an agent nor a handle; it cannot be counted and is refused", dlog.Context{
			"work_id": handle, "origin": detachedOrigin(work),
		})
		return kindUnknown, nil, false
	case kind == kindSubagent && agent.GetValue() == "":
		// STILL AN ANNOUNCEMENT DEFECT, and recorded as one; but the work it
		// announces is live, so it is counted by its handle rather than
		// dropped (admitUnaddressableLocked).
		w.log.Error("daemon.sessionwatcher.detached_subagent_unaddressable", "a detached subagent named no agent to watch; it is counted live by its handle, unwatched, until the shim concludes it", dlog.Context{
			"work_id": handle, "origin": detachedOrigin(work),
		})
		if detached := work.GetDetached(); detached != nil {
			w.rememberWorkLocked(detached.GetDetachedFromId().GetValue(), work.GetWork())
		}
		return kindSubagent, nil, true
	}
	if created := work.GetCreated(); created != nil {
		described, describedAgent := createdKind(created.GetWorkCreated())
		if described != kind || (kind == kindSubagent && describedAgent.GetValue() != agent.GetValue()) {
			w.log.Error("daemon.sessionwatcher.detached_kind_conflict", "a created announcement describes different work than its own kind names; it is refused", dlog.Context{
				"work_id": handle, "kind": kind.String(), "agent_id": agent.GetValue(),
				"described_kind": described.String(), "described_agent_id": describedAgent.GetValue(),
			})
			return kindUnknown, nil, false
		}
		// THE COMMISSION RESTATES THE DESCRIBED START (DetachedWorkKindSubagent.
		// commission), and two statements of one fact that disagree are refused
		// exactly as two kinds are: neither can be chosen.
		start := created.GetWorkCreated().GetSubagent().GetStart()
		if commission := work.GetKind().GetSubagent().GetCommission(); commission != nil && !proto.Equal(commission, start.GetPrompt()) {
			w.log.Error("daemon.sessionwatcher.detached_commission_conflict", "a created announcement's commission disagrees with the start it describes; it is refused", dlog.Context{
				"work_id": handle, "agent_id": agent.GetValue(),
				"commission_description": commission.GetDescription(), "described_description": start.GetPrompt().GetDescription(),
			})
			return kindUnknown, nil, false
		}
	}
	if detached := work.GetDetached(); detached != nil {
		w.rememberWorkLocked(detached.GetDetachedFromId().GetValue(), work.GetWork())
	}
	return kind, agent, true
}

// announcedKind reads the kind an announcement states, and a subagent's agent.
func announcedKind(kind *conversationv1.DetachedWorkKind) (detachedKind, *conversationv1.AgentId) {
	switch arm := kind.GetKind().(type) {
	case *conversationv1.DetachedWorkKind_Subagent:
		return kindSubagent, arm.Subagent.GetAgentId()
	case *conversationv1.DetachedWorkKind_Bash:
		return kindBash, nil
	case *conversationv1.DetachedWorkKind_Monitor:
		return kindMonitor, nil
	case *conversationv1.DetachedWorkKind_Workflow:
		return kindWorkflow, nil
	default:
		return kindUnknown, nil
	}
}

// createdKind reads the kind a `created` description names, and a spawn's
// created agent, so the two statements an announcement makes can be compared.
func createdKind(item *conversationv1.DetachableWork) (detachedKind, *conversationv1.AgentId) {
	switch arm := item.GetWork().(type) {
	case *conversationv1.DetachableWork_Subagent:
		return kindSubagent, arm.Subagent.GetStart().GetCreatedAgentId()
	case *conversationv1.DetachableWork_Bash:
		return kindBash, nil
	case *conversationv1.DetachableWork_Monitor:
		return kindMonitor, nil
	case *conversationv1.DetachableWork_Workflow:
		return kindWorkflow, nil
	default:
		return kindUnknown, nil
	}
}

// detachedOrigin names an announcement's origin arm for a log record.
func detachedOrigin(work *conversationv1.AgentDetachedWork) string {
	switch {
	case work.GetDetached() != nil:
		return "detached:" + work.GetDetached().GetDetachedFromId().GetValue()
	case work.GetCreated() != nil:
		return "created"
	default:
		return "unset"
	}
}

// rememberWorkLocked records the handle a unit detached under, so the unit's
// own terminal can drop the right item from the live set.
//
// A UNIT THIS WATCHER NEVER SAW IS RECORDED TOO. A restored session announces
// work whose unit's frames never reached this watcher, and the terminal that
// retires it arrives later on the spawning agent's book; without the join it
// retired nothing and the work stayed live for the rest of the session.
func (w *watcher) rememberWorkLocked(activityID string, handle *conversationv1.DetachedWorkId) {
	if activityID == "" {
		return
	}
	fact, ok := w.facts[activityID]
	if !ok {
		fact = &activityFact{}
		w.facts[activityID] = fact
	}
	fact.work = handle
}

// ---- one detached shell's stream ----

// routeBashLocked routes one detached shell frame, and concludes the shell at
// its run's terminal.
func (w *watcher) routeBashLocked(s *shellWatch, bash *conversationv1.AgentBash) {
	w.sinks.Feed.OnBash(w.ws, s.work, bash)
	w.sinks.Footer.OnBash(w.ws, s.work, bash)

	if bash.GetSuccess() == nil && bash.GetFailure() == nil {
		return
	}
	w.log.Debug("daemon.sessionwatcher.bash_terminal", "a detached shell ended", dlog.Context{
		"work_id": s.work.GetValue(), "failed": bash.GetFailure() != nil,
	})
	if w.concludeLocked(s.work.GetValue(), concludedBashTerminal) {
		w.publishLiveWorkLocked()
	}
}

// ---- monitors ----

// reapEndedMonitorLocked concludes a monitor at its own terminal. A monitor has
// no stream, so its activity's terminal arm is the only thing that can retire
// it.
func (w *watcher) reapEndedMonitorLocked(act *conversationv1.AgentActivity) {
	monitor := act.GetMonitor()
	if monitor == nil || (monitor.GetEnded() == nil && monitor.GetFailure() == nil) {
		return
	}
	// THE HANDLE IS THE ACTIVITY ID for a `created`-origin monitor:
	// DetachedWorkId.value == the unit's AgentActivityId.value (same bytes),
	// so a re-adopted monitor — which was never announced on this watch and
	// therefore has no recorded fact — is retired by its own terminal. A
	// `detached`-origin monitor keeps resolving through the recorded fact.
	key := act.GetActivityId().GetValue()
	if fact, ok := w.facts[key]; ok && fact.work != nil {
		key = fact.work.GetValue()
	}
	if item, live := w.liveHandleLocked(key); !live || item.kind != kindMonitor {
		return
	}
	if w.concludeLocked(key, concludedMonitorTerminal) {
		w.publishLiveWorkLocked()
	}
}

// ---- what an activity teaches the watcher ----

// recordActivityLocked keeps the two facts about a unit that later frames
// need: a subagent spawn's CREATED AGENT, which tells a nested spawn's frames
// apart from the run that carries them, and the TOOL NAME, which is what a
// permission notification names. The detachable KIND is not among them: an
// announcement states its own (resolveDetachedLocked).
func (w *watcher) recordActivityLocked(act *conversationv1.AgentActivity) {
	id := act.GetActivityId().GetValue()
	if id == "" {
		return
	}
	fact, ok := w.facts[id]
	if !ok {
		fact = &activityFact{}
		w.facts[id] = fact
	}
	fact.tool = ActivityToolName(act)
	if start := act.GetSubagent().GetStart(); start != nil {
		w.log.Debug("daemon.sessionwatcher.routing_decision", "selected a session routing branch", dlog.Context{"function": "recordActivityLocked", "branch": "a subagent start names its created agent"})
		fact.agent = start.GetCreatedAgentId()
	}
}

// ActivityToolName names the tool a unit called, empty for units that are not
// tool calls (prose, thinking, injected context). ONE PLACE: every consumer of
// a tool name reads it from here -- this package's permission notifications and
// the footer's tool-call line alike -- and an arm added to the
// contract without a name here is warned about at the call that needs one.
func ActivityToolName(act *conversationv1.AgentActivity) string {
	switch item := act.GetItem().(type) {
	case *conversationv1.AgentActivity_Read:
		return "Read"
	case *conversationv1.AgentActivity_Write:
		return "Write"
	case *conversationv1.AgentActivity_Edit:
		return "Edit"
	case *conversationv1.AgentActivity_Grep:
		return "Grep"
	case *conversationv1.AgentActivity_Glob:
		return "Glob"
	case *conversationv1.AgentActivity_Bash:
		return "Bash"
	case *conversationv1.AgentActivity_Subagent:
		return "Agent"
	case *conversationv1.AgentActivity_SkillUse:
		return "Skill"
	case *conversationv1.AgentActivity_SendMessage:
		return "SendMessage"
	case *conversationv1.AgentActivity_SubagentHandback:
		return "SubagentHandback"
	case *conversationv1.AgentActivity_TaskAct:
		return "TaskAct"
	case *conversationv1.AgentActivity_Hook:
		return "Hook"
	case *conversationv1.AgentActivity_WebFetch:
		return "WebFetch"
	case *conversationv1.AgentActivity_WebSearch:
		return "WebSearch"
	case *conversationv1.AgentActivity_Monitor:
		return "Monitor"
	case *conversationv1.AgentActivity_ScheduleWakeup:
		return "ScheduleWakeup"
	case *conversationv1.AgentActivity_Artifact:
		return "Artifact"
	case *conversationv1.AgentActivity_PlanMode:
		return "ExitPlanMode"
	case *conversationv1.AgentActivity_ReportFindings:
		return "ReportFindings"
	case *conversationv1.AgentActivity_Worktree:
		return "Worktree"
	case *conversationv1.AgentActivity_Cron:
		return "Cron"
	case *conversationv1.AgentActivity_PushNotification:
		return "PushNotification"
	case *conversationv1.AgentActivity_McpToolCall:
		return mcpToolName(item.McpToolCall)
	case *conversationv1.AgentActivity_Unmodeled:
		return unmodeledToolName(item.Unmodeled)
	default:
		return ""
	}
}

// mcpToolName is the tool an MCP call named, from whichever of its frames
// carried it. A progress beat names none.
func mcpToolName(call *conversationv1.AgentMcpToolCall) string {
	if start := call.GetStart(); start != nil {
		return start.GetTool().GetName()
	}
	if success := call.GetSuccess(); success != nil {
		return success.GetTool().GetName()
	}
	return call.GetFailure().GetTool().GetName()
}

// unmodeledToolName is the tool an unmodeled call named, from whichever of its
// frames carried it.
func unmodeledToolName(unmodeled *conversationv1.AgentUnmodeled) string {
	if start := unmodeled.GetStart(); start != nil {
		return start.GetToolName()
	}
	if success := unmodeled.GetSuccess(); success != nil {
		return success.GetToolName()
	}
	return unmodeled.GetFailure().GetToolName()
}

// ---- notifications ----

// notifyPermissionLocked raises the host notification a blocked permission
// deserves, naming the TOOL the consent gates — and RETIRES that notification
// when the ask settles, because a decided gate is nothing left to see.
func (w *watcher) notifyPermissionLocked(permission *conversationv1.AgentPermission) {
	key := askKey("permission", permission.GetId().GetValue())
	start := permission.GetStart()
	if start == nil {
		// SETTLED, however it settled: the user's answer, a policy denial, or a
		// failure to put the ask at all. Each of them ends the ask, and the
		// marker names asks that have not ended.
		w.askSettledLocked(key)
		return
	}
	w.unseenAsks[key] = struct{}{}
	note := HostNotification{
		Text:     start.GetPrompt().GetTitle(),
		At:       instantOf(start.GetStartedAt().GetAtMs()),
		Kind:     NotificationPermissionRequested,
		ToolName: w.permissionToolNameLocked(permission),
	}
	w.log.Debug("daemon.sessionwatcher.notify", "permission notification raised", dlog.Context{
		"tool_name": note.ToolName,
	})
	w.sinks.Lifecycle.OnNotification(w.ws, note)
}

// permissionToolNameLocked names the gated call's tool. The gated call is an
// activity id, so the unit's own recorded name is the first and best source;
// an ask rule names a tool when the vendor said one; the vendor's short
// display name is the last resort, because it is a phrase rather than a tool.
func (w *watcher) permissionToolNameLocked(permission *conversationv1.AgentPermission) string {
	if fact, ok := w.facts[permission.GetGatedCall().GetValue()]; ok && fact.tool != "" {
		return fact.tool
	}
	if rule := permission.GetStart().GetTrigger().GetAskRule(); rule.GetToolName() != "" {
		return rule.GetToolName()
	}
	return permission.GetStart().GetPrompt().GetDisplayName()
}

// notifyQuestionLocked raises the host notification a blocked question
// deserves: HostNotificationKind.question_asked, which gets a permission ask's
// attention treatment and carries the first question's chip label.
func (w *watcher) notifyQuestionLocked(question *conversationv1.AgentQuestion) {
	key := askKey("question", question.GetId().GetValue())
	start := question.GetStart()
	if start == nil {
		// SETTLED: answered, or concluded with nobody answering. Either way the
		// ask is over and its marker has nothing left to point at.
		w.askSettledLocked(key)
		return
	}
	asked := start.GetBatch().GetQuestions()
	if len(asked) == 0 {
		return
	}
	w.unseenAsks[key] = struct{}{}
	text := asked[0].GetHeader()
	if text == "" {
		text = asked[0].GetQuestion().GetText()
	}
	note := HostNotification{
		Text:   text,
		At:     instantOf(start.GetStartedAt().GetAtMs()),
		Kind:   NotificationQuestionAsked,
		Header: asked[0].GetHeader(),
	}
	w.log.Debug("daemon.sessionwatcher.notify", "question notification raised", dlog.Context{
		"question_id": question.GetId().GetValue(),
	})
	w.sinks.Lifecycle.OnNotification(w.ws, note)
}

// askKey names one ask inside the unseen set. The two ask kinds have their own
// identity spaces — a question joins to no unit of work, where a permission's
// identity is the tool unit it gates — so the kind is part of the key rather
// than trusted not to collide.
func askKey(kind, id string) string {
	return kind + ":" + id
}

// askSettledLocked retires one ask's unseen notification, and reports the
// workspace SEEN once the last of them is gone.
//
// ONLY THE LAST ONE CLEARS. Four permission cards answered one after another
// leave the marker standing until the fourth is answered — while any ask is
// still open there is still something unseen. An ask this watcher never
// announced (a policy denial that never opened) retires nothing: it never
// raised a marker, so its settle must not clear another ask's.
func (w *watcher) askSettledLocked(key string) {
	if _, ok := w.unseenAsks[key]; !ok {
		return
	}
	delete(w.unseenAsks, key)
	if len(w.unseenAsks) > 0 {
		w.log.Debug("daemon.sessionwatcher.notify", "an ask settled with others still open", dlog.Context{
			"ask": key, "open": len(w.unseenAsks),
		})
		return
	}
	w.log.Debug("daemon.sessionwatcher.notify", "the last open ask settled; the attention marker is cleared", dlog.Context{
		"ask": key,
	})
	w.sinks.Lifecycle.OnAsksSettled(w.ws)
}

// instantOf reads a producer's unix-millis instant; an unstated instant is
// now, because a notification always happened at some time.
func instantOf(atMS int64) time.Time {
	if atMS == 0 {
		return time.Now()
	}
	return time.UnixMilli(atMS)
}

// ---- small helpers ----

// isMainAgent reports whether a frame's agent is the session's main agent. It
// is false while the main agent is unnamed: attribution is never guessed.
func (w *watcher) isMainAgent(agent *conversationv1.AgentId) bool {
	return w.mainAgent != nil && agent.GetValue() != "" && agent.GetValue() == w.mainAgent.GetValue()
}

// pageAgent answers the agent the first row of a page that names one states:
// a prompt's recipient or a frame's own agent.
func pageAgent(page *conversationv1.HistoryPage) *conversationv1.AgentId {
	for _, entry := range page.GetEntries() {
		if agent := entry.GetEntry().GetUserPrompt().GetAgent(); agent.GetValue() != "" {
			return agent
		}
		if agent := entry.GetEntry().GetAgentFrame().GetAgentId(); agent.GetValue() != "" {
			return agent
		}
	}
	return nil
}

// watchAgentLocked is the identity a watch's frames belong to: its target, or
// the main agent once named.
func (w *watcher) watchAgentLocked(a *agentWatch) *conversationv1.AgentId {
	if a.id != nil {
		return a.id
	}
	return w.mainAgent
}
