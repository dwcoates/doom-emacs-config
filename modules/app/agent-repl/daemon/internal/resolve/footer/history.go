package footer

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// OnHistoryPage reconciles the strip to a conversation this daemon process
// never watched.
//
// A RELAUNCHED DAEMON INHERITS A CONVERSATION, NOT A HISTORY OF EDGES. The
// footer's idle substatus is `done` once a turn has run and `ready` until one
// has, and both of the facts that raise it — the turn-open edge and the
// terminal that ends a turn — are edges of a turn some earlier process
// watched. So a workspace whose session resumed on a fresh daemon drew its
// rehydrated rows under a strip that read `ready`: the feed said the
// conversation had happened and the footer said it never had.
//
// The opening page is the one statement of the prior conversation this daemon
// does get, so it is what reconciles the strip. Nothing else on the page is
// read: the rows are the feed's, and every LIVE fact still comes from the
// edges.
func (r *resolver) OnHistoryPage(ws ids.WorkspaceID, agent *conversationv1.AgentId, page *conversationv1.HistoryPage) {
	if !pageCarriesAConversation(page) {
		return
	}
	r.mutate(ws, "daemon.footer.history_page", "the replayed history page says a turn has already run",
		dlog.Context{"agent_id": agent.GetValue(), "entries": len(page.GetEntries())}, func(s *wsState) {
			s.turnEverRan = true
			r.restoreLastTurn(ws, s, agent, page)
		})
}

// restoreLastTurn stands the most recent turn's token accounting on a
// relaunched daemon, read off the main agent's first history page.
//
// THE CELL'S FIGURE OUTLIVES ITS TURN until the next submission (cell), and a
// daemon relaunch or handover is not a submission. The figure's facts are
// durable — every usage stamp is a settled frame of the main agent's book —
// so the page restates them: the newest turn the page names, and the usage
// its main-agent frames carried. Frames of any other agent on the page are
// left out, because a relaunched daemon holds no row to attribute them by
// and filing them under the main agent would inflate the figure.
//
// Only the FIRST main page is read, and only while this accounting has never
// opened a turn: a turn this daemon watched (or is watching) is the truth,
// and a later page holds older turns. A turn that straddles the page's
// boundary is restated from the part the page holds.
func (r *resolver) restoreLastTurn(ws ids.WorkspaceID, s *wsState, agent *conversationv1.AgentId, page *conversationv1.HistoryPage) {
	main := agent.GetValue()
	if s.turn != nil || s.tok.opened || s.tok.pageRead || main == "" || main != s.mainAgent {
		return
	}
	s.tok.pageRead = true
	r.restoreLastTokens(ws, s, main, page)
	r.restoreLastClock(ws, s, main, page)
}

// restoreLastClock restates the newest turn's frozen clock from the main
// agent's opening page (restoreLastTurn has decided the page is the one to
// read). The clock outlives its turn until the next submission exactly as the
// tokens figure does, and a relaunch is not a submission.
//
// THE SPAN IS READ OFF TWO DURABLE ENTRIES, never estimated: the place of the
// newest turn's user prompt (its start) and of the main agent's terminal for
// it (its end). A page lacking either — a turn straddling the page's boundary,
// a turn still running, a turn its shim's death cut with no terminal — leaves
// the clock idle rather than drawing a duration measured from a guessed edge.
func (r *resolver) restoreLastClock(ws ids.WorkspaceID, s *wsState, main string, page *conversationv1.HistoryPage) {
	turn, span, missing := lastTurnSpan(page, main)
	if span == nil {
		r.logOf(ws, s).Debug("daemon.footer.clock_not_restored",
			"the main agent's opening page does not hold its newest turn's start and end; the clock stays idle",
			dlog.Context{"turn_id": turn, "missing": missing})
		return
	}
	s.lastTurn = span
	r.logOf(ws, s).Info("daemon.footer.clock_restored",
		"the clock stands the most recent turn's duration, restated from the main agent's opening page",
		dlog.Context{"turn_id": turn, "started_at_ms": epochMs(span.startedAt), "ended_at_ms": epochMs(span.endedAt)})
}

// lastTurnSpan reads the newest turn a page names (newestTurn) and its span:
// the place of its user prompt and the place of the main agent's terminal for
// it. With either absent (or placeless) the span is nil and MISSING names what
// the page lacked.
func lastTurnSpan(page *conversationv1.HistoryPage, main string) (turn string, span *turnSpan, missing string) {
	turn = newestTurn(page)
	if turn == "" {
		return "", nil, "turn"
	}
	var start, end *conversationv1.ConversationPlace
	for _, entry := range page.GetEntries() {
		if entry.GetTurn().GetValue() != turn {
			continue
		}
		if entry.GetEntry().GetUserPrompt() != nil && start == nil {
			start = sessionwatcher.PlaceOf(entry)
		}
		frame := entry.GetEntry().GetAgentFrame()
		if frame != nil && frame.GetAgentId().GetValue() == main && end == nil &&
			(frame.GetSuccess() != nil || frame.GetFailure() != nil) {
			end = sessionwatcher.PlaceOf(entry)
		}
	}
	switch {
	case start == nil:
		return turn, nil, "prompt"
	case end == nil:
		return turn, nil, "terminal"
	}
	return turn, &turnSpan{startedAt: time.UnixMilli(start.GetAtMs()), endedAt: time.UnixMilli(end.GetAtMs())}, ""
}

// restoreLastTokens restates the newest turn's main-agent usage, and its
// alarm, from the main agent's opening page (restoreLastTurn has decided the
// page is the one to read).
func (r *resolver) restoreLastTokens(ws ids.WorkspaceID, s *wsState, main string, page *conversationv1.HistoryPage) {
	turn, units := lastTurnUsage(page, main)
	if len(units) == 0 {
		r.logOf(ws, s).Debug("daemon.footer.tokens_not_restored",
			"the main agent's opening page states no usage for its newest turn; the tokens cell stays idle",
			dlog.Context{"turn_id": turn})
		return
	}
	for _, u := range units {
		s.tok.observeUsage(u.unit, s.usageAgent(main), u.usage)
	}
	s.tok.restored = true
	// The alarm is the turn's too, and stands until the next turn as it would
	// have on the daemon that watched it.
	s.tok.evaluateAlarm(r.opts.alarmTokens)
	fresh, _ := s.tok.mainFresh()
	r.logOf(ws, s).Info("daemon.footer.tokens_restored",
		"the tokens cell stands the most recent turn's figure, restated from the main agent's opening page",
		dlog.Context{"turn_id": turn, "units": len(units), "fresh_input": fresh})
}

// pageUsage is one usage-carrying unit read off a history page.
type pageUsage struct {
	unit  string
	usage *conversationv1.TokenUsage
}

// newestTurn names the newest turn a page names. Its entries are served
// newest first, so the first entry carrying a turn is the newest turn's; an
// entry that names no turn is attributable to none and is skipped. Empty when
// no entry names a turn.
func newestTurn(page *conversationv1.HistoryPage) string {
	for _, entry := range page.GetEntries() {
		if id := entry.GetTurn().GetValue(); id != "" {
			return id
		}
	}
	return ""
}

// lastTurnUsage reads the newest turn a page names (newestTurn) and the usage
// every one of that turn's activity frames by the main agent carried.
func lastTurnUsage(page *conversationv1.HistoryPage, main string) (string, []pageUsage) {
	turn := newestTurn(page)
	var out []pageUsage
	for _, entry := range page.GetEntries() {
		if turn == "" || entry.GetTurn().GetValue() != turn {
			continue
		}
		frame := entry.GetEntry().GetAgentFrame()
		if frame == nil || frame.GetAgentId().GetValue() != main {
			continue
		}
		act := frame.GetUpdate().GetActivity()
		if act.GetUsage() == nil {
			continue
		}
		out = append(out, pageUsage{unit: act.GetActivityId().GetValue(), usage: act.GetUsage()})
	}
	return turn, out
}

// pageCarriesAConversation reports whether a replayed page holds any entry
// that is a real conversation entry. An EMPTY page (a fresh conversation's
// floor) states nothing and must not retire `ready`, and an entry with no arm
// set is not evidence of a turn either — it is a producer's own defect, which
// the feed's own replay records.
func pageCarriesAConversation(page *conversationv1.HistoryPage) bool {
	for _, entry := range page.GetEntries() {
		switch entry.GetEntry().GetEntry().(type) {
		case *conversationv1.HistoryEntry_UserPrompt, *conversationv1.HistoryEntry_AgentFrame:
			return true
		}
	}
	return false
}
