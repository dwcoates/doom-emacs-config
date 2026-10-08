package footer

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
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

// lastTurnUsage reads the newest turn a page names — its entries are served
// newest first, so the first entry carrying a turn is the newest turn's — and
// the usage every one of that turn's activity frames by the main agent
// carried. An entry that names no turn is attributable to none and is skipped.
func lastTurnUsage(page *conversationv1.HistoryPage, main string) (string, []pageUsage) {
	turn := ""
	var out []pageUsage
	for _, entry := range page.GetEntries() {
		id := entry.GetTurn().GetValue()
		if id == "" {
			continue
		}
		if turn == "" {
			turn = id
		}
		if id != turn {
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
