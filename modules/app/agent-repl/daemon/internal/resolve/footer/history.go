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
		})
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
