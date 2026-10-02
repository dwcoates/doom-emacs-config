package sessionwatcher

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
)

// HISTORY IS LOADED ON A READER'S REQUEST, NEVER BY A WATCH (owner ruling,
// feed paging on demand). Every watch opens tail_only or catches up from a held
// pointer, so the opening pages that used to name the main agent for an
// adopted session, and to tell the footer a resumed conversation had already
// run, are empty. The pages a reader's request loads are what states those
// facts now, and the feed's history source hands each one here before the feed
// draws it.

// NoteHistoryLoaded takes up what one loaded page states for the views.
func (w *watcher) NoteHistoryLoaded(agent *conversationv1.AgentId, page *conversationv1.HistoryPage, newest bool) {
	w.mu.Lock()
	defer w.mu.Unlock()
	key := watchKey(agent)
	entries := page.GetEntries()
	w.noteServedPageLocked(key, page)
	adopted := false
	if newest && len(entries) > 0 && w.known[key] == nil {
		// THE NEWEST PAGE'S NEWEST ENTRY IS HELD NOW, so a watch of this agent
		// that opens (or re-opens) from here catches up from it rather than
		// opening tail_only past whatever was written between.
		if ptr := entries[0].GetAt(); ptr.GetValue() != "" {
			w.known[key] = ptr
			adopted = true
		}
	}
	if agent == nil {
		w.knowPagePromptsLocked(page)
		if named := PageAgent(page); named != nil {
			w.nameMainForViewsLocked(named, "history_load", false)
		}
		for _, entry := range entries {
			if prompt := entry.GetEntry().GetUserPrompt(); prompt != nil {
				w.adoptMainAgentLocked(prompt.GetAgent(), "history_load")
				w.releaseHeldTerminalLocked()
				break
			}
		}
	}
	w.log.Debug("daemon.sessionwatcher.history_loaded", "a page a reader's request loaded was taken up by the views", dlog.Context{
		"agent_id": agent.GetValue(), "entries": len(entries), "newest": newest, "pointer_adopted": adopted,
	})
	w.sinks.Footer.OnHistoryPage(w.ws, w.historyAgentLocked(agent), page)
}

// historyAgentLocked is the identity a loaded page belongs to: the named agent,
// or the main agent once named.
func (w *watcher) historyAgentLocked(agent *conversationv1.AgentId) *conversationv1.AgentId {
	if agent != nil {
		return agent
	}
	if w.mainAgent != nil {
		return w.mainAgent
	}
	return w.viewsMain
}
