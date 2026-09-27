// Package contextcut states, once, which context cuts BOUND a conversation:
// the cuts after which what came before is no longer the conversation, so
// the feed begins at the newest of them (owner ruling, 2026-09-14).
package contextcut

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// Bounds reports whether a cut discarded or summarized the context before it.
// Only `cleared` and `compacted` do: a `compaction_failed` cut cut nothing,
// which is the whole of what it says.
func Bounds(cut *conversationv1.ContextCut) bool {
	switch cut.GetCut().(type) {
	case *conversationv1.ContextCut_Cleared, *conversationv1.ContextCut_Compacted:
		return true
	}
	return false
}

// NewestBoundOnPage answers the index, in a page's NEWEST-FIRST entries, of
// the newest cut that bounds the conversation, or -1 when the page carries
// none. Every entry at a higher index is conversation that cut left behind.
func NewestBoundOnPage(page *conversationv1.HistoryPage) int {
	for i, at := range page.GetEntries() {
		if Bounds(at.GetEntry().GetAgentFrame().GetUpdate().GetContextCut()) {
			return i
		}
	}
	return -1
}
