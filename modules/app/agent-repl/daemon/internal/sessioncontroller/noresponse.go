package sessioncontroller

import (
	"strings"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenutilization"
)

// THE DEFECT THIS FILE CURATES AWAY.
//
// When a message reaches the session that asks for no reply, the CLI closes the
// turn by writing its OWN assistant record whose entire body is the sentence
// "No response requested." Nothing generated it — no request was made and no
// tokens were spent — but it is an ordinary `"type": "assistant"` transcript
// line in every other respect, so the feed drew a response bubble carrying that
// sentence and nothing else. Verified live on 118 records across 76 sessions in
// the on-disk transcripts; the parents are every kind of record there is (a
// "Continue from where you left off." nudge, a /clear, an interrupt marker, a
// task-notification, a typed prompt), so the record it answers is no signal.
//
// WITHHELD, NOT DELETED, exactly as in machinery.go and skillbody.go: the store
// keeps the record, and the delta is still pushed so its through_seq advances
// the frontend's cursor. Only the rendered item is suppressed.

// syntheticModel is the model id the CLI stamps on an assistant record IT
// wrote rather than one a model produced.
//
// It is the structural half of the predicate below, and it is not sufficient on
// its own: every terminal API failure the CLI reports — a rate limit, a
// server error, a bad model id — is ALSO written as a `<synthetic>` assistant
// record, and those are exactly the records a user most needs to see.
var syntheticModel = tokenutilization.SyntheticModelIdentity

// noResponseRequestedText is the placeholder's entire body.
//
// WHY THE PROSE IS PART OF THE PREDICATE, reluctantly. The record carries no
// flag separating it from the other `<synthetic>` assistant records: it is not
// isMeta, not isApiErrorMessage, its stop_reason ("stop_sequence") is the same
// one every synthetic record carries, and its zeroed usage is shared with them
// too. Keying on `<synthetic>` alone would swallow the API-failure cards; so
// the text is what distinguishes this record from its siblings, and the model
// id is what distinguishes it from a real answer.
//
// The model id is not optional garnish either — a live transcript holds a
// GENUINE model turn (msg_011Ccs3G4PCbhohA7kKszVu5, model claude-fable-5,
// stop_reason end_turn, 7 output tokens) whose reply was, verbatim, "No
// response requested." Matching text alone would have withheld a real answer.
const noResponseRequestedText = "No response requested."

// isNoResponsePlaceholder reports whether one conversation item is the CLI's
// no-response placeholder.
//
// EXACT AND SOLE, not a prefix or a substring. A synthetic record that opens
// with the sentence and goes on to say something else is not this placeholder,
// and neither is one carrying a second block. Surrounding whitespace is the
// only latitude given.
func isNoResponsePlaceholder(it *frontendv1.ConversationItem) bool {
	am := it.GetAgent().GetResponse().GetBody()
	if am == nil || am.GetModel() != syntheticModel {
		return false
	}
	blocks := am.GetContent()
	if len(blocks) != 1 {
		return false
	}
	text, ok := blocks[0].GetBlock().(*datav1.ContentBlock_Text)
	if !ok {
		return false
	}
	return strings.TrimSpace(text.Text.GetText()) == noResponseRequestedText
}

// withholdNoResponsePlaceholders removes the CLI's no-response placeholder
// records from a curated delta, loud-logging every one it takes out.
//
// Runs beside withholdMachinery (machinery.go) in pushConversation. It touches
// only the assistant_message arm, so it cannot interact with prompt attribution
// the way the user-record curators do.
func (c *consumer) withholdNoResponsePlaceholders(cd *frontendv1.ConversationDelta) {
	items := cd.GetItems()
	kept := items[:0]
	for _, it := range items {
		if !isNoResponsePlaceholder(it) {
			kept = append(kept, it)
			continue
		}
		c.logf("session-controller: assistant turn WITHHELD as the vendor's no-response placeholder ws=%q session=%s seq=%d uuid=%s model=%q — the CLI writes this synthetic assistant record to close a turn nothing was asked of; no model produced it and it says nothing; the store keeps it, the conversation feed does not",
			c.workspace, c.sessionID, cd.GetThroughSeq(), it.GetUuid(), syntheticModel)
	}
	cd.Items = kept
}
