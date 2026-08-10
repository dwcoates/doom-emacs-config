package frontend

import (
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// internalresume.go — WHY A RE-DRIVEN TURN HAS NO PROMPT.
//
// When a planned bounce has to take the shim with it, the turn in flight is
// interrupted and durably recorded as owed (statedb/promptreceipt.go). The
// successor daemon re-drives it, and because the vendor SDK has no
// resume-the-interrupted-turn primitive — `resume`, `continue` and
// `resumeSessionAt` restore CONTEXT and none restarts an aborted turn — the
// re-drive has to be an actual submit.
//
// That submit is a DAEMON FACT. The user wrote no second prompt, and rendering
// one would tell them they had asked twice. So the re-drive's user message is
// removed from the conversation here, at the ONE curation point every route
// from a store event to conversation content passes through (CurateEvent).
//
// WHY HERE AND NOT AT EACH SURFACE. The re-drive's user message is a real line
// in the vendor transcript — it has to be, or the model would not act on it —
// so it arrives on the live push, on a connect snapshot's backfill, on a resync
// replay and on a store re-pull alike. A filter in the webapp would leave Emacs
// showing it; a filter in each publication path would be four filters that
// could drift. One curator serves all four, and a client cannot render what it
// is never sent.
//
// WHY ONLY THE USER MESSAGE. The re-driven turn's OUTPUT is the continuation of
// the work the user asked for, and it belongs in the conversation at the
// position the interrupted turn occupied. Only the instruction that provoked it
// is internal. Filtering the whole request id would delete the work along with
// the ask.

// InternalResumeRequestIDPrefix marks a request id as belonging to the daemon's
// own re-drive of an interrupted turn.
//
// The id is minted at TEARDOWN, before the submit it names exists, which is
// what makes this a durable identity rather than a guess made later from the
// text. It lives in this package because this is where the filter runs and
// because the session controller — which mints it — already depends here, while
// nothing here may depend on the session controller.
const InternalResumeRequestIDPrefix = "resume-after-restart:"

// IsInternalResumeRequestID reports whether a request id names the daemon's own
// re-drive.
//
// An EMPTY id is never one. Plenty of events carry no request id at all, and
// reading absence as "internal" would hide arbitrary user content.
func IsInternalResumeRequestID(requestID string) bool {
	return requestID != "" && strings.HasPrefix(requestID, InternalResumeRequestIDPrefix)
}

// dropInternalResumePrompt removes the re-drive's own user message from a
// curated item list, leaving everything else the event carried.
//
// IT IS KIND-SCOPED, NOT REQUEST-SCOPED. The re-driven turn's assistant
// emissions, tool cards and results carry the SAME request id — they are that
// turn's work — and they are the continuation the user is owed. Only the
// `user_message` arm holds the instruction the daemon wrote.
func dropInternalResumePrompt(items []*frontendv1.ConversationItem) []*frontendv1.ConversationItem {
	kept := items[:0:0]
	for _, item := range items {
		if _, isUserMessage := item.GetItem().(*frontendv1.ConversationItem_UserMessage); isUserMessage &&
			IsInternalResumeRequestID(item.GetRequestId()) {
			continue
		}
		kept = append(kept, item)
	}
	return kept
}
