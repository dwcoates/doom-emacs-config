package frontend

import (
	"fmt"
	"strings"

	datav1 "agentrepl/proto/agentshim/data/v1"
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
//
// # THE REQUEST ID IS NOT ON THE MESSAGE, WHICH IS WHY THE MARKER EXISTS
//
// The event's `request_id` is the identity the filter WANTS, and for a vendor
// user line it is not there to be read. The line reaches the store through the
// FILE plane: the sidecar tails the transcript and wraps each record as an
// event of its own (`shim-claude-sidecar/internal/handler`), and no field in
// that record names the submit that provoked it. The daemon supplies the
// correlation in memory afterwards, oldest-outstanding-receipt first
// (sessioncontroller/promptecho.go) — and a re-drive mints no receipt, because
// the user typed nothing, so a re-drive's line was correlated with nothing and
// arrived at this filter with an EMPTY request id. It rendered, live and on
// every replay, which is the incident this marker closes.
//
// So the identity rides the MESSAGE. The re-drive's submitted text opens with a
// marker line naming the re-drive's own request id, minted at teardown, and the
// vendor writes that text into the transcript verbatim — which puts the
// identity on disk, inside the record, where a store re-pull years later with
// no daemon alive still finds it.
//
// A REAL USER PROMPT IS NEVER SUPPRESSED BY IT. The match is not on the
// instruction's wording — which is the session controller's business and free
// to change — but on a structural envelope: the message's first line must BE
// the marker, and the id it carries must itself be a re-drive id. Text that
// merely quotes the instruction, or mentions the marker mid-message, is the
// user's own and renders.

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

// internalResumeMarkerOpen and internalResumeMarkerClose bracket the durable
// marker the re-drive's submitted text opens with.
//
// The shape is deliberately not prose. It is one line, it is the FIRST line,
// and it names the re-drive's request id — three structural facts a message the
// user wrote does not accidentally have, and the reason the filter never has to
// consult the instruction's wording.
const (
	internalResumeMarkerOpen  = "<<agent-repl-internal resume-after-restart request_id="
	internalResumeMarkerClose = ">>"
)

// MarkInternalResumeInstruction returns the text the daemon SUBMITS for one
// re-drive: the durable marker line naming requestID, then the instruction.
//
// The marker is part of the submitted text rather than something added
// afterwards, because the whole point is that it reaches the vendor transcript.
// A caller that composes the submit any other way produces an unmarked
// instruction, which is a rendered instruction.
//
// A request id that is not a re-drive's is an INVARIANT VIOLATION and panics.
// Marking someone else's request id would make an arbitrary submit invisible,
// which is the one failure worse than the visible re-drive this exists to fix.
func MarkInternalResumeInstruction(requestID, instruction string) string {
	if !IsInternalResumeRequestID(requestID) {
		panic(fmt.Sprintf("frontend: MarkInternalResumeInstruction requires a re-drive request id (prefix %q), got %q",
			InternalResumeRequestIDPrefix, requestID))
	}
	return internalResumeMarkerOpen + requestID + internalResumeMarkerClose + "\n" + instruction
}

// MarkedInternalResumeRequestID returns the re-drive request id a submitted
// text carries, or "" when the text is not a marked re-drive.
//
// THE THREE CONDITIONS ARE ALL STRUCTURAL. The marker opens the text, it closes
// on that same first line, and what it brackets is itself a re-drive request
// id. Nothing here reads the instruction that follows.
func MarkedInternalResumeRequestID(text string) string {
	if !strings.HasPrefix(text, internalResumeMarkerOpen) {
		return ""
	}
	rest := text[len(internalResumeMarkerOpen):]
	line, _, _ := strings.Cut(rest, "\n")
	requestID, closed := strings.CutSuffix(line, internalResumeMarkerClose)
	if !closed || !IsInternalResumeRequestID(requestID) {
		return ""
	}
	return requestID
}

// internalResumeRequestIDOfUserMessage returns the re-drive request id a user
// message's own body carries, across both shapes a body can take.
//
// The BLOCK shape reads only the first block, for the reason the text shape
// reads only the first line: the marker is the head of what the daemon
// submitted, and a marker found anywhere else is text somebody else wrote.
func internalResumeRequestIDOfUserMessage(msg *datav1.ApiUserMessage) string {
	switch c := msg.GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		return MarkedInternalResumeRequestID(c.ContentString)
	case *datav1.ApiUserMessage_ContentBlocks:
		blocks := c.ContentBlocks.GetBlocks()
		if len(blocks) == 0 {
			return ""
		}
		return MarkedInternalResumeRequestID(blocks[0].GetText().GetText())
	default:
		return ""
	}
}

// internalResumeRequestIDOfItem returns the re-drive request id a conversation
// item is the instruction of, or "" for everything else.
//
// IT IS KIND-SCOPED, NOT REQUEST-SCOPED. The re-driven turn's assistant
// emissions, tool cards and results carry the SAME request id — they are that
// turn's work — and they are the continuation the user is owed. Only the
// `user_message` arm holds the instruction the daemon wrote.
//
// TWO EVIDENCE SOURCES, ONE VERDICT. The event's request id is the live push
// path's evidence, and the body's marker is the evidence that survives on disk;
// either alone names the re-drive, and neither is trusted to be the only one
// present.
func internalResumeRequestIDOfItem(item *frontendv1.ConversationItem) string {
	um, isUserMessage := item.GetItem().(*frontendv1.ConversationItem_UserMessage)
	if !isUserMessage {
		return ""
	}
	if id := item.GetRequestId(); IsInternalResumeRequestID(id) {
		return id
	}
	return internalResumeRequestIDOfUserMessage(um.UserMessage)
}

// dropInternalResumePrompt removes the re-drive's own user message from a
// curated item list, leaving everything else the event carried, and reports the
// re-drive request ids it suppressed.
//
// THE SUPPRESSED IDS ARE THE DELIVERY EVIDENCE, not bookkeeping. A curated
// instruction is proof that the re-drive naming it reached the vendor
// conversation, which is the one fact that discharges the owed resumption
// (sessioncontroller/turnresumption.go) — so the curator reports what it hid
// rather than the session controller deriving the same thing a second time.
func dropInternalResumePrompt(items []*frontendv1.ConversationItem) (kept []*frontendv1.ConversationItem, suppressed []string) {
	kept = items[:0:0]
	for _, item := range items {
		if id := internalResumeRequestIDOfItem(item); id != "" {
			suppressed = append(suppressed, id)
			continue
		}
		kept = append(kept, item)
	}
	return kept, suppressed
}
