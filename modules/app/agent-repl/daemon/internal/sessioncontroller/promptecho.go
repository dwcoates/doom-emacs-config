package sessioncontroller

import (
	"claude-repld/internal/frontend"
	"strings"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// promptEcho is ONE prompt the shim accepted, and the work the daemon pushed
// after synchronously publishing that prompt's `thinking` state.
//
// It is the daemon's RECEIPT OF ACCEPTANCE, not the vendor's echo of it. The
// prompt's durable life belongs to the transcript: the shim writes a UserLine,
// the store stamps it with a seq, and it reaches the frontend as an ordinary
// conversation item — eventually. Between the submit and that line the user had
// nothing on screen at all, which is the gap this closes.
type promptEcho struct {
	// requestID is the frontend command's own request id: the identity the
	// frontend already keys a prompt bubble on, and the one the durable line is
	// STAMPED with when it arrives (attributeUserTurn).
	requestID string
	text      string
	item      *frontendv1.Message
}

// echoUUID is the item identity a receipt is pushed under. Derived from the
// request id so a resync re-push REPLACES the standing work rather than
// adding a second one; the frontend keys on the request id first regardless, so
// this only has to be stable.
func echoUUID(requestID string) string { return "prompt-echo:" + requestID }

// promptReceiptItem composes THE receipt item, and is the ONE construction of
// it in the daemon.
//
// Both the live push below and the durable replay (durablereplay.go) build the
// work here, from the same identity, the same shape, and the same accept
// instant, so a receipt served after a bounce is indistinguishable from the one
// the user saw before it. There is no separate "replayed receipt" shape to keep
// in agreement with this one, and no extra marking: an unreconciled receipt is
// already the pending shape every frontend renders for a prompt the transcript
// has not claimed, and that is exactly what a replayed one is.
func promptReceiptItem(requestID, text string, tsMs int64) *frontendv1.Message {
	return &frontendv1.Message{
		Uuid:      echoUUID(requestID),
		TsMs:      tsMs,
		RequestId: requestID,
		Lineage:   frontend.FeedRowLineage(echoUUID(requestID)),
		Payload: &frontendv1.Message_UserMessage{UserMessage: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
		}},
	}
}

// The receipt for one accepted submit is RETAINED until the durable transcript
// line claims it. Its caller must first complete the synchronous
// accepted-prompt state barrier; these functions deliberately own only the
// receipt so the ordering remains explicit at forwardPrompt.
//
// RETAINED AND REPLAYED, exactly like a permission item (pushPermission) and
// for the same reason: it carries no store seq, so no from_seq a resync names
// could ever cover it. A frontend that reconnected between the submit and the
// transcript line would otherwise see nothing where the user's own prompt
// should be.
//
// SUPERSEDED BY THE DURABLE LINE, unlike a permission item: the moment
// attributeUserTurn stamps the transcript's UserLine with this request id, the
// store's own copy carries the prompt — at its real seq, in its real place —
// and the two reconcile onto one work in the frontend. Keeping the receipt
// past that point would mean replaying a daemon-local duplicate of something
// the conversation already holds. A receipt the transcript NEVER claims (the
// shim died mid-submit) is retained indefinitely, which is right: it is then
// the only evidence the prompt was ever sent.
// acceptedAtMs is the instant the daemon committed to the submit, which is the
// same instant the DURABLE receipt was recorded under. Passing it in rather
// than reading the clock again is what makes the live work and a replayed one
// the same item: same uuid, same timestamp, and therefore the same provenance
// verdict from the merge lease's ledger.
// RESERVATION AND PUBLICATION ARE TWO STEPS, and the split is the whole of the
// echo queue's ordering guarantee.
//
// The queue's ORDER is what attribution reads: claimOldestEcho hands a
// transcript line the oldest outstanding receipt, so the queue must be in the
// same order the prompts reached the SDK or two prompts swap identities. That
// used to be left to timing — the submit and the echo were twenty-nine lines
// apart with no mutual exclusion between them, and any two concurrent submit
// paths (an immediate submit against the queue's drain, an interject's head
// jump) could interleave to enqueue B's receipt before A's while the SDK had
// taken A first.
//
// So the slot is RESERVED before the submit, under the session's submit lock,
// and the two are ordered by construction rather than by luck
// (sessionController.submitMu, forwardPrompt). Only the PUBLICATION — which
// reaches the frontend server — is left outside that lock, because it must be:
// a frontend push under a submit lock would serialize the whole session behind
// whatever the push is waiting on.
//
// reserveEcho takes the slot and returns the reservation. It publishes nothing.
func (c *consumer) reserveEcho(requestID, text string, acceptedAtMs int64) *promptEcho {
	e := &promptEcho{requestID: requestID, text: text, item: promptReceiptItem(requestID, text, acceptedAtMs)}
	c.mu.Lock()
	c.echoes = append(c.echoes, e)
	pending := len(c.echoes)
	c.mu.Unlock()
	c.logf("session-controller: prompt echo reserved ws=%q session=%s request_id=%s len=%d unclaimed=%d — the slot is taken ahead of the submit so the queue's order is the SDK's order",
		c.workspace, c.sessionID, requestID, len(text), pending)
	return e
}

// publishEcho pushes a reserved receipt to the frontend. Called only after the
// shim has TAKEN the prompt, which is what preserves the property that a
// refused prompt never draws a bubble.
//
// Must be called with the submit lock and m.mu RELEASED: the push reaches the
// frontend server.
func (c *consumer) publishEcho(e *promptEcho) {
	c.logf("session-controller: prompt echo pushed ws=%q session=%s request_id=%s len=%d",
		c.workspace, c.sessionID, e.requestID, len(e.text))
	c.pushLocalItem(e.item)
}

// retractEcho gives back a slot reserved for a submit the shim REFUSED,
// reporting whether one was still outstanding.
//
// IT IS WHAT KEEPS THE REFUSED PROMPT BUBBLE-LESS through the reordering. The
// echo used to be enqueued after a successful submit, so a refusal simply never
// reached it; now the slot is taken first, and this is the path that undoes it.
// A retraction that finds nothing is ordinary rather than an anomaly — a
// transcript line may already have claimed the slot — and reports false.
func (c *consumer) retractEcho(requestID string) bool {
	c.mu.Lock()
	removed := c.removeEchoLocked(requestID)
	pending := len(c.echoes)
	c.mu.Unlock()
	c.logf("session-controller: prompt echo retracted ws=%q session=%s request_id=%s reservation_found=%v unclaimed=%d — the submit failed, so no bubble is drawn and no slot is left for a later line to claim",
		c.workspace, c.sessionID, requestID, removed, pending)
	return removed
}

// pushUserEcho reserves a slot and publishes its receipt in one step, for
// every caller that is not the submit path.
//
// THE SUBMIT PATH DOES NOT USE IT, and that is the only reason the two halves
// are separable at all: forwardPrompt must hold its reservation inside the
// session's submit lock and publish outside it, which one combined call cannot
// express. Anything with no submit to order against — a durable replay, a test
// harness — has nothing to interleave with and says both at once here.
func (c *consumer) pushUserEcho(requestID, text string, acceptedAtMs int64) {
	c.publishEcho(c.reserveEcho(requestID, text, acceptedAtMs))
}

// echo reserves and immediately publishes a session controller's prompt
// receipt, if the submit has an identity to key one on. A caller with no
// request id behind it (a test harness, an internal re-submit) pushes nothing
// rather than minting an id the frontend has no way to correlate — the durable
// transcript line still draws the prompt.
//
// IT IS NOT THE SUBMIT PATH'S SEAM. forwardPrompt reserves and publishes
// separately, so that its reservation sits inside the submit lock; this
// composition exists for callers that are not submitting anything through that
// lock.
//
// Must be called with m.mu RELEASED: the push reaches the frontend server.
func (m *Manager) echo(d *sessionController, requestID, text string, acceptedAtMs int64) {
	if requestID == "" {
		return
	}
	d.consumer.pushUserEcho(requestID, text, acceptedAtMs)
}

// snapshotEchoes returns the unclaimed receipts in submit order, for replay.
func (c *consumer) snapshotEchoes() []*frontendv1.Message {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.Message, 0, len(c.echoes))
	for _, e := range c.echoes {
		out = append(out, e.item)
	}
	return out
}

// claimEcho retires the receipt for requestID, reporting whether one was
// outstanding. It is the path for a durable line that ALREADY names its
// request (fake mode's engine echoes the id the submit carried): nothing needs
// stamping, but the receipt has still been superseded and must stop replaying.
func (c *consumer) claimEcho(requestID string) bool {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.removeEchoLocked(requestID)
}

// removeEchoLocked takes one receipt out of the queue by request id, reporting
// whether it was there. THE ONE removal-by-identity, shared by the claim a
// durable line makes and the retraction a failed submit makes, so the two
// cannot drift into two different notions of which slot belongs to a request.
//
// Called with c.mu held.
func (c *consumer) removeEchoLocked(requestID string) bool {
	for i, e := range c.echoes {
		if e.requestID == requestID {
			c.echoes = append(c.echoes[:i], c.echoes[i+1:]...)
			return true
		}
	}
	return false
}

// dropEchoes discards every outstanding receipt, reporting how many went.
// Called when the conversation's replay floor rises: a clear or a compaction
// discards the history below it, and a receipt for a prompt from below that
// line has been discarded along with the prompt itself. Replaying it would put
// pre-clear text back above a floor that exists to hide exactly that.
func (c *consumer) dropEchoes() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	n := len(c.echoes)
	c.echoes = nil
	return n
}

// echoClaim is the verdict of one attempt to correlate a transcript line with
// an outstanding receipt.
//
// requestID is non-empty EXACTLY when the line was attributed. `refused` says
// the queue was not empty but its oldest entry disagreed with the line's text,
// which is a different fact from "nothing was outstanding" and gets its own
// loud log: the first means the ordering invariant this queue rests on has been
// broken somewhere, the second is the ordinary resumed-history case.
// The refused entry is COPIED OUT rather than left to be re-read by the log
// line: c.echoes is guarded by c.mu, and reaching back into it after the
// unlock to name the receipt would be an unsynchronized read of live state.
type echoClaim struct {
	requestID        string
	refused          bool
	refusedRequestID string
	refusedText      string
}

// claimOldestEcho removes and returns the oldest unclaimed receipt, provided
// its text matches the arriving line's.
//
// OLDEST-FIRST is the conservative correlation, and the only one available: a
// transcript UserLine carries no request id of its own (that field is empty on
// every line the file plane produces), so the sole fact relating it to a submit
// is that the daemon sent that submit and has not yet seen its line. Prompts
// reach one session's shim in submit order and come back in the same order —
// which is true BY CONSTRUCTION rather than by hope, because the reservation of
// each slot and the submit that fills it happen under one session-scoped lock
// (reserveEcho, forwardPrompt).
//
// THE TEXT COMPARISON IS A SAFETY NET AND NOT THE MECHANISM, and it cannot be
// the mechanism: prompts like "continue" and "yes" repeat constantly in this
// workflow, so content cannot tell two real submits apart. What it CAN do is
// catch the positional scheme having gone wrong — which, before the submit lock
// existed, silently stamped one prompt's line with another prompt's request id
// and swapped two identities downstream. Refusing to attribute converts that
// silent misattribution into a visible non-attribution plus a loud log, which
// is the strictly safer of the two ways to be wrong: the line simply keeps its
// own record identity, exactly as resumed history does.
//
// Compared after trimming surrounding whitespace, and on nothing else: the
// prompt the daemon submitted and the line the transcript wrote for it are the
// same string, so any real difference is news.
func (c *consumer) claimOldestEcho(lineText string) echoClaim {
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.echoes) == 0 {
		return echoClaim{}
	}
	e := c.echoes[0]
	if strings.TrimSpace(e.text) != strings.TrimSpace(lineText) {
		return echoClaim{refused: true, refusedRequestID: e.requestID, refusedText: e.text}
	}
	c.echoes = c.echoes[1:]
	return echoClaim{requestID: e.requestID}
}

// attributeUserTurn stamps a LIVE durable user turn with the request id of the
// submit it answers, and retires that submit's receipt.
//
// This is the attribution the daemon alone can make. The frontend sees two
// deliveries of one prompt — the daemon's receipt (request id, no seq) and the
// transcript's line (seq, no request id) — and nothing in either says they are
// the same prompt. Stamping the line makes them share the identity the frontend
// already reconciles on, so the second REPLACES the first instead of drawing a
// second work of the same text.
//
// A line that matches no outstanding receipt is left UNATTRIBUTED: resumed
// history, a prompt submitted through some other client, or a submit this
// daemon never made. Inventing a correlation there would attach a user's prompt
// to a request that has nothing to do with it.
func (c *consumer) attributeUserTurn(cd *frontendv1.ConversationDelta) {
	for _, it := range cd.GetMessages() {
		if !isPromptUserMessage(it) {
			// A line that already NAMES its request needs no stamp, but it is
			// still this receipt's durable successor: retire the receipt so it
			// stops being replayed beside the line that superseded it.
			if id := it.GetRequestId(); id != "" && it.GetUserMessage() != nil && c.claimEcho(id) {
				c.logf("session-controller: prompt receipt superseded ws=%q session=%s request_id=%s — the durable line arrived carrying the id itself",
					c.workspace, c.sessionID, id)
				c.retireDurableReceipt(id, "durable_line_named_the_request")
			}
			continue
		}
		claim := c.claimOldestEcho(userMessageText(it.GetUserMessage()))
		if claim.refused {
			// LOUD, because this is the ordering invariant reporting itself
			// broken. The oldest outstanding receipt is by construction the one
			// this line answers, so a disagreement means either a submit
			// reached the SDK out of the order its slot was reserved in — which
			// the submit lock is supposed to make impossible — or the text the
			// transcript wrote is not the text the daemon submitted. The line
			// keeps its own identity and the receipt keeps its slot; nothing is
			// stamped with a request it may not answer.
			c.logf("session-controller: user turn ATTRIBUTION REFUSED ws=%q session=%s uuid=%s receipt_request_id=%s receipt_len=%d line_len=%d — the oldest outstanding receipt's text disagrees with this line's, so the positional correlation is not trustworthy here and the line is left unattributed",
				c.workspace, c.sessionID, it.GetUuid(), claim.refusedRequestID, len(claim.refusedText),
				len(userMessageText(it.GetUserMessage())))
			continue
		}
		if claim.requestID == "" {
			c.logf("session-controller: user turn UNATTRIBUTED ws=%q session=%s uuid=%s — no submit of this daemon's is outstanding for it",
				c.workspace, c.sessionID, it.GetUuid())
			continue
		}
		requestID := claim.requestID
		it.RequestId = requestID
		c.logf("session-controller: user turn attributed ws=%q session=%s uuid=%s request_id=%s (the receipt it supersedes is retired)",
			c.workspace, c.sessionID, it.GetUuid(), requestID)
		c.retireDurableReceipt(requestID, "durable_line_attributed_to_the_request")
	}
}

// retireDurableReceipt discards one request's DURABLE receipt record.
//
// THE RETIREMENT POINT, AND WHY IT IS HERE. attributeUserTurn is the single
// place the daemon establishes that a durable transcript line IS a given
// submit's prompt — by the line naming the request id itself, or by the
// oldest-outstanding correlation the durable line carries no id for. Every
// in-memory receipt already retires exactly here, so hanging the durable
// retirement off the same decision means the two records cannot disagree about
// which prompts the conversation now holds. Deriving the fact a second time
// somewhere else would be a second implementation of the correlation, and
// correlations that exist twice drift.
//
// IT IS NOT THE ONLY ONE, and it cannot be. A daemon that dies between the
// accept and the transcript line is never present for this decision, so the
// record it left behind is retired instead by the durable replay that finds the
// prompt already in the store (durablereplay.go). That path and this one are
// the same statement — the conversation carries the prompt now — observed from
// the two places the daemon can observe it from.
//
// IDEMPOTENT BY CONSTRUCTION: retiring a receipt that is already gone reports
// false with no error, so a replay-retired record reaching this path (or the
// reverse) is ordinary rather than an anomaly.
//
// A failure is loud-logged and swallowed: the caller is delivering the
// conversation, and a bookkeeping row that would not delete is not a reason to
// stop doing that. The cost of the failure is bounded and self-correcting — the
// receipt is re-served on a later replay and suppressed there instead.
func (c *consumer) retireDurableReceipt(requestID, reason string) {
	if c.receipts == nil {
		c.logf("session-controller: durable prompt receipt NOT retired ws=%q session=%s request_id=%s reason=%s — no durable receipt store is wired to this session controller",
			c.workspace, c.sessionID, requestID, reason)
		return
	}
	retired, err := c.receipts.Retire(requestID)
	if err != nil {
		c.logf("session-controller: durable prompt receipt retirement FAILED ws=%q session=%s request_id=%s reason=%s: %v (it will be re-served by a later replay and suppressed there)",
			c.workspace, c.sessionID, requestID, reason, err)
		return
	}
	c.logf("session-controller: durable prompt receipt retirement ws=%q session=%s request_id=%s reason=%s row_deleted=%v",
		c.workspace, c.sessionID, requestID, reason, retired)
}

// retireDurableReceiptsThrough discards every durable receipt for this
// workspace accepted at or before throughMs, and reports how many went.
//
// It is dropEchoes's durable twin, called from the same context cut: a clear or
// a compaction discards the history below it, and a receipt for a prompt from
// below that line would otherwise replay pre-cut text above a floor that exists
// to hide exactly that.
func (c *consumer) retireDurableReceiptsThrough(throughMs int64, reason string) {
	if c.receipts == nil {
		c.logf("session-controller: durable prompt receipts NOT retired ws=%q session=%s through_ms=%d reason=%s — no durable receipt store is wired to this session controller",
			c.workspace, c.sessionID, throughMs, reason)
		return
	}
	n, err := c.receipts.RetireWorkspace(c.workspace, throughMs)
	if err != nil {
		c.logf("session-controller: durable prompt receipt sweep FAILED ws=%q session=%s through_ms=%d reason=%s: %v (a receipt from below the new replay floor may still be served)",
			c.workspace, c.sessionID, throughMs, reason, err)
		return
	}
	c.logf("session-controller: durable prompt receipt sweep ws=%q session=%s through_ms=%d reason=%s rows_deleted=%d",
		c.workspace, c.sessionID, throughMs, reason, n)
}

// userMessageText is the prompt text a user_message carries, across both of the
// arms it can carry text in, with no separator between blocks. Empty means the
// message carries no prompt at all — a pure tool-result feedback message rides
// the user_message arm too.
func userMessageText(um *datav1.ApiUserMessage) string {
	switch content := um.GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		return content.ContentString
	case *datav1.ApiUserMessage_ContentBlocks:
		var sb strings.Builder
		for _, b := range content.ContentBlocks.GetBlocks() {
			if t := b.GetText(); t != nil {
				sb.WriteString(t.GetText())
			}
		}
		return sb.String()
	}
	return ""
}

// isPromptUserMessage reports whether an item is a user PROMPT still lacking a
// request id.
//
// Text-bearing only: a pure tool-result feedback message rides the user_message
// arm too, and claiming a receipt for one would attribute a user's prompt to
// the wrong line entirely. An item that already carries a request id is left
// alone — something upstream already knows what it answers.
func isPromptUserMessage(it *frontendv1.Message) bool {
	if it.GetRequestId() != "" {
		return false
	}
	um := it.GetUserMessage()
	if um == nil {
		return false
	}
	return userMessageText(um) != ""
}
