package sessioncontroller

import (
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// promptEcho is ONE prompt the shim accepted, and the bubble the daemon pushed
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
	item      *frontendv1.ConversationItem
}

// echoUUID is the item identity a receipt is pushed under. Derived from the
// request id so a resync re-push REPLACES the standing bubble rather than
// adding a second one; the frontend keys on the request id first regardless, so
// this only has to be stable.
func echoUUID(requestID string) string { return "prompt-echo:" + requestID }

// pushUserEcho pushes the prompt bubble for one accepted submit and retains it
// until the durable transcript line claims it. Its caller must first complete
// the synchronous accepted-prompt state barrier; this function deliberately
// owns only the receipt so the ordering remains explicit at forwardPrompt.
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
// and the two reconcile onto one bubble in the frontend. Keeping the receipt
// past that point would mean replaying a daemon-local duplicate of something
// the conversation already holds. A receipt the transcript NEVER claims (the
// shim died mid-submit) is retained indefinitely, which is right: it is then
// the only evidence the prompt was ever sent.
func (c *consumer) pushUserEcho(requestID, text string) {
	item := &frontendv1.ConversationItem{
		Uuid:      echoUUID(requestID),
		TsMs:      c.now(),
		RequestId: requestID,
		Item: &frontendv1.ConversationItem_UserMessage{UserMessage: &datav1.ApiUserMessage{
			Content: &datav1.ApiUserMessage_ContentString{ContentString: text},
		}},
	}
	c.mu.Lock()
	c.echoes = append(c.echoes, &promptEcho{requestID: requestID, text: text, item: item})
	pending := len(c.echoes)
	c.mu.Unlock()
	c.logf("session-controller: prompt echo pushed ws=%q session=%s request_id=%s len=%d unclaimed=%d",
		c.workspace, c.sessionID, requestID, len(text), pending)
	c.pushLocalItem(item)
}

// echo pushes a session controller's prompt receipt, if the submit has an identity to key
// one on. A caller with no request id behind it (a test harness, an internal
// re-submit) pushes nothing rather than minting an id the frontend has no way
// to correlate — the durable transcript line still draws the prompt.
//
// Must be called with m.mu RELEASED: the push reaches the frontend server.
func (m *Manager) echo(d *sessionController, requestID, text string) {
	if requestID == "" {
		return
	}
	d.consumer.pushUserEcho(requestID, text)
}

// snapshotEchoes returns the unclaimed receipts in submit order, for replay.
func (c *consumer) snapshotEchoes() []*frontendv1.ConversationItem {
	c.mu.Lock()
	defer c.mu.Unlock()
	out := make([]*frontendv1.ConversationItem, 0, len(c.echoes))
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

// claimOldestEcho removes and returns the oldest unclaimed receipt, or "" when
// none is outstanding.
//
// OLDEST-FIRST is the conservative correlation, and the only one available: a
// transcript UserLine carries no request id of its own (that field is empty on
// every line the file plane produces), so the sole fact relating it to a submit
// is that the daemon sent that submit and has not yet seen its line. Prompts
// reach one session's shim in submit order and come back in the same order, so
// the oldest outstanding receipt is the one this line answers.
func (c *consumer) claimOldestEcho() string {
	c.mu.Lock()
	defer c.mu.Unlock()
	if len(c.echoes) == 0 {
		return ""
	}
	e := c.echoes[0]
	c.echoes = c.echoes[1:]
	return e.requestID
}

// attributeUserTurn stamps a LIVE durable user turn with the request id of the
// submit it answers, and retires that submit's receipt.
//
// This is the attribution the daemon alone can make. The frontend sees two
// deliveries of one prompt — the daemon's receipt (request id, no seq) and the
// transcript's line (seq, no request id) — and nothing in either says they are
// the same prompt. Stamping the line makes them share the identity the frontend
// already reconciles on, so the second REPLACES the first instead of drawing a
// second bubble of the same text.
//
// A line that matches no outstanding receipt is left UNATTRIBUTED: resumed
// history, a prompt submitted through some other client, or a submit this
// daemon never made. Inventing a correlation there would attach a user's prompt
// to a request that has nothing to do with it.
func (c *consumer) attributeUserTurn(cd *frontendv1.ConversationDelta) {
	for _, it := range cd.GetItems() {
		if !isPromptUserMessage(it) {
			// A line that already NAMES its request needs no stamp, but it is
			// still this receipt's durable successor: retire the receipt so it
			// stops being replayed beside the line that superseded it.
			if id := it.GetRequestId(); id != "" && it.GetUserMessage() != nil && c.claimEcho(id) {
				c.logf("session-controller: prompt receipt superseded ws=%q session=%s request_id=%s — the durable line arrived carrying the id itself",
					c.workspace, c.sessionID, id)
			}
			continue
		}
		requestID := c.claimOldestEcho()
		if requestID == "" {
			c.logf("session-controller: user turn UNATTRIBUTED ws=%q session=%s uuid=%s — no submit of this daemon's is outstanding for it",
				c.workspace, c.sessionID, it.GetUuid())
			continue
		}
		it.RequestId = requestID
		c.logf("session-controller: user turn attributed ws=%q session=%s uuid=%s request_id=%s (the receipt it supersedes is retired)",
			c.workspace, c.sessionID, it.GetUuid(), requestID)
	}
}

// isPromptUserMessage reports whether an item is a user PROMPT still lacking a
// request id.
//
// Text-bearing only: a pure tool-result feedback message rides the user_message
// arm too, and claiming a receipt for one would attribute a user's prompt to
// the wrong line entirely. An item that already carries a request id is left
// alone — something upstream already knows what it answers.
func isPromptUserMessage(it *frontendv1.ConversationItem) bool {
	if it.GetRequestId() != "" {
		return false
	}
	um := it.GetUserMessage()
	if um == nil {
		return false
	}
	switch content := um.GetContent().(type) {
	case *datav1.ApiUserMessage_ContentString:
		return content.ContentString != ""
	case *datav1.ApiUserMessage_ContentBlocks:
		for _, b := range content.ContentBlocks.GetBlocks() {
			if t := b.GetText(); t.GetText() != "" {
				return true
			}
		}
	}
	return false
}
