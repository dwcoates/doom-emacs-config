package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ② THE PEER MESSAGE ROW. A message another Claude session sent into this
// conversation — an inter-session peer message, drawn as a bubble, or a
// subagent hand-back, drawn as a badge (see drawPeerMessage). It is
// NOT a prompt (never a person's words) and NOT this agent's own work (never a
// turn's response or terminal), so it is neither drawn on the prompt path nor
// filed as an answer: it is its own low-priority row, the abbreviated purple
// bubble the client expands on demand.

// drawPeerMessage draws the one row a peer message produces, on the recipient's
// feed. It opens no turn and stamps none: a peer message drives no response of
// its own, so unlike a delivered prompt it never becomes the turn-in-flight.
func (r *resolver) drawPeerMessage(s *wsState, peer *conversationv1.PeerMessage) {
	log := r.logger(s.id)
	recipient := peer.GetAgent()
	at, ok := r.place(s, recipient)
	if !ok {
		return
	}
	// THE IDENTITY IS THE MESSAGE'S OWN STABLE id (the vendor record uuid),
	// which BOTH planes spell identically — so the live row and the adopted row
	// for one message resolve to one FeedId and upsert in place rather than
	// drawing two bubbles.
	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPeer, ID: peer.GetId()}),
	}
	// A SUBAGENT'S HAND-BACK IS A BADGE, NOT A BUBBLE: its report is the
	// subagent's result and is drawn once, in the subagent's own card, so the
	// main feed only marks where it arrived. The row id is the SAME one a peer
	// bubble takes, so a live and an adopted delivery still upsert one row.
	// Inter-session and UNSET (a producer that stated no kind) draw the bubble.
	if peer.GetSubagentHandback() != nil {
		row.Row = &frontendv1.FeedRow_SubagentHandback{SubagentHandback: &frontendv1.FeedSubagentHandbackBadge{
			Label: &frontendv1.FeedSubagentHandbackBadgeLabel{Text: handbackLabel(peer.GetSender())},
		}}
		log.Debug("daemon.feed.peer_message",
			"a subagent's hand-back was drawn as a badge; its report belongs to the subagent's card",
			dlog.Context{"peer_id": peer.GetId(), "sender": peer.GetSender(), "agent": recipient.GetValue(), "kind": "subagent_handback"})
		r.upsert(s, at, row, true)
		return
	}
	row.Row = &frontendv1.FeedRow_PeerMessage{PeerMessage: &frontendv1.FeedPeerMessage{
		Sender: peerLabel(peer.GetSender()),
		Body:   peer.GetBody(),
	}}
	log.Debug("daemon.feed.peer_message",
		"a message from another Claude session was drawn as a peer bubble",
		dlog.Context{"peer_id": peer.GetId(), "sender": peer.GetSender(), "agent": recipient.GetValue(), "kind": peerKindName(peer)})
	r.upsert(s, at, row, true)
}

// handbackLabel composes the hand-back badge's text from the sender, in the
// same wording the peer bubble names a sender with.
func handbackLabel(sender string) string {
	return peerLabel(sender) + " reported back"
}

// peerKindName names the kind a bubble-drawn peer message stated, for the log.
func peerKindName(peer *conversationv1.PeerMessage) string {
	if peer.GetInterSession() != nil {
		return "inter_session"
	}
	return "unset"
}

// peerLabel composes the collapsed bubble's label from the sender id/name. The
// daemon owns the wording ("agent <sender>"); the client draws it verbatim. An
// empty sender — a producer that stated none — falls back to a plain "another
// agent" rather than an empty label.
func peerLabel(sender string) string {
	if sender == "" {
		return "another agent"
	}
	return "agent " + sender
}
