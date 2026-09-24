package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ② THE PEER MESSAGE ROW. A message another Claude session sent into this
// conversation — an inter-session peer message or a subagent hand-back. It is
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
		Row: &frontendv1.FeedRow_PeerMessage{PeerMessage: &frontendv1.FeedPeerMessage{
			Sender: peerLabel(peer.GetSender()),
			Body:   peer.GetBody(),
		}},
	}
	log.Debug("daemon.feed.peer_message",
		"a message from another Claude session was drawn as a peer bubble",
		dlog.Context{"peer_id": peer.GetId(), "sender": peer.GetSender(), "agent": recipient.GetValue()})
	r.upsert(s, at, row, true)
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
