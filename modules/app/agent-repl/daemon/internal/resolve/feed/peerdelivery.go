package feed

import (
	"context"
	"errors"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE TWO ENDS OF A CROSS-SESSION MESSAGE, joined on its delivery id.
//
// A message one Claude session sends another is two rows on two feeds: the
// SENDER's agent prompt (sendmessage.go) and the RECIPIENT's peer bubble
// (peer.go). Neither record names the other end as an agent-repl workspace: the
// send names the address the caller wrote, and the peer message the session
// name the sender asserted. The one value both records carry is the delivery id
// the sending vendor minted (conversation.v1.PeerDeliveryId), so each row's
// header names the workspace at the other end by joining on it.
//
// THE JOIN HAS TWO SOURCES, and they agree by construction because both read
// the same records. A row this daemon draws IS one end: drawing the send in
// workspace A states that A sent delivery D, and drawing the peer bubble in B
// states that B received it. A row whose other end this daemon has not drawn
// asks the host's store, through a shim (PeerDeliverySource), which answers
// each end as the session holding its record; a session is named as a
// workspace through the main agent the session watcher named for it.
//
// EVERY ROW CONVERGES IN PLACE. A row drawn before its other end is known is
// drawn unresolved and remembered against its delivery; the moment the other
// end becomes known (its row is drawn, the store answers, or the holding
// workspace's main agent is named) every remembered row is drawn again under
// its own FeedId, so only its header changes. Nothing waits on a timer.

// PeerDeliverySource answers which sessions hold the two ends of one
// cross-session message (shim.v1.ResolvePeerDelivery), read through WS's shim.
// ErrNoHistorySource means WS has no shim to ask.
type PeerDeliverySource interface {
	ResolvePeerDelivery(ctx context.Context, ws ids.WorkspaceID, deliveryID string) (PeerDeliveryEnds, error)
}

// PeerDeliveryEnds is each end of a delivery as the store answered it: the main
// agent id of the session holding that end's record, "" when no recorded
// session holds it.
type PeerDeliveryEnds struct {
	Sender    string
	Recipient string
}

// peerDeliveryLookupBound bounds one store lookup. A lookup is one indexed read
// through one shim round trip.
const peerDeliveryLookupBound = 10 * time.Second

// deliveryEnd names one end of a delivery.
type deliveryEnd int

const (
	deliverySender deliveryEnd = iota
	deliveryRecipient
)

func (e deliveryEnd) String() string {
	if e == deliverySender {
		return "sender"
	}
	return "recipient"
}

// peerDelivery is everything known about one delivery: each end's workspace as
// a row drawn here stated it, each end's session as the store answered it, the
// store lookup's progress, and the rows waiting to be drawn again when an end
// becomes known.
type peerDelivery struct {
	// workspace is each end's workspace, stated by a row of that end drawn by
	// this daemon. Empty until one is.
	workspace [2]ids.WorkspaceID
	// session is each end's holding session (its main agent id) as the store
	// answered it. Empty until it answers one.
	session [2]string
	// asked is set once a store lookup was started for this delivery, so one is
	// never started twice. A lookup that failed clears it, so the next draw
	// that needs it asks again.
	asked bool
	// sends are the send units (the sender's end) drawn with this delivery, by
	// workspace and unit id.
	sends map[deliveryRowRef]bool
	// peers are the peer messages (the recipient's end) drawn with this
	// delivery, by workspace and peer message id.
	peers map[deliveryRowRef]bool
}

// deliveryRowRef names one row of one end: its workspace and the unit or peer
// message id it is drawn from.
type deliveryRowRef struct {
	ws ids.WorkspaceID
	id string
}

// delivery answers the record of delivery ID, creating it on first sight.
// Called with r.mu held.
func (r *resolver) delivery(id string) *peerDelivery {
	d, ok := r.deliveries[id]
	if !ok {
		d = &peerDelivery{sends: map[deliveryRowRef]bool{}, peers: map[deliveryRowRef]bool{}}
		r.deliveries[id] = d
	}
	return d
}

// noteDeliveryEnd records that WS holds END of delivery ID, because a row of
// that end is being drawn there, and draws again every row of the OTHER end
// that was waiting to name it. Called with r.mu held.
func (r *resolver) noteDeliveryEnd(id string, end deliveryEnd, ws ids.WorkspaceID, row string) {
	d := r.delivery(id)
	if end == deliverySender {
		d.sends[deliveryRowRef{ws: ws, id: row}] = true
	} else {
		d.peers[deliveryRowRef{ws: ws, id: row}] = true
	}
	if d.workspace[end] == ws {
		return
	}
	if d.workspace[end] != "" {
		// ONE MESSAGE HAS ONE SENDER AND ONE RECIPIENT: a second workspace
		// drawing the same end is a producer defect, and the first stays.
		r.logger(ws).Error("daemon.feed.peer_delivery_end_conflict",
			"two workspaces drew the same end of one cross-session message; the first one drawn is kept",
			dlog.Context{"delivery_id": id, "end": end.String(), "kept_workspace": string(d.workspace[end]), "workspace": string(ws)})
		return
	}
	d.workspace[end] = ws
	r.logger(ws).Debug("daemon.feed.peer_delivery_end",
		"a row drawn here is one end of a cross-session message; the rows naming it are drawn again",
		dlog.Context{"delivery_id": id, "end": end.String()})
	// ONLY THE OTHER END'S ROWS NAME THIS ONE, so only they are drawn again:
	// the row being drawn now composes its own header from what is known.
	r.redrawDelivery(d, end == deliveryRecipient, end == deliverySender)
}

// deliveryWorkspaceName names the workspace holding END of delivery ID for a
// row of the other end drawn in FROM, or reports none known. An end not known
// either way asks the store once, through FROM's shim. Called with r.mu held.
func (r *resolver) deliveryWorkspaceName(id string, end deliveryEnd, from ids.WorkspaceID) (string, bool) {
	d := r.delivery(id)
	ws := d.workspace[end]
	if ws == "" && d.session[end] != "" {
		ws = r.workspaceOfSession(d.session[end])
	}
	if ws == "" {
		r.askDelivery(id, d, from)
		return "", false
	}
	return r.workspaceName(ws, from)
}

// workspaceOfSession is the workspace whose main agent is SESSION, "" when the
// session watcher has named it for none. Called with r.mu held.
func (r *resolver) workspaceOfSession(session string) ids.WorkspaceID {
	for ws, s := range r.workspaces {
		if s.mainAgent == session {
			return ws
		}
	}
	return ""
}

// workspaceName is WS's display name, the one the sidebar draws: the other end
// of a cross-session message, or the workspace that sent an agent-sent prompt
// (turnsender.go). A name that cannot be read is recorded against FROM, the
// workspace drawing the row, and the row draws that end unresolved. Called
// with r.mu held.
func (r *resolver) workspaceName(ws, from ids.WorkspaceID) (string, bool) {
	if r.deps.WorkspaceName == nil {
		return "", false
	}
	name, err := r.deps.WorkspaceName(ws)
	if err != nil {
		r.logger(from).Error("daemon.feed.peer_workspace_name_failed",
			"the workspace at the other end of an agent-to-agent message could not be named; its row draws that end unresolved",
			dlog.Context{"other_workspace": string(ws), "cause": err.Error()})
		return "", false
	}
	if name == "" {
		r.logger(from).Error("daemon.feed.peer_workspace_name_empty",
			"the workspace at the other end of an agent-to-agent message has an empty name; its row draws that end unresolved",
			dlog.Context{"other_workspace": string(ws)})
		return "", false
	}
	return name, true
}

// askDelivery starts the one store lookup for delivery ID, through FROM's shim,
// unless one was already started. The read runs OFF the caller's goroutine: the
// caller holds r.mu, and the read reaches out to a shim. Called with r.mu held.
func (r *resolver) askDelivery(id string, d *peerDelivery, from ids.WorkspaceID) {
	if d.asked || r.deps.PeerDeliveries == nil {
		return
	}
	d.asked = true
	r.logger(from).Debug("daemon.feed.peer_delivery_lookup",
		"an end of a cross-session message is unknown here; the store is asked which session holds it",
		dlog.Context{"delivery_id": id})
	r.deps.Go(func() {
		ctx, cancel := context.WithTimeout(context.Background(), peerDeliveryLookupBound)
		defer cancel()
		ends, err := r.deps.PeerDeliveries.ResolvePeerDelivery(ctx, from, id)
		r.answerDelivery(id, from, ends, err)
	})
}

// answerDelivery takes the store's answer for delivery ID and draws again every
// row it names an end for. A lookup that failed is recorded once, here, where
// its consequence is decided: the rows stay unresolved, and the next row of the
// delivery drawn asks again.
func (r *resolver) answerDelivery(id string, from ids.WorkspaceID, ends PeerDeliveryEnds, err error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	d := r.delivery(id)
	if errors.Is(err, ErrNoHistorySource) {
		// NO SHIM TO ASK IS NO FAILURE: the workspace's shim is not up (a
		// hibernated workspace). The rows stay unresolved, and the next row of
		// the delivery drawn asks again.
		d.asked = false
		r.logger(from).Info("daemon.feed.peer_delivery_lookup_no_source",
			"no shim was up to ask which sessions hold a cross-session message; its rows stay unresolved until one is drawn again",
			dlog.Context{"delivery_id": id, "cause": err.Error()})
		return
	}
	if err != nil {
		d.asked = false
		r.logger(from).Error("daemon.feed.peer_delivery_lookup_failed",
			"the store could not say which sessions hold a cross-session message; its rows stay unresolved until one is drawn again",
			dlog.Context{"delivery_id": id, "cause": err.Error()})
		return
	}
	d.session[deliverySender] = ends.Sender
	d.session[deliveryRecipient] = ends.Recipient
	r.logger(from).Debug("daemon.feed.peer_delivery_answered",
		"the store named the sessions holding a cross-session message; its rows are drawn again",
		dlog.Context{"delivery_id": id, "sender_session": ends.Sender, "recipient_session": ends.Recipient})
	r.redrawDelivery(d, true, true)
}

// redrawDeliveriesOfSession draws again every row of every delivery an end of
// which is held by SESSION, now that the session watcher has named the
// workspace whose main agent it is. Called with r.mu held.
func (r *resolver) redrawDeliveriesOfSession(session string) {
	for _, d := range r.deliveries {
		r.redrawDelivery(d, d.session[deliveryRecipient] == session, d.session[deliverySender] == session)
	}
}

// redrawDelivery draws again the rows remembered against delivery D, each
// under its own FeedId, so the only change a reader sees is the other end's
// name filling in: the SENDS when the recipient's end became known, the PEER
// bubbles when the sender's did. Called with r.mu held.
func (r *resolver) redrawDelivery(d *peerDelivery, sends, peers bool) {
	if sends {
		for ref := range d.sends {
			if s, ok := r.workspaces[ref.ws]; ok {
				r.redrawSendAddress(s, ref.id)
			}
		}
	}
	if !peers {
		return
	}
	for ref := range d.peers {
		if s, ok := r.workspaces[ref.ws]; ok {
			if peer, ok := s.peerMessages[ref.id]; ok {
				r.drawPeerMessage(s, peer)
			}
		}
	}
}

// peerFrom composes a received message's header from what the peer message
// states and what the join knows, drawn in WS.
//
//  1. The sending WORKSPACE, by the delivery-id join.
//  2. Otherwise UNRESOLVED, named by the session name the sender asserted.
//  3. Otherwise UNNAMED, under the daemon's stand-in.
func (r *resolver) peerFrom(ws ids.WorkspaceID, inter *conversationv1.PeerMessageInterSession) *frontendv1.FeedPeerMessageFrom {
	if id := inter.GetDeliveryId().GetValue(); id != "" {
		if name, ok := r.deliveryWorkspaceName(id, deliverySender, ws); ok {
			return &frontendv1.FeedPeerMessageFrom{
				Name:   &frontendv1.FeedPeerMessageFromName{Text: name},
				Sender: &frontendv1.FeedPeerMessageFrom_Workspace{Workspace: &frontendv1.FeedPeerMessageFromWorkspace{}},
			}
		}
	}
	if name := inter.GetSenderSessionName().GetText(); name != "" {
		return &frontendv1.FeedPeerMessageFrom{
			Name:   &frontendv1.FeedPeerMessageFromName{Text: name},
			Sender: &frontendv1.FeedPeerMessageFrom_Unresolved{Unresolved: &frontendv1.FeedPeerMessageFromUnresolved{}},
		}
	}
	return &frontendv1.FeedPeerMessageFrom{
		Name:   &frontendv1.FeedPeerMessageFromName{Text: unnamedPeerSender},
		Sender: &frontendv1.FeedPeerMessageFrom_Unnamed{Unnamed: &frontendv1.FeedPeerMessageFromUnnamed{}},
	}
}

// unnamedPeerSender is the stand-in for a sender no workspace is known for and
// that stated no name. An identity is never invented to fill it.
const unnamedPeerSender = "another session"

// peerSessionAddress composes a cross-session send's header, drawn in WS: the
// recipient WORKSPACE by the delivery-id join, or UNRESOLVED under the address
// the caller wrote.
func (r *resolver) peerSessionAddress(ws ids.WorkspaceID, deliveryID, addressedTo string) *frontendv1.FeedAgentPromptAddress {
	if name, ok := r.deliveryWorkspaceName(deliveryID, deliveryRecipient, ws); ok {
		return sentToPeerSession(name, &frontendv1.FeedAgentPromptAddressPeerSession{
			Recipient: &frontendv1.FeedAgentPromptAddressPeerSession_Workspace{Workspace: &frontendv1.FeedAgentPromptAddressPeerSessionWorkspace{}},
		})
	}
	return sentToPeerSession(nameOr(addressedTo), &frontendv1.FeedAgentPromptAddressPeerSession{
		Recipient: &frontendv1.FeedAgentPromptAddressPeerSession_Unresolved{Unresolved: &frontendv1.FeedAgentPromptAddressPeerSessionUnresolved{}},
	})
}

// nameOr is the address the caller wrote, or the honest admission that it wrote
// none.
func nameOr(addressedTo string) string {
	if addressedTo == "" {
		return sendNotNamed
	}
	return addressedTo
}
