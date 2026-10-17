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

// A MESSAGE FROM AN AGENT OF THIS SESSION (a subagent, to its lead or to a
// sibling) is headed by its sender: by the requested name its spawn gave it,
// or by its identifier when it has none (owner spec, 2026-10-17). The record
// names the sender only by the vendor's task id for it, its LOCATOR, so the
// header asks the record which agent the locator names
// (VendorTaskSource), draws the bubble with the task id until it answers, and
// draws it again in place once it has.

// VendorTaskSource answers which agent of a session one vendor task id names
// (shim.v1.ResolveVendorTask), read through WS's shim within the lineage of
// SESSION, the session's main agent. ErrNoHistorySource means WS has no shim
// to ask.
type VendorTaskSource interface {
	ResolveVendorTask(ctx context.Context, ws ids.WorkspaceID, session, vendorTaskID string) (VendorTaskAgent, error)
}

// VendorTaskAgent is the agent a vendor task id names: its id, and the name its
// spawn gave it. Agent is "" when no agent of the session is paired with the
// task id yet; RequestedName is "" when the agent was spawned unnamed.
type VendorTaskAgent struct {
	Agent         string
	RequestedName string
}

// vendorTaskLookupBound bounds one task lookup: one indexed read through one
// shim round trip.
const vendorTaskLookupBound = 10 * time.Second

// vendorTask is what one workspace knows of one vendor task id: the agent the
// record pairs it with, once answered, and the peer bubbles waiting to name it.
type vendorTask struct {
	// agent is the answer, valid once known is set.
	agent VendorTaskAgent
	known bool
	// asked is set once a lookup was started, so one is never started twice;
	// a lookup that failed, or found no pairing yet, clears it so the next
	// draw asks again.
	asked bool
	// peers are the peer messages headed by this sender, by id.
	peers map[string]bool
}

// vendorTaskOf answers WS's record of task ID, creating it on first sight.
// Called with r.mu held.
func (s *wsState) vendorTaskOf(id string) *vendorTask {
	t, ok := s.vendorTasks[id]
	if !ok {
		t = &vendorTask{peers: map[string]bool{}}
		s.vendorTasks[id] = t
	}
	return t
}

// peerFromAgent composes the header of a message an agent of this session
// sent, drawn in S as peer message PEERID. Called with r.mu held.
func (r *resolver) peerFromAgent(s *wsState, peerID string, in *conversationv1.PeerMessageInSession) *frontendv1.FeedPeerMessageFrom {
	taskID := in.GetSenderTask().GetVendorTaskId()
	if taskID == "" {
		// THE PRODUCER'S INVARIANT: an in-session message names its sender's
		// task. Drawn as the unnamed stand-in, and recorded, rather than
		// inventing a sender.
		r.logger(s.id).Error("daemon.feed.peer_in_session_no_task",
			"a message from an agent of this session named no sender task; it is drawn with no sender",
			dlog.Context{"peer_id": peerID})
		return &frontendv1.FeedPeerMessageFrom{
			Name:   &frontendv1.FeedPeerMessageFromName{Text: unnamedPeerSender},
			Sender: &frontendv1.FeedPeerMessageFrom_Unnamed{Unnamed: &frontendv1.FeedPeerMessageFromUnnamed{}},
		}
	}
	t := s.vendorTaskOf(taskID)
	t.peers[peerID] = true
	if !t.known {
		r.askVendorTask(s, taskID, t)
		return fromAgent(taskID, &frontendv1.FeedPeerMessageFromAgent{
			Identity: &frontendv1.FeedPeerMessageFromAgent_Unresolved{Unresolved: &frontendv1.FeedPeerMessageFromAgentUnresolved{}},
		})
	}
	name := t.agent.RequestedName
	if name == "" {
		// THE SPAWN THIS FEED DREW may know a name the store's record of the
		// commission does not.
		if end := r.agentNamed(s, t.agent.Agent); end.identity.GetNamed() != nil {
			name = end.name
		}
	}
	if name != "" {
		return fromAgent(name, &frontendv1.FeedPeerMessageFromAgent{
			Identity: &frontendv1.FeedPeerMessageFromAgent_Named{Named: &frontendv1.FeedPeerMessageFromAgentNamed{}},
		})
	}
	return fromAgent(t.agent.Agent, &frontendv1.FeedPeerMessageFromAgent{
		Identity: &frontendv1.FeedPeerMessageFromAgent_Identified{Identified: &frontendv1.FeedPeerMessageFromAgentIdentified{}},
	})
}

// fromAgent is a header naming NAME as the agent of this session AGENT says.
func fromAgent(name string, agent *frontendv1.FeedPeerMessageFromAgent) *frontendv1.FeedPeerMessageFrom {
	return &frontendv1.FeedPeerMessageFrom{
		Name:   &frontendv1.FeedPeerMessageFromName{Text: name},
		Sender: &frontendv1.FeedPeerMessageFrom_Agent{Agent: agent},
	}
}

// askVendorTask starts the one lookup of task ID for S, through S's own shim
// and within S's lineage, unless one is under way. Called with r.mu held.
//
// S'S MAIN AGENT IS NAMED BY CONSTRUCTION: a peer bubble is placed only on a
// feed of a named main agent or of a subagent whose spawn was placed on one
// (r.place), so a lookup with no lineage to ask within is a placement defect,
// recorded at ERROR and asked nothing.
func (r *resolver) askVendorTask(s *wsState, id string, t *vendorTask) {
	if t.asked || r.deps.VendorTasks == nil {
		return
	}
	if s.mainAgent == "" {
		r.logger(s.id).Error("daemon.feed.vendor_task_no_lineage",
			"a message from an agent of this session was drawn in a workspace whose main agent is not named; its sender cannot be asked for",
			dlog.Context{"task_id": id})
		return
	}
	t.asked = true
	ws, session := s.id, s.mainAgent
	r.logger(ws).Debug("daemon.feed.vendor_task_lookup",
		"a message from an agent of this session names its sender by task id; the record is asked which agent it is",
		dlog.Context{"task_id": id})
	r.deps.Go(func() {
		ctx, cancel := context.WithTimeout(context.Background(), vendorTaskLookupBound)
		defer cancel()
		agent, err := r.deps.VendorTasks.ResolveVendorTask(ctx, ws, session, id)
		r.answerVendorTask(ws, id, agent, err)
	})
}

// answerVendorTask takes the record's answer for task ID in WS and draws its
// waiting bubbles again. A failed lookup is recorded once, here, where its
// consequence is decided: the bubbles keep the task id, and the next one drawn
// asks again. A task id the record does not pair yet is an answer, and asked
// again the same way.
func (r *resolver) answerVendorTask(ws ids.WorkspaceID, id string, agent VendorTaskAgent, err error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	t := s.vendorTaskOf(id)
	switch {
	case errors.Is(err, ErrNoHistorySource):
		t.asked = false
		r.logger(ws).Info("daemon.feed.vendor_task_lookup_no_source",
			"no shim was up to ask which agent a task id names; its bubbles keep the task id until one is drawn again",
			dlog.Context{"task_id": id, "cause": err.Error()})
		return
	case err != nil:
		t.asked = false
		r.logger(ws).Error("daemon.feed.vendor_task_lookup_failed",
			"the record could not say which agent a task id names; its bubbles keep the task id until one is drawn again",
			dlog.Context{"task_id": id, "cause": err.Error()})
		return
	case agent.Agent == "":
		t.asked = false
		r.logger(ws).Info("daemon.feed.vendor_task_not_recorded",
			"no agent of this session is paired with a task id yet; its bubbles keep the task id until one is drawn again",
			dlog.Context{"task_id": id})
		return
	}
	t.agent, t.known = agent, true
	r.logger(ws).Debug("daemon.feed.vendor_task_answered",
		"the record named the agent a task id is; its bubbles are drawn again",
		dlog.Context{"task_id": id, "agent": agent.Agent, "named": agent.RequestedName != ""})
	r.redrawVendorTask(s, t)
}

// redrawVendorTask draws again every peer bubble headed by task T in S, each
// under its own FeedId. Called with r.mu held.
func (r *resolver) redrawVendorTask(s *wsState, t *vendorTask) {
	for id := range t.peers {
		if peer, ok := s.peerMessages[id]; ok {
			r.drawPeerMessage(s, peer)
		}
	}
}
