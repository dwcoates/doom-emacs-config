package feed

import (
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE HEADER OF A PROMPT BETWEEN TWO AGENTS OF ONE SESSION (owner spec,
// 2026-10-17): the agent at the other end, named by its requested name when
// the spawn gave it one and by its identifier when it did not; the session's
// main agent by the daemon's word for it. The direction arm places the arrow:
// a prompt that went out reads the name then a right arrow, one that came in a
// left arrow then the name. No border, as every agent-to-agent bubble.

// mainAgentName is the daemon's word for the session's main agent.
const mainAgentName = "the main agent"

// agentEnd is an agent of this session at the other end of a prompt: its name
// and what the name is.
type agentEnd struct {
	name     string
	identity *frontendv1.FeedAgentPromptAddressAgent
}

// agentNamed answers the agent ID as a header names it: the main agent, a
// subagent by its requested name, or a subagent (or an agent this workspace
// knows nothing of) by its identifier.
func (r *resolver) agentNamed(s *wsState, id string) agentEnd {
	if id != "" && id == s.mainAgent {
		return agentEnd{name: mainAgentName, identity: &frontendv1.FeedAgentPromptAddressAgent{
			Identity: &frontendv1.FeedAgentPromptAddressAgent_MainAgent{MainAgent: &frontendv1.FeedAgentPromptAddressAgentMain{}},
		}}
	}
	for _, state := range s.subagents {
		if state.created.GetValue() == id && state.requestedName != "" {
			return agentEnd{name: state.requestedName, identity: &frontendv1.FeedAgentPromptAddressAgent{
				Identity: &frontendv1.FeedAgentPromptAddressAgent_Named{Named: &frontendv1.FeedAgentPromptAddressAgentNamed{}},
			}}
		}
	}
	return agentEnd{name: id, identity: &frontendv1.FeedAgentPromptAddressAgent{
		Identity: &frontendv1.FeedAgentPromptAddressAgent_Identified{Identified: &frontendv1.FeedAgentPromptAddressAgentIdentified{}},
	}}
}

// agentOfFeed answers the agent whose feed KEY is: the root is the main
// agent's, a sub-feed its agent's.
func (r *resolver) agentOfFeed(s *wsState, key string) agentEnd {
	if addr, ok := s.feedAddrs[key]; ok && addr.Agent != nil {
		return r.agentNamed(s, addr.Agent.GetValue())
	}
	return r.agentNamed(s, s.mainAgent)
}

// sentToAgent is the header of a prompt that went out to the agent END.
func sentToAgent(end agentEnd) *frontendv1.FeedAgentPromptAddress {
	return &frontendv1.FeedAgentPromptAddress{
		Name: &frontendv1.FeedAgentPromptAddressName{Text: end.name},
		Direction: &frontendv1.FeedAgentPromptAddress_Sent{Sent: &frontendv1.FeedAgentPromptAddressSent{
			Recipient: &frontendv1.FeedAgentPromptAddressSent_Agent{Agent: end.identity},
		}},
	}
}

// receivedFromAgent is the header of a prompt that came in from the agent END.
func receivedFromAgent(end agentEnd) *frontendv1.FeedAgentPromptAddress {
	return &frontendv1.FeedAgentPromptAddress{
		Name: &frontendv1.FeedAgentPromptAddressName{Text: end.name},
		Direction: &frontendv1.FeedAgentPromptAddress_Received{Received: &frontendv1.FeedAgentPromptAddressReceived{
			Sender: &frontendv1.FeedAgentPromptAddressReceived_Agent{Agent: end.identity},
		}},
	}
}

// receivedFromWorkspace is the header of a prompt another workspace's agent
// sent, through the agent API: NAME is its workspace name when NAMED, and the
// daemon's stand-in for a workspace it cannot name otherwise.
func receivedFromWorkspace(name string, named bool) *frontendv1.FeedAgentPromptAddress {
	identity := &frontendv1.FeedAgentPromptAddressWorkspace{
		Identity: &frontendv1.FeedAgentPromptAddressWorkspace_Unnamed{Unnamed: &frontendv1.FeedAgentPromptAddressWorkspaceUnnamed{}},
	}
	if named {
		identity.Identity = &frontendv1.FeedAgentPromptAddressWorkspace_Named{Named: &frontendv1.FeedAgentPromptAddressWorkspaceNamed{}}
	}
	return &frontendv1.FeedAgentPromptAddress{
		Name: &frontendv1.FeedAgentPromptAddressName{Text: name},
		Direction: &frontendv1.FeedAgentPromptAddress_Received{Received: &frontendv1.FeedAgentPromptAddressReceived{
			Sender: &frontendv1.FeedAgentPromptAddressReceived_Workspace{Workspace: identity},
		}},
	}
}

// sentUnclassified is the header of a send that has not said which kind of
// agent it reached, named by the address the caller wrote.
func sentUnclassified(name string) *frontendv1.FeedAgentPromptAddress {
	return &frontendv1.FeedAgentPromptAddress{
		Name: &frontendv1.FeedAgentPromptAddressName{Text: name},
		Direction: &frontendv1.FeedAgentPromptAddress_Sent{Sent: &frontendv1.FeedAgentPromptAddressSent{
			Recipient: &frontendv1.FeedAgentPromptAddressSent_Unclassified{Unclassified: &frontendv1.FeedAgentPromptAddressUnclassified{}},
		}},
	}
}

// sentToPeerSession is the header of a send that reached another Claude
// session, named NAME, resolved as RECIPIENT says.
func sentToPeerSession(name string, recipient *frontendv1.FeedAgentPromptAddressPeerSession) *frontendv1.FeedAgentPromptAddress {
	return &frontendv1.FeedAgentPromptAddress{
		Name: &frontendv1.FeedAgentPromptAddressName{Text: name},
		Direction: &frontendv1.FeedAgentPromptAddress_Sent{Sent: &frontendv1.FeedAgentPromptAddressSent{
			Recipient: &frontendv1.FeedAgentPromptAddressSent_PeerSession{PeerSession: recipient},
		}},
	}
}
