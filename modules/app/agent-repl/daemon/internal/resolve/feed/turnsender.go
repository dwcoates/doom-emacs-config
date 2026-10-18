package feed

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// A PROMPT ANOTHER WORKSPACE'S AGENT SENT (PROMPT_ORIGIN_AGENT_SENT, through
// `agent-repld workspace prompt`) is headed the way every agent-to-agent
// prompt is (agentaddress.go): a left arrow, then the sending workspace's
// name. The sender is a DAEMON fact, recorded on the turn (wsm.Turn.SentBy):
// the prompt queue hands it here as it records the turn (SenderTurn), and a
// replay reads it back (Deps.TurnSenders) into the same table, so the live row
// and every replayed one are headed from one fact.

// unnamedWorkspaceSender is the stand-in for a sending workspace the daemon
// recorded but cannot name. An identity is never invented to fill it.
const unnamedWorkspaceSender = "another workspace"

// SenderTurn records the workspace whose agent sent TURN's prompt, nil for a
// prompt no agent sent.
func (r *resolver) SenderTurn(ws ids.WorkspaceID, turn ids.TurnID, sender *ids.WorkspaceID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.state(ws).senderTurn(turn, sender)
	if sender != nil {
		r.logger(ws).Debug("daemon.feed.turn_sender", "the feed took the workspace that sent a turn's prompt", dlog.Context{
			"turn": string(turn), "sent_by": string(*sender)})
	}
}

// senderTurn stores TURN's sender, or forgets it when it has none.
func (s *wsState) senderTurn(turn ids.TurnID, sender *ids.WorkspaceID) {
	if turn == "" {
		return
	}
	if sender == nil {
		delete(s.turnSenders, turn)
		return
	}
	s.turnSenders[turn] = *sender
}

// PromptHeader answers the header TURN's prompt is drawn under in WS: the
// sending workspace for a prompt of origin PROMPT_ORIGIN_AGENT_SENT, nil for
// every other origin, whose prompt is the user-prompt row.
func (r *resolver) PromptHeader(ws ids.WorkspaceID, turn ids.TurnID, origin conversationv1.PromptOrigin) *frontendv1.FeedAgentPromptAddress {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.promptHeader(r.state(ws), turn, origin)
}

// promptHeader is PromptHeader with r.mu held.
//
// AN AGENT-SENT TURN WITH NO RECORDED SENDER is drawn under the unnamed arm,
// which says the daemon cannot name the sender, and recorded at ERROR: every
// agent-sent turn is recorded with its sender (wsm refuses one without), so
// the gap is a failed read of the turn records (recordedSenders has recorded
// it) or a defect, never a prompt with no sender.
func (r *resolver) promptHeader(s *wsState, turn ids.TurnID, origin conversationv1.PromptOrigin) *frontendv1.FeedAgentPromptAddress {
	if origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT {
		return nil
	}
	sender, known := s.turnSenders[turn]
	if !known {
		r.logger(s.id).Error("daemon.feed.turn_sender_missing",
			"an agent-sent prompt has no recorded sender in the feed; it is headed by a workspace the daemon cannot name",
			dlog.Context{"turn": string(turn)})
		return receivedFromWorkspace(unnamedWorkspaceSender, false)
	}
	name, named := r.workspaceName(sender, s.id)
	if !named {
		return receivedFromWorkspace(unnamedWorkspaceSender, false)
	}
	return receivedFromWorkspace(name, true)
}

// recordedSenders reads the recorded sender of every turn a history page
// names, BEFORE the resolver's lock is taken. A FAILED READ IS RECORDED, NEVER
// SWALLOWED, and the page still draws: an agent-sent prompt of the page is
// headed by a workspace the daemon cannot name until the next replay.
func (r *resolver) recordedSenders(ws ids.WorkspaceID, turns []ids.TurnID) map[ids.TurnID]ids.WorkspaceID {
	if r.deps.TurnSenders == nil || len(turns) == 0 {
		return nil
	}
	senders, err := r.deps.TurnSenders(context.Background(), ws, turns)
	if err != nil {
		r.logger(ws).Error("daemon.feed.turn_senders_unreadable",
			"the replayed turns' recorded senders could not be read; their agent-sent prompts are headed by a workspace the daemon cannot name",
			dlog.Context{"turns": len(turns), "cause": err.Error()})
		return nil
	}
	return senders
}
