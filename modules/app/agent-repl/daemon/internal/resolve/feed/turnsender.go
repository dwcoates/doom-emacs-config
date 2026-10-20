package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
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
	return readRecorded(r, ws, turns, r.deps.TurnSenders, "daemon.feed.turn_senders_unreadable",
		"the replayed turns' recorded senders could not be read; their agent-sent prompts are headed by a workspace the daemon cannot name")
}

// THE SENDER'S END OF A PROMPT ITS AGENT SENT ANOTHER WORKSPACE. The daemon
// takes the send itself (agentrepl.v1.SubmitPrompt naming `sent_by`), so it
// draws the sent row in the sender's root feed at that moment, headed by the
// recipient workspace's name then a right arrow, as a send reaching another
// workspace is (sentToPeerSession). The row is DURABLE, so a restart draws it
// where it stood, and it carries NO turn: it is the daemon's record of the
// send, and a durable row stamped with a turn would keep that turn's working
// wave after the turn ended.

// SentFate is how an agent-sent prompt fared, as the sender's row states it.
type SentFate struct {
	// Queued reports the prompt was held behind the recipient's running work.
	Queued bool
	// Refused, when set, is the daemon's sentence for why the send was
	// refused; nothing reached the recipient.
	Refused *string
}

// SentPrompt draws the row SENDER's feed shows for a prompt its agent sent
// to the workspace named RECIPIENT, under the send's idempotency KEY.
func (r *resolver) SentPrompt(sender ids.WorkspaceID, recipient, key string, said *conversationv1.UserSaid, fate SentFate) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(sender)
	root := feedid.Feed{Root: true}
	prompt := &frontendv1.FeedAgentPrompt{
		Address: sentToPeerSession(recipient, &frontendv1.FeedAgentPromptAddressPeerSession{
			Recipient: &frontendv1.FeedAgentPromptAddressPeerSession_Workspace{Workspace: &frontendv1.FeedAgentPromptAddressPeerSessionWorkspace{}},
		}),
		Body: &frontendv1.FeedAgentPromptBody{Blocks: agentBlocks(r.drawUserBlocks(s, said.GetContent()))},
	}
	switch {
	case fate.Refused != nil:
		prompt.Delivery = &frontendv1.FeedAgentPrompt_Refused{Refused: &frontendv1.FeedAgentPromptRefused{
			Reason: &frontendv1.FeedAgentPromptRefusalReason{Text: *fate.Refused},
		}}
	case fate.Queued:
		prompt.Delivery = &frontendv1.FeedAgentPrompt_QueuedToLive{QueuedToLive: &frontendv1.FeedAgentPromptQueuedToLive{}}
	}
	row := &frontendv1.FeedRow{
		Id:  r.rowID(sender, root, feedid.RowKey{Kind: feedid.KindPrompt, ID: key, Sub: "sent"}),
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: prompt},
	}
	r.logger(sender).Debug("daemon.feed.sent_prompt",
		"a prompt this workspace's agent sent another workspace was drawn as its sent row",
		dlog.Context{"recipient": recipient, "idempotency_key": key, "queued": fate.Queued, "refused": fate.Refused != nil})
	r.upsertDurable(s, placement{feed: root, pin: pinSettle}, row)
}
