package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/feedid"
)

// ①c THE OUTGOING SEND. A SendMessage is a prompt one agent addressed to
// another, so it is drawn with the SAME component as a delivered agent prompt
// (feed.proto: FeedAgentPrompt is "ONE component, both ends: on the SENDER's
// feed it is the outgoing send, on the recipient's the delivered prompt; only
// the address line differs"). This composer draws the SENDER's end; the
// recipient's end is drawn by drawAgentPrompt when the message is delivered as
// that agent's prompt, exactly as a commission is.
//
// THE BODY IS NEVER DRAWN. AgentSendMessage's own contract forbids it: "A
// surface draws the recipient and the summary; it must not fall back to the
// body when no summary was given, since dumping a relay into a feed is the
// outcome the summary exists to prevent." So the drawn body is the caller's
// one-line summary, and a send with no summary draws its address line alone.

// sendNotNamed is the honest address for a send whose recipient nothing on the
// wire names — neither the addressed string the caller wrote nor a resolved
// identity. An identity is never invented to fill it.
const sendNotNamed = "an agent the send did not name"

// drawSendMessage draws one send as an agent_prompt row on the SENDING agent's
// feed. `at` is that feed: drawActivity placed it from the agent whose stream
// carried the call.
//
// EVERY FRAME OF THE SEND UPSERTS THE ONE ROW. The start names the recipient
// and carries the summary, the success resolves the recipient's identity, and
// both are keyed by the same unit — so a queued send that is later reported
// delivered (or that resumed a dormant recipient) redraws its row rather than
// drawing a second one.
func (r *resolver) drawSendMessage(s *wsState, at placement, act *conversationv1.AgentActivity, send *conversationv1.AgentSendMessage) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	var resolved *conversationv1.AgentId
	switch state := send.GetResult().(type) {
	case *conversationv1.AgentSendMessage_Start:
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.sendAddressedTo = state.Start.GetAddressedTo()
		// The summary is OPTIONAL on the wire; its absence is a fact, not a
		// reason to reach for the body.
		if state.Start.Summary != nil {
			u.sendSummary = state.Start.GetSummary().GetText()
		}
	case *conversationv1.AgentSendMessage_Progress:
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
	case *conversationv1.AgentSendMessage_Success:
		resolved = state.Success.GetRecipientAgentId()
	case *conversationv1.AgentSendMessage_Failure:
		// A send that could not be delivered still HAPPENED, and its row is
		// what explains the attempt. It is drawn against what the start said.
	default:
		return nil, errNotARow
	}

	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: unitID, Sub: "send"}),
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: &frontendv1.FeedAgentPromptAddress{
				Text: "→ " + r.sendRecipientLabel(s, resolved, u.sendAddressedTo),
			},
			Body: &frontendv1.FeedAgentPromptBody{Blocks: sendBodyBlocks(u.sendSummary)},
		}},
	}
	u.row = row
	return row, nil
}

// sendRecipientLabel names the recipient for the address line, preferring the
// name a reader can recognize over the identity a machine resolved:
//
//  1. the recipient's own bubble label, when the resolved identity is a
//     subagent this workspace has a feed for — the same label the recipient's
//     own end of this prompt is addressed by;
//  2. the addressed string EXACTLY AS THE CALLER WROTE IT, which the contract
//     says may be an identity or the human-readable name a spawn was given;
//  3. the resolved identity's value, when that is all there is;
//  4. the honest admission that nothing named the recipient.
func (r *resolver) sendRecipientLabel(s *wsState, resolved *conversationv1.AgentId, addressedTo string) string {
	if id := resolved.GetValue(); id != "" {
		if key, ok := s.agentFeeds[id]; ok {
			return feedLabel(s, key)
		}
	}
	if addressedTo != "" {
		return addressedTo
	}
	if id := resolved.GetValue(); id != "" {
		return id
	}
	return sendNotNamed
}

// sendBodyBlocks draws the caller's summary, and NOTHING when there was none:
// the body the send carries is for the recipient and the record, never for a
// feed.
func sendBodyBlocks(summary string) []*frontendv1.FeedAgentPromptBlock {
	if summary == "" {
		return nil
	}
	return []*frontendv1.FeedAgentPromptBlock{{
		Block: &frontendv1.FeedAgentPromptBlock_Text{Text: &frontendv1.FeedTextBlock{Text: summary}},
	}}
}
