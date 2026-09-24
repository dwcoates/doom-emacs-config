package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
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

	switch state := send.GetResult().(type) {
	case *conversationv1.AgentSendMessage_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSendMessage", "branch": "case *conversationv1.AgentSendMessage_Start"})
		u.startHeld = true
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		u.sendAddressedTo = state.Start.GetAddressedTo()
		// The summary is OPTIONAL on the wire; its absence is a fact, not a
		// reason to reach for the body.
		if state.Start.Summary != nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "state.Start.Summary != nil"})
			u.sendSummary = state.Start.GetSummary().GetText()
		}
	case *conversationv1.AgentSendMessage_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSendMessage", "branch": "case *conversationv1.AgentSendMessage_Progress"})
		u.lastProgressMs = state.Progress.GetLastProgressAtMs()
	case *conversationv1.AgentSendMessage_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSendMessage", "branch": "case *conversationv1.AgentSendMessage_Success"})
		if err := r.restateSend(s, act, u, state.Success.GetAddressedTo(), state.Success.GetSummary()); err != nil {
			return nil, err
		}
		u.sendResolved = state.Success.GetRecipientAgentId()
		u.sendDelivery = deliveryOf(state.Success)
	case *conversationv1.AgentSendMessage_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSendMessage", "branch": "case *conversationv1.AgentSendMessage_Failure"})
		if err := r.restateSend(s, act, u, state.Failure.GetAddressedTo(), state.Failure.GetSummary()); err != nil {
			return nil, err
		}
		// A send that could not be delivered still HAPPENED, and its row is
		// what explains the attempt. It is drawn against what the start said,
		// and its delivery arm states the REFUSAL — never left unset, which a
		// reader cannot tell apart from a producer that stated nothing.
		u.sendDelivery = refusedOf(state.Failure)
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSendMessage", "branch": "default"})
		return nil, errNotARow
	}

	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindPrompt, ID: unitID, Sub: "send"}),
		Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{
			Address: &frontendv1.FeedAgentPromptAddress{
				Text: "→ " + r.sendRecipientLabel(s, u.sendResolved, u.sendAddressedTo),
			},
			Body: &frontendv1.FeedAgentPromptBody{Blocks: sendBodyBlocks(u.sendSummary)},
		}},
	}
	// THE DELIVERY IS THE UNIT'S, NOT THIS FRAME'S. See unitState.sendDelivery
	// for the replay that made the difference matter.
	if u.sendDelivery != nil {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "u.sendDelivery != nil"})
		u.sendDelivery(row.GetAgentPrompt())
	}
	u.row = row
	return row, nil
}

// restateSend takes a SETTLED send's address and summary from the settle itself.
//
// THE SETTLE STANDS ALONE. A send's start and its settle upsert one unit, so a
// replay (a workspace open, a transcript select) serves the settle with no start
// beside it; drawn from the start's fields alone, such a send drew with an EMPTY
// BODY (row prompt:toolu_01GiUF1L5VCoxJQZ8nURrUEv:send, whose summary was
// "Scroll fix landed; merge master in"). Both settle arms therefore restate
// addressed_to and the optional summary.
//
// A settle that restates no address has not restated the send at all: graded by
// its producer's contract (an invariant violation at ERROR, or expected old data
// at INFO), drawn from the held start when this process saw one and drawn NOT
// AT ALL otherwise (restatedOrHeld). A restated
// address makes the settle authoritative for the summary too, whose absence then
// means the caller supplied none.
func (r *resolver) restateSend(s *wsState, act *conversationv1.AgentActivity, u *unitState, addressedTo string, summary *conversationv1.AgentSendMessageSummary) error {
	if addressedTo == "" {
		_, err := r.restatedOrHeld(s, act, u, "send_message", "", u.sendAddressedTo)
		return err
	}
	u.sendAddressedTo = addressedTo
	u.sendSummary = summary.GetText()
	return nil
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
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "id := resolved.GetValue(); id != \"\""})
		if key, ok := s.agentFeeds[id]; ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "key, ok := s.agentFeeds[id]; ok"})
			return feedLabel(s, key)
		}
	}
	if addressedTo != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "addressedTo != \"\""})
		return addressedTo
	}
	if id := resolved.GetValue(); id != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "id := resolved.GetValue(); id != \"\""})
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

// deliveryArm sets HOW THE SEND WAS DELIVERED on the sender's row. A setter
// rather than the generated oneof interface, whose method is unexported and
// unimplementable from here.
type deliveryArm func(*frontendv1.FeedAgentPrompt)

// deliveryOf relays AgentSendMessageSuccess's delivery arm onto the SENDER's
// row (feed.proto: "UNSET on the recipient's copy and when the producer
// observed nothing; the row's presence already says the attempt happened").
// A producer that stated no arm leaves the field unset — nil here — rather
// than having one guessed for it.
func deliveryOf(success *conversationv1.AgentSendMessageSuccess) deliveryArm {
	switch success.GetDelivery().(type) {
	case *conversationv1.AgentSendMessageSuccess_QueuedToLive:
		return func(p *frontendv1.FeedAgentPrompt) {
			p.Delivery = &frontendv1.FeedAgentPrompt_QueuedToLive{
				QueuedToLive: &frontendv1.FeedAgentPromptQueuedToLive{},
			}
		}
	case *conversationv1.AgentSendMessageSuccess_ResumedRecipient:
		// The recipient WAS RESUMED to receive this — the cause of that
		// agent's renewed activity and renewed cost.
		return func(p *frontendv1.FeedAgentPrompt) {
			p.Delivery = &frontendv1.FeedAgentPrompt_ResumedRecipient{
				ResumedRecipient: &frontendv1.FeedAgentPromptResumedRecipient{},
			}
		}
	}
	return nil
}

// refusedOf relays AgentSendMessageFailure onto the SENDER's row as the
// `refused` delivery arm (feed.proto, landing 14: "a refusal is not an
// absence"). ALWAYS AN ARM, even when the failure carried no account at all —
// the arm is the refusal, and the reason is only its detail, so a contentless
// refusal is still drawn as one rather than falling back to the unset oneof
// that means "the producer stated nothing".
//
// The reason is the producer's own words, taken by the same reading every
// failed tool call's account is taken by (failureText): the text blocks the
// tool answered with, joined in order. No refusal KIND is derived from them —
// the vendor declares none, and it lives only inside prose.
func refusedOf(failure *conversationv1.AgentSendMessageFailure) deliveryArm {
	refused := &frontendv1.FeedAgentPromptRefused{}
	if text := failureText(failure.GetError()); text != "" {
		refused.Reason = &frontendv1.FeedAgentPromptRefusalReason{Text: text}
	}
	return func(p *frontendv1.FeedAgentPrompt) {
		p.Delivery = &frontendv1.FeedAgentPrompt_Refused{Refused: refused}
	}
}
