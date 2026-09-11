package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// THE PROSE FOLD. The shim forwards each fragment as the vendor emits it and
// accumulates nothing; the DAEMON accumulates and re-pushes the whole row, so
// the client renders what arrives and accumulates nothing either. A missed
// push self-corrects on the next one, and the terminal frame restates the
// whole regardless — which is why a settled bubble is always right.

// drawResponse folds one response block into its bubble.
func (r *resolver) drawResponse(s *wsState, at placement, agent *conversationv1.AgentId, act *conversationv1.AgentActivity, response *conversationv1.AgentResponse) (*frontendv1.FeedRow, error) {
	unit := act.GetActivityId().GetValue()
	fold := s.prose(unit)
	log := r.logger(s.id)

	// THE STAMP IS THE API RESPONSE'S FIGURES, not this unit's: usage rides
	// exactly one unit per API response and it is usually a sibling (the
	// thinking block's). Filing is idempotent, so drawing this bubble from a
	// direct call files what the sink would have filed.
	s.fileAPIResponse(unit, act.GetUsage())
	if stamp := s.apiResponseStamp(unit); stamp != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "stamp := s.apiResponseStamp(unit); stamp != \"\""})
		fold.usage = stamp
	}

	bubble := &frontendv1.FeedResponse{}
	if fold.usage != "" {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "fold.usage != \"\""})
		bubble.Usage = &frontendv1.FeedResponseUsageStamp{Text: fold.usage}
	}

	switch state := response.GetResult().(type) {
	case *conversationv1.AgentResponse_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawResponse", "branch": "case *conversationv1.AgentResponse_Start"})
		fold.markdown = ""
		fold.settled = false
		bubble.Result = &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose: &frontendv1.FeedResponseProse{Markdown: ""},
		}}
	case *conversationv1.AgentResponse_Update:
		if fold.settled {
			// A fragment after the terminal cannot re-open a closed bubble;
			// the settled whole is authoritative.
			//
			// THIS IS ORDINARY, NOT A FAULT, AND IT IS RECORDED AT DEBUG.
			// One block's frames reach this fold from TWO STORE PLANES that
			// share an upsert key and are not ordered against one another:
			// the shim's stream plane pays out `start` and delta `update`s as
			// the vendor emits them, while the sidecar's file plane converts
			// the same assistant message out of the transcript into a single
			// settled `success`. The sidecar's success routinely lands
			// BETWEEN the shim's start and its own trailing deltas — measured
			// on 2026-09-04 in TestPerfSubmitPromptAck, where the live order
			// was start, success (file plane), update, update, success
			// (stream plane) for one prose block.
			//
			// Nothing is lost when it happens: a settled frame restates the
			// WHOLE, so the dropped delta was already inside the text on
			// screen. The daemon cannot tell this apart from a single
			// producer disordering its own stream — HistoryEntryAt carries no
			// plane — so warning here can only ever be a false alarm, and a
			// warning that is always false is worse than no warning.
			log.Debug("daemon.feed.response_fragment_after_settle",
				"a prose fragment arrived after the block settled and was not folded in; the settled whole stands",
				dlog.Context{"unit": unit})
			return nil, errNotARow
		}
		fold.markdown += state.Update.GetNewMarkdown()
		bubble.Result = &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	case *conversationv1.AgentResponse_Success:
		// THE TERMINAL RESTATES THE WHOLE. Whatever the fold accumulated is
		// replaced outright, which is what makes a lost fragment harmless.
		//
		// AND THE WHOLE IS WRAPPED HERE, ONCE. The response is the tree the
		// metaprompt prescribes, and the daemon is the one place both clients
		// read it from, so the tree is wrapped to its column limit before it
		// is served rather than by each client for itself (tree.go). Only the
		// settled whole is wrapped: a streaming delta is a fragment of a line
		// the formatter has not yet seen the end of.
		fold.markdown = formatResponseTree(log, unit, state.Success.GetProse().GetMarkdown())
		fold.settled = true
		if notice, ok := state.Success.GetAuthorship().(*conversationv1.AgentResponseSuccess_SynthesizedNotice); ok {
			// THE VENDOR SYNTHESIZES ERROR NOTICES AS ASSISTANT PROSE. Drawing
			// one as the agent's answer would present an outage as something
			// the agent said, so the daemon states the authorship in the
			// bubble's OWN NOTICE FIELD, which puts the row in the notice
			// register. The PROSE STAYS VERBATIM: a heading spliced into the
			// markdown would be indistinguishable from words the vendor
			// actually wrote, and nothing downstream could pull them apart
			// again.
			bubble.Notice = &frontendv1.FeedResponseNotice{
				Heading: noticeHeading(notice.SynthesizedNotice),
			}
			log.Debug("daemon.feed.response_synthesized_notice",
				"a vendor-synthesized notice was drawn as a notice rather than as the agent's answer",
				dlog.Context{"unit": unit, "subject": noticeSubject(notice.SynthesizedNotice)})
		}
		bubble.Result = &frontendv1.FeedResponse_Success{Success: &frontendv1.FeedResponseSuccess{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	case *conversationv1.AgentResponse_Failure:
		// The prose that landed stays drawn, marked broken. WHY it died is the
		// turn's terminal row, never this bubble's business.
		//
		// A REFUSAL IS REMEMBERED FOR THE TERMINAL, though: the failure the
		// producer ends the run with (AgentModelError) is an empty message, so
		// this is the only frame that says the vendor refused rather than
		// errored, and feed.proto's `refusal` arm is drawn from it.
		if _, refused := state.Failure.GetReason().GetReason().(*conversationv1.AgentResponseFailureReason_Refused); refused && s.turnInFlight != nil {
			s.turnRefusals[string(*s.turnInFlight)] = true
			log.Debug("daemon.feed.response_refused",
				"a response ended on the vendor's refusal; the turn's terminal draws the refusal arm",
				dlog.Context{"unit": unit, "turn": string(*s.turnInFlight)})
		}
		fold.markdown = state.Failure.GetProse().GetMarkdown()
		fold.settled = true
		bubble.Result = &frontendv1.FeedResponse_Error{Error: &frontendv1.FeedResponseError{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	default:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawResponse", "branch": "default"})
		return nil, errNotARow
	}

	id := r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unit})
	s.answerRows[unit] = id
	return &frontendv1.FeedRow{
		Id: id,
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_Response{Response: bubble},
		}},
	}, nil
}

// noticeHeading composes the notice register's heading. It is a HEADING, not
// markdown prose: the client draws it above the bubble in its own treatment,
// so it carries no emphasis markup of its own.
func noticeHeading(notice *conversationv1.AgentResponseSynthesizedNotice) string {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return "Notice — your allowance is exhausted. This is the vendor's own message, not the agent's."
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return "Notice — your allowance window changed. This is the vendor's own message, not the agent's."
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return "Notice — you are approaching an allowance limit. This is the vendor's own message, not the agent's."
	}
	return "Notice from the vendor's tooling, not the agent's answer."
}

// noticeSubject names the notice's subject for a log record.
func noticeSubject(notice *conversationv1.AgentResponseSynthesizedNotice) string {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return "usage_limit"
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return "usage_transition"
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return "usage_warning"
	}
	return "unclassified"
}
