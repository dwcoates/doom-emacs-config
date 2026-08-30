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

	// The usage stamp is the EXPENSIVE sum and nothing else: both cache-miss
	// buckets together, because both were processed fresh. A stamp read off
	// any single counter would understate the bill.
	if usage := act.GetUsage(); usage != nil {
		misses := usage.GetInputMisses()
		fold.usage = formatTokens(misses.GetWritten() + misses.GetUnwritten())
	}

	bubble := &frontendv1.FeedResponse{}
	if fold.usage != "" {
		bubble.Usage = &frontendv1.FeedResponseUsageStamp{Text: fold.usage}
	}

	switch state := response.GetResult().(type) {
	case *conversationv1.AgentResponse_Start:
		fold.markdown = ""
		fold.settled = false
		bubble.Result = &frontendv1.FeedResponse_Update{Update: &frontendv1.FeedResponseUpdate{
			Prose: &frontendv1.FeedResponseProse{Markdown: ""},
		}}
	case *conversationv1.AgentResponse_Update:
		if fold.settled {
			// A fragment after the terminal cannot re-open a closed bubble;
			// the settled whole is authoritative.
			log.Warn("daemon.feed.response_fragment_after_settle",
				"a prose fragment arrived after the block settled and was not folded in",
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
		fold.markdown = state.Success.GetProse().GetMarkdown()
		fold.settled = true
		markdown := fold.markdown
		if notice, ok := state.Success.GetAuthorship().(*conversationv1.AgentResponseSuccess_SynthesizedNotice); ok {
			// THE VENDOR SYNTHESIZES ERROR NOTICES AS ASSISTANT PROSE. Drawing
			// one as the agent's answer would present an outage as something
			// the agent said, so the daemon marks the authorship in the drawn
			// prose itself — the one place a client reads without knowing the
			// distinction exists.
			markdown = noticeHeading(notice.SynthesizedNotice) + "\n\n" + markdown
			log.Debug("daemon.feed.response_synthesized_notice",
				"a vendor-synthesized notice was drawn as a notice rather than as the agent's answer",
				dlog.Context{"unit": unit, "subject": noticeSubject(notice.SynthesizedNotice)})
		}
		bubble.Result = &frontendv1.FeedResponse_Success{Success: &frontendv1.FeedResponseSuccess{
			Prose: &frontendv1.FeedResponseProse{Markdown: markdown},
		}}
	case *conversationv1.AgentResponse_Failure:
		// The prose that landed stays drawn, marked broken. WHY it died is the
		// turn's terminal row, never this bubble's business.
		fold.markdown = state.Failure.GetProse().GetMarkdown()
		fold.settled = true
		bubble.Result = &frontendv1.FeedResponse_Error{Error: &frontendv1.FeedResponseError{
			Prose: &frontendv1.FeedResponseProse{Markdown: fold.markdown},
		}}
	default:
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

// noticeHeading composes the line that marks synthesized prose as a notice.
func noticeHeading(notice *conversationv1.AgentResponseSynthesizedNotice) string {
	switch notice.GetSubject().(type) {
	case *conversationv1.AgentResponseSynthesizedNotice_UsageLimit:
		return "**Notice — your allowance is exhausted.** This is the vendor's own message, not the agent's."
	case *conversationv1.AgentResponseSynthesizedNotice_UsageTransition:
		return "**Notice — your allowance window changed.** This is the vendor's own message, not the agent's."
	case *conversationv1.AgentResponseSynthesizedNotice_UsageWarning:
		return "**Notice — you are approaching an allowance limit.** This is the vendor's own message, not the agent's."
	}
	return "**Notice from the vendor's tooling**, not the agent's answer."
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
