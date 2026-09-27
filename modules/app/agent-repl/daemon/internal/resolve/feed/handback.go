package feed

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
)

// ---- THE SUBAGENT'S RETURNED RESULT ----
//
// A subagent ends by handing its final report back (conversation.v1
// AgentSubagentHandback). The report IS the subagent's result, so it is drawn
// ONCE, as the `subagent_result` unit on the subagent's OWN feed — inside its
// card — and never as an ordinary tool card. The main feed carries only the
// hand-back's badge (peer.go), never the report.

// drawSubagentResult draws a hand-back as the subagent's result unit, keyed by
// its activity id like every unit, so the live frames and a replay upsert one
// row. Start and progress draw it delivering; success delivered; failure
// undelivered, with the vendor's refusal text when it stated one.
func (r *resolver) drawSubagentResult(s *wsState, at placement, act *conversationv1.AgentActivity, handback *conversationv1.AgentSubagentHandback) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)
	result := &frontendv1.FeedSubagentResult{}

	switch state := handback.GetResult().(type) {
	case *conversationv1.AgentSubagentHandback_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSubagentResult", "branch": "case *conversationv1.AgentSubagentHandback_Start"})
		holdHandbackReport(u, state.Start.GetReport())
		result.State = &frontendv1.FeedSubagentResult_Delivering{Delivering: &frontendv1.FeedSubagentResultDelivering{}}
	case *conversationv1.AgentSubagentHandback_Progress:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSubagentResult", "branch": "case *conversationv1.AgentSubagentHandback_Progress"})
		if !u.handbackReportHeld {
			// A beat restates nothing, and no frame of this unit stated the
			// report: there is no result to draw, and drawing an empty one would
			// claim the subagent reported nothing.
			r.logger(s.id).Info("daemon.feed.handback_beat_without_report",
				"a hand-back's liveness beat arrived with no report held for its unit; nothing is drawn until a frame states the report",
				dlog.Context{"unit": unitID})
			return nil, errNotARow
		}
		result.State = &frontendv1.FeedSubagentResult_Delivering{Delivering: &frontendv1.FeedSubagentResultDelivering{}}
	case *conversationv1.AgentSubagentHandback_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSubagentResult", "branch": "case *conversationv1.AgentSubagentHandback_Success"})
		holdHandbackReport(u, state.Success.GetReport())
		result.State = &frontendv1.FeedSubagentResult_Delivered{Delivered: &frontendv1.FeedSubagentResultDelivered{}}
	case *conversationv1.AgentSubagentHandback_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawSubagentResult", "branch": "case *conversationv1.AgentSubagentHandback_Failure"})
		holdHandbackReport(u, state.Failure.GetReport())
		undelivered := &frontendv1.FeedSubagentResultUndelivered{}
		if text := failureText(state.Failure.GetError()); text != "" {
			undelivered.Reason = &frontendv1.FeedSubagentResultUndeliveredReason{Text: text}
		}
		result.State = &frontendv1.FeedSubagentResult_Undelivered{Undelivered: undelivered}
	default:
		// No arm, or one this build does not know: the producer stated a
		// hand-back this resolver cannot read. Refused loudly by the caller.
		return nil, fmt.Errorf("feed: a subagent hand-back carried no result arm this resolver knows (%T)", state)
	}

	result.Report = &frontendv1.FeedSubagentResultReport{Text: u.handbackReport}
	row := &frontendv1.FeedRow{
		Id: r.rowID(s.id, at.feed, feedid.RowKey{Kind: feedid.KindActivity, ID: unitID}),
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{
			Unit: &frontendv1.FeedTurnActivity_SubagentResult{SubagentResult: result},
		}},
	}
	u.row = row
	return row, nil
}

// holdHandbackReport keeps the report a frame stated, so a later beat that
// restates nothing still draws it.
func holdHandbackReport(u *unitState, report *conversationv1.AgentSubagentHandbackReport) {
	u.handbackReport = report.GetText()
	u.handbackReportHeld = true
}
