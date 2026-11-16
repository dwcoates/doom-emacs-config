package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
)

// readNotificationsToolName heads the card, as the vendor names the tool.
const readNotificationsToolName = "ReadNotifications"

// drawReadNotifications draws the agent reading its queued notifications as
// the ORDINARY tool card: the tool's name as the head, no input line (the call
// takes none), and what the read returned as plain text.
//
// THE CALL TAKES NO INPUT, so a settled frame restates nothing and needs to:
// a replay that serves the settle alone draws the whole card from it.
func (r *resolver) drawReadNotifications(s *wsState, at placement, act *conversationv1.AgentActivity, read *conversationv1.AgentReadNotifications) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)

	switch state := read.GetResult().(type) {
	case *conversationv1.AgentReadNotifications_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawReadNotifications", "branch": "case *conversationv1.AgentReadNotifications_Start"})
		u.startHeld = true
		u.startedAtMs = state.Start.GetStartedAt().GetAtMs()
		if u.denied {
			return r.toolRow(s, at, unitID, readNotificationsToolName, deniedOutcome()), nil
		}
		return r.toolRow(s, at, unitID, readNotificationsToolName, runningOutcome(u)), nil
	case *conversationv1.AgentReadNotifications_Success:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawReadNotifications", "branch": "case *conversationv1.AgentReadNotifications_Success"})
		return r.toolRow(s, at, unitID, readNotificationsToolName,
			r.settledOutcome(s, act, u, "read_notifications", true, textForm(state.Success.GetReturnedText()),
				state.Success.GetSettledAt())), nil
	case *conversationv1.AgentReadNotifications_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawReadNotifications", "branch": "case *conversationv1.AgentReadNotifications_Failure"})
		return r.toolRow(s, at, unitID, readNotificationsToolName,
			r.settledOutcome(s, act, u, "read_notifications", false, r.failureForm(s, state.Failure.GetError()),
				state.Failure.GetError().GetSettledAt())), nil
	}
	return nil, errNotARow
}
