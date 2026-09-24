package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/sessionwatcher"
)

// ---- MONITOR ----
//
// A MONITOR CALL DRAWS THE ORDINARY TOOL-CALL CARD (owner ruling, 2026-09-23):
// named "Monitor", its input line the watched command (or socket URL), placed
// in its OWNER's feed at the call's position like every other call. That card
// is the monitor's feed entry: it is announced to the footer by the monitor's
// id (Deps.EntryPlaced), and the footer's monitor row jumps to it.
//
// A monitor is detached from birth, so its detachment announcement continues
// nothing: the card is already the entry, and it is never turned into a shell
// head (see detachUnit and applyHeldDetachment).

// monitorName is the card's drawn name.
const monitorName = "Monitor"

// drawMonitor draws one monitor frame as the unit's tool-call card.
func (r *resolver) drawMonitor(s *wsState, at placement, act *conversationv1.AgentActivity, monitor *conversationv1.AgentMonitor) (*frontendv1.FeedRow, error) {
	unitID := act.GetActivityId().GetValue()
	u := s.unit(unitID)
	u.monitor = true
	previous := u.row.GetId()

	var row *frontendv1.FeedRow
	switch state := monitor.GetResult().(type) {
	case *conversationv1.AgentMonitor_Start:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawMonitor", "branch": "case *conversationv1.AgentMonitor_Start"})
		u.startHeld = true
		armMonitor(u, state.Start)
		if u.denied {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "drawMonitor", "condition": "u.denied"})
			row = r.toolRow(s, at, unitID, monitorName, deniedOutcome())
		} else {
			row = r.toolRow(s, at, unitID, monitorName, runningOutcome(u))
		}
	case *conversationv1.AgentMonitor_Ended:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawMonitor", "branch": "case *conversationv1.AgentMonitor_Ended"})
		if err := r.restateMonitor(s, u, unitID, state.Ended.GetCall()); err != nil {
			return nil, err
		}
		// The watch left the live set: an ordinary ending, with no output of its
		// own (its events reached the agent as turn input) and no settled instant
		// stated.
		row = r.toolRow(s, at, unitID, monitorName, returnedOutcome(u, true, nil, 0))
	case *conversationv1.AgentMonitor_Failure:
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row decision", dlog.Context{"function": "drawMonitor", "branch": "case *conversationv1.AgentMonitor_Failure"})
		if err := r.restateMonitor(s, u, unitID, state.Failure.GetCall()); err != nil {
			return nil, err
		}
		failure := state.Failure.GetFailure()
		row = r.toolRow(s, at, unitID, monitorName,
			returnedOutcome(u, false, r.failureForm(s, failure), failureSettledMs(failure)))
	default:
		return nil, errNotARow
	}
	r.announceEntry(s, unitID, previous, row.GetId())
	return row, nil
}

// armMonitor takes what the call armed the watch with: its input line and
// the instant it was armed.
func armMonitor(u *unitState, start *conversationv1.AgentMonitorStart) {
	u.input, u.inputForm = monitorInput(start)
	if ms := start.GetStartedAtMs(); ms != 0 {
		u.startedAtMs = ms
	}
}

// monitorInput is the card's input line: the watched command as a shell line,
// or the socket URL as plain text. A call that named neither draws an empty
// line (the shim records that it left the source unstated).
func monitorInput(start *conversationv1.AgentMonitorStart) (string, inputForm) {
	switch source := start.GetSource().(type) {
	case *conversationv1.AgentMonitorStart_Command:
		return source.Command.GetCommand(), inputFormCommand
	case *conversationv1.AgentMonitorStart_Websocket:
		return source.Websocket.GetUrl(), inputFormNone
	}
	return "", inputFormNone
}

// restateMonitor takes the call a settled monitor frame RESTATES, the settle
// standing alone as every other call's does. A settle that restated no call is
// its producer's invariant violation (restatedOrHeld): drawn from the held
// start and recorded at ERROR, or, with no start held, no row at all.
func (r *resolver) restateMonitor(s *wsState, u *unitState, unitID string, call *conversationv1.AgentMonitorStart) error {
	if call != nil {
		armMonitor(u, call)
		return nil
	}
	input, err := r.restatedOrHeld(s, u, unitID, "monitor", "", u.input)
	if err != nil {
		return err
	}
	u.input = input
	return nil
}

// settleMonitorsLeftLive settles the card of every monitor that left the
// session watcher's live set while its card still read running. The vendor
// says only that the watch is gone, which is the monitor's ordinary ending,
// so the card is settled as returned; the footer retires the row on the same
// edge. A monitor never listed is not settled: it may simply not be listed YET.
func (r *resolver) settleMonitorsLeftLive(s *wsState, live sessionwatcher.LiveWorkSet) {
	now := make(map[string]struct{}, len(live.Monitors))
	for _, work := range live.Monitors {
		now[work.GetValue()] = struct{}{}
	}
	for unitID := range s.liveMonitors {
		if _, still := now[unitID]; still {
			continue
		}
		delete(s.liveMonitors, unitID)
		u, drawn := s.units[unitID]
		if !drawn || !u.monitor || u.row == nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "settleMonitorsLeftLive", "condition": "!drawn || !u.monitor || u.row == nil"})
			continue
		}
		if u.row.GetActivity().GetSimpleToolCall().GetRunning() == nil {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "settleMonitorsLeftLive", "condition": "card not running"})
			continue
		}
		row := r.toolRow(s, u.at, unitID, monitorName, returnedOutcome(u, true, nil, r.deps.Now().UnixMilli()))
		r.stampTurn(s, row, nil)
		r.upsert(s, u.at, row, true)
		r.logger(s.id).Info("daemon.feed.monitor_left_live",
			"a monitor left the live set with its card running; the card is settled as ended",
			dlog.Context{"unit": unitID, "row": row.GetId().GetValue()})
	}
	for unitID := range now {
		s.liveMonitors[unitID] = struct{}{}
	}
}
