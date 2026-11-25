package feed

import (
	"context"

	"agentrepl/logging/logsite"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// THE NETWORK COMING BACK ANSWERS A CONNECTION FAILURE. A turn whose request
// never reached the API ends on a "connection failed" card (marker.go), and
// the session's network fault stands from the same failure until the API
// answers again (ladder.NetworkTurnFault). When the workspace's last network
// fault closes (NetworkFaultClosed), every connection-failed card drawn
// before it leaves the feed: the connection is back, and a card still on
// screen would report a fault that no longer stands.
//
// THE REFUSAL LIVES AT THE ONE UPSERT DOOR (reconnectedRow), as a sign-in's
// does (signin.go), so a live re-delivery, a history page replayed after a
// restart and a page loaded on scroll-back all stay retired. The answered
// turns are recorded durably and loaded when a process first builds the
// workspace's feed state.
//
// A LATER SERVED TURN answers the same card from the feed itself
// (vendorrecovery.go), which needs no fault edge at all.

// ReconnectedTurnStore is the durable record of turns whose connection
// failure the network coming back answered (wsm.DB).
type ReconnectedTurnStore interface {
	RecordReconnectedTurns(ctx context.Context, id ids.WorkspaceID, turns []ids.TurnID) error
	ReconnectedTurns(ctx context.Context, id ids.WorkspaceID) ([]ids.TurnID, error)
}

const (
	opReconnected        = "daemon.feed.reconnected"
	opReconnectedRestore = "daemon.feed.reconnected_restore"
)

// unreachableEnding reports whether ROW is a turn's errored ending on a
// request that never reached the API: the connection-failed card.
func unreachableEnding(row *frontendv1.FeedRow) bool {
	return row.GetTurnEnded().GetErrored().GetUnreachable() != nil
}

// reconnectedRow reports whether ROW is the connection-failed card of a turn
// the network coming back answered, which the upsert door refuses to draw.
func (r *resolver) reconnectedRow(s *wsState, f *feedState, row *frontendv1.FeedRow) bool {
	turn := row.GetTurn().GetValue()
	if turn == "" || !s.reconnected[ids.TurnID(turn)] || !unreachableEnding(row) {
		return false
	}
	r.logger(s.id).Debug("daemon.feed.reconnected_row_refused",
		"a connection-failed card of a turn the network coming back answered was not drawn",
		dlog.Context{"feed": f.key, "row": row.GetId().GetValue(), "turn": turn})
	return true
}

// NetworkFaultOpened takes a network fault standing for the workspace, by its
// record id.
func (r *resolver) NetworkFaultOpened(ws ids.WorkspaceID, id string) {
	r.prime(ws)
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.networkFaults[id] = true
	r.logger(ws).Debug(opReconnected, "the feed took a standing network fault",
		dlog.Context{"fault": id, "standing": len(s.networkFaults)})
}

// NetworkFaultClosed retracts a network fault by id; one the feed does not
// hold changes nothing. When the last one closes the connection is back:
// every drawn connection-failed card is retired from every feed and refused
// from now on, and its turn recorded so a later process refuses it too. The
// removal always happens; a recording that fails is logged at ERROR and
// raised on the topbar, since a daemon restart would draw those cards again.
func (r *resolver) NetworkFaultClosed(ws ids.WorkspaceID, id string) {
	r.prime(ws)
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	log := r.logger(ws)
	if !s.networkFaults[id] {
		log.Debug(opReconnected, "a fault the feed does not hold as a network fault closed; nothing changed",
			dlog.Context{"fault": id})
		return
	}
	delete(s.networkFaults, id)
	if len(s.networkFaults) > 0 {
		log.Debug(opReconnected, "a network fault closed while another stands; the connection is not back yet",
			dlog.Context{"fault": id, "standing": len(s.networkFaults)})
		return
	}
	s.networkBackAtMs = r.deps.Now().UnixMilli()
	answer := turnAnswer{
		op:       opReconnected,
		names:    unreachableEnding,
		retires:  unreachableEnding,
		into:     s.reconnected,
		fields:   dlog.Context{"fault": id},
		nothing:  logsite.Msg("the connection came back and no connection-failed card was drawn; nothing was retired"),
		failed:   logsite.Msg("the connection came back and its connection-failed cards were removed but could not be recorded; a daemon restart will draw them again"),
		answered: logsite.Msg("the connection came back; the connection-failed cards drawn before it were retired and recorded"),
	}
	if r.deps.Reconnected != nil {
		answer.record = r.deps.Reconnected.RecordReconnectedTurns
	}
	// The recording's failure is logged and raised where it runs, and the
	// fault sink this edge comes from has no caller to return it to.
	r.answerTurns(s, answer)
}

// answerLateUnreachable answers ROW, a turn's ending about to be drawn, when it
// is a connection-failed card whose failure the network's return already
// answered: it failed (FeedTurnEnded.ended_at_ms, the failure's own record
// time) at or before the last network fault closed. Its turn joins the
// answered set, so the upsert door refuses the card as it refuses one drawn
// before the return, and is recorded so a later process refuses it too. A
// card that failed after the return is a new outage's, and stands.
//
// THE RETURN'S INSTANT IS THIS PROCESS'S: a terminal first delivered after a
// daemon restart that followed the return is not judged by it, and stands
// until a later served turn answers it (vendorrecovery.go).
func (r *resolver) answerLateUnreachable(s *wsState, row *frontendv1.FeedRow) {
	turn := ids.TurnID(row.GetTurn().GetValue())
	endedAt := row.GetTurnEnded().GetEndedAtMs()
	if !unreachableEnding(row) || s.networkBackAtMs == 0 || endedAt > s.networkBackAtMs || s.reconnected[turn] {
		return
	}
	s.reconnected[turn] = true
	log := r.logger(s.id)
	fields := dlog.Context{"turn": string(turn), "failed_at_ms": endedAt, "network_back_at_ms": s.networkBackAtMs}
	answered := func() {
		log.Info(opReconnected, "a connection-failed card that failed before the network came back arrived late; it was answered and not drawn", fields)
	}
	if r.deps.Reconnected == nil {
		answered()
		return
	}
	ws := s.id
	r.writeAfterUnlock(func(ctx context.Context) error {
		return r.deps.Reconnected.RecordReconnectedTurns(ctx, ws, []ids.TurnID{turn})
	}, func(err error) {
		fields["error"] = err.Error()
		log.Error(opReconnected, "a late connection-failed card the network's return answered was not drawn but could not be recorded; a daemon restart will draw it again", fields)
	}, answered)
}

// restoreReconnected loads the workspace's reconnection-answered turns when a
// process first builds its feed state, before any row is drawn.
func (r *resolver) restoreReconnected(s *wsState, primed *primedWorkspace) {
	if r.deps.Reconnected == nil {
		return
	}
	r.restoreTurnSet(s, turnSetRestore{
		op:     opReconnectedRestore,
		read:   primed.turnSets[turnSetReconnected],
		failed: logsite.Msg("the workspace's reconnection-answered turns could not be loaded; their connection-failed cards may be drawn again"),
		loaded: logsite.Msg("the workspace's reconnection-answered turns were loaded; their connection-failed cards are not drawn"),
	}, s.reconnected)
}
