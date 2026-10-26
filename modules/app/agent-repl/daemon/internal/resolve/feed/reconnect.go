package feed

import (
	"context"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
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
	var turns []ids.TurnID
	r.eachStoredRow(s, func(_ feedid.Feed, _ *feedState, _ string, row *frontendv1.FeedRow) {
		if turn := row.GetTurn().GetValue(); unreachableEnding(row) && turn != "" {
			turns = append(turns, ids.TurnID(turn))
		}
	})
	if len(turns) == 0 {
		log.Debug(opReconnected, "the connection came back and no connection-failed card was drawn; nothing was retired",
			dlog.Context{"fault": id})
		return
	}
	for _, turn := range turns {
		s.reconnected[turn] = true
	}
	retired := 0
	r.eachStoredRow(s, func(addr feedid.Feed, _ *feedState, rowID string, row *frontendv1.FeedRow) {
		if s.reconnected[ids.TurnID(row.GetTurn().GetValue())] && unreachableEnding(row) && r.retire(s, addr, rowID) {
			retired++
		}
	})
	fields := dlog.Context{"fault": id, "turns": len(turns), "first": firstTurn(turns), "retired": retired}
	if r.deps.Reconnected != nil {
		if err := r.deps.Reconnected.RecordReconnectedTurns(context.Background(), ws, turns); err != nil {
			fields["error"] = err.Error()
			log.Error(opReconnected,
				"the connection came back and its connection-failed cards were removed but could not be recorded; a daemon restart will draw them again", fields)
			r.raiseWarning(s, opReconnected,
				fmt.Sprintf("the answered connection failures could not be saved and will reappear after a daemon restart: %v", err))
			return
		}
	}
	log.Info(opReconnected, "the connection came back; the connection-failed cards drawn before it were retired and recorded", fields)
}

// restoreReconnected loads the workspace's reconnection-answered turns when a
// process first builds its feed state, before any row is drawn.
func (r *resolver) restoreReconnected(s *wsState) {
	if r.deps.Reconnected == nil {
		return
	}
	r.restoreTurnSet(s, turnSetRestore{
		op:      opReconnectedRestore,
		load:    r.deps.Reconnected.ReconnectedTurns,
		failed:  "the workspace's reconnection-answered turns could not be loaded; their connection-failed cards may be drawn again",
		warning: "answered connection failures could not be loaded and may be drawn again",
		loaded:  "the workspace's reconnection-answered turns were loaded; their connection-failed cards are not drawn",
	}, s.reconnected)
}
