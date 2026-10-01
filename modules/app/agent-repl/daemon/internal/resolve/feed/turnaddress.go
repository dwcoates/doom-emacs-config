package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
)

// A ROW IS DRAWN WHERE ITS OWN TURN DRAWS. The prompt queue records every turn
// it opens with the address the turn draws at (wsm.Turn.Address), choosing it
// by the turn's origin: a turn a lease holder starts itself (a merge's repair
// or configured prompt) takes the standing output address, and every other
// turn -- the user's own prompts included, while a merge runs -- takes none.
// The queue hands that same address to the resolver as it records the turn
// (AddressTurn), and a replay reads it back from the record
// (Deps.TurnAddresses) into the same table, so a turn is drawn live and on
// every replay from ONE fact: what was recorded for it.
//
// A row of an addressed turn goes on the addressed feed under the addressed
// row, whichever agent drew it. A row of any other turn, or of no turn, goes
// on the root feed and its agents' sub-feeds. NOTHING IS COPIED BETWEEN FEEDS:
// a row stands on exactly one.

// AddressTurn records the address TURN draws at; nil draws it on the root
// feed.
func (r *resolver) AddressTurn(ws ids.WorkspaceID, turn ids.TurnID, addr *sessionwatcher.OutputAddress) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	s.addressTurn(turn, addr)
	target := "root"
	if addr != nil {
		target = r.feedKey(ws, addr.Feed)
	}
	r.logger(ws).Debug("daemon.feed.turn_address",
		"the feed took the address a turn draws at", dlog.Context{"turn": string(turn), "feed": target})
}

// addressTurn stores a copy of the address TURN draws at, or forgets the turn
// when it has none.
func (s *wsState) addressTurn(turn ids.TurnID, addr *sessionwatcher.OutputAddress) {
	if turn == "" {
		return
	}
	if addr == nil {
		delete(s.turnAddresses, turn)
		return
	}
	copied := *addr
	s.turnAddresses[turn] = &copied
}

// addressedPlacement answers where a row of TURN is drawn when the turn is
// addressed, and false when it is not (no turn, or a turn drawn on the root).
//
// AN ADDRESSED TURN OUTLIVES THE ADDRESS IT WAS RECORDED AT. A merge that
// ends while its turn still runs (an eviction mid-repair) withdraws the
// standing address, but the turn's late frames are still that turn's: they
// go to its tab, where every replay of the turn draws them too. That is said
// once per turn, at INFO.
func (r *resolver) addressedPlacement(s *wsState, turn *ids.TurnID) (placement, bool) {
	if turn == nil {
		return placement{}, false
	}
	addr, addressed := s.turnAddresses[*turn]
	if !addressed {
		return placement{}, false
	}
	if s.address == nil && !s.plane.replayed() && !s.outlivedReported[*turn] {
		s.outlivedReported[*turn] = true
		r.logger(s.id).Info("daemon.feed.turn_address_outlived",
			"a frame of an addressed turn arrived after the address it was recorded at was withdrawn; it is drawn at the turn's recorded address",
			dlog.Context{"turn": string(*turn), "feed": r.feedKey(s.id, addr.Feed)})
	}
	r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "the turn is addressed"})
	at := placement{feed: addr.Feed}
	if addr.Parent != nil {
		at.parent = &frontendv1.FeedRowParent{Row: r.deps.Encode(*addr.Parent)}
	}
	return at, true
}

// turnOf answers a wire turn id as the turn it names, nil when it names none.
func turnOf(turn *conversationv1.TurnId) *ids.TurnID {
	value := turn.GetValue()
	if value == "" {
		return nil
	}
	named := ids.TurnID(value)
	return &named
}
