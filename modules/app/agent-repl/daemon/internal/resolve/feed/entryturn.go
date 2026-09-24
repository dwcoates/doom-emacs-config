package feed

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// ATTRIBUTION BY TURN ID. Every entry the shim serves carries the turn it was
// produced within (conversation.v1 HistoryEntryAt.turn), and a row, a
// terminal, a turn's usage and its evidence are charged to THAT turn — never to
// whichever turn happens to be in flight or was last prompted on the page.
//
// An entry with NO stamp is old data (or one no producer could name). It keeps
// the positional attribution the feed used before stamps existed — the turn in
// flight live, the page's last prompt on replay — and a replay that leaned on
// it says so once, at INFO.

// drawingEntry puts one entry's stamp in force for the duration of drawing it
// and answers the restore, so every family the entry reaches reads the same
// turn. An unset stamp leaves no turn in force, which is what selects the
// positional fallback.
func (s *wsState) drawingEntry(turn *conversationv1.TurnId) func() {
	prior := s.entryTurn
	s.entryTurn = nil
	if value := turn.GetValue(); value != "" {
		stamped := ids.TurnID(value)
		s.entryTurn = &stamped
	}
	return func() { s.entryTurn = prior }
}

// rowTurn is the turn a row being drawn belongs to: the entry's own stamp, else
// the turn rows are positionally attributed to (turnStamp).
func (s *wsState) rowTurn() *ids.TurnID {
	if s.entryTurn != nil {
		return s.entryTurn
	}
	return s.turnStamp
}

// evidenceTurn is the turn a fact about a running turn (evidence, a refusal, a
// directive's cut) belongs to: the entry's own stamp, else the turn in flight.
func (s *wsState) evidenceTurn() *ids.TurnID {
	if s.entryTurn != nil {
		return s.entryTurn
	}
	return s.turnInFlight
}

// knowTurn records that a turn was OPENED where this resolver could see it: its
// prompt was drawn, or the daemon handed it over. Only these make a turn known;
// a stamp on any other entry is that entry's claim about a turn, not proof one
// was opened.
func (s *wsState) knowTurn(turn ids.TurnID) {
	if turn != "" {
		s.knownTurns[turn] = true
	}
}

// replayStampKnown judges one replayed entry's stamp against the turns this
// feed has seen opened, and reports whether its turn is one it can draw for.
//
// A PAGE CAN OPEN MID-TURN. Entries at its head — before the page has drawn any
// prompt of the main agent — can belong to a turn whose prompt is older than
// the page, and that is not a gap unless the page claims to reach the book's
// floor. Such a turn is excused at DEBUG and remembered, so the rest of its
// entries (a child's book included) are not judged again.
//
// ANY OTHER UNKNOWN STAMP IS A GAP: the record names a turn whose prompt the
// book does not carry where it must. That is the store or a producer losing a
// prompt, and it is an ERROR, recorded once per turn. The entry is still drawn
// under its own turn — its stamp is the one fact about it that is certain.
func (r *resolver) replayStampKnown(s *wsState, agent *conversationv1.AgentId, turn ids.TurnID) bool {
	if s.knownTurns[turn] || s.predatesPage[turn] {
		return true
	}
	if !s.replayPromptDrawn && !s.replayAtFloor {
		s.predatesPage[turn] = true
		r.logger(s.id).Debug("daemon.feed.replayed_turn_predates_page",
			"a replayed entry belongs to a turn whose prompt is older than the page",
			dlog.Context{"turn": string(turn), "agent": agent.GetValue()})
		return true
	}
	if !s.unknownTurnsReported[turn] {
		s.unknownTurnsReported[turn] = true
		r.logger(s.id).Error("daemon.feed.replayed_turn_unknown",
			"a replayed entry names a turn whose prompt its book does not carry; it is drawn under its own turn",
			dlog.Context{"turn": string(turn), "agent": agent.GetValue(), "at_floor": s.replayAtFloor})
	}
	return false
}
