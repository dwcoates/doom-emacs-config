package ladder

import "claude-repld/internal/wsm"

// TurnEnd is how a turn's end reads: the roster's turn-end arm, and the
// desktop banner a turn end raises. Both read it from ResolveTurnEnd, so the
// tab's colour and the banner can never disagree about how one turn ended.
type TurnEnd int

// The turn ends.
const (
	// TurnEndDone is a completion, drawn `done` (green).
	TurnEndDone TurnEnd = iota
	// TurnEndInterrupted is a user interrupt, drawn `interrupted` (green).
	TurnEndInterrupted
	// TurnEndFailed is the turn failing, drawn `turn_failed` (turquoise).
	TurnEndFailed
)

// String names a turn end: the roster arm it draws.
func (e TurnEnd) String() string {
	switch e {
	case TurnEndDone:
		return "done"
	case TurnEndInterrupted:
		return "interrupted"
	case TurnEndFailed:
		return "turn_failed"
	default:
		return "unknown"
	}
}

// ResolveTurnEnd is THE ONE TABLE from a turn's close, and the class of the
// failure its terminal carried, to how its end reads. It reports false for a
// close this build does not know, which every caller surfaces loudly.
//
// A FAILURE IS A TURN END (owner ruling, 2026-09-28): the turn failing on its
// own, a turn the daemon closed as orphaned on reconcile, and a turn the agent
// process cut by dying under it all read `turn_failed`. AN EXPECTED STOP IS A
// COMPLETION: a Stop hook that forbade continuing and a deferred tool close
// the turn as failed, but they await the human, so they read `done`.
func ResolveTurnEnd(how wsm.TurnClose, class FailureClass) (TurnEnd, bool) {
	switch how {
	case wsm.CloseCompleted:
		return TurnEndDone, true
	case wsm.CloseKilled:
		return TurnEndInterrupted, true
	case wsm.CloseFailed, wsm.CloseOrphaned, wsm.CloseAgentDied:
		if class == ExpectedStop {
			return TurnEndDone, true
		}
		return TurnEndFailed, true
	default:
		return 0, false
	}
}
