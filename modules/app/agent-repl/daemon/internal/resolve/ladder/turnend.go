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
	case wsm.CloseFolded:
		// A FOLDED PROMPT'S TURN NEVER RAN, so it has no end to read: the turn
		// it joined ends for it. No caller asks, and one that did is told the
		// close is not a turn end.
		return 0, false
	default:
		return 0, false
	}
}

// TurnFault is the fault a turn's end raises on the workspace status (owner
// ruling, 2026-10-06): the domain whose machinery ended the turn, or none.
// The footer and the roster both read it from ResolveTurnFault, so the strip,
// the dot and the tab cannot disagree about a failed turn's fault, and the
// feed's outcome marker takes its family from the same table.
type TurnFault int

// The turn faults.
const (
	// NoTurnFault is a turn end that raises no fault: a completion, an
	// interrupt, an expected stop.
	NoTurnFault TurnFault = iota
	// VendorTurnFault is a turn the vendor ended or refused: the footer's
	// `vendor_fault`, the roster's `vendor_blocked`, turquoise.
	VendorTurnFault
	// AgentReplTurnFault is a turn agent-repl's own machinery ended (the
	// vendor query died, the agent process died): the footer's
	// `agent_repl_fault · turn_died`, the roster's `turn_died`, blue.
	AgentReplTurnFault
)

// String names a turn fault, for the record.
func (f TurnFault) String() string {
	switch f {
	case NoTurnFault:
		return "none"
	case VendorTurnFault:
		return "vendor"
	case AgentReplTurnFault:
		return "agent_repl"
	default:
		return "unknown"
	}
}

// ResolveTurnFault is THE ONE TABLE from a turn's close, and the class of the
// failure its terminal carried (NoFailure when no terminal reached the
// resolver), to the fault the turn's end raises. It reports false for a close
// this build does not know, which every caller surfaces loudly.
//
// A FAILED close is read by its class; one no terminal explained is the run
// failing with no account of why, which the feed draws as the `closed:failed`
// stop reason, a vendor turn fault. An ORPHANED close is the `closed:orphaned`
// stop reason, a vendor turn fault too: the ruling names every turn_failed
// stop reason a vendor fault. The AGENT PROCESS dying under the turn is
// agent-repl's own machinery.
//
// AN EXPECTED STOP RAISES NO FAULT on any failing close, exactly as
// ResolveTurnEnd reads it `done`: the terminal said the stop was configured or
// asked for, whatever closed the row after it.
func ResolveTurnFault(how wsm.TurnClose, class FailureClass) (TurnFault, bool) {
	switch how {
	case wsm.CloseCompleted, wsm.CloseKilled, wsm.CloseFolded:
		return NoTurnFault, true
	case wsm.CloseFailed, wsm.CloseOrphaned, wsm.CloseAgentDied:
	default:
		return NoTurnFault, false
	}
	switch {
	case class == ExpectedStop:
		return NoTurnFault, true
	case how == wsm.CloseAgentDied, class == AgentReplFailed:
		return AgentReplTurnFault, true
	default:
		return VendorTurnFault, true
	}
}
