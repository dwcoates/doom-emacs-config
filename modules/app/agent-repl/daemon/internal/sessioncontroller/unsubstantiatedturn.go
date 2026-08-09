package sessioncontroller

import (
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// unsubstantiatedturn.go — NARROWING THE LIVE-CONTROLLER DECLINE.
//
// # The deadlock
//
// A turn claim is retired by an OBSERVATION, and the two reconciliations that
// can synthesize one both refuse while anything is alive:
//
//   - hibernation is refused outright while `turn_active` stands, and
//   - the orphaned-turn reconciliation DECLINES while a controller drives the
//     workspace, because a live controller might be driving a real turn.
//
// A shim that is CONNECTED BUT WEDGED satisfies both refusals forever. Its
// query died without producing a terminal, so no `TurnEnded` is coming; its
// controller is alive, so the orphan proof can never be assembled; and the
// standing claim then refuses every hibernation and every restart guard for as
// long as the daemon lives. Observed live across several workspaces at once,
// with deploys refused on a "turn in flight" that had been over for hours.
//
// # The evidence that narrows it, and why nothing weaker will do
//
// The decline is RIGHT by default: a live controller usually IS driving a real
// turn, and a claim closed under one paints a running turn idle. So it is
// narrowed only by evidence the SHIM ITSELF produced, and two facts must hold
// together:
//
//  1. THE SHIM ACKED A STOP AND THEN NEVER ANSWERED IT. An
//     `INTERRUPT_OUTCOME_INTERRUPTED` ack is a positive claim that a turn was
//     live and has now been aborted, so a terminal is owed. The mark that ack
//     leaves is spent by the stopped turn's own end and dropped by any later
//     start, so a mark STILL STANDING long afterwards is the shim contradicting
//     its own ack. That is the shape of the phantom turn, and it is the only
//     evidence obtainable without asking the shim to stop something again.
//  2. NOTHING HAS HAPPENED ON THE WORKSPACE SINCE. The state log is already the
//     activity record — a turn boundary, a bring-up, a permission, a task all
//     append to it, and nothing appends on a timer — so a log whose newest row
//     predates the window is a workspace where the stream has produced nothing
//     at all.
//
// Either alone is too weak. A standing mark on its own could be a stop that
// landed a moment ago on a turn still winding down; quiet on its own is an agent
// thinking, which is precisely what a long turn looks like. Together they say
// the shim claimed to abort a turn and then produced neither its end nor
// anything else.
//
// # Why this is not a destructive probe
//
// The obvious way to ask a live shim whether a turn is in flight is to interrupt
// it: `ALREADY_COMPLETE` would settle the question outright. It is also
// unusable, because the other answer KILLS A LIVE TURN — a reconciliation that
// stops the user's work to find out whether the user had work is worse than the
// wedge it cures. So the evidence used here is an interrupt THE USER ALREADY
// ASKED FOR, whose answer the shim already gave and then failed to honour.
//
// # It never deletes the decline
//
// A workspace with no standing mark, or one whose log moved inside the window,
// takes the original decline unchanged. The narrowing adds a proof; it does not
// weaken the rule.

// unansweredInterruptWindow is how long a shim's own INTERRUPTED ack may go
// unanswered before the claim behind it is judged unsubstantiated.
//
// The shim gives ITSELF fifteen seconds to produce the terminal an interrupt ack
// owes before synthesizing one (`interruptTerminalGraceMs`). Five minutes is
// twenty times that, and it is deliberately far past it: this is the daemon
// second-guessing the only party that can see the process, so it must be wrong
// only when the shim is provably not answering at all — not when it is slow, not
// when a store write is retrying, and not when a rotation is in flight.
const unansweredInterruptWindow = 5 * time.Minute

// staleTurnReasonUnsubstantiated names this reconciliation in the closing row's
// cause.
//
// It is distinct from staleTurnReasonOrphaned because the PROOF is different and
// a reader must be able to tell them apart. The orphaned reason means nothing is
// alive here at all; this one means something IS alive and has contradicted its
// own claim, which is a shim defect rather than a vanished process, and the two
// call for entirely different investigations.
const staleTurnReasonUnsubstantiated = "unsubstantiated_turn_shim_never_answered_its_own_interrupt"

// reconcileUnsubstantiatedTurn closes a live workspace's standing turn claim
// when the SHIM ITSELF has contradicted it, and reports whether it did.
//
// Called from reconcileOrphanedTurn's live-controller branch, with the manager
// mutex RELEASED.
func (m *Manager) reconcileUnsubstantiatedTurn(workspace string, d *sessionController, st *frontendv1.WorkspaceState) bool {
	ageMs, marked := m.cfg.SSM.UnansweredInterruptAgeMs(workspace)
	if !marked {
		// No stop was ever acked here, so there is no claim the shim has made
		// about this turn for it to be contradicting. Not evidence of anything.
		return false
	}
	windowMs := unansweredInterruptWindow.Milliseconds()
	if ageMs < windowMs {
		m.logf("session-controller: unsubstantiated-turn reconciliation DECLINED ws=%q session=%s interrupt_unanswered_ms=%d window_ms=%d — the shim acked a stop recently enough that the turn it aborted may still be producing its end",
			workspace, d.sessionID, ageMs, windowMs)
		return false
	}
	lastMs, known, err := m.cfg.SSM.LastActivityMs(workspace)
	if err != nil {
		m.logf("session-controller: unsubstantiated-turn reconciliation REFUSED ws=%q session=%s — cannot read when anything last happened here (%v). A turn is never guessed dead, so the claim stands.",
			workspace, d.sessionID, err)
		return false
	}
	if !known {
		// EVERY UNKNOWN ANSWERS NO. A workspace with no state history at all is
		// one this reconciliation knows nothing about, and reading an absent
		// answer as a very old one is how a sweeper reaps what it knows least.
		m.logf("session-controller: unsubstantiated-turn reconciliation REFUSED ws=%q session=%s — the log carries no activity record at all, so quiet cannot be distinguished from unknown",
			workspace, d.sessionID)
		return false
	}
	quietMs := m.now() - lastMs
	if quietMs < windowMs {
		m.logf("session-controller: unsubstantiated-turn reconciliation DECLINED ws=%q session=%s interrupt_unanswered_ms=%d quiet_ms=%d window_ms=%d — something moved on this workspace inside the window, so the stream behind the claim is not silent",
			workspace, d.sessionID, ageMs, quietMs, windowMs)
		return false
	}
	m.logf("session-controller: turn claim UNSUBSTANTIATED ws=%q session=%s state=%s interrupt_unanswered_ms=%d quiet_ms=%d window_ms=%d — the shim acked a stop as INTERRUPTED, which is a claim that a turn was live and has been aborted, and then produced neither that turn's end nor any other activity for the whole window; a live controller is not a shield against its own shim's contradiction, so the claim is reconciled",
		workspace, d.sessionID, st.GetState(), ageMs, quietMs, windowMs)
	// THE SAME CLOSE THE ORPHAN PROOF TAKES, and deliberately so: there is
	// exactly one writer of a synthesized closing row, and only the REASON
	// distinguishes the proof that licensed it. The scope is the workspace's
	// because the contradiction is about the workspace's stream, not about which
	// daemon generation happens to hold the row.
	closed, err := m.cfg.SSM.CloseOrphanedTurn(workspace, st.GetSessionId(), staleTurnReasonUnsubstantiated)
	if err != nil {
		m.logf("session-controller: unsubstantiated-turn reconciliation FAILED ws=%q session=%q reason=%s: %v — the workspace stays latched in a turn it cannot leave",
			workspace, st.GetSessionId(), staleTurnReasonUnsubstantiated, err)
		return false
	}
	if !closed {
		m.logf("session-controller: unsubstantiated-turn reconciliation WROTE NOTHING ws=%q session=%q reason=%s — the workspace held no open claim and its axis carried no live turn, so there was nothing to reconcile",
			workspace, st.GetSessionId(), staleTurnReasonUnsubstantiated)
		return false
	}
	return true
}
