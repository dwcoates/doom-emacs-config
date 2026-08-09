package sessioncontroller

import (
	"context"

	"claude-repld/internal/ssm"
)

// mergeturnstop.go — THE MERGE RUN'S CLOSING EDGE OVER ITS OWN TURN.
//
// # The leak
//
// SubmitMergePromptAwaitingTurn binds a waiter to the turn its prompt started
// and then waits for that turn's end. One exit leaves the turn behind: the work
// phase's `ctx.Done()` arm, where the run is being abandoned while the turn it
// started is STILL RUNNING. The wait ends, the run reaches a terminal, the merge
// lease is released — and the turn goes on, with its claim open, holding the
// workspace `thinking` for as long as the shim lives. Observed live as
// `merge-resume:q_8ddc6f0fe33bef58be0af777` still open after a `merge_failed`.
//
// # Why the turn is stopped rather than merely disowned
//
// It is the MERGE'S work, not the user's. It runs under the merge's own lease,
// which refuses every user prompt for its duration, and it was submitted with a
// machine attribution. Once the run terminates, that turn's output has no
// consumer: nothing is waiting for it, the cherry-pick it was resolving will not
// be resumed, and the lease release hands the workspace straight back to a user
// who would be fighting a ghost turn for their own shim. Leaving it running is
// not conservatism, it is abandoning work in a session somebody else is about to
// use.
//
// Closing the CLAIM without stopping the TURN would be worse still: the ledger
// would say idle while an agent went on editing the tree.
//
// # The boundary, and how it is kept honest
//
// This fires for exactly ONE turn id — the waiter's own bound id — and never for
// whatever the workspace happens to be running. The lease makes a foreign turn
// unlikely, but "unlikely" is not the standard: the ledger is consulted first,
// and an id that is no longer among this session's open claims is a turn that
// already ended, so nothing is interrupted. An UNREADABLE ledger stops the whole
// path rather than licensing a guess, because the failure it would admit is
// interrupting a user's turn.
//
// # Why the interrupt is preferred to a synthesized close
//
// The interrupt produces the turn's OWN terminal through the ordinary lifecycle,
// which is the honest close and the one every consumer already understands. The
// synthesized close is the fallback for the case where that route does not
// exist: an unreachable shim, a nack, an ack that never comes. Only then is
// ssm.TurnCloseMergeRunTerminal written, and it says exactly which of the two
// happened.

// stopAbandonedMergeTurn ends the turn a merge run started and then abandoned.
//
// Called with the caller's context ALREADY CANCELLED, which is why it takes none
// and builds its own: an interrupt sent on a dead context has nothing to travel
// over, and this path exists precisely for the case where the caller gave up.
func (m *Manager) stopAbandonedMergeTurn(d *sessionController, workspace, requestID, turnID string) {
	if turnID == "" {
		// Unreachable from the work phase, whose entry condition is a bound
		// waiter, and stated rather than assumed: a stop that could not name its
		// turn would have to interrupt whatever was running.
		m.logf("session-controller: abandoned merge turn NOT STOPPED ws=%q session=%s request_id=%s — the waiter bound no turn id, so there is no turn of this run's own to stop and nothing may be interrupted on its behalf",
			workspace, d.sessionID, requestID)
		return
	}
	open, err := m.cfg.SSM.ActiveTurnIDs(workspace, d.sessionID)
	if err != nil {
		m.logf("session-controller: abandoned merge turn NOT STOPPED ws=%q session=%s request_id=%s turn_id=%q: cannot read which turns this session holds open (%v) — the stop is refused rather than aimed at an unverified turn, because the failure that would admit is interrupting the user's own work",
			workspace, d.sessionID, requestID, turnID, err)
		return
	}
	if !containsTurnID(open, turnID) {
		m.logf("session-controller: abandoned merge turn ALREADY ENDED ws=%q session=%s request_id=%s turn_id=%q open_claims=%v — the turn closed between the wait giving up and this stop, so nothing is interrupted",
			workspace, d.sessionID, requestID, turnID, open)
		return
	}
	// A FRESH CONTEXT, deliberately not the caller's. The caller's is cancelled
	// by definition here, and the merge's root context is cancelled by a daemon
	// shutdown — which is exactly the run whose turn most needs stopping.
	bound := m.drainTimeout()
	ctx, cancel := context.WithTimeout(context.Background(), bound)
	defer cancel()
	outcome, err := d.client.Interrupt(ctx)
	if err == nil {
		m.logf("session-controller: abandoned merge turn INTERRUPTED ws=%q session=%s request_id=%s turn_id=%q outcome=%s — the run is over and its turn had no consumer left, so the shim's own terminal closes the claim through the ordinary lifecycle",
			workspace, d.sessionID, requestID, turnID, outcome)
		return
	}
	m.logf("session-controller: abandoned merge turn interrupt FAILED ws=%q session=%s request_id=%s turn_id=%q outcome=%s timeout=%s: %v — the shim cannot produce this turn's terminal, so the claim is closed durably instead",
		workspace, d.sessionID, requestID, turnID, outcome, bound, err)
	closed, closeErr := m.cfg.SSM.CloseOriginTurns(workspace, []string{turnID}, ssm.TurnCloseMergeRunTerminal)
	if closeErr != nil {
		m.logf("session-controller: abandoned merge turn CLAIM CLOSE FAILED ws=%q session=%s request_id=%s turn_id=%q: %v — the workspace stays latched in a turn whose run has already terminated",
			workspace, d.sessionID, requestID, turnID, closeErr)
		return
	}
	m.logf("session-controller: abandoned merge turn CLAIM CLOSED ws=%q session=%s request_id=%s turn_id=%q closed=%v cause=%s — the shim was unreachable, so the daemon wrote the end the abandoned turn cannot",
		workspace, d.sessionID, requestID, turnID, closed, ssm.TurnCloseMergeRunTerminal)
}

// containsTurnID reports whether ids names turnID.
func containsTurnID(ids []string, turnID string) bool {
	for _, id := range ids {
		if id == turnID {
			return true
		}
	}
	return false
}
