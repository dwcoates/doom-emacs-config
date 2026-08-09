package sessioncontroller

// interruptunacked.go — A USER'S STOP THAT WAS DELIVERED AND NEVER ANSWERED.
//
// # The window
//
// A user-commanded stop is a correlated control exchange: the daemon writes an
// Interrupt and blocks for the shim's Ack, which carries the OUTCOME — the only
// place the daemon can learn whether a turn was stopped, was already over, or
// could not be reached. When the UDS connection dies between the write and the
// receipt, the exchange fails with shimclient.ErrDeliveredUnacked and the daemon
// has the request's fate and not its outcome.
//
// # What is nevertheless known, and what is not
//
// KNOWN: the shim received it. The shim's control dispatch is SYNCHRONOUS — it
// runs the SDK abort and only then hands back a receipt (uds/server.ts dispatch,
// uds/control.ts handleInterrupt) — so an Interrupt that reached the socket has
// already had its effect on the query, whatever became of the reply. The shim
// completes the interrupt locally by construction; the daemon losing the answer
// changes nothing about the abort.
//
// NOT KNOWN: which of the three outcomes it was. So nothing that depends on the
// outcome is applied: no `interrupted` turn mark (that names a verdict this
// exchange never produced), and no footer interrupt window (its whole content is
// the outcome).
//
// # What IS applied, and why it follows from delivery alone
//
// THE QUEUE PAUSES. The user asked the work to stop and the request reached the
// agent; draining the next held prompt into that silence starts exactly the work
// they stopped, and it would do so on the reattach, seconds later, with nothing
// on screen to explain it. The pause follows from the stop having been
// delivered, not from what it did, which is why it is the one consequence taken
// here.
//
// # Resolution, and the one thing that must never happen
//
// NO SECOND INTERRUPT IS EVER SENT. Re-asking is the obvious way to recover the
// lost answer and it is unusable: by the time the shim is reachable again the
// turn the user stopped may be over and a NEW one running, so the retry would
// stop work nobody asked to stop. The outcome is recovered instead from evidence
// that already exists — the reattach hello, and the durable turn record the
// handshake reconciliation consults (turnlifecycle.go, durableturnevidence.go) —
// which is exactly the machinery a bounce already runs. This file adds a reading
// of that reconciliation, not a second mechanism beside it.

// unackedInterrupt is one delivered-but-unanswered user stop, held from the
// failed exchange to the next handshake.
type unackedInterrupt struct {
	// requestID is the FRONTEND command's own id, so the resolution can be
	// joined to the stop the user pressed.
	requestID string
	// turnIDs are the turns the session was observed to hold when the stop went
	// out. They are what the reattach hello is read against: a hello that no
	// longer names them says the stop (or the disconnect) ended them, and a
	// hello that still does says the turn outlived both.
	turnIDs []string
	atMs    int64
}

// noteUnackedInterrupt records a delivered stop whose Ack was lost and pauses
// the queue on it. Caller must NOT hold m.mu.
func (m *Manager) noteUnackedInterrupt(d *sessionController, requestID string, cause error) {
	// THE DURABLE LEDGER NAMES THE TARGET, not process memory. The session
	// controller's own record holds at most one turn and cannot name an adopted
	// or accepted one at all, while the reattach hello this is read against
	// enumerates every turn the shim holds — so the two must be compared on the
	// same vocabulary or the comparison decides nothing.
	turnIDs, err := m.cfg.SSM.ActiveTurnIDs(d.workspace, d.sessionID)
	if err != nil {
		// Never swallowed, and never a reason to skip the pause: the stop was
		// delivered whether or not the daemon can say which turns it hit, and
		// the resolution below degrades to "cannot tell" rather than to silence.
		m.logf("session-controller: reading the turns a delivered-but-unanswered stop targeted FAILED ws=%q session=%s request_id=%s: %v — the queue still pauses on the stop, and the reattach cannot report which turns it ended",
			d.workspace, d.sessionID, requestID, err)
	}
	m.mu.Lock()
	d.unackedInterrupt = &unackedInterrupt{requestID: requestID, turnIDs: turnIDs, atMs: m.now()}
	d.paused = true
	held := len(d.queue.entries)
	m.mu.Unlock()
	m.logf("session-controller: user stop DELIVERED BUT UNANSWERED ws=%q session=%s request_id=%s turns=%s held=%d cause=%v — the shim ran the interrupt before the connection died, so the queue PAUSES on the stop the user asked for; the outcome is recovered from the reattach handshake and the durable record, and NO second interrupt is ever sent",
		d.workspace, d.sessionID, requestID, formatTurnIDs(turnIDs), held, cause)
}

// resolveUnackedInterrupt reads a pending unanswered stop against the reattach
// handshake's own result and records what became of it. Caller must NOT hold
// m.mu.
//
// It writes no state. Every state consequence the reattach has was already
// applied by the parties that own it: the durable claim by the handshake
// reconciliation (settled COMPLETED where the store proves the turn finished,
// cut where it does not), and the queue's pause by the failed exchange itself.
// What is left is the ACCOUNT — which of the three outcomes the lost Ack would
// have carried — and it is written here because nothing else is in a position
// to join the stop to the reattach that answered it.
func (m *Manager) resolveUnackedInterrupt(d *sessionController, helloTurnIDs []string) {
	m.mu.Lock()
	pending := d.unackedInterrupt
	d.unackedInterrupt = nil
	paused := d.paused
	m.mu.Unlock()
	if pending == nil {
		return
	}
	surviving := intersectTurnIDs(pending.turnIDs, helloTurnIDs)
	if len(surviving) > 0 {
		// THE TURN OUTLIVED THE STOP. Loud, and deliberately inert: the user's
		// stop is not re-delivered, because the daemon cannot tell this turn
		// from a new one the user has since started and stopping the wrong turn
		// is worse than reporting an ineffective stop.
		m.logf("session-controller: user stop APPARENTLY INEFFECTIVE ws=%q session=%s request_id=%s stopped_turns=%s still_running=%s paused=%v — the reattaching shim still names turns this stop targeted; NO second interrupt is sent, because a stop replayed onto a reattach can land on work the user never asked to stop",
			d.workspace, d.sessionID, pending.requestID,
			formatTurnIDs(pending.turnIDs), formatTurnIDs(surviving), paused)
		return
	}
	m.logf("session-controller: user stop RESOLVED BY REATTACH ws=%q session=%s request_id=%s stopped_turns=%s hello_turns=%s paused=%v — the returning shim names none of the turns the stop targeted, so the lost Ack is answered by the handshake reconciliation rather than by asking again: each of those turns is settled COMPLETED where the store proves it finished and cut as interrupted where it does not",
		d.workspace, d.sessionID, pending.requestID,
		formatTurnIDs(pending.turnIDs), formatTurnIDs(helloTurnIDs), paused)
}

// intersectTurnIDs reports the members of want that also appear in have, in
// want's order.
func intersectTurnIDs(want, have []string) []string {
	if len(want) == 0 || len(have) == 0 {
		return nil
	}
	present := make(map[string]struct{}, len(have))
	for _, id := range have {
		present[id] = struct{}{}
	}
	var out []string
	for _, id := range want {
		if _, ok := present[id]; ok {
			out = append(out, id)
		}
	}
	return out
}
