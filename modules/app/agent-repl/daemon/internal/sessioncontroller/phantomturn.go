package sessioncontroller

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/ssm"
)

// This file closes the one gap that made a turn PHANTOM: two authorities on
// "is a turn in flight" — the daemon's durable claim and the live shim — with
// no edge between them.
//
// The daemon's claim is durable on purpose: it must survive a restart, because
// a turn genuinely running behind a shim the daemon just reattached to is still
// running. The shim's hello (and its interrupt Ack) is live on purpose: it is
// the only vantage point from which the process a turn would be running in can
// be seen at all. When they disagree in the UNAMBIGUOUS direction — the shim
// says there is no turn, the ledger says there is — the shim wins, and the
// claim it contradicts is ENDED rather than left to a `TurnEnded` no process
// exists to send.
//
// WHAT THE GAP COST. A daemon restart left `daemon-prompt-1-...` claimed. The
// returning shim's hello reported turn_in_flight=false with no ids, the ledger
// kept the claim, and the session controller latched turnActive=true off it. The
// user's next prompt was therefore QUEUED, the queue interjected, the shim acked
// INTERRUPT_OUTCOME_ALREADY_COMPLETE — "there is nothing to stop" — and the
// entry then waited for the TurnEnded that reconciliation proves is never
// coming. The workspace rendered `thinking` forever and the prompt was never
// delivered.
//
// NOTHING HERE REDRIVES A PROMPT. The interrupted turn is reported as
// interrupted; whatever it was running is not silently retried. Only prompts the
// user has submitted and the queue is holding are released.

// notePhantomTurnClosed records that the handshake synthesized an end for claims
// the returning shim contradicted, so the queue can be released once the
// connection is actually driveable. Caller must NOT hold m.mu.
//
// THE RELEASE WAITS FOR ShimReady, and that is the whole reason this is a latch
// rather than an immediate drain. The handshake hook runs BEFORE DaemonHello
// opens the store subscription, so a prompt submitted from inside it would be
// handed to a shim that is not yet driveable — which the delivery path would
// (correctly) report as a failure and requeue as an ERROR entry. The boundary
// the queue needs is real either way; it is simply delivered at the first moment
// the session can carry it.
func (m *Manager) notePhantomTurnClosed(d *sessionController, closed []string) {
	if len(closed) == 0 {
		return
	}
	m.mu.Lock()
	d.phantomTurnClosed = append([]string(nil), closed...)
	held := len(d.queue.entries)
	m.mu.Unlock()
	m.logf("session-controller: phantom turn close PENDING RELEASE ws=%q session=%s closed=%s held=%d — the queue is released at ShimReady, once the session can actually carry a prompt",
		d.workspace, d.sessionID, formatTurnIDs(closed), held)
}

// releasePhantomTurn delivers the synthesized boundary to the queue, at
// ShimReady. A session with no phantom close pending does nothing.
//
// It goes through onTurnBoundary — the SAME path a real `TurnEnded` takes —
// rather than poking the latch directly, because everything that must happen at
// a turn end must happen here too: the interject waiting on exactly this
// boundary, the FIFO drain behind it, the paused queue's head jump, the queue
// view every frontend renders from.
func (m *Manager) releasePhantomTurn(d *sessionController) {
	m.mu.Lock()
	closed := d.phantomTurnClosed
	d.phantomTurnClosed = nil
	held := len(d.queue.entries)
	m.mu.Unlock()
	if len(closed) == 0 {
		return
	}
	m.logf("session-controller: phantom turn close RELEASING the queue ws=%q session=%s closed=%s held=%d — the turn these prompts were held behind was interrupted by a restart, so the boundary they are waiting for is delivered here",
		d.workspace, d.sessionID, formatTurnIDs(closed), held)
	// A SYNTHESIZED boundary has no event behind it and therefore no instant of
	// its own: the turn it closes was interrupted by a restart and never
	// reported an end. Now is the only instant the daemon can honestly claim
	// the boundary happened at, and it is stated here rather than defaulted
	// silently inside the boundary handler.
	m.onTurnBoundary(d, false, m.now())
}

// settleInterjectAlreadyComplete reconciles BOTH axes to an interject's
// ALREADY_COMPLETE Ack and then releases the queue.
//
// The Ack is a live shim statement that no foreground turn exists. The interject
// that sent the Interrupt is, by construction, waiting for a TurnEnded before it
// submits — so if the daemon believes the Ack and does nothing else, the entry
// waits forever on a turn the shim has just said is not there. That is the
// wedge, and this is its edge:
//
//   - the STATUS axis is reconciled by ReconcileAlreadyComplete, which retires a
//     `submitting`/`thinking`/`permission` row this session owns;
//   - the TURN LIFECYCLE is reconciled by SynthesizeTurnClose, which ends the
//     durable claim the queue is holding prompts behind;
//   - the QUEUE is then released with the same synthesized boundary, which is
//     what actually submits the prompt the user typed.
//
// A FAILURE OF EITHER RECONCILIATION IS LOUD AND DOES NOT STOP THE RELEASE. The
// shim's statement stands whether or not the daemon managed to record it, and
// stranding the user's prompt is a strictly worse outcome than a state row this
// log line accounts for.
func (m *Manager) settleInterjectAlreadyComplete(d *sessionController, entryID string) {
	publish := func(state *frontendv1.WorkspaceState) {
		d.consumer.push.PushWorkspaceState(state)
	}
	closedState, err := m.cfg.SSM.ReconcileAlreadyComplete(d.workspace, d.sessionID, publish)
	if err != nil {
		m.logf("session-controller: interject already-complete status reconciliation FAILED ws=%s session=%s entry=%s: %v — the workspace may hold `thinking` for a turn the shim says is over; releasing the queue regardless so the held prompt is not stranded",
			d.workspace, d.sessionID, entryID, err)
	}
	closedClaims, err := m.cfg.SSM.SynthesizeTurnClose(d.workspace, d.sessionID, ssm.TurnCloseAlreadyComplete)
	if err != nil {
		m.logf("session-controller: interject already-complete turn-claim close FAILED ws=%s session=%s entry=%s: %v — the durable claim may outlive the turn it names; releasing the queue regardless so the held prompt is not stranded",
			d.workspace, d.sessionID, entryID, err)
	}
	// A synthesized close is the ONLY end these turns will ever get, so it owns
	// both releases: the terminal result they left retained, and the durable
	// cursor hold their start took.
	d.consumer.ReleaseSynthesizedTurnClose(closedClaims, ssm.TurnCloseAlreadyComplete)
	d.client.UnpinAccountingTurn(closedClaims...)
	m.logf("session-controller: interject already-complete SETTLED ws=%s session=%s entry=%s status_closed=%v claims_closed=%s — the shim reports no foreground turn, so the boundary this interject is waiting for is delivered here rather than awaited forever",
		d.workspace, d.sessionID, entryID, closedState, formatTurnIDs(closedClaims))
	// Synthesized, exactly as releasePhantomTurn's is: the shim's
	// ALREADY_COMPLETE Ack says the turn is over but names no instant, so now
	// is the only one the daemon can honestly claim.
	m.onTurnBoundary(d, false, m.now())
}

// closeTurnClaimsOnAlreadyComplete ends the durable claim behind a USER-commanded
// stop the shim acked ALREADY_COMPLETE.
//
// It is the ledger half of what noteUserInterrupt already does to the status
// axis, and it deliberately does NOT release the queue: the user asked for work
// to stop, the same Ack pauses the queue, and delivering the next held prompt
// would start exactly the work they just stopped. The claim still has to go —
// nothing else can ever close it — or the next prompt queues behind a turn that
// does not exist.
//
// A failure is loud and swallowed: the caller's own outcome is the interrupt's,
// and a ledger row it failed to tidy must not replace that answer.
func (m *Manager) closeTurnClaimsOnAlreadyComplete(d *sessionController) {
	closed, err := m.cfg.SSM.SynthesizeTurnClose(d.workspace, d.sessionID, ssm.TurnCloseAlreadyComplete)
	if err != nil {
		m.logf("session-controller: user-stop already-complete turn-claim close FAILED ws=%s session=%s: %v — the durable claim may outlive the turn it names, and the next prompt could queue behind it",
			d.workspace, d.sessionID, err)
		return
	}
	if len(closed) == 0 {
		return
	}
	d.consumer.ReleaseSynthesizedTurnClose(closed, ssm.TurnCloseAlreadyComplete)
	d.client.UnpinAccountingTurn(closed...)
	m.logf("session-controller: user-stop already-complete closed the durable turn claim ws=%s session=%s closed=%s — the shim reports no foreground turn, and the queue stays PAUSED because the user asked the work to stop",
		d.workspace, d.sessionID, formatTurnIDs(closed))
}
