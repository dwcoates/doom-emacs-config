package sessioncontroller

import (
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

// closeTurnClaimsOnAlreadyComplete ends the durable claim behind a user-commanded
// stop the shim acked ALREADY_COMPLETE.
//
// It is the ledger half of what noteUserInterrupt already does to the status
// axis. The claim has to go — nothing else can ever close it — or the next
// prompt queues behind a turn that does not exist.
//
// IT DOES NOT RELEASE THE QUEUE, and it is not the thing that decides whether
// the queue moves. noteUserInterrupt delivers the synthesized boundary itself,
// after pausing, so what a paused queue will accept is the only gate: a head
// jump goes (which is how an interject's own prompt is submitted) and every
// retained entry stays retained. There is no second policy here to disagree
// with that one.
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
