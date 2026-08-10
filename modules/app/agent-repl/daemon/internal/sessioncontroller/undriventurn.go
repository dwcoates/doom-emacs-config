package sessioncontroller

import (
	"fmt"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

// undriventurn.go — A BOUND TURN MUST HAVE A DRIVER.
//
// # The wedge this closes
//
// A full backend bounce at 16:53 on 2026-08-10 took workspace
// `marcos-pr-remediation` (session s_9de8689040244f34) down mid keep-alive ping.
// The successor daemon brought the session up cleanly: the handshake reconciled
// the turn ledger against a shim reporting no turn in flight (turnlifecycle.go,
// reconcileTurnHandshake) and the workspace came back idle. Then the standing
// subscription caught up on the store's backlog, and the LAST event in it —
// seq=4245, `TurnStarted turn_id=ka_33c76a4c9e0092832e938eb2`, the ping the
// bounce had cut — arrived on the LIVE plane with replayed=false. The ledger
// accepted it, opened a claim, and the turn record bound `named` off the
// derivation (shutdownlease.go, noteTurnLiveness).
//
// Nothing was ever going to end it. The process that started that ping died
// twenty seconds earlier; the fresh shim had never heard of it; no vendor query
// existed. For 25 minutes the workspace rendered `thinking`, the hibernation
// sweep refused to reclaim it every 30 seconds, and the user saw a session that
// had silently stopped working with nothing to interrupt. A hard restart-session
// was the only way out.
//
// # Why nothing already caught it
//
// Every existing bound on an unending turn is keyed to an IN-MEMORY owner that
// this generation minted:
//
//   - the keep-alive deadline (keepalivedeadline.go) skips any claim whose turn
//     id is not this generation's `d.keepAliveTurnID`, and the wedged ping
//     belonged to a dead one;
//   - the handshake reconcile runs at bring-up, BEFORE the backlog that reopens
//     the claim arrives, so it saw an empty ledger and passed;
//   - the owed-resumption re-drive (turnresumption.go) is level-triggered on a
//     durable row that only a graceful teardown writes, and a hard bounce writes
//     none.
//
// So a claim whose driver lived in a previous daemon process had no owner, no
// deadline, and no re-drive: it was outside every bound the system had.
//
// # The invariant
//
// A turn record BOUND is a promise that something is driving that turn. This
// file makes the promise checkable and then enforces it: a bound turn with no
// driver is detected within a bounded window and RESOLVED — the owed re-drive is
// attempted once, and the claim is closed honestly with a surfaced card either
// way. A workspace that thinks forever with nothing behind it is the failure
// mode this makes unrepresentable.
//
// # Why "driver" is a set of ids rather than an activity timer
//
// The obvious watchdog — "no shim events for 60s" — cannot tell a wedged turn
// from a turn whose tool call is genuinely taking a while, and closing the
// second is far worse than the first. So the discriminator is provenance, not
// silence: a turn is DRIVEN when this daemon generation submitted it
// (promptdispatch.go, at the one SubmitPrompt call site) or when the returning
// shim positively announced it in flight at the handshake. Both are facts about
// who is waiting on the turn, and both are recorded before the turn's own
// TurnStarted can reach the ledger. A bound turn outside that set is one no
// process in the running system claims — which is exactly, and only, the wedge.
//
// # Why there is no restart grace
//
// The keep-alive deadline extends itself across a planned bounce, because a gap
// a ping merely lived through is not the ping's fault. Here the bounce is the
// CAUSE rather than an accident of timing: the turn is undriven precisely
// because the generation that drove it is gone. Granting it a grace would only
// prolong the wedge it names.

// undrivenTurnDeadlineMs bounds how long a turn record may stand bound with no
// driver before the watchdog resolves it.
//
// It is a FAILURE bound, not a tuned delay. The only thing it has to be longer
// than is the window in which a legitimately driven turn's ledger claim can be
// observed before its driver is recorded — which is zero, because both drive
// records are written before the submit that can produce the claim. The minute
// is head-room for a bind this process observes through some path a future
// change adds, and it is short enough that a wedged workspace is resolved inside
// two idle sweeps rather than inside a user's patience.
const undrivenTurnDeadlineMs int64 = 60_000

// undrivenTurnWatch is one standing watch over a turn bound with no driver.
//
// resolved is a one-shot latch rather than a deletion, so a sweep that has
// already acted cannot act twice on the same bind while the record is still
// being torn down. The watch is dropped whole when the record leaves the turn.
type undrivenTurnWatch struct {
	turnID    string
	boundAtMs int64
	resolved  bool
}

// noteTurnDrivenLocked records that THIS generation has a driver for each named
// turn. Empty ids are ignored: an identity-less claim cannot be matched against
// anything, and the watchdog only ever watches named turns.
//
// Caller holds m.mu.
func (d *sessionController) noteTurnDrivenLocked(turnIDs ...string) {
	for _, id := range turnIDs {
		if id == "" {
			continue
		}
		if d.drivenTurns == nil {
			d.drivenTurns = make(map[string]struct{}, 4)
		}
		d.drivenTurns[id] = struct{}{}
	}
}

// noteTurnDriven is noteTurnDrivenLocked for callers outside the mutex.
func (m *Manager) noteTurnDriven(d *sessionController, turnIDs ...string) {
	if d == nil {
		return
	}
	m.mu.Lock()
	d.noteTurnDrivenLocked(turnIDs...)
	m.mu.Unlock()
}

// forgetTurnDrivenLocked drops a drive record for a submit that FAILED, so a
// prompt the shim never took cannot vouch for a claim some other path opens.
//
// Caller holds m.mu.
func (d *sessionController) forgetTurnDrivenLocked(turnID string) {
	if turnID == "" || d.drivenTurns == nil {
		return
	}
	delete(d.drivenTurns, turnID)
}

// forgetTurnDriven is forgetTurnDrivenLocked for callers outside the mutex.
func (m *Manager) forgetTurnDriven(d *sessionController, turnID string) {
	if d == nil {
		return
	}
	m.mu.Lock()
	d.forgetTurnDrivenLocked(turnID)
	m.mu.Unlock()
}

// noteUndrivenWatchLocked moves the watch to match the record the turn-liveness
// projection just applied, and reports the watch it ARMED (nil when it armed
// none), so the caller can report the arming with the mutex released.
//
// It is level-triggered off the record itself rather than off the transition:
// the record is the one answer to "is a turn in flight and which turn is it", so
// deriving the watch from it means the watch cannot describe a turn the record
// does not hold.
//
// AN UNNAMEABLE TURN IS NOT WATCHED, and the caller says so. The adopted phase
// carries no id, so there is nothing to match against the drive records and
// nothing to hand CloseOriginTurns — a close keyed by an empty id would match
// every legacy claim in the workspace, including turns some other origin is
// still running (ssm/turnorigin.go).
//
// Caller holds m.mu.
func (d *sessionController) noteUndrivenWatchLocked(p turnClaimProjection, nowMs int64) *undrivenTurnWatch {
	id, named := p.after.name()
	if !named {
		d.undriven = nil
		return nil
	}
	if _, driven := d.drivenTurns[id]; driven {
		d.undriven = nil
		return nil
	}
	// A RE-BIND OF THE SAME TURN DOES NOT RESTAMP THE CLOCK. The derivation is
	// re-projected on every boundary the workspace sees, and a watch whose
	// instant moved on each of them would never reach its own deadline.
	if d.undriven != nil && d.undriven.turnID == id {
		return nil
	}
	d.undriven = &undrivenTurnWatch{turnID: id, boundAtMs: nowMs}
	return d.undriven
}

// SweepUndrivenTurns resolves every turn record that has stood bound past the
// deadline with no driver, and reports how many it resolved.
//
// It is a COMPARISON AT A SWEEP rather than a timer, for the reason the
// keep-alive deadline is (keepalivedeadline.go): a timer dies with the daemon,
// does not advance across a laptop sleep, and cannot tell "the turn is overdue"
// from "the deadline was missed while the machine was asleep".
func (m *Manager) SweepUndrivenTurns() int {
	nowMs := m.now()
	type undrivenTurn struct {
		d      *sessionController
		turnID string
		openMs int64
	}
	var overdue []undrivenTurn
	m.mu.Lock()
	for _, d := range m.byWS {
		w := d.undriven
		if w == nil || w.resolved {
			continue
		}
		// THE RECORD IS RE-READ, never trusted from the watch. A turn that
		// ended, or a workspace that moved on to a different turn, leaves a
		// watch this sweep must not act on.
		if id, named := d.turn.name(); !named || id != w.turnID {
			continue
		}
		if _, driven := d.drivenTurns[w.turnID]; driven {
			d.undriven = nil
			continue
		}
		open := nowMs - w.boundAtMs
		if open < undrivenTurnDeadlineMs {
			continue
		}
		// LATCHED UNDER THE SAME ACQUISITION THAT SELECTED IT. The resolution
		// below runs with the mutex released, so latching afterwards would let
		// a second sweep select the same bind and close it twice.
		w.resolved = true
		overdue = append(overdue, undrivenTurn{d: d, turnID: w.turnID, openMs: open})
	}
	m.mu.Unlock()

	for _, t := range overdue {
		m.resolveUndrivenTurn(t.d, t.turnID, t.openMs)
	}
	return len(overdue)
}

// resolveUndrivenTurn discharges one undriven turn: the owed re-drive is
// attempted once, and the claim is closed honestly whether or not one existed.
//
// BOTH HALVES RUN, and that is deliberate. The re-drive continues the work a
// bounce interrupted; the close retires the DEAD claim, which the re-drive
// cannot do because the turn it starts carries a new identity of its own.
// Leaving the dead claim standing on the strength of a re-drive would keep the
// workspace pinned exactly as it was.
//
// Must be called with m.mu RELEASED: it reaches the SSM, the queue's
// publication path and the frontend.
func (m *Manager) resolveUndrivenTurn(d *sessionController, turnID string, openMs int64) {
	m.warnf("session-controller: turn UNDRIVEN ws=%q session=%s turn_id=%s open_ms=%d deadline_ms=%d — the turn record has stood BOUND with nothing driving it: this daemon never submitted this turn and no shim announced it in flight, so no vendor query exists behind it and no boundary is ever coming. The workspace has been rendering `thinking` for the whole window. The claim is being retired and any work the last bounce interrupted is re-driven once",
		d.workspace, d.sessionID, turnID, openMs, undrivenTurnDeadlineMs)

	// THE DURABLE CLAIM GOES BEFORE THE IN-MEMORY RELEASE, for the reason the
	// keep-alive deadline states: the release below can deliver a queued prompt,
	// and a prompt delivered while the ledger still holds this turn open would
	// be starting a turn the derivation says cannot start.
	closed, err := m.cfg.SSM.CloseOriginTurns(d.workspace, []string{turnID}, ssm.TurnCloseUndriven)
	if err != nil {
		// NEVER SWALLOWED, AND NEVER A REASON TO STOP. Leaving the in-memory
		// record bound on top of a failed ledger write keeps the workspace
		// pinned, which is strictly worse than holding only the durable half.
		m.warnf("session-controller: turn UNDRIVEN CLAIM CLOSE FAILED ws=%q session=%s turn_id=%s error=%v — the durable claim stands, so this workspace keeps reading as a turn in flight until another edge retires it; the in-memory record is released regardless",
			d.workspace, d.sessionID, turnID, err)
	}
	if d.consumer != nil {
		d.consumer.ReleaseSynthesizedTurnClose(closed, ssm.TurnCloseUndriven)
	}
	if d.client != nil {
		// The start took a durable-cursor pin that only a stream TurnEnded
		// releases, and no TurnEnded is coming for this turn.
		d.client.UnpinAccountingTurn(closed...)
	}

	// THE RENDER STATE IS A SEPARATE RECORD FROM THE LEDGER, and republishing it
	// is the point: a frontend already holding `thinking` learns nothing from a
	// state it is never handed (turnlifecycle.go, healStaleThinkingOnBringUp).
	if m.cfg.Push != nil {
		if cleared, err := m.cfg.SSM.ReconcileAlreadyComplete(d.workspace, d.sessionID, m.cfg.Push.PushWorkspaceState); err != nil {
			m.warnf("session-controller: turn UNDRIVEN render-state republish FAILED ws=%q session=%s turn_id=%s: %v — the ledger no longer holds the turn, but the workspace may stay latched in `thinking` until another edge supersedes it",
				d.workspace, d.sessionID, turnID, err)
		} else if cleared {
			m.logf("session-controller: turn UNDRIVEN render state CLEARED ws=%q session=%s turn_id=%s — the `thinking` standing over the retired claim is republished as settled",
				d.workspace, d.sessionID, turnID)
		}
	}

	// THE SYNTHESIZED BOUNDARY, through the SAME path a real TurnEnded takes.
	// It is what releases the turn record itself and everything hanging off a
	// turn end: the interject waiting on this boundary, the FIFO drain behind
	// it, and the queue view every frontend renders from (queue.go,
	// onTurnBoundary).
	m.onTurnBoundary(d, false, m.now())

	// THE CARD IS THE ONLY THING THE USER WILL EVER SEE ABOUT THIS. The turn is
	// gone, the colour is settled, and without this the session would simply
	// have stopped working on what they asked for and never said so.
	if d.consumer != nil {
		d.consumer.pushFailure(
			d.consumer.undrivenTurnUUID(turnID),
			errclass.TurnUndriven(fmt.Sprintf("turn_id=%s open_ms=%d: the turn was bound with no driver — nothing in this daemon submitted it and no agent process announced it in flight, so no boundary was ever coming. It has been closed; work it stood for was not resumed and must be asked for again.", turnID, openMs)),
		)
	}

	// THE RE-DRIVE COMES LAST, AND THE ORDER IS LOAD-BEARING. A re-drive
	// submitted while the dead turn was still bound would be QUEUED behind it —
	// the queue reads the very record this resolution is releasing — so the work
	// would sit exactly where the wedge left it. Driving after the synthesized
	// boundary means the submit meets an idle session and reaches the shim.
	//
	// It is AT MOST ONCE: driveOwedResumptions is level-triggered on the durable
	// record and claims each row before submitting, so a workspace that owes
	// nothing pays nothing here and one that owes something cannot be driven
	// twice (turnresumption.go).
	m.driveOwedResumptions(d.workspace, d.sessionID)

	m.logf("session-controller: turn UNDRIVEN RESOLVED ws=%q session=%s turn_id=%s open_ms=%d durable_closed=%v — the claim, the record and the workspace colour are all released, and the user has been told",
		d.workspace, d.sessionID, turnID, openMs, len(closed) > 0)
}

// undrivenTurnUUID is the stable card identity for ONE undriven turn's
// resolution. Keyed by the turn, so a workspace that wedges twice reports two
// cards rather than one card that keeps changing its mind.
func (c *consumer) undrivenTurnUUID(turnID string) string {
	return "turn-undriven:" + c.sessionID + ":" + turnID
}
