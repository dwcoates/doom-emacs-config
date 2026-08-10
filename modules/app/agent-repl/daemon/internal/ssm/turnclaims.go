package ssm

import (
	"database/sql"
	"errors"
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/statedb"
)

// ErrTurnBridgeDeadClaim marks the ONE turn-bridge refusal that says nothing
// about protocol health: the bridge names a turn whose durable claim is already
// CLOSED.
//
// A closed claim is a turn that can no longer be running. Its end was either
// observed on the stream or synthesized by the daemon (TurnCloseRestartInterrupted,
// TurnCloseAlreadyComplete). Either way the epoch that owned the turn is retired,
// and a bridge arriving for it is evidence about the past, not a claim on the
// present. There is nothing to correlate and nothing to corrupt: the ledger row
// is final and this refusal leaves it exactly as it was.
//
// It is separated from the other bridge refusals because those DO say the shim
// and the daemon disagree about a LIVE turn — two event sessions claiming one
// in-flight turn, or a bridge naming a previous session the durable start does
// not belong to. Those stay terminal. This one does not, because escalating it
// cost a live session: a /clear whose claim had been closed by restart
// reconciliation replayed its bridge on every reattach, and each replay tore the
// session down before last_seen_seq could advance past it — an unbounded
// teardown loop driven by a turn that had been dead for minutes.
var ErrTurnBridgeDeadClaim = errors.New("ssm: turn claim bridge names an already-completed claim")

// ErrTurnStartConflict marks the ledger's refusal of a `TurnStarted` whose
// identity is ALREADY OPEN at a different store coordinate: two live starts
// contending for one turn.
//
// It is a sentinel so the refusal can be scoped to the TURN. Every turn-start
// refusal used to reach shimclient as an anonymous lifecycle rejection, and
// that is terminal for the SESSION — the controller exits and the render state
// severs. The duplicate that caused it is durable in the vendor event stream,
// so every later resume replayed it and severed the session again: the session
// became permanently unresumable over one redelivered turn. A conflict is still
// surfaced loudly; what it may no longer do is outlive the turn it is about.
var ErrTurnStartConflict = errors.New("ssm: turn start identity conflicts with an open claim")

// The two SYNTHESIZED terminal causes: the reasons a turn claim is ended by the
// daemon rather than by a `TurnEnded` off the stream.
//
// Both exist because the durable claim and the live shim are two authorities on
// "is a turn in flight", and only one of them can still observe the process the
// turn was running in. When the shim states, from that vantage point, that
// nothing is running, a claim standing against that statement names a turn
// whose end can never arrive — so the daemon writes the end itself and says
// which observation made it do so.
const (
	// TurnCloseRestartInterrupted closes a claim that survived a restart or a
	// shim bounce: the returning shim's hello reports no turn in flight and no
	// turn ids at all. The turn was cut when the process behind it went away,
	// and the cause is named so the user can see that rather than a turn that
	// merely disappeared.
	TurnCloseRestartInterrupted = "interrupted_by_restart"
	// TurnCloseAlreadyComplete closes a claim contradicted by an interrupt
	// Ack of INTERRUPT_OUTCOME_ALREADY_COMPLETE — the shim answering, live,
	// that there is no foreground turn to stop.
	TurnCloseAlreadyComplete = "interrupt_already_complete"
	// TurnCloseTerminalResult closes a claim whose turn has ALREADY PRODUCED ITS
	// RESULT. The vendor's `result` message is the SDK's own end of turn; the
	// shim's `TurnEnded` is a second announcement of that same fact, and a turn
	// state that waits for the second one is hostage to whatever delays it.
	//
	// One observed session waited ten minutes: the result landed at 18:14:30 and
	// the shim's turn-lifecycle watchdog produced the `TurnEnded` at 18:24:30,
	// with the workspace rendering `thinking` for the whole gap. The result is
	// evidence enough on its own, so the daemon writes the end from it and the
	// later `TurnEnded` is admitted idempotently as the replay it is.
	TurnCloseTerminalResult = "terminal_result_observed"
)

// moveTurnLedgerLocked durably validates and records one STREAM turn boundary
// inside the boundary's own transaction.
//
// The ledger is separate from workspace_state because queued turns have
// multiple simultaneous identities while the rendered session-status lifecycle remains one
// `thinking` claim. Completed claims remain as replay receipts: if the daemon
// crashes after recording a boundary but before advancing last_seen_seq, the
// replayed event is admitted only when both its identity and seq match the
// durable receipt.
//
// IT NO LONGER OWNS ITS OWN TRANSACTION, and that is the point. The
// session-status row this boundary paints is derived from the ledger this
// method just moved, so both must land or neither may — see ApplyTurnBoundary,
// which is the only caller.
//
// liveQueryInstanceID is the query() invocation the CALLER is bound to. It is
// compared, once, against the query the event's producer stamped on its
// envelope; see turnEndIsHistorical.
//
// Caller holds m.mu and owns tx.
func (m *Manager) moveTurnLedgerLocked(tx *sql.Tx, workspace, claimantSessionID, liveQueryInstanceID string, ev *corev1.Event) (before, after []string, replayed bool, err error) {
	id := turnCorrelation(ev)
	before, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, nil, false, err
	}
	if _, started := ev.GetPayload().(*corev1.Event_TurnStarted); started && m.hibernationLeases[workspace] != 0 {
		isReplay, _, _, probeErr := recordTurnStart(tx, workspace, claimantSessionID, ev.GetSessionId(), id, ev.GetSeq())
		if probeErr != nil {
			return before, before, false, probeErr
		}
		if !isReplay {
			return before, before, false, m.rejectStartDuringHibernationLocked(workspace, "TurnStarted claim")
		}
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		var reconstructedEnd, settledReplay string
		replayed, reconstructedEnd, settledReplay, err = recordTurnStart(
			tx, workspace, claimantSessionID, ev.GetSessionId(), id, ev.GetSeq(),
		)
		if settledReplay != "" {
			// REPLAY TOLERANCE, VISIBLE. The identity this start names is
			// already settled in the ledger, so the redelivery is admitted as
			// the idempotent replay it is instead of rejected as a duplicate.
			// The rejection used to be fatal to the whole session, and because
			// the duplicate lives in the durable vendor stream, every resume
			// replayed it and killed the session again.
			m.logf("ssm: turn start ADMITTED AS SETTLED REPLAY workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q detail=%s — the ledger already holds a start and an end for this identity, so the redelivered start mutates nothing and the session stays resumable",
				workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id, settledReplay)
		}
		if reconstructedEnd != "" {
			// PART B, VISIBLE. A replacement session replayed the start of a
			// turn this daemon had already killed, and the durable end recorded
			// against that start's store coordinate closed it in the same
			// statement that admitted it. The workspace derives idle rather than
			// standing behind a turn whose `TurnEnded` no process exists to send.
			m.logf("ssm: turn start ADMITTED ALREADY ENDED workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q cause=%s — this start replays a turn the daemon killed; the durable end recorded against its store coordinate closed the claim as it was created, so the replay reconstructs a matched start/end pair rather than a turn nothing can finish",
				workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id, reconstructedEnd)
		}
	case *corev1.Event_TurnEnded:
		var crossGeneration bool
		replayed, crossGeneration, err = recordTurnEnd(
			tx, workspace, claimantSessionID, ev.GetSessionId(), id, ev.GetSeq(),
			turnEndIsHistorical(liveQueryInstanceID, ev),
		)
		if crossGeneration {
			// THE CLAIM OUTLIVES ITS CLAIMANT. A workspace's daemon session id is
			// re-minted on every CreateSession (hibernate/revive/reopen all mint a
			// fresh claimant for the SAME underlying vendor conversation), but
			// turn_lifecycle_claim rows stay keyed to whichever claimant opened
			// them and are never rebound. A turn's own end, replayed for a
			// generation that never opened it, is not evidence of anything wrong
			// with the turn — it is the ordinary cost of the claimant rotating out
			// from under a claim that outlived it. The durable identity is the
			// turn id within the workspace, not the claimant, so the lookup
			// widens to the whole workspace exactly once (only after the caller's
			// own generation proves it holds NOTHING for this turn id) and this
			// is the loud record of that widened match, kept separate from the
			// ordinary accept/reject line below.
			m.logf("ssm: turn ledger CROSS-GENERATION CLAIM MATCH workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q request_id=%q — the caller's own claimant generation held no row for this turn id, so the lookup widened to the workspace and found the claim under a retired claimant; the turn's own identity is what matched, not the claimant",
				workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id, ev.GetRequestId())
		}
	default:
		err = fmt.Errorf("ssm: turn lifecycle resolver received %T", ev.GetPayload())
	}
	if err != nil {
		m.logf("ssm: turn ledger decision=reject workspace=%s claimant_session=%s event_session=%s seq=%d plane=%s turn_id=%q request_id=%q active_before=%v error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
			id, ev.GetRequestId(), before, err)
		return before, before, false, err
	}
	after, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return before, nil, false, err
	}
	m.logf("ssm: turn ledger decision=accept workspace=%s claimant_session=%s event_session=%s seq=%d plane=%s turn_id=%q request_id=%q active_before=%v active_after=%v replayed=%v",
		workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
		id, ev.GetRequestId(), before, after, replayed)
	return before, after, replayed, nil
}

// ResolveTurnClaimBridge records rotation correlation evidence without
// creating a lifecycle edge. This method is the payload's only daemon
// destination: it updates the durable claim ledger but never calls Apply,
// appends workspace state, or publishes anything.
func (m *Manager) ResolveTurnClaimBridge(workspace, claimantSessionID string, ev *corev1.Event) (replayed bool, err error) {
	bridge := ev.GetTurnClaimBridge()
	if workspace == "" || claimantSessionID == "" {
		err := fmt.Errorf("ssm: turn claim bridge requires workspace and claimant session id")
		m.logf("ssm: turn bridge decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return false, err
	}
	if ev == nil || bridge == nil || ev.GetSessionId() == "" || ev.GetSeq() == 0 {
		err := fmt.Errorf("ssm: turn claim bridge requires a persistent event with payload, session_id, and seq")
		m.logf("ssm: turn bridge decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return false, err
	}
	if ev.GetPlane() != corev1.Plane_PLANE_STREAM ||
		ev.GetClass() != corev1.EventClass_EVENT_CLASS_PERSISTENT {
		err := fmt.Errorf("ssm: turn claim bridge requires persistent stream plane, got plane=%s class=%s",
			ev.GetPlane().String(), ev.GetClass().String())
		m.logf("ssm: turn bridge decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d plane=%s class=%s turn_id=%q previous_session=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
			ev.GetClass().String(), bridge.GetTurnId(), bridge.GetPreviousSessionId(), ev.GetRequestId(), err)
		return false, err
	}
	id := bridge.GetTurnId()
	previousSessionID := bridge.GetPreviousSessionId()
	if id == "" || ev.GetRequestId() != id || previousSessionID == "" ||
		previousSessionID == ev.GetSessionId() {
		err := fmt.Errorf("ssm: invalid turn claim bridge turn_id=%q request_id=%q previous_session=%q event_session=%q",
			id, ev.GetRequestId(), previousSessionID, ev.GetSessionId())
		m.logf("ssm: turn bridge decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d turn_id=%q previous_session=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id,
			previousSessionID, ev.GetRequestId(), err)
		return false, err
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return false, fmt.Errorf("ssm: begin turn bridge transaction: %w", err)
	}
	defer tx.Rollback()
	replayed, err = recordTurnBridge(
		tx, workspace, claimantSessionID, previousSessionID,
		ev.GetSessionId(), id, ev.GetSeq(),
	)
	if err != nil {
		decision := "reject"
		if errors.Is(err, ErrTurnBridgeDeadClaim) {
			decision = "refuse_dead_claim"
		}
		m.logf("ssm: turn bridge decision=%s workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q previous_session=%q request_id=%q error=%v",
			decision, workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id,
			previousSessionID, ev.GetRequestId(), err)
		return false, err
	}
	if err := tx.Commit(); err != nil {
		return false, fmt.Errorf("ssm: commit turn bridge transaction: %w", err)
	}
	m.logf("ssm: turn bridge decision=accept workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q previous_session=%q request_id=%q replayed=%v",
		workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id,
		previousSessionID, ev.GetRequestId(), replayed)
	return replayed, nil
}

// ReconcileTurnHandshake durably folds the shim's pre-subscription active-turn
// snapshot into the ledger. A mismatch is returned before DaemonHello is sent,
// so the store subscription and ShimReady gate never open on contradictory
// identities.
//
// closed names the claims this handshake SYNTHESIZED an end for: the phantom
// turns a returning shim has just contradicted. It is what the caller releases
// its queue on, because a prompt held behind one of those claims is waiting for
// a `TurnEnded` that no process will ever send.
//
// durablyEnded names the turns the caller has PROVED already carry a terminal
// event in the durable store. THE STORE'S RECORD OUTRANKS ANY PROCESS'S MEMORY:
// a hello is one process's account of what it is running now, while a stored
// `TurnEnded` is the recorded fact that a turn finished — so a claim named here
// is never cut as interrupted. It stays open and is settled COMPLETED by its own
// replayed boundary, through the same lifecycle a live end takes, which is what
// keeps its accounting and its result publishing rather than being replaced by a
// synthesized cut.
func (m *Manager) ReconcileTurnHandshake(workspace, claimantSessionID string, ids []string, legacyActive bool, durablyEnded []string) (before, after, closed []string, err error) {
	if workspace == "" || claimantSessionID == "" {
		err := fmt.Errorf("ssm: turn handshake requires workspace and claimant session id")
		m.logf("ssm: turn handshake decision=reject_validation workspace=%q claimant_session=%q hello_ids=%v legacy_active=%v error=%v",
			workspace, claimantSessionID, ids, legacyActive, err)
		return nil, nil, nil, err
	}
	seen := make(map[string]struct{}, len(ids))
	for _, id := range ids {
		if id == "" {
			err := fmt.Errorf("ssm: turn handshake active_turn_ids contains an empty identity")
			m.logf("ssm: turn handshake decision=reject_validation workspace=%q claimant_session=%q hello_ids=%v legacy_active=%v error=%v",
				workspace, claimantSessionID, ids, legacyActive, err)
			return nil, nil, nil, err
		}
		if _, duplicate := seen[id]; duplicate {
			err := fmt.Errorf("ssm: turn handshake active_turn_ids contains duplicate identity %q", id)
			m.logf("ssm: turn handshake decision=reject_validation workspace=%q claimant_session=%q hello_ids=%v legacy_active=%v error=%v",
				workspace, claimantSessionID, ids, legacyActive, err)
			return nil, nil, nil, err
		}
		seen[id] = struct{}{}
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return nil, nil, nil, fmt.Errorf("ssm: begin turn handshake transaction: %w", err)
	}
	defer tx.Rollback()
	before, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, nil, nil, err
	}
	switch {
	case len(ids) > 0 && len(before) == 0:
		for _, id := range ids {
			if _, err := tx.Exec(`INSERT INTO turn_lifecycle_claim(
				workspace, claimant_session_id, turn_id, start_seq
			) VALUES (?,?,?,0)`, workspace, claimantSessionID, id); err != nil {
				return before, before, nil, fmt.Errorf("ssm: persist handshake turn %q: %w", id, err)
			}
		}
	case len(ids) > 0 && !equalStrings(before, ids):
		err = fmt.Errorf("handshake active_turn_ids=%v disagree with durable active turns=%v", ids, before)
	case legacyActive && len(before) == 0:
		if _, insertErr := tx.Exec(`INSERT INTO turn_lifecycle_claim(
			workspace, claimant_session_id, turn_id, start_seq
		) VALUES (?,?,?,0)`, workspace, claimantSessionID, ""); insertErr != nil {
			return before, before, nil, fmt.Errorf("ssm: persist legacy handshake turn: %w", insertErr)
		}
	case len(ids) == 0 && !legacyActive && len(before) > 0:
		// THE PHANTOM CLAIM, AND THE EDGE THAT CLOSES IT. The hello says
		// turn_in_flight=false with no turn ids at all, and the ledger still
		// holds claims for this very claimant. The shim is the only party that
		// can see the process a turn would be running in, so its statement wins
		// — and the claims it contradicts are ENDED here, durably, rather than
		// left to a `TurnEnded` that the process which owed it no longer exists
		// to send. Leaving them standing is what latched the whole workspace in
		// `thinking` and made every later prompt queue behind a boundary that
		// was never coming.
		//
		// This is the UNAMBIGUOUS case and the only one that gets it. A hello
		// naming ids the ledger disagrees with, or a legacy shim positively
		// asserting a turn under a protocol too old to name it, both stay on
		// their existing loud paths above.
		//
		// EXCEPT WHERE THE STORE SAYS OTHERWISE. The hello's silence about a
		// turn means the shim is not running it NOW, which is equally true of a
		// turn that was cut and of a turn that FINISHED while the daemon was
		// away. The durable terminal record is the only thing that tells those
		// two apart, and it outranks this hello: a turn it names completed, and
		// the boundary proving so is already in the store waiting for the
		// subscription this handshake precedes.
		spare := make(map[string]struct{}, len(durablyEnded))
		for _, id := range durablyEnded {
			spare[id] = struct{}{}
		}
		closed, err = synthesizeTurnEndsExcept(tx, workspace, claimantSessionID, TurnCloseRestartInterrupted, m.nextAt(), spare)
	}
	if err != nil {
		m.logf("ssm: turn handshake decision=reject workspace=%s claimant_session=%s hello_ids=%v legacy_active=%v durable_before=%v error=%v",
			workspace, claimantSessionID, ids, legacyActive, before, err)
		return before, before, nil, err
	}
	after, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return before, nil, nil, err
	}
	if err := tx.Commit(); err != nil {
		return before, nil, nil, fmt.Errorf("ssm: commit turn handshake transaction: %w", err)
	}
	if len(closed) > 0 {
		m.logf("ssm: turn claims INTERRUPTED BY RESTART workspace=%s claimant_session=%s closed=%v cause=%s — the returning shim reports no turn in flight, so these claims name turns that were cut when the process behind them went away and no `TurnEnded` can ever arrive for them",
			workspace, claimantSessionID, formatClosedTurnIDs(closed), TurnCloseRestartInterrupted)
	}
	m.logf("ssm: turn handshake decision=accept workspace=%s claimant_session=%s hello_ids=%v legacy_active=%v durable_before=%v durable_after=%v",
		workspace, claimantSessionID, ids, legacyActive, before, after)

	// THE HANDSHAKE SNAPSHOT IS AUTHORITATIVE OVER THE session-status lifecycle TOO, and this
	// is the edge that was missing. A shim reporting zero turn ids is stating,
	// from the only vantage point that can, that nothing is running behind it —
	// the same statement already trusted to seed `turn_lifecycle_claim` above
	// and to set the session controller's process-local latch in
	// sessioncontroller.reconcileTurnSnapshot. A workspace whose axis still tops out in
	// `thinking` under that statement is holding a claim whose `TurnEnded` will
	// never arrive, because the turn it named is over and the process that would
	// have reported it is a previous one.
	//
	// This cures ledgers already poisoned — by a shim crash the daemon never
	// initiated, by a kill that predates the teardown obligation, by the
	// unattributed `thinking` a permission close restores — which is why it
	// exists ALONGSIDE the teardown's own closing write rather than instead of
	// it. Neither subsumes the other: the teardown covers stops this daemon
	// makes, and this covers everything already latched when a shim comes back.
	//
	// A LEGACY SHIM CLAIMING A TURN IS BELIEVED. `legacy_active` with no ids is a
	// positive assertion that a turn IS in flight under a protocol too old to
	// name it, so the empty id list says nothing there and the axis is left
	// exactly as it stands.
	//
	// A CLAIM HELD OPEN BY DURABLE EVIDENCE KEEPS THE AXIS TOO. `after` is
	// non-empty only when this handshake deliberately spared a turn whose end is
	// already in the store; that turn's own replayed boundary is what paints the
	// axis idle, moments from now, and retiring `thinking` ahead of it would
	// flash a settled workspace for a turn that has not been settled yet.
	if len(ids) == 0 && !legacyActive && len(after) == 0 {
		// The reason NAMES the observation. A handshake that found no durable
		// claim to close is tidying a latch whose turn is merely unaccounted
		// for; one that just synthesized an end for a phantom claim is
		// reporting a turn CUT BY A RESTART, and the row is the only place a
		// user can see that it was.
		reason := causeShimHandshake
		if len(closed) > 0 {
			reason = TurnCloseRestartInterrupted
		}
		// CLAIMANT SCOPE, because the shim behind this handshake IS ALIVE: it
		// just said hello. The workspace-scoped retirement is licensed only by a
		// proof that nothing alive can end any turn here, which is the opposite
		// of what a handshake reports.
		if _, err := m.closeStaleTurnLocked(workspace, claimantSessionID, reason, true, turnCloseScopeClaimant); err != nil {
			// Never swallowed, and never allowed to fail the handshake: the
			// reconciliation above has already committed and DaemonHello is
			// gated on this call's error, so returning here would refuse a
			// perfectly good session over a stale row it merely failed to tidy.
			m.warn("ssm: closing the stale turn on the shim handshake FAILED workspace=%s claimant_session=%s hello_ids=%v: %v — the session-status lifecycle may stay latched in `thinking` until the next edge supersedes it",
				workspace, claimantSessionID, ids, err)
		}
	}
	return before, after, closed, nil
}

// SynthesizeTurnClose ends every active durable turn claim held by
// claimantSessionID WITHOUT a `TurnEnded`, recording cause as the terminal
// reason, and reports the identities it closed.
//
// IT IS THE OTHER HALF OF ReconcileAlreadyComplete. That method reconciles the
// STATUS axis to the shim's live "there is no foreground turn"; this reconciles
// the durable CLAIM ledger to the same statement. Without it the two disagree,
// and the disagreement is not cosmetic: the claim is what the session
// controller's queue holds prompts behind, so a prompt submitted after an
// ALREADY_COMPLETE Ack waits on a boundary the ledger says is still coming and
// the shim says already happened.
//
// A workspace with no active claim closes nothing and returns nil, nil. That is
// the ordinary outcome — the turn's own end got there first — and it is not an
// error.
func (m *Manager) SynthesizeTurnClose(workspace, claimantSessionID, cause string) (closed []string, err error) {
	if workspace == "" {
		return nil, fmt.Errorf("ssm: SynthesizeTurnClose got an empty workspace")
	}
	if claimantSessionID == "" {
		return nil, fmt.Errorf("ssm: SynthesizeTurnClose for workspace %q got an empty claimant session id; a turn claim can only be ended on behalf of the session that holds it", workspace)
	}
	if cause != TurnCloseRestartInterrupted && cause != TurnCloseAlreadyComplete && cause != TurnCloseShimStopped && cause != TurnCloseTerminalResult {
		return nil, fmt.Errorf("ssm: SynthesizeTurnClose for workspace %q session %q got cause %q; a synthesized end must name one of the observations that authorize it (%q, %q, %q, %q)",
			workspace, claimantSessionID, cause, TurnCloseRestartInterrupted, TurnCloseAlreadyComplete, TurnCloseShimStopped, TurnCloseTerminalResult)
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return nil, fmt.Errorf("ssm: begin synthesized turn close transaction: %w", err)
	}
	defer tx.Rollback()
	closed, err = synthesizeTurnEnds(tx, workspace, claimantSessionID, cause, m.nextAt())
	if err != nil {
		m.logf("ssm: synthesized turn close decision=reject workspace=%s claimant_session=%s cause=%s error=%v",
			workspace, claimantSessionID, cause, err)
		return nil, err
	}
	if err := tx.Commit(); err != nil {
		return nil, fmt.Errorf("ssm: commit synthesized turn close transaction: %w", err)
	}
	if len(closed) == 0 {
		m.logf("ssm: synthesized turn close workspace=%s claimant_session=%s cause=%s decision=no_active_claim — the turn's own end already retired every claim, so nothing is synthesized",
			workspace, claimantSessionID, cause)
		return nil, nil
	}
	m.logf("ssm: turn claims CLOSED WITHOUT A TurnEnded workspace=%s claimant_session=%s closed=%s cause=%s — the shim states there is no turn behind these claims, so the daemon writes the end the vanished turn cannot",
		workspace, claimantSessionID, formatClosedTurnIDs(closed), cause)
	return closed, nil
}

// TurnCloseCauseSupersededByRewind is the end_cause a claim carries when a
// transcript rewind discarded the turn behind it. The turn ran and its history
// is kept; what changed is that the conversation no longer contains it, so the
// claim must not stay open waiting for an end that the retired seq space will
// never deliver.
const TurnCloseCauseSupersededByRewind = "superseded_by_rewind"

// SupersedeTurnClaims closes the named turns' claims, attributing the close to
// cause.
//
// IT REUSES THE LEDGER'S EXISTING VOCABULARY rather than adding a column. A
// superseded turn is exactly what end_cause already describes: a claim the
// daemon ended without a TurnEnded, because the turn behind it can no longer
// produce one. `end_seq IS NULL` stays the ONE definition of an active claim,
// so nothing downstream has to learn a second one — which is the same reason
// synthesizeTurnEnds writes end_seq=0 rather than inventing a sentinel.
//
// IT ATTRIBUTES ALREADY-CLOSED CLAIMS TOO, and that is the ORDINARY path, not
// the exception. A keep-alive ping ends normally before the rewind that
// discards it ever runs, so by the time this is called the dropped turn's claim
// is nearly always already closed by its own `TurnEnded` — with an EMPTY
// end_cause, because nothing had yet explained the close. Leaving it that way
// would make the ledger unable to answer the one question the rewind exists to
// record: which closes happened because the daemon dropped the turn. So a
// second update stamps cause onto those rows.
//
//   - end_seq is PRESERVED. It is the real boundary's replay receipt, and
//     recordTurnEnd matches a replayed `TurnEnded` on (end_seq, end_event_session_id)
//     before it looks at end_cause at all — overwriting it would turn an
//     idempotent replay into a contradiction.
//   - A NON-EMPTY end_cause is NEVER overwritten. An existing attribution (a
//     restart interruption, an earlier supersession) is the daemon's own record
//     of why that claim ended; a later rewind does not get to rewrite it.
//
// An unknown turn id is a NO-OP, not an error: a rewind can legitimately name a
// turn this workspace holds no claim for.
//
// It reports the two outcomes separately: closed names the claims that were
// still open and were ended here, attributed names the already-closed claims
// that gained the cause.
func (m *Manager) SupersedeTurnClaims(workspace, claimantSessionID string, turnIDs []string, cause string) (closed, attributed []string, err error) {
	if len(turnIDs) == 0 {
		return nil, nil, nil
	}
	if cause == "" {
		return nil, nil, fmt.Errorf("ssm: refusing to supersede turn claims for workspace %q without a cause; an unattributed close is indistinguishable from a lost one", workspace)
	}
	tx, err := m.db.Begin()
	if err != nil {
		return nil, nil, fmt.Errorf("ssm: begin supersede turn claims for workspace %q: %w", workspace, err)
	}
	defer func() { _ = tx.Rollback() }()
	for _, turnID := range turnIDs {
		if turnID == "" {
			continue
		}
		openChanged, execErr := supersedeOpenTurnClaim(tx, workspace, claimantSessionID, turnID, cause)
		if execErr != nil {
			return nil, nil, execErr
		}
		if openChanged > 0 {
			closed = append(closed, turnID)
		}
		closedChanged, execErr := attributeClosedTurnClaim(tx, workspace, claimantSessionID, turnID, cause)
		if execErr != nil {
			return nil, nil, execErr
		}
		if closedChanged > 0 {
			attributed = append(attributed, turnID)
		}
	}
	if err := tx.Commit(); err != nil {
		return nil, nil, fmt.Errorf("ssm: commit supersede turn claims for workspace %q: %w", workspace, err)
	}
	m.logf("ssm: turn claims SUPERSEDED workspace=%s claimant_session=%s requested=%d closed_here=%s attributed=%s cause=%s — the rewind discarded these turns from the conversation, so the claims still open are closed and the claims their own ends already closed are stamped with the cause naming why",
		workspace, claimantSessionID, len(turnIDs), formatClosedTurnIDs(closed), formatClosedTurnIDs(attributed), cause)
	return closed, attributed, nil
}

// supersedeOpenTurnClaim ends a dropped turn's STILL-OPEN claim, writing end_seq=0
// exactly as synthesizeTurnEnds does: no event produced this close, and that is
// what distinguishes it in the ledger.
func supersedeOpenTurnClaim(tx *sql.Tx, workspace, claimantSessionID, turnID, cause string) (int64, error) {
	result, err := tx.Exec(`UPDATE turn_lifecycle_claim
			SET end_seq=0, end_cause=?
			WHERE workspace=? AND claimant_session_id=? AND turn_id=? AND end_seq IS NULL`,
		cause, workspace, claimantSessionID, turnID)
	if err != nil {
		return 0, fmt.Errorf("ssm: supersede turn claim %q for workspace %q: %w", turnID, workspace, err)
	}
	changed, err := result.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("ssm: inspect superseded turn claim %q for workspace %q: %w", turnID, workspace, err)
	}
	return changed, nil
}

// attributeClosedTurnClaim stamps cause onto a dropped turn's ALREADY-CLOSED
// claim, touching only rows whose close is still unexplained.
//
// `end_seq IS NOT NULL` keeps the real boundary intact, and matching only an
// EMPTY end_cause keeps an existing attribution intact; together they make this
// update the narrowest one that can answer "was this close the daemon's doing?".
func attributeClosedTurnClaim(tx *sql.Tx, workspace, claimantSessionID, turnID, cause string) (int64, error) {
	result, err := tx.Exec(`UPDATE turn_lifecycle_claim
			SET end_cause=?
			WHERE workspace=? AND claimant_session_id=? AND turn_id=?
				AND end_seq IS NOT NULL AND end_cause=''`,
		cause, workspace, claimantSessionID, turnID)
	if err != nil {
		return 0, fmt.Errorf("ssm: attribute closed turn claim %q for workspace %q: %w", turnID, workspace, err)
	}
	changed, err := result.RowsAffected()
	if err != nil {
		return 0, fmt.Errorf("ssm: inspect attributed turn claim %q for workspace %q: %w", turnID, workspace, err)
	}
	return changed, nil
}

// synthesizeTurnEnds ends every active claim held by claimantSessionID inside
// tx, stamping cause so a later genuine `TurnEnded` for the same identity can be
// recognized as already accounted for rather than read as a contradiction.
//
// end_seq is 0 rather than a real store seq because no event produced this
// close. That is exactly what distinguishes it in the ledger, and it is why
// end_cause is written alongside: `end_seq IS NULL` remains the ONE definition
// of an active claim, so nothing downstream has to learn a second one.
//
// IT ALSO WRITES THE END WHERE A REPLAY WILL SEE IT. Closing the claim closes it
// for THIS claimant only, and a superseded workspace re-mints its claimant and
// replays the same store stream — which reconstructs the killed turn as a fresh
// open claim under a session that never ran it. So each closed claim's START
// COORDINATE is recorded durably too (turninterruption.go), because that
// coordinate is what the replayed event itself carries. Both writes are in this
// one transaction: a claim closed without its durable end is the orphan this
// exists to prevent.
func synthesizeTurnEnds(tx *sql.Tx, workspace, claimantSessionID, cause string, at int64) ([]string, error) {
	return synthesizeTurnEndsExcept(tx, workspace, claimantSessionID, cause, at, nil)
}

// synthesizeTurnEndsExcept is synthesizeTurnEnds with a SPARED set: turn ids the
// caller holds durable evidence about and therefore may not close from process
// memory.
//
// THE STORE'S RECORD OUTRANKS ANY PROCESS'S MEMORY. A synthesized end is written
// because no `TurnEnded` can ever arrive for the claim; a turn whose end is
// already sitting in the durable store contradicts that premise outright, so its
// claim stays OPEN here and is settled by its own replayed boundary through the
// ordinary lifecycle. Sparing it is not leniency — closing it would overwrite a
// recorded terminal with a synthesized one and report a completed turn as cut.
func synthesizeTurnEndsExcept(tx *sql.Tx, workspace, claimantSessionID, cause string, at int64, spare map[string]struct{}) ([]string, error) {
	active, err := activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, err
	}
	closing := make([]string, 0, len(active))
	for _, id := range active {
		if _, spared := spare[id]; !spared {
			closing = append(closing, id)
		}
	}
	if len(closing) == 0 {
		return nil, nil
	}
	starts, err := activeInterruptedStarts(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, err
	}
	interrupting := make([]interruptedStart, 0, len(starts))
	for _, start := range starts {
		if _, spared := spare[start.turnID]; !spared {
			interrupting = append(interrupting, start)
		}
	}
	if _, _, err := recordTurnInterruptions(tx, workspace, interrupting, cause, at); err != nil {
		return nil, err
	}
	args := []any{cause, workspace, claimantSessionID}
	query := `UPDATE turn_lifecycle_claim
			SET end_seq=0, end_cause=?
			WHERE workspace=? AND claimant_session_id=? AND end_seq IS NULL`
	if len(spare) > 0 {
		for id := range spare {
			args = append(args, id)
		}
		query += ` AND turn_id NOT IN (` + statedb.Placeholders(len(spare)) + `)`
	}
	result, err := tx.Exec(query, args...)
	if err != nil {
		return nil, fmt.Errorf("ssm: synthesize turn end for workspace %q session %q: %w", workspace, claimantSessionID, err)
	}
	changed, err := result.RowsAffected()
	if err != nil {
		return nil, fmt.Errorf("ssm: inspect synthesized turn end for workspace %q session %q: %w", workspace, claimantSessionID, err)
	}
	if int(changed) != len(closing) {
		return nil, fmt.Errorf("ssm: synthesized turn end for workspace %q session %q closed %d claims, want exactly the %d active ones not spared by durable evidence",
			workspace, claimantSessionID, changed, len(closing))
	}
	return closing, nil
}

// formatClosedTurnIDs renders closed identities for the log, naming the legacy
// (identity-less) claim rather than printing an empty string nobody can read.
func formatClosedTurnIDs(ids []string) string {
	printable := make([]string, len(ids))
	for i, id := range ids {
		printable[i] = id
		if id == "" {
			printable[i] = "<legacy>"
		}
	}
	return "[" + strings.Join(printable, ",") + "]"
}

// recordTurnStart admits one turn start into the ledger.
//
// A START WHOSE TURN THIS DAEMON ALREADY KILLED IS ADMITTED CLOSED. This is the
// edge PART B of the turn-liveness work exists for. When a supersede or teardown
// interrupts a live turn, the end is recorded against the start's STORE
// coordinate (turninterruption.go) — the identity a later replay of the same
// stream presents, which is claimant-independent. A replacement session
// subscribing to that stream replays the very same `turn_started`, and here it
// reconstructs a MATCHED START/END PAIR rather than an open claim for a turn
// that was killed minutes ago and whose `TurnEnded` no process exists to send.
//
// reconstructedEnd names the cause when that happened, so the caller can say out
// loud that a replayed start arrived for an already-ended turn. It is "" on the
// ordinary path.
//
// settledReplay names the OTHER tolerated replay: a start whose identity already
// carries a durable start AND a durable end at a DIFFERENT store coordinate. See
// recordTurnStartRow.
func recordTurnStart(tx *sql.Tx, workspace, claimantSessionID, eventSessionID, id string, seq uint64) (replayed bool, reconstructedEnd string, settledReplay string, err error) {
	killedCause, killed, err := interruptedStartCause(tx, workspace, eventSessionID, id)
	if err != nil {
		return false, "", "", err
	}
	endSeqValue := any(nil)
	endCauseValue := ""
	if killed {
		// end_seq=0 with a named end_cause is the ledger's existing vocabulary
		// for "the daemon ended this, no event produced the close" — the same
		// shape synthesizeTurnEnds writes. `end_seq IS NULL` stays the ONE
		// definition of an active claim, so the derivation needs no second rule.
		endSeqValue = int64(0)
		endCauseValue = killedCause
		reconstructedEnd = killedCause
	}
	replayed, settledReplay, err = recordTurnStartRow(tx, workspace, claimantSessionID, eventSessionID, id, seq, endSeqValue, endCauseValue)
	if err != nil || replayed {
		return replayed, "", settledReplay, err
	}
	return false, reconstructedEnd, "", nil
}

// recordTurnStartRow writes (or recognizes) the claim row one start names.
//
// A SETTLED IDENTITY IS REPLAYED, NOT REJECTED. When the claim this identity
// names already holds a durable start AND a durable end, a second `TurnStarted`
// for it is a redelivery of a turn whose whole life is already recorded. That
// happens for real, on two known paths: a shim control-request timeout declares
// a submit unknown-fate while the shim actually took it, so the queue redelivers
// the same turn identity at a NEW store coordinate; and a multi-delivered
// interject crossing a warm compaction mints the same identity twice. Refusing
// it used to be terminal for the SESSION — shimclient classified the refusal as
// a fatal lifecycle rejection, the controller exited, and because the duplicate
// is durable in the vendor stream, every later resume replayed it and killed the
// session again. The turn is over either way, so admitting the redelivery
// idempotently loses nothing and keeps the session resumable.
//
// A CONFLICTING duplicate — the identity's claim is still OPEN, so two live
// starts contend for one turn — is still refused, and loudly. That refusal now
// carries ErrTurnStartConflict so its blast radius is the TURN rather
// than the session.
//
// settledReplay is the human-readable account of the tolerated replay, "" when
// nothing was tolerated.
func recordTurnStartRow(tx *sql.Tx, workspace, claimantSessionID, eventSessionID, id string, seq uint64, endSeqValue any, endCauseValue string) (replayed bool, settledReplay string, err error) {
	if id != "" {
		var startSeq uint64
		var startEventSessionID string
		var endSeq sql.NullInt64
		err := tx.QueryRow(`SELECT start_seq, start_event_session_id, end_seq FROM turn_lifecycle_claim
			WHERE workspace=? AND claimant_session_id=? AND turn_id=?`,
			workspace, claimantSessionID, id).Scan(&startSeq, &startEventSessionID, &endSeq)
		switch {
		case err == sql.ErrNoRows:
		case err != nil:
			return false, "", fmt.Errorf("ssm: read turn start claim %q: %w", id, err)
		case startSeq == 0 && !endSeq.Valid &&
			(startEventSessionID == "" || startEventSessionID == eventSessionID):
			_, err = tx.Exec(`UPDATE turn_lifecycle_claim
				SET start_seq=?, start_event_session_id=?,
					end_seq=COALESCE(end_seq, ?), end_cause=CASE WHEN ?='' THEN end_cause ELSE ? END
				WHERE workspace=? AND claimant_session_id=? AND turn_id=?`,
				int64(seq), eventSessionID, endSeqValue, endCauseValue, endCauseValue,
				workspace, claimantSessionID, id)
			return false, "", err
		case startEventSessionID == eventSessionID && startSeq == seq:
			return true, "", nil
		case endSeq.Valid:
			// THE TOLERATED REPLAY. The identity is settled: it has a durable
			// start and a durable end, so nothing about this redelivery can be
			// in flight and nothing it could contend for is still open. The
			// claim is left exactly as it is, except that a claim which never
			// bound a store coordinate (a handshake-adopted row closed before
			// its start arrived) takes this start's coordinate as evidence.
			if startSeq == 0 && (startEventSessionID == "" || startEventSessionID == eventSessionID) {
				if _, err := tx.Exec(`UPDATE turn_lifecycle_claim
					SET start_seq=?, start_event_session_id=?
					WHERE workspace=? AND claimant_session_id=? AND turn_id=? AND start_seq=0`,
					int64(seq), eventSessionID, workspace, claimantSessionID, id); err != nil {
					return false, "", fmt.Errorf("ssm: bind settled turn start %q to seq=%d: %w", id, seq, err)
				}
			}
			return true, fmt.Sprintf("settled turn start identity %q replayed at event_session=%q seq=%d (durable event_session=%q start_seq=%d end_seq=%d)",
				id, eventSessionID, seq, startEventSessionID, startSeq, endSeq.Int64), nil
		default:
			return false, "", fmt.Errorf("%w: duplicate turn start identity %q at event_session=%q seq=%d (durable event_session=%q start_seq=%d end_seq=%v)",
				ErrTurnStartConflict, id, eventSessionID, seq, startEventSessionID, startSeq, endSeq)
		}
	}
	if id == "" {
		var claimID int64
		var startSeq uint64
		// THE STORE COORDINATE IS PART OF A LEGACY CLAIM'S IDENTITY TOO. A
		// vendor uuid rotation restarts the store's seq space at 1 under the
		// SAME claimant, so a check on seq alone reads the NEW space's first
		// start as a replay of the retired space's and silently drops the turn
		// it opens. The named path has always compared the event session; the
		// legacy path must, because it is the only other thing distinguishing
		// two starts that both carry no turn id.
		var startEventSessionID string
		err := tx.QueryRow(`SELECT claim_id, start_seq, start_event_session_id FROM turn_lifecycle_claim
			WHERE workspace=? AND claimant_session_id=? AND turn_id='' AND end_seq IS NULL
			ORDER BY claim_id LIMIT 1`,
			workspace, claimantSessionID).Scan(&claimID, &startSeq, &startEventSessionID)
		sameSpace := startEventSessionID == "" || eventSessionID == "" || startEventSessionID == eventSessionID
		switch {
		case err == nil && startSeq == 0:
			if _, err := tx.Exec(`UPDATE turn_lifecycle_claim SET start_seq=? WHERE claim_id=?`,
				int64(seq), claimID); err != nil {
				return false, "", fmt.Errorf("ssm: bind legacy handshake claim to seq=%d: %w", seq, err)
			}
			return false, "", nil
		case err == nil && startSeq == seq && sameSpace:
			return true, "", nil
		case err == nil:
			// A second legacy turn may be queued behind the first. Its empty
			// identity cannot correlate, so strict FIFO ordering is the only
			// proof available and each start gets its own claim row.
		case err != sql.ErrNoRows:
			return false, "", fmt.Errorf("ssm: read active legacy turn start seq=%d: %w", seq, err)
		}
		var one int
		err = tx.QueryRow(`SELECT 1 FROM turn_lifecycle_claim
			WHERE workspace=? AND claimant_session_id=? AND turn_id='' AND start_seq=?
				AND (start_event_session_id='' OR start_event_session_id=?) LIMIT 1`,
			workspace, claimantSessionID, int64(seq), eventSessionID).Scan(&one)
		if err == nil {
			return true, "", nil
		}
		if err != sql.ErrNoRows {
			return false, "", fmt.Errorf("ssm: read completed legacy turn start seq=%d: %w", seq, err)
		}
	}
	_, insertErr := tx.Exec(`INSERT INTO turn_lifecycle_claim(
		workspace, claimant_session_id, turn_id, start_seq, start_event_session_id,
		end_seq, end_cause
	) VALUES (?,?,?,?,?,?,?)`, workspace, claimantSessionID, id, int64(seq), eventSessionID,
		endSeqValue, endCauseValue)
	if insertErr != nil {
		return false, "", fmt.Errorf("ssm: persist turn start %q seq=%d: %w", id, seq, insertErr)
	}
	return false, "", nil
}

func recordTurnBridge(
	tx *sql.Tx,
	workspace, claimantSessionID, previousSessionID, eventSessionID, id string,
	seq uint64,
) (bool, error) {
	var startEventSessionID string
	var bridgeSeq sql.NullInt64
	var bridgeEventSessionID string
	var endSeq sql.NullInt64
	err := tx.QueryRow(`SELECT start_event_session_id, bridge_seq,
			bridge_event_session_id, end_seq
		FROM turn_lifecycle_claim
		WHERE workspace=? AND claimant_session_id=? AND turn_id=?`,
		workspace, claimantSessionID, id,
	).Scan(&startEventSessionID, &bridgeSeq, &bridgeEventSessionID, &endSeq)
	switch {
	case err == sql.ErrNoRows:
		_, err = tx.Exec(`INSERT INTO turn_lifecycle_claim(
				workspace, claimant_session_id, turn_id, start_seq,
				start_event_session_id, bridge_seq, bridge_event_session_id
			) VALUES (?,?,?,?,?,?,?)`,
			workspace, claimantSessionID, id, 0, previousSessionID,
			int64(seq), eventSessionID,
		)
		if err != nil {
			return false, fmt.Errorf("ssm: persist turn bridge %q seq=%d: %w", id, seq, err)
		}
		return false, nil
	case err != nil:
		return false, fmt.Errorf("ssm: read turn bridge claim %q: %w", id, err)
	case bridgeSeq.Valid && uint64(bridgeSeq.Int64) == seq &&
		bridgeEventSessionID == eventSessionID &&
		startEventSessionID == previousSessionID:
		return true, nil
	case endSeq.Valid:
		// Refused, not accepted: the claim keeps its recorded end and this bridge
		// is written nowhere. Only the ESCALATION differs — see ErrTurnBridgeDeadClaim.
		return false, fmt.Errorf("%w: turn bridge id %q seq=%d conflicts with completed claim end_seq=%d",
			ErrTurnBridgeDeadClaim, id, seq, endSeq.Int64)
	case bridgeSeq.Valid:
		return false, fmt.Errorf("turn bridge id %q at event_session=%q seq=%d conflicts with durable event_session=%q seq=%d",
			id, eventSessionID, seq, bridgeEventSessionID, bridgeSeq.Int64)
	case startEventSessionID != "" && startEventSessionID != previousSessionID:
		return false, fmt.Errorf("turn bridge id %q names previous_session=%q but durable start belongs to %q",
			id, previousSessionID, startEventSessionID)
	}
	result, err := tx.Exec(`UPDATE turn_lifecycle_claim
		SET start_event_session_id=CASE
				WHEN start_event_session_id='' THEN ?
				ELSE start_event_session_id
			END,
			bridge_seq=?, bridge_event_session_id=?
		WHERE workspace=? AND claimant_session_id=? AND turn_id=? AND end_seq IS NULL`,
		previousSessionID, int64(seq), eventSessionID,
		workspace, claimantSessionID, id,
	)
	if err != nil {
		return false, fmt.Errorf("ssm: record turn bridge %q: %w", id, err)
	}
	changed, err := result.RowsAffected()
	if err != nil {
		return false, fmt.Errorf("ssm: inspect turn bridge update %q: %w", id, err)
	}
	if changed != 1 {
		return false, fmt.Errorf("ssm: turn bridge %q updated %d claims, want exactly one", id, changed)
	}
	return false, nil
}

// turnEndIsHistorical reports whether ev was produced by a query other than the
// one the caller is bound to.
//
// ONE COMPARISON, nothing else. The producer stamped the query it was running
// onto the envelope at construction time, so the answer is carried by the event
// rather than reconstructed here from delivery order or ledger archaeology.
//
// EMPTY IS LIVE. FAIL CLOSED. A producer that predates query_instance_id stamps
// nothing, and the claim check must then apply to it exactly as it did before
// the field existed. A caller with no bound query likewise admits no history.
func turnEndIsHistorical(liveQueryInstanceID string, ev *corev1.Event) bool {
	if liveQueryInstanceID == "" {
		return false
	}
	eventQuery := ev.GetQueryInstanceId()
	return eventQuery != "" && eventQuery != liveQueryInstanceID
}

// turnEndClaimScan is one query's classification of a turn end's candidate
// claim rows: at most one of activeClaim, sessionConflict, or
// synthesizedCause is set, on the same terms the original single-scope scan
// enforced. rowCount is the RAW row total — including rows that resolved
// neither an active claim, a conflict, nor a synthesized cause — so a caller
// can tell "found nothing at all" from "found rows that did not classify".
type turnEndClaimScan struct {
	activeClaim      int64
	sessionConflict  string
	synthesizedCause string
	rowCount         int
}

// scanTurnEndCandidates classifies rows already scoped to one claim lookup.
// exact is true the instant a row proves this exact boundary (its own seq and
// event_session_id) already closed the claim — an idempotent replay receipt,
// returned immediately without classifying the remaining rows, exactly as the
// single-scope version did.
func scanTurnEndCandidates(rows *sql.Rows, id string, seq uint64, eventSessionID string) (turnEndClaimScan, bool, error) {
	var scan turnEndClaimScan
	for rows.Next() {
		scan.rowCount++
		var claimID int64
		var startEventSessionID string
		var bridgeEventSessionID string
		var endSeq sql.NullInt64
		var endEventSessionID string
		var endCause string
		if err := rows.Scan(
			&claimID, &startEventSessionID, &bridgeEventSessionID,
			&endSeq, &endEventSessionID, &endCause,
		); err != nil {
			return scan, false, fmt.Errorf("ssm: scan turn end claim %q: %w", id, err)
		}
		if endSeq.Valid && uint64(endSeq.Int64) == seq &&
			endEventSessionID == eventSessionID {
			return scan, true, nil
		}
		if endSeq.Valid && endCause != "" && scan.activeClaim == 0 && scan.synthesizedCause == "" {
			// The claim was ended by a SYNTHESIZED close, and this is the real
			// boundary arriving afterwards — the pre-restart turn's own
			// `TurnEnded`, replayed off the store once the subscription
			// reopened. It reports the very thing the synthesized close already
			// recorded, so it is admitted as an already-accounted boundary
			// rather than rejected as a contradiction.
			scan.synthesizedCause = endCause
			continue
		}
		if !endSeq.Valid && scan.activeClaim == 0 {
			expectedSessionID := startEventSessionID
			if bridgeEventSessionID != "" {
				expectedSessionID = bridgeEventSessionID
			}
			if expectedSessionID != "" && expectedSessionID != eventSessionID {
				scan.sessionConflict = expectedSessionID
				continue
			}
			scan.activeClaim = claimID
		}
	}
	if err := rows.Err(); err != nil {
		return scan, false, fmt.Errorf("ssm: iterate turn end claim %q: %w", id, err)
	}
	return scan, false, nil
}

// queryTurnEndCandidates runs one turn-end lookup and classifies it.
// scopeToClaimant narrows the WHERE clause to claimantSessionID; the legacy
// (id=="") path always scopes to the claimant, since an empty identity can
// only be correlated by FIFO order within one claimant's own queue.
func queryTurnEndCandidates(tx *sql.Tx, workspace, claimantSessionID, id string, seq uint64, eventSessionID string, scopeToClaimant bool) (turnEndClaimScan, bool, error) {
	query := `SELECT claim_id, start_event_session_id, bridge_event_session_id,
			end_seq, end_event_session_id, end_cause
		FROM turn_lifecycle_claim
		WHERE workspace=?`
	args := []any{workspace}
	if scopeToClaimant {
		query += ` AND claimant_session_id=?`
		args = append(args, claimantSessionID)
	}
	query += ` AND turn_id=?`
	args = append(args, id)
	if id == "" {
		query += ` ORDER BY claim_id`
	}
	rows, err := tx.Query(query, args...)
	if err != nil {
		return turnEndClaimScan{}, false, fmt.Errorf("ssm: read turn end claim %q: %w", id, err)
	}
	defer rows.Close()
	scan, exact, err := scanTurnEndCandidates(rows, id, seq, eventSessionID)
	if err != nil {
		return scan, false, err
	}
	if err := rows.Close(); err != nil {
		return scan, false, fmt.Errorf("ssm: close turn end claim rows %q: %w", id, err)
	}
	return scan, exact, nil
}

// historical says the end was produced by a retired query. Such an end reports
// a turn whose owning query() invocation is gone: there is no live claim it
// could belong to, and no live state it may touch.
//
// crossGeneration reports whether the claim this end resolved against was
// found only by widening the lookup past the caller's own claimant — see the
// CROSS-GENERATION log line at the call site for why that is expected rather
// than suspicious.
func recordTurnEnd(
	tx *sql.Tx,
	workspace, claimantSessionID, eventSessionID, id string,
	seq uint64,
	historical bool,
) (replayed bool, crossGeneration bool, err error) {
	scan, exact, err := queryTurnEndCandidates(tx, workspace, claimantSessionID, id, seq, eventSessionID, true)
	if err != nil {
		return false, false, err
	}
	if exact {
		return true, false, nil
	}
	// THE CLAIM'S IDENTITY IS ITS TURN ID, NOT ITS CLAIMANT. A workspace's
	// daemon session id is re-minted on every CreateSession — hibernate,
	// revive, and reopen all mint a fresh claimant for the very same vendor
	// conversation — while turn_lifecycle_claim rows stay keyed to whichever
	// claimant opened them and are never rebound to the replacement. A named
	// turn id is generated fresh per turn, so at most one claimant in a
	// workspace will ever hold a row for it; when the CALLER's OWN generation
	// proves it holds NOTHING for this turn id (rowCount==0, not merely no
	// active/conflicting/synthesized match among rows it did find), the search
	// widens to the whole workspace exactly once before this boundary is
	// judged a violation. A row found only there is not a contradiction: it is
	// the SAME claim, opened by a generation that no longer exists to answer
	// for it.
	if id != "" && scan.rowCount == 0 {
		widened, widenedExact, widenErr := queryTurnEndCandidates(tx, workspace, "", id, seq, eventSessionID, false)
		if widenErr != nil {
			return false, false, widenErr
		}
		if widenedExact {
			return true, true, nil
		}
		if widened.rowCount > 0 {
			scan = widened
			crossGeneration = true
		}
	}
	if scan.activeClaim == 0 {
		if scan.sessionConflict != "" {
			return false, crossGeneration, fmt.Errorf("turn end id %q seq=%d belongs to event_session=%q, durable claim expects %q",
				id, seq, eventSessionID, scan.sessionConflict)
		}
		if scan.synthesizedCause != "" {
			// Idempotent, and deliberately reported as a REPLAY: the boundary is
			// real, the daemon already accounted for it, and nothing about the
			// state it produces is left to write a second time.
			return true, crossGeneration, nil
		}
		if historical {
			// AN END FROM A RETIRED QUERY IS HISTORY, NOT A CONTRADICTION.
			// The query that produced it no longer exists, so it can own no
			// live claim and there is nothing here to be inconsistent with.
			// Accepted, reported as a replay, and no row is written: the
			// transaction leaves the ledger exactly as it found it.
			return true, crossGeneration, nil
		}
		return false, crossGeneration, fmt.Errorf("turn end id %q seq=%d has no durable active claim", id, seq)
	}
	if _, err := tx.Exec(`UPDATE turn_lifecycle_claim
			SET end_seq=?, end_event_session_id=?
			WHERE claim_id=? AND end_seq IS NULL`,
		int64(seq), eventSessionID, scan.activeClaim); err != nil {
		return false, crossGeneration, fmt.Errorf("ssm: persist turn end %q seq=%d: %w", id, seq, err)
	}
	return false, crossGeneration, nil
}

// ActiveTurnIDs reports every turn claim the session holds OPEN, in claim order.
//
// It is the READ half of the ledger the boundary resolvers write, and it exists
// for one caller: the scheduled shutdown's drain hold, which must name the turn
// it is waiting on even when the daemon committed to the prompt before it
// observed the turn's start (sessioncontroller, nameAcceptedHold). Reading the
// ledger rather than process memory is what lets that hold be named from the
// same authority the log and the webapp name it from.
//
// A workspace or session that holds nothing answers with an empty slice and no
// error: holding nothing is an answer, not a failure.
func (m *Manager) ActiveTurnIDs(workspace, claimantSessionID string) ([]string, error) {
	if workspace == "" || claimantSessionID == "" {
		err := fmt.Errorf("ssm: reading active turn claims requires workspace and claimant session id")
		m.logf("ssm: turn ledger decision=reject_validation operation=active_turn_ids workspace=%q claimant_session=%q error=%v",
			workspace, claimantSessionID, err)
		return nil, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return activeTurnIDs(m.db, workspace, claimantSessionID)
}

// TurnClaimExists answers whether the ledger holds ANY claim for one turn
// identity in a workspace, open or closed.
//
// It is the reconciliation a caller owes an UNKNOWN-FATE submit. A control
// request that times out has not failed: the shim may have taken the prompt and
// started its turn while the ack was still in flight. Resubmitting that same
// identity on the strength of the timeout alone is what MINTED the duplicate
// turn starts that used to sever sessions, so the caller asks the ledger first
// and a claim under the identity is proof the submit landed.
//
// The lookup is workspace-wide on purpose. A claimant session id is re-minted
// by hibernate, revive and reopen while claims stay keyed to whichever claimant
// opened them, so scoping the question to the CURRENT claimant would answer
// "no claim" for a turn this very workspace is running.
func (m *Manager) TurnClaimExists(workspace, turnID string) (bool, error) {
	if workspace == "" || turnID == "" {
		err := fmt.Errorf("ssm: probing a turn claim requires workspace and turn id")
		m.logf("ssm: turn ledger decision=reject_validation operation=turn_claim_exists workspace=%q turn_id=%q error=%v",
			workspace, turnID, err)
		return false, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	var one int
	err := m.db.QueryRow(`SELECT 1 FROM turn_lifecycle_claim
		WHERE workspace=? AND turn_id=? LIMIT 1`, workspace, turnID).Scan(&one)
	switch {
	case err == sql.ErrNoRows:
		m.logf("ssm: turn ledger decision=probe_absent operation=turn_claim_exists workspace=%s turn_id=%q — no claim was ever opened under this identity",
			workspace, turnID)
		return false, nil
	case err != nil:
		readErr := fmt.Errorf("ssm: probe turn claim %q: %w", turnID, err)
		m.logf("ssm: turn ledger decision=probe_unreadable operation=turn_claim_exists workspace=%s turn_id=%q error=%v",
			workspace, turnID, readErr)
		return false, readErr
	}
	m.logf("ssm: turn ledger decision=probe_present operation=turn_claim_exists workspace=%s turn_id=%q — a claim exists under this identity",
		workspace, turnID)
	return true, nil
}

func activeTurnIDs(q interface {
	Query(query string, args ...any) (*sql.Rows, error)
}, workspace, claimantSessionID string) ([]string, error) {
	rows, err := q.Query(`SELECT turn_id FROM turn_lifecycle_claim
		WHERE workspace=? AND claimant_session_id=? AND end_seq IS NULL ORDER BY claim_id`,
		workspace, claimantSessionID)
	if err != nil {
		return nil, fmt.Errorf("ssm: read active turn claims: %w", err)
	}
	defer rows.Close()
	var ids []string
	for rows.Next() {
		var id string
		if err := rows.Scan(&id); err != nil {
			return nil, fmt.Errorf("ssm: scan active turn claim: %w", err)
		}
		ids = append(ids, id)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("ssm: iterate active turn claims: %w", err)
	}
	return ids, nil
}

func equalStrings(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}
