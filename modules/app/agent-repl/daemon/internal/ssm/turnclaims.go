package ssm

import (
	"database/sql"
	"errors"
	"fmt"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
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
)

// ResolveTurnLifecycle durably validates and records one STREAM turn boundary.
//
// The ledger is separate from workspace_state because queued turns have
// multiple simultaneous identities while the rendered session-status lifecycle remains one
// `thinking` claim. Completed claims remain as replay receipts: if the daemon
// crashes after recording a boundary but before advancing last_seen_seq, the
// replayed event is admitted only when both its identity and seq match the
// durable receipt.
func (m *Manager) ResolveTurnLifecycle(workspace, claimantSessionID string, ev *corev1.Event) (before, after []string, replayed bool, err error) {
	if workspace == "" {
		err := fmt.Errorf("ssm: turn lifecycle got an empty workspace")
		m.logf("ssm: turn ledger decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return nil, nil, false, err
	}
	if claimantSessionID == "" {
		err := fmt.Errorf("ssm: turn lifecycle for workspace %q got an empty claimant session id", workspace)
		m.logf("ssm: turn ledger decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return nil, nil, false, err
	}
	if ev == nil || ev.GetSessionId() == "" || ev.GetSeq() == 0 {
		err := fmt.Errorf("ssm: turn lifecycle requires a persistent event with session_id and seq")
		m.logf("ssm: turn ledger decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return nil, nil, false, err
	}
	if ev.GetPlane() != corev1.Plane_PLANE_STREAM {
		err := fmt.Errorf("ssm: turn lifecycle rejected plane=%s workspace=%s claimant_session=%s event_session=%s seq=%d",
			ev.GetPlane().String(), workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq())
		m.logf("ssm: turn ledger decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d plane=%s turn_id=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
			turnCorrelation(ev), ev.GetRequestId(), err)
		return nil, nil, false, err
	}
	id := turnCorrelation(ev)
	if ev.GetRequestId() != id {
		err := fmt.Errorf("ssm: turn lifecycle envelope mismatch workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q request_id=%q",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id, ev.GetRequestId())
		m.logf("ssm: turn ledger decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d plane=%s turn_id=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
			id, ev.GetRequestId(), err)
		return nil, nil, false, err
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return nil, nil, false, fmt.Errorf("ssm: begin turn lifecycle transaction: %w", err)
	}
	defer tx.Rollback()

	before, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, nil, false, err
	}
	if _, started := ev.GetPayload().(*corev1.Event_TurnStarted); started && m.hibernationLeases[workspace] != 0 {
		isReplay, probeErr := recordTurnStart(tx, workspace, claimantSessionID, ev.GetSessionId(), id, ev.GetSeq())
		if probeErr != nil {
			return before, before, false, probeErr
		}
		if !isReplay {
			return before, before, false, m.rejectStartDuringHibernationLocked(workspace, "TurnStarted claim")
		}
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted:
		replayed, err = recordTurnStart(
			tx, workspace, claimantSessionID, ev.GetSessionId(), id, ev.GetSeq(),
		)
	case *corev1.Event_TurnEnded:
		replayed, err = recordTurnEnd(
			tx, workspace, claimantSessionID, ev.GetSessionId(), id, ev.GetSeq(),
		)
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
	if err := tx.Commit(); err != nil {
		return before, nil, false, fmt.Errorf("ssm: commit turn lifecycle transaction: %w", err)
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
func (m *Manager) ReconcileTurnHandshake(workspace, claimantSessionID string, ids []string, legacyActive bool) (before, after, closed []string, err error) {
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
		closed, err = synthesizeTurnEnds(tx, workspace, claimantSessionID, TurnCloseRestartInterrupted)
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
	if len(ids) == 0 && !legacyActive {
		// The reason NAMES the observation. A handshake that found no durable
		// claim to close is tidying a latch whose turn is merely unaccounted
		// for; one that just synthesized an end for a phantom claim is
		// reporting a turn CUT BY A RESTART, and the row is the only place a
		// user can see that it was.
		reason := causeShimHandshake
		if len(closed) > 0 {
			reason = TurnCloseRestartInterrupted
		}
		if _, err := m.closeStaleTurnLocked(workspace, claimantSessionID, reason, true); err != nil {
			// Never swallowed, and never allowed to fail the handshake: the
			// reconciliation above has already committed and DaemonHello is
			// gated on this call's error, so returning here would refuse a
			// perfectly good session over a stale row it merely failed to tidy.
			m.logf("ssm: closing the stale turn on the shim handshake FAILED workspace=%s claimant_session=%s hello_ids=%v: %v — the session-status lifecycle may stay latched in `thinking` until the next edge supersedes it",
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
	if cause != TurnCloseRestartInterrupted && cause != TurnCloseAlreadyComplete {
		return nil, fmt.Errorf("ssm: SynthesizeTurnClose for workspace %q session %q got cause %q; a synthesized end must name one of the observations that authorize it (%q, %q)",
			workspace, claimantSessionID, cause, TurnCloseRestartInterrupted, TurnCloseAlreadyComplete)
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return nil, fmt.Errorf("ssm: begin synthesized turn close transaction: %w", err)
	}
	defer tx.Rollback()
	closed, err = synthesizeTurnEnds(tx, workspace, claimantSessionID, cause)
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

// synthesizeTurnEnds ends every active claim held by claimantSessionID inside
// tx, stamping cause so a later genuine `TurnEnded` for the same identity can be
// recognized as already accounted for rather than read as a contradiction.
//
// end_seq is 0 rather than a real store seq because no event produced this
// close. That is exactly what distinguishes it in the ledger, and it is why
// end_cause is written alongside: `end_seq IS NULL` remains the ONE definition
// of an active claim, so nothing downstream has to learn a second one.
func synthesizeTurnEnds(tx *sql.Tx, workspace, claimantSessionID, cause string) ([]string, error) {
	active, err := activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, err
	}
	if len(active) == 0 {
		return nil, nil
	}
	result, err := tx.Exec(`UPDATE turn_lifecycle_claim
			SET end_seq=0, end_cause=?
			WHERE workspace=? AND claimant_session_id=? AND end_seq IS NULL`,
		cause, workspace, claimantSessionID)
	if err != nil {
		return nil, fmt.Errorf("ssm: synthesize turn end for workspace %q session %q: %w", workspace, claimantSessionID, err)
	}
	changed, err := result.RowsAffected()
	if err != nil {
		return nil, fmt.Errorf("ssm: inspect synthesized turn end for workspace %q session %q: %w", workspace, claimantSessionID, err)
	}
	if int(changed) != len(active) {
		return nil, fmt.Errorf("ssm: synthesized turn end for workspace %q session %q closed %d claims, want exactly the %d active ones",
			workspace, claimantSessionID, changed, len(active))
	}
	return active, nil
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

func recordTurnStart(tx *sql.Tx, workspace, claimantSessionID, eventSessionID, id string, seq uint64) (bool, error) {
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
			return false, fmt.Errorf("ssm: read turn start claim %q: %w", id, err)
		case startSeq == 0 && !endSeq.Valid &&
			(startEventSessionID == "" || startEventSessionID == eventSessionID):
			_, err = tx.Exec(`UPDATE turn_lifecycle_claim
				SET start_seq=?, start_event_session_id=?
				WHERE workspace=? AND claimant_session_id=? AND turn_id=?`,
				int64(seq), eventSessionID, workspace, claimantSessionID, id)
			return false, err
		case startEventSessionID == eventSessionID && startSeq == seq:
			return true, nil
		default:
			return false, fmt.Errorf("duplicate turn start identity %q at event_session=%q seq=%d (durable event_session=%q start_seq=%d end_seq=%v)",
				id, eventSessionID, seq, startEventSessionID, startSeq, endSeq)
		}
	}
	if id == "" {
		var claimID int64
		var startSeq uint64
		err := tx.QueryRow(`SELECT claim_id, start_seq FROM turn_lifecycle_claim
			WHERE workspace=? AND claimant_session_id=? AND turn_id='' AND end_seq IS NULL
			ORDER BY claim_id LIMIT 1`,
			workspace, claimantSessionID).Scan(&claimID, &startSeq)
		switch {
		case err == nil && startSeq == 0:
			if _, err := tx.Exec(`UPDATE turn_lifecycle_claim SET start_seq=? WHERE claim_id=?`,
				int64(seq), claimID); err != nil {
				return false, fmt.Errorf("ssm: bind legacy handshake claim to seq=%d: %w", seq, err)
			}
			return false, nil
		case err == nil && startSeq == seq:
			return true, nil
		case err == nil:
			// A second legacy turn may be queued behind the first. Its empty
			// identity cannot correlate, so strict FIFO ordering is the only
			// proof available and each start gets its own claim row.
		case err != sql.ErrNoRows:
			return false, fmt.Errorf("ssm: read active legacy turn start seq=%d: %w", seq, err)
		}
		var one int
		err = tx.QueryRow(`SELECT 1 FROM turn_lifecycle_claim
			WHERE workspace=? AND claimant_session_id=? AND turn_id='' AND start_seq=? LIMIT 1`,
			workspace, claimantSessionID, int64(seq)).Scan(&one)
		if err == nil {
			return true, nil
		}
		if err != sql.ErrNoRows {
			return false, fmt.Errorf("ssm: read completed legacy turn start seq=%d: %w", seq, err)
		}
	}
	_, err := tx.Exec(`INSERT INTO turn_lifecycle_claim(
		workspace, claimant_session_id, turn_id, start_seq, start_event_session_id
	) VALUES (?,?,?,?,?)`, workspace, claimantSessionID, id, int64(seq), eventSessionID)
	if err != nil {
		return false, fmt.Errorf("ssm: persist turn start %q seq=%d: %w", id, seq, err)
	}
	return false, nil
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

func recordTurnEnd(
	tx *sql.Tx,
	workspace, claimantSessionID, eventSessionID, id string,
	seq uint64,
) (bool, error) {
	query := `SELECT claim_id, start_event_session_id, bridge_event_session_id,
			end_seq, end_event_session_id, end_cause
		FROM turn_lifecycle_claim
		WHERE workspace=? AND claimant_session_id=? AND turn_id=?`
	if id == "" {
		query += ` ORDER BY claim_id`
	}
	rows, err := tx.Query(query, workspace, claimantSessionID, id)
	if err != nil {
		return false, fmt.Errorf("ssm: read turn end claim %q: %w", id, err)
	}
	defer rows.Close()
	var activeClaim int64
	var sessionConflict string
	var synthesizedCause string
	for rows.Next() {
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
			return false, fmt.Errorf("ssm: scan turn end claim %q: %w", id, err)
		}
		if endSeq.Valid && uint64(endSeq.Int64) == seq &&
			endEventSessionID == eventSessionID {
			return true, nil
		}
		if endSeq.Valid && endCause != "" && activeClaim == 0 && synthesizedCause == "" {
			// The claim was ended by a SYNTHESIZED close, and this is the real
			// boundary arriving afterwards — the pre-restart turn's own
			// `TurnEnded`, replayed off the store once the subscription
			// reopened. It reports the very thing the synthesized close already
			// recorded, so it is admitted as an already-accounted boundary
			// rather than rejected as a contradiction.
			synthesizedCause = endCause
			continue
		}
		if !endSeq.Valid && activeClaim == 0 {
			expectedSessionID := startEventSessionID
			if bridgeEventSessionID != "" {
				expectedSessionID = bridgeEventSessionID
			}
			if expectedSessionID != "" && expectedSessionID != eventSessionID {
				sessionConflict = expectedSessionID
				continue
			}
			activeClaim = claimID
		}
	}
	if err := rows.Err(); err != nil {
		return false, fmt.Errorf("ssm: iterate turn end claim %q: %w", id, err)
	}
	if err := rows.Close(); err != nil {
		return false, fmt.Errorf("ssm: close turn end claim rows %q: %w", id, err)
	}
	if activeClaim == 0 {
		if sessionConflict != "" {
			return false, fmt.Errorf("turn end id %q seq=%d belongs to event_session=%q, durable claim expects %q",
				id, seq, eventSessionID, sessionConflict)
		}
		if synthesizedCause != "" {
			// Idempotent, and deliberately reported as a REPLAY: the boundary is
			// real, the daemon already accounted for it, and nothing about the
			// state it produces is left to write a second time.
			return true, nil
		}
		return false, fmt.Errorf("turn end id %q seq=%d has no durable active claim", id, seq)
	}
	if _, err := tx.Exec(`UPDATE turn_lifecycle_claim
			SET end_seq=?, end_event_session_id=?
			WHERE claim_id=? AND end_seq IS NULL`,
		int64(seq), eventSessionID, activeClaim); err != nil {
		return false, fmt.Errorf("ssm: persist turn end %q seq=%d: %w", id, seq, err)
	}
	return false, nil
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
