package ssm

import (
	"database/sql"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// ResolveTurnLifecycle durably validates and records one STREAM turn boundary.
//
// The ledger is separate from workspace_state because queued turns have
// multiple simultaneous identities while the rendered agent axis remains one
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
		m.logf("ssm: turn bridge decision=reject workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q previous_session=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id,
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
func (m *Manager) ReconcileTurnHandshake(workspace, claimantSessionID string, ids []string, legacyActive bool) (before, after []string, err error) {
	if workspace == "" || claimantSessionID == "" {
		err := fmt.Errorf("ssm: turn handshake requires workspace and claimant session id")
		m.logf("ssm: turn handshake decision=reject_validation workspace=%q claimant_session=%q hello_ids=%v legacy_active=%v error=%v",
			workspace, claimantSessionID, ids, legacyActive, err)
		return nil, nil, err
	}
	seen := make(map[string]struct{}, len(ids))
	for _, id := range ids {
		if id == "" {
			err := fmt.Errorf("ssm: turn handshake active_turn_ids contains an empty identity")
			m.logf("ssm: turn handshake decision=reject_validation workspace=%q claimant_session=%q hello_ids=%v legacy_active=%v error=%v",
				workspace, claimantSessionID, ids, legacyActive, err)
			return nil, nil, err
		}
		if _, duplicate := seen[id]; duplicate {
			err := fmt.Errorf("ssm: turn handshake active_turn_ids contains duplicate identity %q", id)
			m.logf("ssm: turn handshake decision=reject_validation workspace=%q claimant_session=%q hello_ids=%v legacy_active=%v error=%v",
				workspace, claimantSessionID, ids, legacyActive, err)
			return nil, nil, err
		}
		seen[id] = struct{}{}
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	tx, err := m.db.Begin()
	if err != nil {
		return nil, nil, fmt.Errorf("ssm: begin turn handshake transaction: %w", err)
	}
	defer tx.Rollback()
	before, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return nil, nil, err
	}
	switch {
	case len(ids) > 0 && len(before) == 0:
		for _, id := range ids {
			if _, err := tx.Exec(`INSERT INTO turn_lifecycle_claim(
				workspace, claimant_session_id, turn_id, start_seq
			) VALUES (?,?,?,0)`, workspace, claimantSessionID, id); err != nil {
				return before, before, fmt.Errorf("ssm: persist handshake turn %q: %w", id, err)
			}
		}
	case len(ids) > 0 && !equalStrings(before, ids):
		err = fmt.Errorf("handshake active_turn_ids=%v disagree with durable active turns=%v", ids, before)
	case legacyActive && len(before) == 0:
		if _, insertErr := tx.Exec(`INSERT INTO turn_lifecycle_claim(
			workspace, claimant_session_id, turn_id, start_seq
		) VALUES (?,?,?,0)`, workspace, claimantSessionID, ""); insertErr != nil {
			return before, before, fmt.Errorf("ssm: persist legacy handshake turn: %w", insertErr)
		}
	}
	if err != nil {
		m.logf("ssm: turn handshake decision=reject workspace=%s claimant_session=%s hello_ids=%v legacy_active=%v durable_before=%v error=%v",
			workspace, claimantSessionID, ids, legacyActive, before, err)
		return before, before, err
	}
	after, err = activeTurnIDs(tx, workspace, claimantSessionID)
	if err != nil {
		return before, nil, err
	}
	if err := tx.Commit(); err != nil {
		return before, nil, fmt.Errorf("ssm: commit turn handshake transaction: %w", err)
	}
	m.logf("ssm: turn handshake decision=accept workspace=%s claimant_session=%s hello_ids=%v legacy_active=%v durable_before=%v durable_after=%v",
		workspace, claimantSessionID, ids, legacyActive, before, after)

	// THE HANDSHAKE SNAPSHOT IS AUTHORITATIVE OVER THE AGENT AXIS TOO, and this
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
		if _, err := m.closeStaleTurnLocked(workspace, claimantSessionID, causeShimHandshake, true); err != nil {
			// Never swallowed, and never allowed to fail the handshake: the
			// reconciliation above has already committed and DaemonHello is
			// gated on this call's error, so returning here would refuse a
			// perfectly good session over a stale row it merely failed to tidy.
			m.logf("ssm: closing the stale turn on the shim handshake FAILED workspace=%s claimant_session=%s hello_ids=%v: %v — the agent axis may stay latched in `thinking` until the next edge supersedes it",
				workspace, claimantSessionID, ids, err)
		}
	}
	return before, after, nil
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
		return false, fmt.Errorf("turn bridge id %q seq=%d conflicts with completed claim end_seq=%d",
			id, seq, endSeq.Int64)
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
			end_seq, end_event_session_id
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
	for rows.Next() {
		var claimID int64
		var startEventSessionID string
		var bridgeEventSessionID string
		var endSeq sql.NullInt64
		var endEventSessionID string
		if err := rows.Scan(
			&claimID, &startEventSessionID, &bridgeEventSessionID,
			&endSeq, &endEventSessionID,
		); err != nil {
			return false, fmt.Errorf("ssm: scan turn end claim %q: %w", id, err)
		}
		if endSeq.Valid && uint64(endSeq.Int64) == seq &&
			endEventSessionID == eventSessionID {
			return true, nil
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
