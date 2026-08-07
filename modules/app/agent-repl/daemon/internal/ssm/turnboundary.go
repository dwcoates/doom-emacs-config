package ssm

import (
	"database/sql"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// TurnBoundary is one accepted turn boundary's outcome: what the durable
// ledger held before and after it, whether the ledger recognized it as a
// replay of a boundary it had already recorded, and — the part every consumer
// actually acts on — the ONE derived turn liveness the boundary produced.
//
// Liveness and the session-status row the boundary appended come from the same
// derivation inside the same transaction, so the workspace color and the prompt
// queue are reading one value rather than two that agree.
type TurnBoundary struct {
	// Before and After are the CLAIMANT-scoped active ids, kept because the
	// boundary log names them and a reader correlating a claim to the session
	// that opened it needs them. Neither is a liveness answer; Liveness is.
	Before []string
	After  []string
	// Replayed reports the ledger's own idempotency verdict: this exact
	// boundary (same identity, same store coordinate) was already recorded.
	Replayed bool
	// Liveness is THE answer. See turnliveness.go.
	Liveness TurnLiveness
	// AppendedState names the session-status token this boundary appended, or
	// "" when the axis already agreed with the derivation and nothing was
	// written. It is a log fact, not a second authority.
	AppendedState string
}

// ApplyTurnBoundary is the ONE door a turn boundary enters the daemon's state
// through. It moves the durable turn ledger, derives turn liveness from the
// ledger it just moved, and paints the session-status axis from that
// derivation — all in ONE transaction.
//
// THE DEDUP QUESTION IS GONE, not answered twice. The axis used to skip a row
// when (event_session_id, cause_seq) had already produced one, while the ledger
// judged the same event fresh whenever a NEW claimant replayed it. Here the axis
// is not deduplicated at all: it is RECONCILED to the derivation. A replayed
// start over an axis already painting `thinking` writes nothing because the axis
// already says what the derivation says, and a replayed start over an axis
// painting `ready` writes the `thinking` the derivation demands. Idempotence is
// a consequence of agreement rather than a rule that can drift from another one.
//
// liveQueryInstanceID is the query() invocation the CALLER is bound to; see
// turnEndIsHistorical.
func (m *Manager) ApplyTurnBoundary(workspace, claimantSessionID, liveQueryInstanceID string, ev *corev1.Event) (TurnBoundary, error) {
	if err := m.validateTurnBoundary(workspace, claimantSessionID, ev); err != nil {
		return TurnBoundary{}, err
	}
	eventSessionID := ev.GetSessionId()
	endState, endCause, ok := agentOrTaskSignal(ev)
	if !ok {
		err := fmt.Errorf("ssm: turn boundary resolver received %T", ev.GetPayload())
		m.logf("ssm: turn boundary decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d error=%v",
			workspace, claimantSessionID, eventSessionID, ev.GetSeq(), err)
		return TurnBoundary{}, err
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	owner, err := m.turnBoundaryOwnerLocked(workspace, claimantSessionID, ev)
	if err != nil {
		return TurnBoundary{}, err
	}

	tx, err := m.db.Begin()
	if err != nil {
		return TurnBoundary{}, fmt.Errorf("ssm: begin turn boundary transaction: %w", err)
	}
	defer tx.Rollback()

	before, after, replayed, err := m.moveTurnLedgerLocked(tx, workspace, claimantSessionID, liveQueryInstanceID, ev)
	if err != nil {
		return TurnBoundary{Before: before, After: before}, err
	}

	// THE ONE FOLD, run on the ledger this transaction has just moved.
	liveness, err := deriveTurnLiveness(tx, workspace)
	if err != nil {
		return TurnBoundary{Before: before, After: after}, err
	}

	state, causeKind, spendMark := m.turnBoundaryMarkLocked(workspace, ev, endState, endCause)
	appended, err := m.paintTurnBandLocked(tx, workspace, owner, eventSessionID, ev, liveness, state, causeKind)
	if err != nil {
		return TurnBoundary{Before: before, After: after, Liveness: liveness}, err
	}
	if err := tx.Commit(); err != nil {
		return TurnBoundary{Before: before, After: after}, fmt.Errorf("ssm: commit turn boundary transaction: %w", err)
	}
	// The interrupt mark's in-memory mutation is deferred past the commit
	// deliberately: a rolled-back boundary must leave the mark exactly as it
	// found it, or a stop the daemon delivered would be spent on a turn end
	// that never reached the log.
	spendMark()

	m.logf("ssm: turn boundary decision=accept workspace=%s claimant_session=%s event_session=%s owner_session=%s seq=%d kind=%s turn_id=%q active_before=%v active_after=%v replayed=%v liveness=%s appended=%q",
		workspace, claimantSessionID, eventSessionID, owner, ev.GetSeq(), causeKind,
		turnCorrelation(ev), before, after, replayed, liveness, appended)

	// A COMPACTION CANNOT OUTLIVE ITS TURN — see the same note in Apply. The
	// compaction axis is not turn liveness, so it is closed after the boundary's
	// own transaction rather than inside it.
	if _, ended := ev.GetPayload().(*corev1.Event_TurnEnded); ended {
		m.closeCompactingLocked(workspace, causeTurnEnded)
	}
	if err := m.reresolveLocked(workspace, causeKind, ev.GetSeq()); err != nil {
		return TurnBoundary{Before: before, After: after, Replayed: replayed, Liveness: liveness, AppendedState: appended}, err
	}
	return TurnBoundary{
		Before:        before,
		After:         after,
		Replayed:      replayed,
		Liveness:      liveness,
		AppendedState: appended,
	}, nil
}

// validateTurnBoundary is the boundary's envelope check, unchanged from the
// ledger resolver it was factored out of.
func (m *Manager) validateTurnBoundary(workspace, claimantSessionID string, ev *corev1.Event) error {
	if workspace == "" {
		err := fmt.Errorf("ssm: turn boundary got an empty workspace")
		m.logf("ssm: turn boundary decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return err
	}
	if claimantSessionID == "" {
		err := fmt.Errorf("ssm: turn boundary for workspace %q got an empty claimant session id", workspace)
		m.logf("ssm: turn boundary decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return err
	}
	if ev == nil || ev.GetSessionId() == "" || ev.GetSeq() == 0 {
		err := fmt.Errorf("ssm: turn boundary requires a persistent event with session_id and seq")
		m.logf("ssm: turn boundary decision=reject_validation workspace=%q claimant_session=%q event=%v error=%v",
			workspace, claimantSessionID, ev, err)
		return err
	}
	if ev.GetPlane() != corev1.Plane_PLANE_STREAM {
		err := fmt.Errorf("ssm: turn boundary rejected plane=%s workspace=%s claimant_session=%s event_session=%s seq=%d",
			ev.GetPlane().String(), workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq())
		m.logf("ssm: turn boundary decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d plane=%s turn_id=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
			turnCorrelation(ev), ev.GetRequestId(), err)
		return err
	}
	id := turnCorrelation(ev)
	if ev.GetRequestId() != id {
		err := fmt.Errorf("ssm: turn boundary envelope mismatch workspace=%s claimant_session=%s event_session=%s seq=%d turn_id=%q request_id=%q",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), id, ev.GetRequestId())
		m.logf("ssm: turn boundary decision=reject_validation workspace=%q claimant_session=%q event_session=%q seq=%d plane=%s turn_id=%q request_id=%q error=%v",
			workspace, claimantSessionID, ev.GetSessionId(), ev.GetSeq(), ev.GetPlane().String(),
			id, ev.GetRequestId(), err)
		return err
	}
	return nil
}

// turnBoundaryOwnerLocked resolves the DAEMON-minted session id the axis row is
// owned by. See Apply's note on why the row is never owned by the event's own
// (vendor) identity.
//
// Caller holds m.mu.
func (m *Manager) turnBoundaryOwnerLocked(workspace, claimantSessionID string, ev *corev1.Event) (string, error) {
	sid := ev.GetSessionId()
	if m.resolver == nil {
		return "", fmt.Errorf("ssm: no resolver injected; cannot bind session %s to a workspace", sid)
	}
	binding, bound := m.resolver.Session(sid)
	if !bound {
		return "", fmt.Errorf("ssm: no workspace bound to session %s (turn boundary seq=%d)", sid, ev.GetSeq())
	}
	if binding.SessionID == "" {
		return "", fmt.Errorf("ssm: session %s resolved to workspace %q with no daemon session id (turn boundary seq=%d); a status row with no owner can never be claimed or closed", sid, binding.Workspace, ev.GetSeq())
	}
	if binding.Workspace != workspace {
		m.logf("ssm: turn boundary workspace DISAGREEMENT caller_ws=%s resolved_ws=%s event_session=%s owner_session=%s claimant_session=%s seq=%d — the ledger moves under the caller's workspace and the row is filed there; the registry believes this conversation belongs elsewhere",
			workspace, binding.Workspace, sid, binding.SessionID, claimantSessionID, ev.GetSeq())
	}
	if binding.SessionID != sid {
		m.logf("ssm: event identity canonicalized ws=%s event_session=%s owner_session=%s kind=%s seq=%d — the store files this conversation under its vendor uuid; the row is owned by the daemon session so claim checks compare one identity",
			workspace, sid, binding.SessionID, payloadKind(ev), ev.GetSeq())
	}
	return binding.SessionID, nil
}

// turnBoundaryMarkLocked resolves the workspace's pending interrupt mark for
// this boundary and returns the token/cause to paint plus the deferred mutation
// that SPENDS the mark. The mutation runs only after the boundary's transaction
// commits — see the call site.
//
// Caller holds m.mu.
func (m *Manager) turnBoundaryMarkLocked(ws string, ev *corev1.Event, state, causeKind string) (string, string, func()) {
	mark := m.interruptedTurn[ws]
	if mark == nil {
		return state, causeKind, func() {}
	}
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnEnded:
		return sigInterrupted, causeInterrupted, func() {
			delete(m.interruptedTurn, ws)
			m.logf("ssm: turn end reported as `interrupted` ws=%s session=%s seq=%d (superseding %s) — a user-commanded stop was delivered to this turn",
				ws, ev.GetSessionId(), ev.GetSeq(), state)
		}
	case *corev1.Event_TurnStarted:
		if mark.tolerateLateStart {
			return state, causeKind, func() {
				mark.tolerateLateStart = false
				m.logf("ssm: interrupt mark kept ws=%s session=%s seq=%d — the stopped turn's own late start arrived after the stop",
					ws, ev.GetSessionId(), ev.GetSeq())
			}
		}
		return state, causeKind, func() {
			delete(m.interruptedTurn, ws)
			m.logf("ssm: interrupt mark dropped ws=%s session=%s seq=%d — a new turn started before the stopped turn's end was observed",
				ws, ev.GetSessionId(), ev.GetSeq())
		}
	}
	return state, causeKind, func() {}
}

// paintTurnBandLocked reconciles the session-status axis to the derivation.
//
// IT IS NOT A SECOND FOLD. The token it writes is chosen by turnBandToken from
// the TurnLiveness value the caller derived, and the only other input is the
// axis's own top row — read so an axis that ALREADY says what the derivation
// says is left alone. There is no dedup key here and no event-stream memory:
// "have I seen this event" is not a question the color needs to answer once the
// color is a function of the ledger.
//
// endState/endCause are the boundary's own account of HOW the turn ended
// (`done`, `vendor_blocked`, `interrupted`). They are consulted ONLY when the
// derivation holds no live turn, because "how it ended" is not something
// liveness can answer and "whether it ended" is not something the event can.
//
// Caller holds m.mu and owns tx.
func (m *Manager) paintTurnBandLocked(
	tx *sql.Tx,
	workspace, owner, eventSessionID string,
	ev *corev1.Event,
	l TurnLiveness,
	endState, endCause string,
) (string, error) {
	if !l.Derived() {
		err := fmt.Errorf("%w: painting the turn band for workspace %q", ErrTurnLivenessUnderived, workspace)
		m.logf("ssm: INVARIANT VIOLATION ws=%s event_session=%s seq=%d — the turn band was about to be painted from a turn-liveness value nothing derived; refusing to write a color that no fold produced: %v",
			workspace, eventSessionID, ev.GetSeq(), err)
		return "", err
	}
	want, causeKind := endState, endCause
	if band, live := turnBandToken(l); live {
		want, causeKind = band, causeTurnStarted
	} else if _, started := ev.GetPayload().(*corev1.Event_TurnStarted); started {
		// A START THAT ARRIVES ALREADY ENDED PAINTS NOTHING. The derivation
		// holds no live turn because this start replays a turn the daemon
		// killed, and its durable end closed the claim in the same statement
		// that admitted it (turninterruption.go). A start's own token says
		// `thinking`, which would be a lie here, and the end it carries was
		// already painted when the turn was killed — so the axis is left
		// exactly as the derivation already has it.
		m.logf("ssm: turn band unchanged ws=%s event_session=%s seq=%d turn_id=%q liveness=%s — this start replays an already-ended turn, so there is no colour for it to move",
			workspace, eventSessionID, ev.GetSeq(), turnCorrelation(ev), l)
		return "", nil
	}
	// AGREEMENT IS ON THE PAIR, NOT THE TOKEN. `thinking` written by
	// MarkPromptDelivered is the DAEMON's commitment to a prompt whose start has
	// not reached the store; `thinking` written by a turn boundary is the stream
	// confirming it. They render identically and mean different things, and the
	// submit leg of the derivation reads the cause to tell them apart — so a
	// boundary arriving over a commitment must SUPERSEDE it rather than find the
	// axis already agreeable. Leaving the commitment row standing let it outlive
	// the very turn it predicted: the turn's end closed the claim, and the stale
	// prompt cause underneath resurrected liveness and painted `submitting` over
	// a finished turn.
	top, topCause, err := sessionStatusTopCause(tx, workspace)
	if err != nil {
		return "", err
	}
	if top == want && topCause == causeKind {
		return "", nil
	}
	causeSeq := sql.NullInt64{Int64: int64(ev.GetSeq()), Valid: true}
	if err := appendEventRow(tx, workspace, owner, eventSessionID, want, causeKind, causeSeq, m.nextAt(), ""); err != nil {
		return "", err
	}
	return want, nil
}
