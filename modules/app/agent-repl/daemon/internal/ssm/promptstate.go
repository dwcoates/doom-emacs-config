package ssm

import (
	"database/sql"
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// MarkPromptAccepted moves the session-status lifecycle to `submitting` when the
// daemon commits to submitting an immediately delivered prompt.
//
// This is the daemon's earliest turn-start observation, and it is deliberately
// written BEFORE the prompt is handed to the shim rather than after the shim
// Acks it. The Ack is a round-trip to a node process that is often busy serving
// the very stream this workspace is about to produce, so a status gated on it
// left the user watching a green workspace for as long as that took — the exact
// window in which they cannot tell a sent prompt from a lost one.
//
// The claim is therefore made on the daemon's own intent, and its truthfulness
// is maintained by MarkPromptRejected, which withdraws it if the submit fails.
//
// The shim's durable TurnStarted follows over the store stream and remains the
// lifecycle authority; the daemon-local prompt receipt still reaches frontends
// only after this edge, so a real prompt bubble can never sit beside a green
// workspace while the SSM is healthy.
//
// PUBLISH is the synchronous frontend barrier. It runs while the SSM lock is
// still held, after the accepted state has resolved, so no later SSM transition
// can overtake this `submitting` publication. The ordinary SSM subscription still
// receives the same transition for progress mirroring and reconnect recovery;
// its duplicate frontend push is intentionally harmless.
//
// The later TurnStarted remains the durable lifecycle authority. It appends its
// own seq-bearing row and is harmless over this daemon-local row.
func (m *Manager) MarkPromptAccepted(
	workspace, sessionID, requestID string,
	publish func(*frontendv1.WorkspaceState),
) error {
	if workspace == "" {
		return fmt.Errorf("ssm: MarkPromptAccepted got an empty workspace")
	}
	if sessionID == "" {
		return fmt.Errorf("ssm: MarkPromptAccepted for workspace %q got an empty session id", workspace)
	}
	if publish == nil {
		return fmt.Errorf("ssm: MarkPromptAccepted for workspace %q session %q got a nil synchronous publisher", workspace, sessionID)
	}

	m.mu.Lock()
	defer m.mu.Unlock()
	if err := m.rejectStartDuringHibernationLocked(workspace, "prompt acceptance"); err != nil {
		return err
	}

	active, claimant, err := turnClaim(m.db, workspace)
	if err != nil {
		return fmt.Errorf("ssm: MarkPromptAccepted read turn claim for workspace %q: %w", workspace, err)
	}
	if active {
		if claimant != "" && claimant != sessionID {
			err := fmt.Errorf("ssm: prompt accepted for workspace %q session %q while session %q owns the active turn", workspace, sessionID, claimant)
			m.logf("ssm: prompt accepted REJECTED ws=%s session=%s request_id=%q active_claimant=%q error=%v",
				workspace, sessionID, requestID, claimant, err)
			return err
		}
		m.logf("ssm: prompt accepted IDEMPOTENT ws=%s session=%s request_id=%q active_claimant=%q — session-status lifecycle already claims this turn",
			workspace, sessionID, requestID, claimant)
		return m.publishPromptAcceptedLocked(workspace, sessionID, requestID, "idempotent", publish)
	}

	if err := appendRow(
		m.db, workspace, sessionID, sigSubmitting, causePromptAccepted,
		sql.NullInt64{}, m.nextAt(), "",
	); err != nil {
		return fmt.Errorf("ssm: record accepted prompt for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	m.logf("ssm: prompt accepted ws=%s session=%s request_id=%q — appended daemon-local `submitting` before command completion; the shim ack advances it to `thinking`",
		workspace, sessionID, requestID)
	// THE ACCEPTED EDGE IS ALL-OR-NOTHING FROM HERE. Everything below runs
	// under the same lock hold as the append above, so a failure retracts the
	// row this call wrote rather than returning over it. See
	// retractUnpublishedAcceptLocked for why leaving it standing wedged the
	// workspace permanently.
	if err := m.reresolveLocked(workspace, causePromptAccepted, 0); err != nil {
		return m.retractUnpublishedAcceptLocked(workspace, sessionID, requestID, err)
	}
	if err := m.publishPromptAcceptedLocked(workspace, sessionID, requestID, "appended", publish); err != nil {
		return m.retractUnpublishedAcceptLocked(workspace, sessionID, requestID, err)
	}
	return nil
}

// retractUnpublishedAcceptLocked withdraws the `submitting` row THIS call just
// appended, for an accepted edge that failed before any frontend could be shown
// it. It returns the caller's original CAUSE, never its own success.
//
// THE FAILURE PATH USED TO RETURN OVER THE APPENDED ROW, and that is what
// wedged a workspace forever. The row is a turn CLAIM: `turnClaim` reads it,
// the queue latches on it, and only a turn that genuinely began ever reports an
// end that could retire it. So a claim left behind by a rejected accept is a
// claim nothing will ever close — and the very next prompt then took the
// IDEMPOTENT branch above ("the lifecycle already claims this turn"), failed the
// same publish invariant, and returned over the same row again. Every prompt
// after it did the same. The session-controller's own retraction cannot reach
// this: it restores its daemon-local latch and its comment promises "the latch
// is put back where it was found", but the SSM's half of the edge is ours to
// put back.
//
// IT RETRACTS ONLY WHAT THIS CALL WROTE, exactly as MarkPromptRejected does. The
// top row must still be this session's own `prompt_accepted` row; anything else
// means something authoritative owns the axis, and closing a turn on that
// evidence would report an idle workspace over a working session. Nothing can
// legitimately land in that window — the whole span runs under m.mu — so a top
// row that is not ours is logged as the invariant violation it is.
//
// A retraction failure is JOINED onto the cause rather than replacing it: the
// caller's error is the news, and a workspace left holding an unwithdrawn claim
// is a second, independent fault that must not be swallowed to report the first.
func (m *Manager) retractUnpublishedAcceptLocked(workspace, sessionID, requestID string, cause error) error {
	var (
		topState string
		topCause string
		topSID   sql.NullString
	)
	err := m.db.QueryRow(
		`SELECT state, cause_kind, session_id FROM workspace_state
		 WHERE workspace = ? AND state IN `+sessionStatusMembers+`
		 ORDER BY at DESC LIMIT 1`,
		workspace,
	).Scan(&topState, &topCause, &topSID)
	if err != nil {
		readErr := fmt.Errorf("ssm: read back the accepted row to retract for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
		m.warn("ssm: prompt accepted RETRACTION FAILED ws=%s session=%s request_id=%q stage=read_back accept_error=%v error=%v — the workspace holds a turn claim for a turn that never began",
			workspace, sessionID, requestID, cause, readErr)
		return errors.Join(cause, readErr)
	}
	if topState != sigSubmitting || topCause != causePromptAccepted ||
		(topSID.String != "" && topSID.String != sessionID) {
		invariant := fmt.Errorf("ssm: the accepted row to retract for workspace %q session %q request %q is no longer on top: state=%s cause_kind=%s session=%q",
			workspace, sessionID, requestID, topState, topCause, topSID.String)
		m.warn("ssm: prompt accepted RETRACTION FAILED ws=%s session=%s request_id=%q stage=identify state=%s cause_kind=%s top_session=%q accept_error=%v error=%v — nothing may write this axis inside the accept's own lock hold",
			workspace, sessionID, requestID, topState, topCause, topSID.String, cause, invariant)
		return errors.Join(cause, invariant)
	}

	if err := appendRow(
		m.db, workspace, sessionID, sigIdle, causePromptRejected,
		sql.NullInt64{}, m.nextAt(), "",
	); err != nil {
		writeErr := fmt.Errorf("ssm: retract the unpublished accepted prompt for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
		m.warn("ssm: prompt accepted RETRACTION FAILED ws=%s session=%s request_id=%q stage=append accept_error=%v error=%v — the workspace holds a turn claim for a turn that never began",
			workspace, sessionID, requestID, cause, writeErr)
		return errors.Join(cause, writeErr)
	}
	m.logf("ssm: prompt accepted RETRACTED ws=%s session=%s request_id=%q accept_error=%v — the accepted edge failed before publication, so the turn claim it appended is withdrawn and the next prompt starts from an unclaimed axis",
		workspace, sessionID, requestID, cause)
	if err := m.reresolveLocked(workspace, causePromptRejected, 0); err != nil {
		resolveErr := fmt.Errorf("ssm: re-resolve after retracting the unpublished accepted prompt for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
		m.warn("ssm: prompt accepted RETRACTION FAILED ws=%s session=%s request_id=%q stage=reresolve accept_error=%v error=%v — the claim is withdrawn in the log but no frontend was told",
			workspace, sessionID, requestID, cause, resolveErr)
		return errors.Join(cause, resolveErr)
	}
	return cause
}

// publishPromptAcceptedLocked resolves and synchronously publishes the state
// whose accepted-prompt invariant the caller is about to expose as a prompt
// bubble. Caller holds m.mu; keeping it held is the ordering barrier.
func (m *Manager) publishPromptAcceptedLocked(
	workspace, sessionID, requestID, decision string,
	publish func(*frontendv1.WorkspaceState),
) error {
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return fmt.Errorf("ssm: resolve synchronous prompt state for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	if !r.found {
		return fmt.Errorf("ssm: synchronous prompt state missing for workspace %q session %q request %q",
			workspace, sessionID, requestID)
	}
	state, err := m.workspaceMessageLocked(workspace, r)
	if err != nil {
		return fmt.Errorf("ssm: resolve synchronous composite prompt state for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	// EITHER half of a turn satisfies this. The appended branch resolves to
	// `submitting`, and the IDEMPOTENT branch — a durable TurnStarted that won
	// the race — legitimately resolves to `thinking`. What the invariant
	// actually guards is that a turn is claimed at all before a prompt bubble
	// is exposed, which both states assert.
	inTurn := state.GetState() == frontendv1.RenderState_RENDER_STATE_SUBMITTING ||
		state.GetState() == frontendv1.RenderState_RENDER_STATE_THINKING
	if !inTurn || !state.GetTurnActive() {
		return fmt.Errorf("ssm: synchronous prompt state invariant failed for workspace %q session %q request %q: state=%s turn_active=%t",
			workspace, sessionID, requestID, state.GetState(), state.GetTurnActive())
	}
	m.logf("ssm: prompt accepted PUBLISH_SYNC ws=%s session=%s request_id=%q decision=%s state=%s turn_active=%t cause_kind=%s cause_seq=%d at_ms=%d",
		workspace, sessionID, requestID, decision, state.GetState(), state.GetTurnActive(),
		state.GetCauseKind(), state.GetCauseSeq(), state.GetAtMs())
	publish(state)
	return nil
}

// MarkPromptDelivered advances `submitting` to `thinking` when the shim ACKS
// the prompt, which is the moment the agent genuinely holds it.
//
// THIS IS THE EDGE THE SPLIT EXISTS FOR. MarkPromptAccepted publishes on the
// daemon's own intent so the user sees a turn begin at the speed of their
// keystroke, but during that window nothing has been handed to the agent yet,
// and calling it `thinking` overstated what was happening. The ack is the first
// moment that word is true.
//
// NOT the durable TurnStarted, deliberately: the shim MINTS TurnStarted inside
// the same synchronous block that acks the submit, so the two describe one
// instant and the delay between them is store round-trip time. Cutting here
// instead would make the phase word move with store load rather than with the
// agent.
//
// IT ADVANCES ONLY ITS OWN ROW, for the same reason MarkPromptRejected retracts
// only its own: any other top row means something more authoritative landed in
// the window, and overwriting it would report the agent thinking over a
// permission question, a context cut or another session's turn. Reports whether
// it wrote the row; false with a nil error is the benign already-superseded
// outcome.
//
// A failure is returned to the caller but is NOT fatal to the prompt: the shim
// already has it, and the durable TurnStarted still arrives to state the same
// thing. The caller logs loudly and carries on.
func (m *Manager) MarkPromptDelivered(workspace, sessionID, requestID string) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: MarkPromptDelivered got an empty workspace")
	}
	if sessionID == "" {
		return false, fmt.Errorf("ssm: MarkPromptDelivered for workspace %q got an empty session id", workspace)
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	var (
		topState string
		topCause string
		topSID   sql.NullString
	)
	err := m.db.QueryRow(
		`SELECT state, cause_kind, session_id FROM workspace_state
		 WHERE workspace = ? AND state IN `+sessionStatusMembers+`
		 ORDER BY at DESC LIMIT 1`,
		workspace,
	).Scan(&topState, &topCause, &topSID)
	if err == sql.ErrNoRows {
		m.logf("ssm: prompt delivered ws=%s session=%s request_id=%q decision=no_agent_axis — nothing claims the turn this ack belongs to",
			workspace, sessionID, requestID)
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: MarkPromptDelivered read session-status lifecycle for workspace %q: %w", workspace, err)
	}

	if topSID.String != "" && topSID.String != sessionID {
		err := fmt.Errorf("ssm: prompt delivered for workspace %q session %q cannot advance state owned by session %q", workspace, sessionID, topSID.String)
		m.logf("ssm: prompt delivered REJECTED ws=%s session=%s request_id=%q state=%s cause_kind=%s active_claimant=%q error=%v",
			workspace, sessionID, requestID, topState, topCause, topSID.String, err)
		return false, err
	}
	if topState != sigSubmitting || topCause != causePromptAccepted {
		m.logf("ssm: prompt delivered ws=%s session=%s request_id=%q decision=preserve state=%s cause_kind=%s — the submitting row this would advance is already superseded",
			workspace, sessionID, requestID, topState, topCause)
		return false, nil
	}

	if err := appendRow(
		m.db, workspace, sessionID, sigThinking, causePromptDelivered,
		sql.NullInt64{}, m.nextAt(), "",
	); err != nil {
		return false, fmt.Errorf("ssm: record delivered prompt for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	m.logf("ssm: prompt delivered ws=%s session=%s request_id=%q — the shim acked the submit, so the turn advances from `submitting` to `thinking`",
		workspace, sessionID, requestID)
	if err := m.reresolveLocked(workspace, causePromptDelivered, 0); err != nil {
		return false, err
	}
	return true, nil
}

// MarkPromptRejected retracts the daemon-local `submitting` edge for a prompt the
// shim did NOT take.
//
// MarkPromptAccepted is written before the submit rather than after its Ack, so
// that `thinking` reaches every frontend at the speed of the user's keystroke
// rather than at the speed of a shim round-trip. That trade buys the latency
// with a claim the daemon has not yet confirmed, and this is the other half of
// it: when the submit then fails, the claim must be withdrawn by the same
// caller that made it. Nothing else can withdraw it — the session-status
// lifecycle retires a turn on a TurnEnded, and no turn that never began will
// ever report an end — so without this the workspace reads `submitting` until
// the watchdog or a shim stop closes it.
//
// IT RETRACTS ONLY WHAT IT WROTE. The row on top must still be this session's
// `prompt_accepted` row: any other top row means something authoritative landed
// in the meantime (a durable TurnStarted, a permission question, a context cut,
// a rotation), and closing a turn on that evidence would report an idle
// workspace over a session that is genuinely working. A top row owned by
// ANOTHER session is the same hazard and is refused loudly rather than
// preserved silently, because it means two submitters disagree about who owns
// the workspace's turn.
//
// Reports whether it WROTE the closing row. false with a nil error is the
// benign outcome: something more authoritative already owns the axis.
//
// PUBLISH is the synchronous frontend barrier, exactly as on the accepted side:
// it runs under the SSM lock so no later transition can overtake the retraction
// the failing frontend command is about to report.
func (m *Manager) MarkPromptRejected(
	workspace, sessionID, requestID string,
	publish func(*frontendv1.WorkspaceState),
) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: MarkPromptRejected got an empty workspace")
	}
	if sessionID == "" {
		return false, fmt.Errorf("ssm: MarkPromptRejected for workspace %q got an empty session id", workspace)
	}
	if publish == nil {
		return false, fmt.Errorf("ssm: MarkPromptRejected for workspace %q session %q got a nil synchronous publisher", workspace, sessionID)
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	var (
		topState string
		topCause string
		topSID   sql.NullString
	)
	err := m.db.QueryRow(
		`SELECT state, cause_kind, session_id FROM workspace_state
		 WHERE workspace = ? AND state IN `+sessionStatusMembers+`
		 ORDER BY at DESC LIMIT 1`,
		workspace,
	).Scan(&topState, &topCause, &topSID)
	if err == sql.ErrNoRows {
		m.logf("ssm: prompt rejected ws=%s session=%s request_id=%q decision=no_agent_axis — nothing claims a turn for the refused prompt",
			workspace, sessionID, requestID)
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: MarkPromptRejected read session-status lifecycle for workspace %q: %w", workspace, err)
	}

	if topSID.String != "" && topSID.String != sessionID {
		err := fmt.Errorf("ssm: prompt rejected for workspace %q session %q cannot retract state owned by session %q", workspace, sessionID, topSID.String)
		m.warn("ssm: prompt rejected REJECTED ws=%s session=%s request_id=%q state=%s cause_kind=%s active_claimant=%q error=%v",
			workspace, sessionID, requestID, topState, topCause, topSID.String, err)
		return false, err
	}
	if topState != sigSubmitting || topCause != causePromptAccepted {
		m.logf("ssm: prompt rejected ws=%s session=%s request_id=%q decision=preserve state=%s cause_kind=%s — the accepted row this would retract has already been superseded",
			workspace, sessionID, requestID, topState, topCause)
		return false, nil
	}

	if err := appendRow(
		m.db, workspace, sessionID, sigIdle, causePromptRejected,
		sql.NullInt64{}, m.nextAt(), "",
	); err != nil {
		return false, fmt.Errorf("ssm: retract accepted prompt for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	m.logf("ssm: prompt rejected ws=%s session=%s request_id=%q — the shim never took the prompt, so the daemon-local `thinking` it published is withdrawn",
		workspace, sessionID, requestID)
	if err := m.reresolveLocked(workspace, causePromptRejected, 0); err != nil {
		return false, err
	}
	if err := m.publishPromptRejectedLocked(workspace, sessionID, requestID, publish); err != nil {
		return false, err
	}
	return true, nil
}

// publishPromptRejectedLocked resolves and synchronously publishes the state a
// retraction just produced, refusing to publish one that still claims the turn
// the retraction exists to withdraw. Caller holds m.mu.
func (m *Manager) publishPromptRejectedLocked(
	workspace, sessionID, requestID string,
	publish func(*frontendv1.WorkspaceState),
) error {
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return fmt.Errorf("ssm: resolve synchronous retracted-prompt state for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	if !r.found {
		return fmt.Errorf("ssm: synchronous retracted-prompt state missing for workspace %q session %q request %q",
			workspace, sessionID, requestID)
	}
	state, err := m.workspaceMessageLocked(workspace, r)
	if err != nil {
		return fmt.Errorf("ssm: resolve synchronous composite retracted-prompt state for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	if state.GetTurnActive() {
		return fmt.Errorf("ssm: synchronous retracted-prompt invariant failed for workspace %q session %q request %q: state=%s turn_active=true",
			workspace, sessionID, requestID, state.GetState())
	}
	m.logf("ssm: prompt rejected PUBLISH_SYNC ws=%s session=%s request_id=%q state=%s turn_active=%t cause_kind=%s cause_seq=%d at_ms=%d",
		workspace, sessionID, requestID, state.GetState(), state.GetTurnActive(),
		state.GetCauseKind(), state.GetCauseSeq(), state.GetAtMs())
	publish(state)
	return nil
}

// ReconcileAlreadyComplete makes the shim's ALREADY_COMPLETE interrupt verdict
// agree with the session-status lifecycle before the progress footer is allowed to display
// "already finished".
//
// ALREADY_COMPLETE is a live observation from the shim that no foreground turn
// exists. Its TurnEnded can still be in flight through the store while the
// control Ack has already arrived, so the SSM may temporarily retain
// `submitting` or `thinking`. A stale permission row can hide the same
// contradiction. Those active shapes are reconciled to `idle`; a settled outcome such as
// `done` or `vendor_blocked` is preserved because the Ack says only that the
// turn is over, not how it ended.
//
// A `submitting` or `thinking` claim owned by another session is an invariant violation and is
// rejected without writing. The caller must then withhold the interrupt window
// rather than publish two mutually exclusive claims.
func (m *Manager) ReconcileAlreadyComplete(
	workspace, sessionID string,
	publish func(*frontendv1.WorkspaceState),
) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete got an empty workspace")
	}
	if sessionID == "" {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete for workspace %q got an empty session id", workspace)
	}
	if publish == nil {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete for workspace %q session %q got a nil synchronous publisher", workspace, sessionID)
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	var (
		topState string
		topSID   sql.NullString
	)
	err := m.db.QueryRow(
		`SELECT state, session_id FROM workspace_state
		 WHERE workspace = ? AND state IN `+sessionStatusMembers+`
		 ORDER BY at DESC LIMIT 1`,
		workspace,
	).Scan(&topState, &topSID)
	closed := false
	if err == sql.ErrNoRows {
		m.logf("ssm: already-complete reconciliation ws=%s session=%s decision=no_agent_axis — nothing active can contradict the footer verdict",
			workspace, sessionID)
	} else if err != nil {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete read session-status lifecycle for workspace %q: %w", workspace, err)
	} else if topState != sigThinking && topState != sigSubmitting && topState != sigPermission {
		m.logf("ssm: already-complete reconciliation ws=%s session=%s decision=preserve_settled state=%s claimant=%q",
			workspace, sessionID, topState, topSID.String)
	} else {
		// A permission row carries no claimant of its own. Inspect the row it
		// covered so a stale question cannot authorize this session to close a
		// replacement session's live turn.
		claimant := topSID.String
		if topState == sigPermission {
			var beneathSID sql.NullString
			beneathErr := m.db.QueryRow(
				`SELECT session_id FROM workspace_state
			 WHERE workspace = ? AND state IN `+sessionStatusMembers+`
			   AND state <> 'permission'
			 ORDER BY at DESC LIMIT 1`,
				workspace,
			).Scan(&beneathSID)
			if beneathErr != nil && beneathErr != sql.ErrNoRows {
				return false, fmt.Errorf("ssm: ReconcileAlreadyComplete read permission-covered claim for workspace %q: %w", workspace, beneathErr)
			}
			claimant = beneathSID.String
		}
		if claimant != "" && claimant != sessionID {
			err := fmt.Errorf("ssm: already-complete verdict for workspace %q session %q cannot close active state owned by session %q", workspace, sessionID, claimant)
			m.logf("ssm: already-complete reconciliation REJECTED ws=%s session=%s state=%s active_claimant=%q error=%v",
				workspace, sessionID, topState, claimant, err)
			return false, err
		}

		// THE LEDGER GOES WITH THE AXIS. The shim has stated, live, that no
		// foreground turn exists; turn liveness is derived from the durable
		// claims, so retiring the row without retiring the claims would leave
		// the color idle while the prompt queue still held every prompt behind
		// a turn the shim says is over — the exact disagreement this whole
		// design removes. See retireTurnsLocked.
		tx, txErr := m.db.Begin()
		if txErr != nil {
			return false, fmt.Errorf("ssm: begin already-complete reconciliation for workspace %q: %w", workspace, txErr)
		}
		defer tx.Rollback()
		retired, liveness, retireErr := m.retireTurnsLocked(tx, workspace, sessionID, TurnCloseAlreadyComplete)
		if retireErr != nil {
			return false, retireErr
		}
		if err := appendRow(
			tx, workspace, sessionID, sigIdle, causeInterruptAlreadyComplete,
			sql.NullInt64{}, m.nextAt(), "",
		); err != nil {
			return false, fmt.Errorf("ssm: reconcile already-complete verdict for workspace %q session %q: %w",
				workspace, sessionID, err)
		}
		if err := tx.Commit(); err != nil {
			return false, fmt.Errorf("ssm: commit already-complete reconciliation for workspace %q session %q: %w",
				workspace, sessionID, err)
		}
		if len(retired) > 0 {
			m.logf("ssm: turn claims RETIRED BY AN ALREADY-COMPLETE ACK ws=%s session=%s closed=%s cause=%s liveness=%s — the shim answered live that no foreground turn exists, so these claims name turns whose ends can never arrive",
				workspace, sessionID, formatClosedTurnIDs(retired), TurnCloseAlreadyComplete, liveness)
		}
		m.logf("ssm: already-complete reconciliation CLOSED ws=%s session=%s previous=%s active_claimant=%q — shim reports no foreground turn, so the footer cannot coexist with an active state",
			workspace, sessionID, topState, claimant)
		if err := m.reresolveLocked(workspace, causeInterruptAlreadyComplete, 0); err != nil {
			return false, err
		}
		closed = true
	}
	state, found, err := m.currentLocked(workspace)
	if err != nil {
		return false, fmt.Errorf("ssm: resolve synchronous already-complete state for workspace %q session %q: %w", workspace, sessionID, err)
	}
	if !found {
		m.logf("ssm: already-complete reconciliation PUBLISH_SYNC ws=%s session=%s decision=no_render_state closed=%t",
			workspace, sessionID, closed)
		return closed, nil
	}
	if state.GetTurnActive() || state.GetStatus() == frontendv1.SessionStatus_SESSION_STATUS_THINKING || state.GetStatus() == frontendv1.SessionStatus_SESSION_STATUS_PERMISSION {
		return false, fmt.Errorf("ssm: synchronous already-complete state invariant failed for workspace %q session %q: state=%s status=%s turn_active=%t",
			workspace, sessionID, state.GetState(), state.GetStatus(), state.GetTurnActive())
	}
	m.logf("ssm: already-complete reconciliation PUBLISH_SYNC ws=%s session=%s state=%s status=%s turn_active=%t cause_kind=%s cause_seq=%d at_ms=%d",
		workspace, sessionID, state.GetState(), state.GetStatus(), state.GetTurnActive(),
		state.GetCauseKind(), state.GetCauseSeq(), state.GetAtMs())
	publish(state)
	return closed, nil
}
