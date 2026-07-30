package ssm

import (
	"database/sql"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// MarkPromptAccepted moves the agent axis to `thinking` when the shim accepts
// an immediately delivered prompt.
//
// This is the daemon's earliest authoritative turn-start observation. The
// shim's durable TurnStarted follows over the store stream, but the command Ack
// and the daemon-local prompt receipt can reach frontends first. Waiting for
// that later event therefore permits a real prompt bubble beside a green
// workspace. Appending the accepted edge before the frontend command returns
// makes that contradiction unrepresentable while the SSM is healthy.
//
// PUBLISH is the synchronous frontend barrier. It runs while the SSM lock is
// still held, after the accepted state has resolved, so no later SSM transition
// can overtake this `thinking` publication. The ordinary SSM subscription still
// receives the same transition for progress mirroring and reconnect recovery;
// its duplicate frontend push is intentionally harmless.
//
// The later TurnStarted remains the durable lifecycle authority. It appends its
// own seq-bearing row and is harmless over this daemon-local `thinking` row.
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
		m.logf("ssm: prompt accepted IDEMPOTENT ws=%s session=%s request_id=%q active_claimant=%q — agent axis already reads `thinking`",
			workspace, sessionID, requestID, claimant)
		return m.publishPromptAcceptedLocked(workspace, sessionID, requestID, "idempotent", publish)
	}

	if err := appendRow(
		m.db, workspace, sessionID, sigThinking, causePromptAccepted,
		sql.NullInt64{}, m.nextAt(), "",
	); err != nil {
		return fmt.Errorf("ssm: record accepted prompt for workspace %q session %q request %q: %w",
			workspace, sessionID, requestID, err)
	}
	m.logf("ssm: prompt accepted ws=%s session=%s request_id=%q — appended daemon-local `thinking` before command completion; durable TurnStarted will supersede it",
		workspace, sessionID, requestID)
	if err := m.reresolveLocked(workspace, causePromptAccepted, 0); err != nil {
		return err
	}
	return m.publishPromptAcceptedLocked(workspace, sessionID, requestID, "appended", publish)
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
	state := r.toProto(workspace)
	if state.GetState() != frontendv1.RenderState_RENDER_STATE_THINKING || !state.GetTurnActive() {
		return fmt.Errorf("ssm: synchronous prompt state invariant failed for workspace %q session %q request %q: state=%s turn_active=%t",
			workspace, sessionID, requestID, state.GetState(), state.GetTurnActive())
	}
	m.logf("ssm: prompt accepted PUBLISH_SYNC ws=%s session=%s request_id=%q decision=%s state=%s turn_active=%t cause_kind=%s cause_seq=%d at_ms=%d",
		workspace, sessionID, requestID, decision, state.GetState(), state.GetTurnActive(),
		state.GetCauseKind(), state.GetCauseSeq(), state.GetAtMs())
	publish(state)
	return nil
}

// ReconcileAlreadyComplete makes the shim's ALREADY_COMPLETE interrupt verdict
// agree with the agent axis before the progress footer is allowed to display
// "already finished".
//
// ALREADY_COMPLETE is a live observation from the shim that no foreground turn
// exists. Its TurnEnded can still be in flight through the store while the
// control Ack has already arrived, so the SSM may temporarily retain
// `thinking`. A stale permission row can hide the same contradiction. Those two
// active shapes are reconciled to `idle`; an already-settled outcome such as
// `done` or `vendor_blocked` is preserved because the Ack says only that the
// turn is over, not how it ended.
//
// A `thinking` claim owned by another session is an invariant violation and is
// rejected without writing. The caller must then withhold the interrupt window
// rather than publish two mutually exclusive claims.
func (m *Manager) ReconcileAlreadyComplete(workspace, sessionID string) (bool, error) {
	if workspace == "" {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete got an empty workspace")
	}
	if sessionID == "" {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete for workspace %q got an empty session id", workspace)
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	var (
		topState string
		topSID   sql.NullString
	)
	err := m.db.QueryRow(
		`SELECT state, session_id FROM workspace_state
		 WHERE workspace = ? AND state IN `+agentAxisMembers+`
		 ORDER BY at DESC LIMIT 1`,
		workspace,
	).Scan(&topState, &topSID)
	if err == sql.ErrNoRows {
		m.logf("ssm: already-complete reconciliation ws=%s session=%s decision=no_agent_axis — nothing active can contradict the footer verdict",
			workspace, sessionID)
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("ssm: ReconcileAlreadyComplete read agent axis for workspace %q: %w", workspace, err)
	}

	if topState != sigThinking && topState != sigPermission {
		m.logf("ssm: already-complete reconciliation ws=%s session=%s decision=preserve_settled state=%s claimant=%q",
			workspace, sessionID, topState, topSID.String)
		return false, nil
	}

	// A permission row carries no claimant of its own. Inspect the row it
	// covered so a stale question cannot authorize this session to close a
	// replacement session's live turn.
	claimant := topSID.String
	if topState == sigPermission {
		var beneathSID sql.NullString
		beneathErr := m.db.QueryRow(
			`SELECT session_id FROM workspace_state
			 WHERE workspace = ? AND state IN `+agentAxisMembers+`
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

	if err := appendRow(
		m.db, workspace, sessionID, sigIdle, causeInterruptAlreadyComplete,
		sql.NullInt64{}, m.nextAt(), "",
	); err != nil {
		return false, fmt.Errorf("ssm: reconcile already-complete verdict for workspace %q session %q: %w",
			workspace, sessionID, err)
	}
	m.logf("ssm: already-complete reconciliation CLOSED ws=%s session=%s previous=%s active_claimant=%q — shim reports no foreground turn, so the footer cannot coexist with `thinking`",
		workspace, sessionID, topState, claimant)
	if err := m.reresolveLocked(workspace, causeInterruptAlreadyComplete, 0); err != nil {
		return false, err
	}
	return true, nil
}
