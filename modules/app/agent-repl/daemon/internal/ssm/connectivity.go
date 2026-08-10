package ssm

import (
	"database/sql"
	"errors"
	"fmt"
	"sort"
	"strings"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// ControllerGenerationID identifies one daemon-local session-controller
// incarnation. Its persisted value makes delayed edges attributable after the
// incarnation has retired; it is never interpreted or reused by the SSM.
type ControllerGenerationID string

// SessionConnectivity answers whether the current session-controller
// generation can operate the session reliably now.
type SessionConnectivity string

const (
	SessionConnectivityHibernated  SessionConnectivity = "hibernated"
	SessionConnectivityConnecting  SessionConnectivity = "connecting"
	SessionConnectivityOperational SessionConnectivity = "operational"
	SessionConnectivityDegraded    SessionConnectivity = "degraded"
	SessionConnectivityUnavailable SessionConnectivity = "unavailable"
)

// SessionStatus answers what the session is doing independently of
// connectivity.
type SessionStatus string

const (
	SessionStatusReady         SessionStatus = "ready"
	SessionStatusThinking      SessionStatus = "thinking"
	SessionStatusSubmitting    SessionStatus = "submitting"
	SessionStatusPermission    SessionStatus = "permission"
	SessionStatusDone          SessionStatus = "done"
	SessionStatusInterrupted   SessionStatus = "interrupted"
	SessionStatusVendorBlocked SessionStatus = "vendor-blocked"
	SessionStatusMonitoring    SessionStatus = "monitoring"
)

// FaultImpact is a closed classification of the scope a runtime fault impairs.
type FaultImpact string

const (
	FaultImpactConnectivity FaultImpact = "connectivity"
	FaultImpactFeature      FaultImpact = "feature"
	FaultImpactCommand      FaultImpact = "command"
	FaultImpactTurnTerminal FaultImpact = "turn-terminal"
)

// RuntimeFault is one currently-open fault window for the selected controller
// generation.
type RuntimeFault struct {
	Workspace              string
	AgentReplSessionID     string
	ControllerGenerationID ControllerGenerationID
	Component              string
	FaultType              string
	Impact                 FaultImpact
	CauseKind              string
	OpenedAtMs             int64
}

// CompositeState is the daemon-owned pair of current session facts plus the
// runtime faults explaining any impairment.
type CompositeState struct {
	Workspace              string
	AgentReplSessionID     string
	ControllerGenerationID ControllerGenerationID
	LifecycleTop           SessionConnectivity
	Connectivity           SessionConnectivity
	Status                 SessionStatus
	StatusCauseKind        string
	StatusCauseSeq         uint64
	StatusAtMs             int64
	ActiveFaults           []RuntimeFault
	LiveTaskCount          int64
	connectivityCauseKind  string
	connectivityAtMs       int64
}

var (
	// ErrStaleControllerGeneration reports an event addressed to a session or
	// generation that is not the workspace's current controller generation.
	ErrStaleControllerGeneration = errors.New("ssm: stale session-controller generation")
	// ErrFaultWindowNotOpen reports a closing edge without a matching open
	// window. Treating it as a no-op would hide producer ordering defects.
	ErrFaultWindowNotOpen = errors.New("ssm: runtime fault window is not open")
	// ErrFaultWindowAlreadyOpen reports a duplicate opening edge.
	ErrFaultWindowAlreadyOpen = errors.New("ssm: runtime fault window is already open")
	// ErrConnectivityTransition reports a lifecycle edge that cannot follow
	// the persisted current generation.
	ErrConnectivityTransition = errors.New("ssm: invalid session-connectivity transition")
)

type connectivityLifecycle struct {
	found        bool
	workspace    string
	sessionID    string
	generationID ControllerGenerationID
	state        SessionConnectivity
	causeKind    string
	atMs         int64
}

type stateQueryer interface {
	Query(query string, args ...any) (*sql.Rows, error)
	QueryRow(query string, args ...any) *sql.Row
}

// ApplySessionConnectivity appends one validated lifecycle edge. A new
// generation can become current only by entering connecting, and operational
// can be asserted only for the current generation after connecting.
func (m *Manager) ApplySessionConnectivity(
	workspace, sessionID, generationID string,
	state SessionConnectivity,
	causeKind string,
) error {
	m.logf("ssm: session connectivity ws=%q session=%q generation=%q next=%q cause=%q branch=enter",
		workspace, sessionID, generationID, state, causeKind)
	if err := validateConnectivityIdentity(workspace, sessionID, generationID, state, causeKind); err != nil {
		m.logf("ssm: session connectivity REJECTED ws=%q session=%q generation=%q prior=%q next=%q cause=%q branch=validation error=%q",
			workspace, sessionID, generationID, "", state, causeKind, err)
		return err
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	tx, err := m.db.Begin()
	if err != nil {
		err = fmt.Errorf("ssm: begin session-connectivity edge ws=%q session=%q generation=%q next=%q cause=%q: %w",
			workspace, sessionID, generationID, state, causeKind, err)
		m.logf("ssm: session connectivity ERROR ws=%q session=%q generation=%q prior=%q next=%q cause=%q branch=begin error=%q",
			workspace, sessionID, generationID, "", state, causeKind, err)
		return err
	}
	defer tx.Rollback()

	prior, err := latestConnectivity(tx, workspace)
	if err != nil {
		err = fmt.Errorf("ssm: read prior session connectivity ws=%q session=%q generation=%q next=%q cause=%q: %w",
			workspace, sessionID, generationID, state, causeKind, err)
		m.logf("ssm: session connectivity ERROR ws=%q session=%q generation=%q prior=%q next=%q cause=%q branch=read-prior error=%q",
			workspace, sessionID, generationID, "", state, causeKind, err)
		return err
	}
	if err := validateConnectivityTransition(prior, sessionID, ControllerGenerationID(generationID), state); err != nil {
		m.logf("ssm: session connectivity REJECTED ws=%q session=%q generation=%q prior=%q prior_session=%q prior_generation=%q next=%q cause=%q branch=transition error=%q",
			workspace, sessionID, generationID, prior.state, prior.sessionID, prior.generationID, state, causeKind, err)
		return err
	}

	at := m.nextAt()
	if _, err := tx.Exec(
		`INSERT INTO session_connectivity(
			workspace, agent_repl_session_id, controller_generation_id, state, cause_kind, at
		) VALUES (?,?,?,?,?,?)`,
		workspace, sessionID, generationID, string(state), causeKind, at,
	); err != nil {
		err = fmt.Errorf("ssm: append session connectivity ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d: %w",
			workspace, sessionID, generationID, prior.state, state, causeKind, at, err)
		m.logf("ssm: session connectivity ERROR ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=append error=%q",
			workspace, sessionID, generationID, prior.state, state, causeKind, at, err)
		return err
	}
	// A BRING-UP RETIRES A STICKY `merged`. Reopening a merged workspace is
	// exactly a new generation entering `connecting`, and without this the
	// workspace's live session would lose every row to a merge that already
	// finished — `merged` outranks the whole color ladder and nothing but another
	// merge row supersedes it. See mergereopen.go; the durable merged-at fact is
	// untouched.
	mergeAxisRetired := false
	if state == SessionConnectivityConnecting {
		retired, err := supersedeMergedAxisOnReopen(tx, workspace, m.nextAt())
		if err != nil {
			m.logf("ssm: session connectivity ERROR ws=%q session=%q generation=%q next=%q cause=%q branch=merge-axis-retire error=%q",
				workspace, sessionID, generationID, state, causeKind, err)
			return err
		}
		if retired {
			mergeAxisRetired = true
			m.logf("ssm: merge axis RETIRED ON REOPEN ws=%q session=%q generation=%q cause=%q — the workspace rested on `merged` and is being brought up again, so the axis is cleared and its live session resolves its own state (merged_at_ms is untouched)",
				workspace, sessionID, generationID, causeKind)
		}
	}
	if err := tx.Commit(); err != nil {
		err = fmt.Errorf("ssm: commit session connectivity ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d: %w",
			workspace, sessionID, generationID, prior.state, state, causeKind, at, err)
		m.logf("ssm: session connectivity ERROR ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=commit error=%q",
			workspace, sessionID, generationID, prior.state, state, causeKind, at, err)
		return err
	}
	m.logf("ssm: session connectivity ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=applied",
		workspace, sessionID, generationID, prior.state, state, causeKind, at)
	// THE RETAINED RUN GOES WITH THE AXIS IT DESCRIBED, and it goes AFTER the
	// commit and BEFORE the push. After the commit because the retirement is not a
	// fact until the `merge_none` row is durable — a rolled-back transaction that
	// had already dropped the status in memory would leave the axis still reading
	// `merged` with nothing to report about the run behind it. Before the push
	// because the very frame this edge publishes is the first one that must not
	// carry the retired run.
	if mergeAxisRetired {
		m.retirePipelineStatusLocked(workspace, causeMergeReopened)
	}
	if err := m.publishCompositeLocked(workspace, causeKind); err != nil {
		return err
	}
	if state == SessionConnectivityOperational {
		m.releaseControllerRegistrationLocked(workspace, generationID)
	}
	return nil
}

// ApplyRuntimeFault appends one component- and generation-scoped fault edge.
// The entire current-generation validation and append share one transaction,
// so a stale controller can never race a replacement into the current state.
func (m *Manager) ApplyRuntimeFault(
	workspace, sessionID, generationID, component, faultType string,
	impact FaultImpact,
	open bool,
	causeKind string,
) error {
	m.logf("ssm: runtime fault ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=enter",
		workspace, sessionID, generationID, component, faultType, impact, open, causeKind)
	if err := validateFaultIdentity(workspace, sessionID, generationID, component, faultType, impact, causeKind); err != nil {
		m.logf("ssm: runtime fault REJECTED ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=validation error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		return err
	}

	m.mu.Lock()
	defer m.mu.Unlock()

	tx, err := m.db.Begin()
	if err != nil {
		err = fmt.Errorf("ssm: begin runtime-fault edge ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=begin error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		return err
	}
	defer tx.Rollback()

	lifecycle, err := latestConnectivity(tx, workspace)
	if err != nil {
		err = fmt.Errorf("ssm: read current connectivity for runtime fault ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=read-connectivity error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		return err
	}
	if !lifecycle.found ||
		lifecycle.state == SessionConnectivityHibernated ||
		lifecycle.sessionID != sessionID ||
		lifecycle.generationID != ControllerGenerationID(generationID) {
		err := fmt.Errorf(
			"%w: workspace=%q current_session=%q current_generation=%q current_connectivity=%q event_session=%q event_generation=%q",
			ErrStaleControllerGeneration,
			workspace,
			lifecycle.sessionID,
			lifecycle.generationID,
			lifecycle.state,
			sessionID,
			generationID,
		)
		m.logf("ssm: runtime fault REJECTED ws=%q session=%q generation=%q current_session=%q current_generation=%q current_connectivity=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=stale-controller error=%q",
			workspace, sessionID, generationID, lifecycle.sessionID, lifecycle.generationID, lifecycle.state, component, faultType, impact, open, causeKind, err)
		return err
	}

	wasOpen, priorImpact, err := faultWindowTop(tx, workspace, ControllerGenerationID(generationID), component, faultType)
	if err != nil {
		err = fmt.Errorf("ssm: read runtime-fault window ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=read-window error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		return err
	}
	if open && wasOpen {
		err := fmt.Errorf("%w: workspace=%q generation=%q component=%q fault_type=%q",
			ErrFaultWindowAlreadyOpen, workspace, generationID, component, faultType)
		m.logf("ssm: runtime fault REJECTED ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q prior_impact=%q open=%t cause=%q branch=already-open error=%q",
			workspace, sessionID, generationID, component, faultType, impact, priorImpact, open, causeKind, err)
		return err
	}
	if !open && !wasOpen {
		err := fmt.Errorf("%w: workspace=%q generation=%q component=%q fault_type=%q",
			ErrFaultWindowNotOpen, workspace, generationID, component, faultType)
		m.logf("ssm: runtime fault REJECTED ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q prior_impact=%q open=%t cause=%q branch=not-open error=%q",
			workspace, sessionID, generationID, component, faultType, impact, priorImpact, open, causeKind, err)
		return err
	}
	if !open && priorImpact != impact {
		err := fmt.Errorf("ssm: runtime fault close impact mismatch: workspace=%q generation=%q component=%q fault_type=%q open_impact=%q close_impact=%q",
			workspace, generationID, component, faultType, priorImpact, impact)
		m.logf("ssm: runtime fault REJECTED ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q prior_impact=%q open=%t cause=%q branch=impact-mismatch error=%q",
			workspace, sessionID, generationID, component, faultType, impact, priorImpact, open, causeKind, err)
		return err
	}

	before, _, err := resolveComposite(tx, workspace)
	if err != nil {
		err = fmt.Errorf("ssm: resolve before runtime-fault edge ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=resolve-before error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		return err
	}

	at := m.nextAt()
	if _, err := tx.Exec(
		`INSERT INTO session_fault(
			workspace, agent_repl_session_id, controller_generation_id,
			component, fault_type, impact, open, cause_kind, at
		) VALUES (?,?,?,?,?,?,?,?,?)`,
		workspace, sessionID, generationID, component, faultType, string(impact), boolInt(open), causeKind, at,
	); err != nil {
		err = fmt.Errorf("ssm: append runtime fault ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q at=%d: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, at, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q at=%d branch=append error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, at, err)
		return err
	}
	after, _, err := resolveComposite(tx, workspace)
	if err != nil {
		err = fmt.Errorf("ssm: resolve after runtime-fault edge ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q branch=resolve-after error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, err)
		return err
	}
	if err := tx.Commit(); err != nil {
		err = fmt.Errorf("ssm: commit runtime fault ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q at=%d: %w",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, at, err)
		m.logf("ssm: runtime fault ERROR ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q open=%t cause=%q at=%d branch=commit error=%q",
			workspace, sessionID, generationID, component, faultType, impact, open, causeKind, at, err)
		return err
	}
	m.logf("ssm: runtime fault ws=%q session=%q generation=%q component=%q fault_type=%q impact=%q prior_open=%t next_open=%t prior_connectivity=%q next_connectivity=%q cause=%q at=%d branch=applied",
		workspace, sessionID, generationID, component, faultType, impact, wasOpen, open, before.Connectivity, after.Connectivity, causeKind, at)
	if err := m.publishCompositeLocked(workspace, causeKind); err != nil {
		return err
	}
	return nil
}

func (m *Manager) publishCompositeLocked(workspace, causeKind string) error {
	if len(m.subs) == 0 {
		return nil
	}
	r, err := resolve(m.db, workspace, m.logf)
	if err != nil {
		return fmt.Errorf("ssm: resolve projection after composite edge workspace=%q cause=%q: %w", workspace, causeKind, err)
	}
	composite, found, err := resolveComposite(m.db, workspace)
	if err != nil {
		return fmt.Errorf("ssm: resolve composite after edge workspace=%q cause=%q: %w", workspace, causeKind, err)
	}
	if !found {
		return fmt.Errorf("ssm: composite edge left workspace %q without a composite state", workspace)
	}
	if !r.found {
		r = resolved{found: true, state: frontendv1.RenderState_RENDER_STATE_UNSPECIFIED}
	}
	// THROUGH THE FUNNEL, not a hand-built message. This used to call
	// compositeWorkspaceState directly, which skipped stampMergeFactsLocked
	// (a connectivity-edge push silently dropped merge_lease_held and the
	// queue facts) and skipped the freshness watermark below it. Every
	// WorkspaceState that leaves the manager goes through
	// workspaceMessageLocked so no push path can under-stamp.
	msg, err := m.workspaceMessageLocked(workspace, r)
	if err != nil {
		return err
	}
	m.last[workspace] = msg.GetState()
	m.lastTasks[workspace] = msg.GetLiveTaskCount()
	// Recorded here too, so the merge-progress push key below stays in step with
	// what actually went out. A connectivity edge that carried the run's newest
	// status and did not record it would leave the next re-resolve believing the
	// status had moved, and republish a run that said nothing new.
	m.lastMergeStatus[workspace] = msg.GetMergeStatus()
	for id, ch := range m.subs {
		select {
		case ch <- msg:
		default:
			m.logf("ssm: subscriber %d slow; dropped composite ws=%s connectivity=%s status=%s cause=%s branch=will_resync",
				id, workspace, composite.Connectivity, composite.Status, causeKind)
		}
	}
	return nil
}

func compositeWorkspaceState(workspace string, projection resolved, composite CompositeState) (*frontendv1.WorkspaceState, error) {
	state, err := compositeRenderState(projection, composite)
	if err != nil {
		return nil, fmt.Errorf("ssm: project composite workspace=%q connectivity=%q status=%q: %w",
			workspace, composite.Connectivity, composite.Status, err)
	}
	msg := projection.toProto(workspace)
	msg.State = state
	msg.SessionId = composite.AgentReplSessionID
	msg.Connectivity = connectivityProto(composite.Connectivity)
	msg.Status = statusProto(composite.Status)
	msg.ControllerGenerationId = string(composite.ControllerGenerationID)
	// The fence is the renderer-facing PROJECTION of the two identities above.
	// WorkspaceState remains the sole authority; every per-workspace push
	// carries a copy of this token so a client can compare and discard a stale
	// one without ever learning what a session is.
	//
	// AN ABSENT CONTROLLER GENERATION YIELDS AN ABSENT FENCE, and this is the
	// whole point of the branch. WorkspaceState's fence is not one fence among
	// many: it is the ANSWER every other fenced push is measured against, and
	// the client adopts it verbatim. Composing one over an empty generation —
	// the post-bounce hibernated window, before any controller is minted —
	// produced a token that NOTHING could ever match, because every fenced view
	// is published from a live generation and the resync eligibility ladder
	// rejects an empty generation outright (identity_mismatch). A client that
	// adopted it had its whole fenced chrome discarded and its every resync
	// refused, and stayed that way until some later WorkspaceState happened to
	// reach it — which for a DETACHED workspace nobody focuses never happens.
	//
	// The empty fence is the honest statement of the same fact and the one both
	// ends already handle: `admitFenced` treats "" as "no ruling yet" and
	// refuses every view against it, and a resync request carrying "" is
	// refused loudly rather than being guessed at. Nothing is invented.
	if composite.ControllerGenerationID == "" {
		msg.Fence = ""
	} else {
		msg.Fence = Fence(composite.AgentReplSessionID, string(composite.ControllerGenerationID))
	}
	switch {
	// THE STATUS AXIS WON THE RENDER STATE, so the frame's cause, instant and
	// turn flag come from the row that won it. Keyed on the RESOLVED state
	// rather than on connectivity, because compositeRenderState can now hand
	// the win to a live turn claim while the connectivity lifecycle is still
	// mid-bring-up; a frame that painted SUBMITTING while reporting
	// turn_active=false would state both halves of the contradiction at once.
	case isSessionStatusRenderState(state):
		msg.CauseKind = composite.StatusCauseKind
		msg.CauseSeq = composite.StatusCauseSeq
		msg.AtMs = composite.StatusAtMs
		// BOTH halves of a turn are active. A composite that only counted
		// `thinking` would report no turn for the whole `submitting`
		// window, which is when a second prompt must be queued rather than
		// forwarded.
		msg.TurnActive = composite.Status == SessionStatusThinking ||
			composite.Status == SessionStatusSubmitting
	case composite.Connectivity == SessionConnectivityOperational:
		// An operational workspace resting on a projection state — a merge
		// phase, a context cut, a dead shim — keeps the projection's own cause
		// and instant, which is what named the row that produced it.
	case composite.Connectivity == SessionConnectivityDegraded:
		if fault, ok := newestConnectivityFault(composite.ActiveFaults); ok {
			msg.CauseKind = fault.CauseKind
			msg.CauseSeq = 0
			msg.AtMs = fault.OpenedAtMs
			msg.TurnActive = false
		}
	case composite.Connectivity == SessionConnectivityHibernated:
		// THE BOOT SWEEP'S VERDICT OUTRANKS THE HIBERNATION'S OWN CAUSE, and
		// only here. Every hibernated row a daemon restart writes carries the
		// anonymous cause `daemon_restart`, which says nothing about the
		// session: it is what EVERY surviving session's row says. When the
		// sweep has since reached a conclusion about THIS one — the shim is
		// gone, a live holder never dialled in, a probe could not tell — that
		// conclusion is the cause the frame should name, so a reader is told
		// which of them happened rather than only that the daemon restarted.
		// resolveComposite reads no other fault while hibernated, so nothing
		// but a boot-sweep verdict can win this branch.
		if fault, ok := newestConnectivityFault(composite.ActiveFaults); ok {
			msg.CauseKind = fault.CauseKind
			msg.CauseSeq = 0
			msg.AtMs = fault.OpenedAtMs
			msg.TurnActive = false
			break
		}
		msg.CauseKind = compositeConnectivityCause(composite)
		msg.CauseSeq = 0
		msg.AtMs = compositeConnectivityAt(composite)
		msg.TurnActive = false
	default:
		msg.CauseKind = compositeConnectivityCause(composite)
		msg.CauseSeq = 0
		msg.AtMs = compositeConnectivityAt(composite)
		msg.TurnActive = false
	}
	msg.ActiveFaults = make([]*frontendv1.RuntimeFault, 0, len(composite.ActiveFaults))
	for _, fault := range composite.ActiveFaults {
		msg.ActiveFaults = append(msg.ActiveFaults, &frontendv1.RuntimeFault{
			Component:  fault.Component,
			FaultType:  fault.FaultType,
			Impact:     string(fault.Impact),
			CauseKind:  fault.CauseKind,
			OpenedAtMs: fault.OpenedAtMs,
		})
	}
	return msg, nil
}

func isSessionStatusRenderState(state frontendv1.RenderState) bool {
	switch state {
	case frontendv1.RenderState_RENDER_STATE_READY,
		frontendv1.RenderState_RENDER_STATE_SUBMITTING,
		frontendv1.RenderState_RENDER_STATE_THINKING,
		frontendv1.RenderState_RENDER_STATE_PERMISSION,
		frontendv1.RenderState_RENDER_STATE_DONE,
		frontendv1.RenderState_RENDER_STATE_INTERRUPTED,
		frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED,
		frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC:
		return true
	default:
		return false
	}
}

func newestConnectivityFault(faults []RuntimeFault) (RuntimeFault, bool) {
	var newest RuntimeFault
	found := false
	for _, fault := range faults {
		if fault.Impact == FaultImpactConnectivity &&
			(!found || fault.OpenedAtMs > newest.OpenedAtMs) {
			newest = fault
			found = true
		}
	}
	return newest, found
}

func compositeConnectivityCause(composite CompositeState) string {
	return composite.connectivityCauseKind
}

func compositeConnectivityAt(composite CompositeState) int64 {
	return composite.connectivityAtMs
}

// claimedTurnRenderState reports the render state a turn CLAIM on the
// session-status axis is entitled to, and whether the axis carries one at all.
//
// Both halves of a turn count, for the same reason the composite's TurnActive
// counts both: `submitting` is the daemon's own commitment to submit and
// `thinking` is the shim holding the prompt, and a claim is claimed in either.
func claimedTurnRenderState(composite CompositeState) (frontendv1.RenderState, bool) {
	switch composite.Status {
	case SessionStatusSubmitting:
		return frontendv1.RenderState_RENDER_STATE_SUBMITTING, true
	case SessionStatusThinking:
		return frontendv1.RenderState_RENDER_STATE_THINKING, true
	default:
		return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED, false
	}
}

// turnClaimOutranksBringUp reports whether a turn claim was written by the
// wiring that is currently coming up, rather than left over from an older one.
//
// A `connecting` lifecycle normally outranks everything: it says there is no
// usable route, which is a stronger statement about what the user can do than
// anything the agent last said. The one claim it must NOT outrank is one this
// bring-up itself produced — the shim's readiness releases the send path before
// the operational edge is written, so a prompt accepted in that window resolved
// INIT with turn_active=false and its own publish invariant refused it.
//
// The discriminator is the claim's instant against the lifecycle edge's: a row
// appended AFTER the connecting edge was written by the wiring that edge
// belongs to. A stale `thinking` from a session resumed across a daemon restart
// is older than the new connecting edge and keeps losing, so a reconnecting
// workspace still paints as one.
func turnClaimOutranksBringUp(composite CompositeState) bool {
	return composite.StatusAtMs > composite.connectivityAtMs
}

func compositeRenderState(projection resolved, composite CompositeState) (frontendv1.RenderState, error) {
	claimed, hasClaim := claimedTurnRenderState(composite)
	switch composite.Connectivity {
	case SessionConnectivityHibernated:
		return frontendv1.RenderState_RENDER_STATE_HIBERNATED, nil
	case SessionConnectivityConnecting:
		if hasClaim && turnClaimOutranksBringUp(composite) {
			return claimed, nil
		}
		return frontendv1.RenderState_RENDER_STATE_INIT, nil
	case SessionConnectivityDegraded:
		return frontendv1.RenderState_RENDER_STATE_DEGRADED, nil
	case SessionConnectivityUnavailable:
		return frontendv1.RenderState_RENDER_STATE_SEVERED, nil
	case SessionConnectivityOperational:
		switch projection.state {
		case frontendv1.RenderState_RENDER_STATE_DEAD,
			frontendv1.RenderState_RENDER_STATE_CLEARING,
			frontendv1.RenderState_RENDER_STATE_COMPACTING,
			frontendv1.RenderState_RENDER_STATE_MERGE_ENQUEUING,
			frontendv1.RenderState_RENDER_STATE_MERGING,
			frontendv1.RenderState_RENDER_STATE_MERGE_QUEUED,
			frontendv1.RenderState_RENDER_STATE_MERGE_CONFLICT,
			frontendv1.RenderState_RENDER_STATE_MERGED:
			// THESE OUTRANK A LIVE TURN, AND merge_conflict IS THE REASON THE
			// LIST IS SPELLED OUT. A workspace parked on a conflict IS
			// `merge_conflict`: the user's only move is to resolve it, the
			// merge machinery owns the session until they do, and a prompt
			// submitted into it is refused on exactly that premise. The cuts,
			// the dead shim and the in-flight merge phases each own the session
			// the same way.
			return projection.state, nil
		case frontendv1.RenderState_RENDER_STATE_INIT,
			frontendv1.RenderState_RENDER_STATE_MERGE_FAILED:
			// NEITHER OWNS THE SESSION, so neither may mask a turn the user
			// just started. `merge_failed` is TERMINAL — the run is over,
			// nothing is coming to clear it, and the only way out is to drive
			// the session — so ranking it above the status axis refused every
			// prompt on the workspace forever. INIT here is a projection that
			// has not caught up with a session the daemon is already driving.
			// Both keep winning while nothing claims a turn, which is what
			// leaves their badge and their color on an idle workspace.
			if !hasClaim {
				return projection.state, nil
			}
		}
		switch composite.Status {
		case SessionStatusReady:
			return frontendv1.RenderState_RENDER_STATE_READY, nil
		case SessionStatusSubmitting:
			return frontendv1.RenderState_RENDER_STATE_SUBMITTING, nil
		case SessionStatusThinking:
			return frontendv1.RenderState_RENDER_STATE_THINKING, nil
		case SessionStatusPermission:
			return frontendv1.RenderState_RENDER_STATE_PERMISSION, nil
		case SessionStatusDone:
			return frontendv1.RenderState_RENDER_STATE_DONE, nil
		case SessionStatusInterrupted:
			return frontendv1.RenderState_RENDER_STATE_INTERRUPTED, nil
		case SessionStatusVendorBlocked:
			return frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED, nil
		case SessionStatusMonitoring:
			return frontendv1.RenderState_RENDER_STATE_IDLE_ASYNC, nil
		default:
			return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED,
				fmt.Errorf("operational controller has invalid session status %q", composite.Status)
		}
	default:
		return frontendv1.RenderState_RENDER_STATE_UNSPECIFIED,
			fmt.Errorf("invalid session connectivity %q", composite.Connectivity)
	}
}

func connectivityProto(state SessionConnectivity) frontendv1.SessionConnectivity {
	return map[SessionConnectivity]frontendv1.SessionConnectivity{
		SessionConnectivityHibernated:  frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED,
		SessionConnectivityConnecting:  frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_CONNECTING,
		SessionConnectivityOperational: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL,
		SessionConnectivityDegraded:    frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_DEGRADED,
		SessionConnectivityUnavailable: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_UNAVAILABLE,
	}[state]
}

func statusProto(status SessionStatus) frontendv1.SessionStatus {
	return map[SessionStatus]frontendv1.SessionStatus{
		SessionStatusReady:         frontendv1.SessionStatus_SESSION_STATUS_READY,
		SessionStatusSubmitting:    frontendv1.SessionStatus_SESSION_STATUS_SUBMITTING,
		SessionStatusThinking:      frontendv1.SessionStatus_SESSION_STATUS_THINKING,
		SessionStatusPermission:    frontendv1.SessionStatus_SESSION_STATUS_PERMISSION,
		SessionStatusDone:          frontendv1.SessionStatus_SESSION_STATUS_DONE,
		SessionStatusInterrupted:   frontendv1.SessionStatus_SESSION_STATUS_INTERRUPTED,
		SessionStatusVendorBlocked: frontendv1.SessionStatus_SESSION_STATUS_VENDOR_BLOCKED,
		SessionStatusMonitoring:    frontendv1.SessionStatus_SESSION_STATUS_MONITORING,
	}[status]
}

// Composite resolves the authoritative connectivity and session status pair.
// Legacy wired and degraded workspace_state rows are deliberately absent from
// every query in this path and therefore have no authority over the result.
func (m *Manager) Composite(workspace string) (CompositeState, bool, error) {
	m.logf("ssm: composite resolution ws=%q branch=enter", workspace)
	if workspace == "" {
		err := fmt.Errorf("ssm: Composite got an empty workspace")
		m.logf("ssm: composite resolution REJECTED ws=%q branch=validation error=%q", workspace, err)
		return CompositeState{}, false, err
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	state, found, err := resolveComposite(m.db, workspace)
	if err != nil {
		err = fmt.Errorf("ssm: resolve composite workspace=%q: %w", workspace, err)
		m.logf("ssm: composite resolution ERROR ws=%q branch=query error=%q", workspace, err)
		return CompositeState{}, false, err
	}
	if !found {
		m.logf("ssm: composite resolution ws=%q branch=not-found", workspace)
		return CompositeState{}, false, nil
	}
	m.logCompositeResolution(state)
	return state, true, nil
}

func resolveComposite(q stateQueryer, workspace string) (CompositeState, bool, error) {
	lifecycle, err := latestConnectivity(q, workspace)
	if err != nil {
		return CompositeState{}, false, err
	}
	statusResolution, err := resolveSessionStatus(q, workspace, lifecycle)
	if err != nil {
		return CompositeState{}, false, err
	}
	if !lifecycle.found && !statusResolution.found {
		return CompositeState{}, false, nil
	}

	state := CompositeState{
		Workspace:             workspace,
		Connectivity:          SessionConnectivityHibernated,
		Status:                statusResolution.status,
		StatusCauseKind:       statusResolution.causeKind,
		StatusCauseSeq:        statusResolution.causeSeq,
		StatusAtMs:            statusResolution.atMs,
		LiveTaskCount:         statusResolution.taskCount,
		connectivityCauseKind: lifecycle.causeKind,
		connectivityAtMs:      lifecycle.atMs,
	}
	if lifecycle.found {
		state.AgentReplSessionID = lifecycle.sessionID
		state.LifecycleTop = lifecycle.state
		state.Connectivity = lifecycle.state
		if lifecycle.state != SessionConnectivityHibernated {
			state.ControllerGenerationID = lifecycle.generationID
			faults, err := activeFaults(q, workspace, lifecycle.sessionID, lifecycle.generationID, "")
			if err != nil {
				return CompositeState{}, false, err
			}
			state.ActiveFaults = faults
			if lifecycle.state == SessionConnectivityOperational && hasConnectivityFault(faults) {
				state.Connectivity = SessionConnectivityDegraded
			}
		} else {
			// A HIBERNATED WORKSPACE HAS NO CURRENT GENERATION, so no
			// generation-scoped fault is implied — with exactly ONE exception,
			// read here and nowhere else: the boot sweep's verdict about this
			// very session (bootsweepverdict.go). It is the only fault that
			// predates every generation by construction, so it is the only one
			// entitled to be read while none is current, and the read is
			// narrowed to its component so a dead generation's ordinary open
			// windows stay retired.
			//
			// ControllerGenerationID stays EMPTY regardless: hibernated means
			// no generation is current, and publishing the retired one would
			// contradict the axis the frame also carries.
			faults, err := activeFaults(q, workspace, lifecycle.sessionID, lifecycle.generationID, BootSweepFaultComponent)
			if err != nil {
				return CompositeState{}, false, err
			}
			state.ActiveFaults = faults
		}
	} else {
		state.AgentReplSessionID = statusResolution.sessionID
	}
	return state, true, nil
}

func (m *Manager) logCompositeResolution(state CompositeState) {
	keys := make([]string, 0, len(state.ActiveFaults))
	for _, fault := range state.ActiveFaults {
		keys = append(keys, fmt.Sprintf("%s/%s:%s", fault.Component, fault.FaultType, fault.Impact))
	}
	sort.Strings(keys)
	m.logf("ssm: composite resolution ws=%q session=%q generation=%q lifecycle_top=%q active_faults=%q connectivity=%q status=%q live_task_count=%d branch=resolved",
		state.Workspace,
		state.AgentReplSessionID,
		state.ControllerGenerationID,
		state.LifecycleTop,
		strings.Join(keys, ","),
		state.Connectivity,
		state.Status,
		state.LiveTaskCount,
	)
}

func latestConnectivity(q stateQueryer, workspace string) (connectivityLifecycle, error) {
	var row connectivityLifecycle
	row.workspace = workspace
	var state string
	err := q.QueryRow(
		`SELECT agent_repl_session_id, controller_generation_id, state, cause_kind, at
		 FROM session_connectivity
		 WHERE workspace = ?
		 ORDER BY at DESC LIMIT 1`,
		workspace,
	).Scan(&row.sessionID, &row.generationID, &state, &row.causeKind, &row.atMs)
	if err == sql.ErrNoRows {
		return row, nil
	}
	if err != nil {
		return connectivityLifecycle{}, fmt.Errorf("read latest session connectivity for workspace %q: %w", workspace, err)
	}
	row.state = SessionConnectivity(state)
	if !validPersistedConnectivity(row.state) {
		return connectivityLifecycle{}, fmt.Errorf("invalid persisted session connectivity %q for workspace %q session %q generation %q",
			state, workspace, row.sessionID, row.generationID)
	}
	row.found = true
	return row, nil
}

type sessionStatusResolution struct {
	status    SessionStatus
	taskCount int64
	sessionID string
	found     bool
	causeKind string
	causeSeq  uint64
	atMs      int64
}

func resolveSessionStatus(
	q stateQueryer,
	workspace string,
	lifecycle connectivityLifecycle,
) (sessionStatusResolution, error) {
	var (
		token     string
		sessionID sql.NullString
		causeKind sql.NullString
		causeSeq  sql.NullInt64
		atMs      int64
	)
	lowerBound, bounded, err := statusGenerationLowerBound(q, workspace, lifecycle)
	if err != nil {
		return sessionStatusResolution{}, err
	}
	query := `SELECT state, session_id, cause_kind, cause_seq, at
		FROM workspace_state
		WHERE workspace = ?
		  AND state IN ('submitting','thinking','permission','done','ready','idle','vendor_blocked','interrupted')`
	args := []any{workspace}
	if bounded {
		query += ` AND at >= ?`
		args = append(args, lowerBound)
	}
	query += ` ORDER BY at DESC LIMIT 1`
	err = q.QueryRow(query, args...).Scan(&token, &sessionID, &causeKind, &causeSeq, &atMs)
	if err == sql.ErrNoRows {
		return sessionStatusResolution{}, nil
	}
	if err != nil {
		return sessionStatusResolution{}, fmt.Errorf("read session status for workspace %q: %w", workspace, err)
	}
	taskCount, err := liveTaskCount(q, workspace, lowerBound, bounded)
	if err != nil {
		return sessionStatusResolution{}, err
	}
	status, err := sessionStatusOf(token, taskCount)
	if err != nil {
		return sessionStatusResolution{}, err
	}
	return sessionStatusResolution{
		status:    status,
		taskCount: taskCount,
		sessionID: sessionID.String,
		found:     true,
		causeKind: causeKind.String,
		causeSeq:  uint64(causeSeq.Int64),
		atMs:      atMs,
	}, nil
}

// statusGenerationLowerBound keeps two different identities distinct without
// confusing either one with the vendor transcript UUID stored on stream rows.
// A replacement agent-repl session starts a new status epoch at its first
// controller-generation edge; a replacement generation for the SAME session
// keeps the prior status, including vendor UUID rotations inside that session.
func statusGenerationLowerBound(
	q stateQueryer,
	workspace string,
	lifecycle connectivityLifecycle,
) (int64, bool, error) {
	if !lifecycle.found || lifecycle.sessionID == "" || lifecycle.generationID == "" {
		return 0, false, nil
	}
	var firstAt int64
	if err := q.QueryRow(
		`SELECT MIN(at)
		 FROM session_connectivity
		 WHERE workspace = ?
		   AND agent_repl_session_id = ?
		   AND controller_generation_id = ?`,
		workspace, lifecycle.sessionID, lifecycle.generationID,
	).Scan(&firstAt); err != nil {
		return 0, false, fmt.Errorf(
			"read controller-generation start for workspace %q session %q generation %q: %w",
			workspace, lifecycle.sessionID, lifecycle.generationID, err)
	}
	var priorSessionID string
	err := q.QueryRow(
		`SELECT agent_repl_session_id
		 FROM session_connectivity
		 WHERE workspace = ? AND at < ?
		 ORDER BY at DESC LIMIT 1`,
		workspace, firstAt,
	).Scan(&priorSessionID)
	if err == sql.ErrNoRows {
		return 0, false, nil
	}
	if err != nil {
		return 0, false, fmt.Errorf(
			"read prior connectivity identity for workspace %q session %q generation %q at=%d: %w",
			workspace, lifecycle.sessionID, lifecycle.generationID, firstAt, err)
	}
	if priorSessionID == lifecycle.sessionID {
		return 0, false, nil
	}
	return firstAt, true, nil
}

func liveTaskCount(q stateQueryer, workspace string, lowerBound int64, bounded bool) (int64, error) {
	var count int64
	query := `WITH rows AS (
			SELECT state, task_id FROM workspace_state WHERE workspace = ?`
	args := []any{workspace}
	if bounded {
		query += ` AND at >= ?`
		args = append(args, lowerBound)
	}
	query += `
		)
		SELECT
			(SELECT COUNT(*) FROM (
				SELECT DISTINCT task_id FROM rows
				WHERE state = 'task_started' AND task_id IS NOT NULL
				EXCEPT
				SELECT DISTINCT task_id FROM rows
				WHERE state = 'task_ended' AND task_id IS NOT NULL
			))
			+ MAX(
				(SELECT COUNT(*) FROM rows WHERE state = 'task_started' AND task_id IS NULL)
				- (SELECT COUNT(*) FROM rows WHERE state = 'task_ended' AND task_id IS NULL),
				0
			)`
	err := q.QueryRow(query, args...).Scan(&count)
	if err != nil {
		return 0, fmt.Errorf("read live task count for workspace %q: %w", workspace, err)
	}
	return count, nil
}

func sessionStatusOf(token string, liveTasks int64) (SessionStatus, error) {
	switch token {
	case sigSubmitting:
		return SessionStatusSubmitting, nil
	case sigThinking:
		return SessionStatusThinking, nil
	case sigPermission:
		return SessionStatusPermission, nil
	case sigDone:
		if liveTasks > 0 {
			return SessionStatusMonitoring, nil
		}
		return SessionStatusDone, nil
	case sigInterrupted:
		if liveTasks > 0 {
			return SessionStatusMonitoring, nil
		}
		return SessionStatusInterrupted, nil
	case sigVendorBlocked:
		return SessionStatusVendorBlocked, nil
	case sigReady, sigIdle:
		if liveTasks > 0 {
			return SessionStatusMonitoring, nil
		}
		return SessionStatusReady, nil
	default:
		return "", fmt.Errorf("invalid persisted session status %q", token)
	}
}

// activeFaults reads the open fault windows for one (session, generation).
//
// component NARROWS the read to a single component and is not a convenience:
// it is what lets a HIBERNATED workspace resolve the boot-sweep verdict alone
// (bootsweepverdict.go) without also resurrecting whatever ordinary faults the
// dead generation happened to leave open. Empty means every component, which
// is what a workspace with a current generation reads.
func activeFaults(
	q stateQueryer,
	workspace, sessionID string,
	generationID ControllerGenerationID,
	component string,
) ([]RuntimeFault, error) {
	componentFilter := ""
	args := []any{workspace, sessionID, generationID}
	if component != "" {
		componentFilter = " AND component = ?"
		args = append(args, component)
	}
	rows, err := q.Query(
		`WITH latest AS (
			SELECT
				workspace,
				agent_repl_session_id,
				controller_generation_id,
				component,
				fault_type,
				impact,
				open,
				cause_kind,
				at,
				ROW_NUMBER() OVER (
					PARTITION BY controller_generation_id, component, fault_type
					ORDER BY at DESC
				) AS row_number
			FROM session_fault
			WHERE workspace = ?
			  AND agent_repl_session_id = ?
			  AND controller_generation_id = ?`+componentFilter+`
		)
		SELECT
			workspace,
			agent_repl_session_id,
			controller_generation_id,
			component,
			fault_type,
			impact,
			cause_kind,
			at
		FROM latest
		WHERE row_number = 1 AND open = 1
		ORDER BY component, fault_type`,
		args...,
	)
	if err != nil {
		return nil, fmt.Errorf("read active runtime faults for workspace %q session %q generation %q: %w",
			workspace, sessionID, generationID, err)
	}
	defer rows.Close()

	var faults []RuntimeFault
	for rows.Next() {
		var fault RuntimeFault
		var impact string
		if err := rows.Scan(
			&fault.Workspace,
			&fault.AgentReplSessionID,
			&fault.ControllerGenerationID,
			&fault.Component,
			&fault.FaultType,
			&impact,
			&fault.CauseKind,
			&fault.OpenedAtMs,
		); err != nil {
			return nil, fmt.Errorf("scan active runtime fault for workspace %q session %q generation %q: %w",
				workspace, sessionID, generationID, err)
		}
		fault.Impact = FaultImpact(impact)
		if !validFaultImpact(fault.Impact) {
			return nil, fmt.Errorf("invalid persisted runtime-fault impact %q for workspace %q session %q generation %q component %q fault_type %q",
				impact, workspace, sessionID, generationID, fault.Component, fault.FaultType)
		}
		faults = append(faults, fault)
	}
	if err := rows.Err(); err != nil {
		return nil, fmt.Errorf("iterate active runtime faults for workspace %q session %q generation %q: %w",
			workspace, sessionID, generationID, err)
	}
	return faults, nil
}

func faultWindowTop(
	q stateQueryer,
	workspace string,
	generationID ControllerGenerationID,
	component, faultType string,
) (bool, FaultImpact, error) {
	var (
		open   int
		impact string
	)
	err := q.QueryRow(
		`SELECT open, impact
		 FROM session_fault
		 WHERE workspace = ?
		   AND controller_generation_id = ?
		   AND component = ?
		   AND fault_type = ?
		 ORDER BY at DESC LIMIT 1`,
		workspace, generationID, component, faultType,
	).Scan(&open, &impact)
	if err == sql.ErrNoRows {
		return false, "", nil
	}
	if err != nil {
		return false, "", fmt.Errorf("read runtime-fault window for workspace %q generation %q component %q fault_type %q: %w",
			workspace, generationID, component, faultType, err)
	}
	typedImpact := FaultImpact(impact)
	if !validFaultImpact(typedImpact) {
		return false, "", fmt.Errorf("invalid persisted runtime-fault impact %q for workspace %q generation %q component %q fault_type %q",
			impact, workspace, generationID, component, faultType)
	}
	return open == 1, typedImpact, nil
}

func validateConnectivityIdentity(
	workspace, sessionID, generationID string,
	state SessionConnectivity,
	causeKind string,
) error {
	switch {
	case workspace == "":
		return fmt.Errorf("ssm: session connectivity got an empty workspace")
	case sessionID == "":
		return fmt.Errorf("ssm: session connectivity for workspace %q got an empty agent-repl session id", workspace)
	case generationID == "":
		return fmt.Errorf("ssm: session connectivity for workspace %q session %q got an empty controller generation id", workspace, sessionID)
	case causeKind == "":
		return fmt.Errorf("ssm: session connectivity for workspace %q session %q generation %q got an empty cause kind", workspace, sessionID, generationID)
	case !validLifecycleConnectivity(state):
		return fmt.Errorf("ssm: session connectivity for workspace %q session %q generation %q got invalid lifecycle state %q", workspace, sessionID, generationID, state)
	default:
		return nil
	}
}

func validateFaultIdentity(
	workspace, sessionID, generationID, component, faultType string,
	impact FaultImpact,
	causeKind string,
) error {
	switch {
	case workspace == "":
		return fmt.Errorf("ssm: runtime fault got an empty workspace")
	case sessionID == "":
		return fmt.Errorf("ssm: runtime fault for workspace %q got an empty agent-repl session id", workspace)
	case generationID == "":
		return fmt.Errorf("ssm: runtime fault for workspace %q session %q got an empty controller generation id", workspace, sessionID)
	case component == "":
		return fmt.Errorf("ssm: runtime fault for workspace %q session %q generation %q got an empty component", workspace, sessionID, generationID)
	case faultType == "":
		return fmt.Errorf("ssm: runtime fault for workspace %q session %q generation %q component %q got an empty fault type", workspace, sessionID, generationID, component)
	case !validFaultImpact(impact):
		return fmt.Errorf("ssm: runtime fault for workspace %q session %q generation %q component %q fault_type %q got invalid impact %q",
			workspace, sessionID, generationID, component, faultType, impact)
	case causeKind == "":
		return fmt.Errorf("ssm: runtime fault for workspace %q session %q generation %q component %q fault_type %q impact %q got an empty cause kind",
			workspace, sessionID, generationID, component, faultType, impact)
	default:
		return nil
	}
}

func validateConnectivityTransition(
	prior connectivityLifecycle,
	sessionID string,
	generationID ControllerGenerationID,
	next SessionConnectivity,
) error {
	if !prior.found {
		if next == SessionConnectivityConnecting || next == SessionConnectivityHibernated {
			return nil
		}
		return fmt.Errorf("%w: first edge must be connecting or hibernated, got %q", ErrConnectivityTransition, next)
	}
	if prior.sessionID != sessionID || prior.generationID != generationID {
		if prior.generationID == generationID {
			return fmt.Errorf("%w: generation %q cannot move from session %q to session %q",
				ErrStaleControllerGeneration, generationID, prior.sessionID, sessionID)
		}
		if next == SessionConnectivityConnecting {
			return nil
		}
		return fmt.Errorf("%w: replacement session/generation must enter connecting before %q", ErrStaleControllerGeneration, next)
	}
	if prior.state == next {
		return fmt.Errorf("%w: state already %q", ErrConnectivityTransition, next)
	}
	if prior.state == SessionConnectivityHibernated {
		return fmt.Errorf("%w: hibernated generation %q is retired and cannot become current again",
			ErrStaleControllerGeneration, generationID)
	}
	if next == SessionConnectivityOperational && prior.state != SessionConnectivityConnecting {
		return fmt.Errorf("%w: operational requires a preceding connecting edge for the same session/generation, prior=%q",
			ErrConnectivityTransition, prior.state)
	}
	return nil
}

func validLifecycleConnectivity(state SessionConnectivity) bool {
	switch state {
	case SessionConnectivityHibernated,
		SessionConnectivityConnecting,
		SessionConnectivityOperational,
		SessionConnectivityUnavailable:
		return true
	default:
		return false
	}
}

func validPersistedConnectivity(state SessionConnectivity) bool {
	return validLifecycleConnectivity(state)
}

func validFaultImpact(impact FaultImpact) bool {
	switch impact {
	case FaultImpactConnectivity,
		FaultImpactFeature,
		FaultImpactCommand,
		FaultImpactTurnTerminal:
		return true
	default:
		return false
	}
}

func hasConnectivityFault(faults []RuntimeFault) bool {
	for _, fault := range faults {
		if fault.Impact == FaultImpactConnectivity {
			return true
		}
	}
	return false
}

func boolInt(value bool) int {
	if value {
		return 1
	}
	return 0
}

// hibernatePersistedConnectivityLocked invalidates every operational claim
// that belonged to the previous daemon process. Controller generations are
// daemon-local and cannot remain current across Open.
func (m *Manager) hibernatePersistedConnectivityLocked() error {
	tx, err := m.db.Begin()
	if err != nil {
		err = fmt.Errorf("ssm: begin persisted session-connectivity reset: %w", err)
		m.logf("ssm: persisted connectivity reset ERROR branch=begin error=%q", err)
		return err
	}
	defer tx.Rollback()

	rows, err := tx.Query(`
		WITH latest AS (
			SELECT
				workspace,
				agent_repl_session_id,
				controller_generation_id,
				state,
				ROW_NUMBER() OVER (PARTITION BY workspace ORDER BY at DESC) AS row_number
			FROM session_connectivity
		)
		SELECT workspace, agent_repl_session_id, controller_generation_id, state
		FROM latest
		WHERE row_number = 1 AND state <> 'hibernated'
		ORDER BY workspace`)
	if err != nil {
		err = fmt.Errorf("ssm: list persisted session connectivity at daemon restart: %w", err)
		m.logf("ssm: persisted connectivity reset ERROR branch=list error=%q", err)
		return err
	}
	type prior struct {
		workspace    string
		sessionID    string
		generationID string
		state        string
		resetAt      int64
	}
	var pending []prior
	for rows.Next() {
		var item prior
		if err := rows.Scan(&item.workspace, &item.sessionID, &item.generationID, &item.state); err != nil {
			rows.Close()
			err = fmt.Errorf("ssm: scan persisted session connectivity at daemon restart: %w", err)
			m.logf("ssm: persisted connectivity reset ERROR branch=scan error=%q", err)
			return err
		}
		pending = append(pending, item)
	}
	if err := rows.Err(); err != nil {
		rows.Close()
		err = fmt.Errorf("ssm: iterate persisted session connectivity at daemon restart: %w", err)
		m.logf("ssm: persisted connectivity reset ERROR branch=iterate error=%q", err)
		return err
	}
	if err := rows.Close(); err != nil {
		err = fmt.Errorf("ssm: close persisted session-connectivity rows: %w", err)
		m.logf("ssm: persisted connectivity reset ERROR branch=close-rows error=%q", err)
		return err
	}
	for index := range pending {
		item := &pending[index]
		at := m.nextAt()
		if _, err := tx.Exec(
			`INSERT INTO session_connectivity(
				workspace, agent_repl_session_id, controller_generation_id, state, cause_kind, at
			) VALUES (?,?,?,?,?,?)`,
			item.workspace,
			item.sessionID,
			item.generationID,
			string(SessionConnectivityHibernated),
			"daemon_restart",
			at,
		); err != nil {
			err = fmt.Errorf("ssm: append daemon-restart hibernation ws=%q session=%q generation=%q prior=%q at=%d: %w",
				item.workspace, item.sessionID, item.generationID, item.state, at, err)
			m.logf("ssm: persisted connectivity reset ERROR ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=append error=%q",
				item.workspace, item.sessionID, item.generationID, item.state, SessionConnectivityHibernated, "daemon_restart", at, err)
			return err
		}
		item.resetAt = at
	}
	if err := tx.Commit(); err != nil {
		err = fmt.Errorf("ssm: commit persisted session-connectivity reset count=%d: %w", len(pending), err)
		m.logf("ssm: persisted connectivity reset ERROR count=%d branch=commit error=%q", len(pending), err)
		return err
	}
	for _, item := range pending {
		m.logf("ssm: persisted connectivity reset ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=applied",
			item.workspace, item.sessionID, item.generationID, item.state, SessionConnectivityHibernated, "daemon_restart", item.resetAt)
	}
	if len(pending) == 0 {
		m.logf("ssm: persisted connectivity reset branch=no-active-generations count=0")
	}
	return nil
}

// seedMissingConnectivityLocked gives every restored legacy workspace an
// explicit hibernated lifecycle without fabricating a live controller
// generation. This is the coordinated-migration boundary: legacy projection
// rows stay immutable history, while every frontend snapshot after Open has a
// mandatory composite connectivity verdict.
func (m *Manager) seedMissingConnectivityLocked() error {
	rows, err := m.db.Query(`
		WITH restored_workspaces AS (
			SELECT DISTINCT workspace FROM workspace_state
		)
		SELECT
			restored_workspaces.workspace,
			COALESCE((
				SELECT session_id
				FROM workspace_state
				WHERE workspace_state.workspace = restored_workspaces.workspace
				  AND session_id IS NOT NULL
				  AND session_id <> ''
				ORDER BY at DESC LIMIT 1
			), '')
		FROM restored_workspaces
		WHERE 1 = 1
		  AND NOT EXISTS (
			SELECT 1 FROM session_connectivity
			WHERE session_connectivity.workspace = restored_workspaces.workspace
		  )
		ORDER BY restored_workspaces.workspace`)
	if err != nil {
		return fmt.Errorf("ssm: list restored workspaces missing connectivity: %w", err)
	}
	type missing struct{ workspace, sessionID string }
	var pending []missing
	for rows.Next() {
		var item missing
		if err := rows.Scan(&item.workspace, &item.sessionID); err != nil {
			return fmt.Errorf("ssm: scan restored workspace missing connectivity: %w", err)
		}
		pending = append(pending, item)
	}
	if err := rows.Err(); err != nil {
		rows.Close()
		return fmt.Errorf("ssm: iterate restored workspaces missing connectivity: %w", err)
	}
	if err := rows.Close(); err != nil {
		return fmt.Errorf("ssm: close restored workspaces missing connectivity: %w", err)
	}
	for _, item := range pending {
		at := m.nextAt()
		if _, err := m.db.Exec(
			`INSERT INTO session_connectivity(
				workspace, agent_repl_session_id, controller_generation_id,
				state, cause_kind, at
			) VALUES (?,?,?,?,?,?)`,
			item.workspace, item.sessionID, "",
			string(SessionConnectivityHibernated),
			"daemon_restart_no_controller_generation",
			at,
		); err != nil {
			return fmt.Errorf("ssm: seed restored hibernation workspace=%q session=%q at=%d: %w",
				item.workspace, item.sessionID, at, err)
		}
		m.logf("ssm: restored connectivity ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=seed_no_controller_generation",
			item.workspace, item.sessionID, "", "", SessionConnectivityHibernated,
			"daemon_restart_no_controller_generation", at)
	}
	return nil
}
