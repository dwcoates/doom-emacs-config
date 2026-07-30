package ssm

import (
	"database/sql"
	"errors"
	"fmt"
	"sort"
	"strings"
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
	ActiveFaults           []RuntimeFault
	LiveTaskCount          int64
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
	if err := tx.Commit(); err != nil {
		err = fmt.Errorf("ssm: commit session connectivity ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d: %w",
			workspace, sessionID, generationID, prior.state, state, causeKind, at, err)
		m.logf("ssm: session connectivity ERROR ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=commit error=%q",
			workspace, sessionID, generationID, prior.state, state, causeKind, at, err)
		return err
	}
	m.logf("ssm: session connectivity ws=%q session=%q generation=%q prior=%q next=%q cause=%q at=%d branch=applied",
		workspace, sessionID, generationID, prior.state, state, causeKind, at)
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
	return nil
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
	status, taskCount, statusSessionID, statusFound, err := resolveSessionStatus(q, workspace, lifecycle.sessionID)
	if err != nil {
		return CompositeState{}, false, err
	}
	if !lifecycle.found && !statusFound {
		return CompositeState{}, false, nil
	}

	state := CompositeState{
		Workspace:     workspace,
		Connectivity:  SessionConnectivityHibernated,
		Status:        status,
		LiveTaskCount: taskCount,
	}
	if lifecycle.found {
		state.AgentReplSessionID = lifecycle.sessionID
		state.LifecycleTop = lifecycle.state
		state.Connectivity = lifecycle.state
		if lifecycle.state != SessionConnectivityHibernated {
			state.ControllerGenerationID = lifecycle.generationID
			faults, err := activeFaults(q, workspace, lifecycle.sessionID, lifecycle.generationID)
			if err != nil {
				return CompositeState{}, false, err
			}
			state.ActiveFaults = faults
			if lifecycle.state == SessionConnectivityOperational && hasConnectivityFault(faults) {
				state.Connectivity = SessionConnectivityDegraded
			}
		}
	} else {
		state.AgentReplSessionID = statusSessionID
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

func resolveSessionStatus(
	q stateQueryer,
	workspace, currentSessionID string,
) (SessionStatus, int64, string, bool, error) {
	var (
		token     string
		sessionID sql.NullString
	)
	query := `SELECT state, session_id
		FROM workspace_state
		WHERE workspace = ?
		  AND state IN ('thinking','permission','done','ready','idle','vendor_blocked','interrupted')`
	args := []any{workspace}
	if currentSessionID != "" {
		query += ` AND session_id = ?`
		args = append(args, currentSessionID)
	}
	query += ` ORDER BY at DESC LIMIT 1`
	err := q.QueryRow(query, args...).Scan(&token, &sessionID)
	if err == sql.ErrNoRows {
		return "", 0, "", false, nil
	}
	if err != nil {
		return "", 0, "", false, fmt.Errorf("read session status for workspace %q: %w", workspace, err)
	}
	taskCount, err := liveTaskCount(q, workspace, currentSessionID)
	if err != nil {
		return "", 0, "", false, err
	}
	status, err := sessionStatusOf(token, taskCount)
	if err != nil {
		return "", 0, "", false, err
	}
	return status, taskCount, sessionID.String, true, nil
}

func liveTaskCount(q stateQueryer, workspace, currentSessionID string) (int64, error) {
	var count int64
	query := `WITH rows AS (
			SELECT state, task_id FROM workspace_state WHERE workspace = ?`
	args := []any{workspace}
	if currentSessionID != "" {
		query += ` AND session_id = ?`
		args = append(args, currentSessionID)
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

func activeFaults(
	q stateQueryer,
	workspace, sessionID string,
	generationID ControllerGenerationID,
) ([]RuntimeFault, error) {
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
			  AND controller_generation_id = ?
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
		workspace, sessionID, generationID,
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
