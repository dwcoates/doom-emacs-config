package ssm

import (
	"errors"
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
)

// ErrControllerRegistrationInProgress means a controller generation owns the
// workspace's bring-up admission. A hibernation cannot begin until that
// generation either reaches operational or abandons registration.
var ErrControllerRegistrationInProgress = errors.New("ssm: controller registration is in progress")

// ErrHibernationInProgress means hibernation owns the workspace's turn and
// controller admission. A controller generation cannot publish itself until
// the stop completes and the hibernation lease is released.
var ErrHibernationInProgress = errors.New("ssm: hibernation is in progress")

// AcquireControllerRegistration excludes hibernation from the beginning of a
// controller generation's bring-up through its operational edge. Multiple
// concurrent bring-up contenders may hold reservations; the manager selects
// one winner, and every contender releases its own reservation exactly once.
func (m *Manager) AcquireControllerRegistration(workspace, sessionID, generationID string) (func(), error) {
	if workspace == "" || sessionID == "" || generationID == "" {
		return nil, fmt.Errorf("ssm: AcquireControllerRegistration requires workspace, session, and generation identity")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.hibernationLeases[workspace] != 0 {
		return nil, fmt.Errorf("%w for workspace %q", ErrHibernationInProgress, workspace)
	}
	registrations := m.controllerRegistrations[workspace]
	if registrations == nil {
		registrations = make(map[string]struct{})
		m.controllerRegistrations[workspace] = registrations
	}
	if _, exists := registrations[generationID]; exists {
		return nil, fmt.Errorf("ssm: controller generation %q already has a registration for workspace %q", generationID, workspace)
	}
	registrations[generationID] = struct{}{}
	released := false
	return func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		if released {
			panic(fmt.Sprintf("ssm: controller registration for workspace %q released more than once", workspace))
		}
		released = true
		m.releaseControllerRegistrationLocked(workspace, generationID)
	}, nil
}

func (m *Manager) releaseControllerRegistrationLocked(workspace, generationID string) {
	registrations := m.controllerRegistrations[workspace]
	delete(registrations, generationID)
	if len(registrations) == 0 {
		delete(m.controllerRegistrations, workspace)
	}
}

// AcquireHibernationLease atomically snapshots a workspace and excludes every
// new prompt/turn start until release. Durable active claims are folded into
// TurnActive because a claim may commit immediately before its render-state
// event is applied.
func (m *Manager) AcquireHibernationLease(workspace string) (*frontendv1.WorkspaceState, bool, func(), error) {
	if workspace == "" {
		return nil, false, nil, fmt.Errorf("ssm: AcquireHibernationLease got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.hibernationLeases[workspace] != 0 {
		return nil, false, nil, fmt.Errorf("ssm: workspace %q already has a hibernation lease", workspace)
	}
	if len(m.controllerRegistrations[workspace]) != 0 {
		return nil, false, nil, fmt.Errorf("%w for workspace %q", ErrControllerRegistrationInProgress, workspace)
	}
	state, found, err := m.currentLocked(workspace)
	if err != nil {
		return nil, false, nil, fmt.Errorf("ssm: resolve workspace %q for hibernation: %w", workspace, err)
	}
	var activeClaims int64
	if err := m.db.QueryRow(`SELECT COUNT(*) FROM turn_lifecycle_claim WHERE workspace=? AND end_seq IS NULL`, workspace).Scan(&activeClaims); err != nil {
		return nil, false, nil, fmt.Errorf("ssm: read active turn claims for workspace %q before hibernation: %w", workspace, err)
	}
	if activeClaims > 0 {
		if !found {
			state = &frontendv1.WorkspaceState{Workspace: workspace}
			found = true
		} else {
			state = proto.Clone(state).(*frontendv1.WorkspaceState)
		}
		state.TurnActive = true
	}
	m.nextHibernationLease++
	token := m.nextHibernationLease
	m.hibernationLeases[workspace] = token
	released := false
	release := func() {
		m.mu.Lock()
		defer m.mu.Unlock()
		if released {
			panic(fmt.Sprintf("ssm: hibernation lease for workspace %q released more than once", workspace))
		}
		released = true
		if m.hibernationLeases[workspace] != token {
			panic(fmt.Sprintf("ssm: hibernation lease token changed for workspace %q", workspace))
		}
		delete(m.hibernationLeases, workspace)
	}
	return state, found, release, nil
}

func (m *Manager) rejectStartDuringHibernationLocked(workspace, operation string) error {
	if m.hibernationLeases[workspace] == 0 {
		return nil
	}
	return fmt.Errorf("ssm: %s rejected for workspace %q while hibernation owns turn admission", operation, workspace)
}
