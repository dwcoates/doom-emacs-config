package ssm

import (
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func TestControllerRegistrationAndHibernationAreMutuallyExclusive(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})

	releaseRegistration, err := m.AcquireControllerRegistration("ws", "session", "generation-1")
	if err != nil {
		t.Fatalf("AcquireControllerRegistration: %v", err)
	}
	if _, _, _, err := m.AcquireHibernationLease("ws"); !errors.Is(err, ErrControllerRegistrationInProgress) {
		t.Fatalf("AcquireHibernationLease during registration = %v, want ErrControllerRegistrationInProgress", err)
	}
	releaseRegistration()

	_, _, releaseHibernation, err := m.AcquireHibernationLease("ws")
	if err != nil {
		t.Fatalf("AcquireHibernationLease: %v", err)
	}
	if _, err := m.AcquireControllerRegistration("ws", "session", "generation-2"); !errors.Is(err, ErrHibernationInProgress) {
		t.Fatalf("AcquireControllerRegistration during hibernation = %v, want ErrHibernationInProgress", err)
	}
	releaseHibernation()
}

func TestEveryConcurrentControllerRegistrationMustReleaseBeforeHibernation(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	first, err := m.AcquireControllerRegistration("ws", "session", "generation-1")
	if err != nil {
		t.Fatalf("first AcquireControllerRegistration: %v", err)
	}
	second, err := m.AcquireControllerRegistration("ws", "session", "generation-2")
	if err != nil {
		t.Fatalf("second AcquireControllerRegistration: %v", err)
	}
	first()
	if _, _, _, err := m.AcquireHibernationLease("ws"); !errors.Is(err, ErrControllerRegistrationInProgress) {
		t.Fatalf("AcquireHibernationLease with one registration left = %v, want ErrControllerRegistrationInProgress", err)
	}
	second()
	_, _, releaseHibernation, err := m.AcquireHibernationLease("ws")
	if err != nil {
		t.Fatalf("AcquireHibernationLease after both releases: %v", err)
	}
	releaseHibernation()
}

func TestOperationalEdgeReleasesItsExactControllerRegistration(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"session": "ws"})
	release, err := m.AcquireControllerRegistration("ws", "session", "generation-1")
	if err != nil {
		t.Fatalf("AcquireControllerRegistration: %v", err)
	}
	if err := m.ApplySessionConnectivity("ws", "session", "generation-1", SessionConnectivityConnecting, "test_connecting"); err != nil {
		t.Fatalf("ApplySessionConnectivity(connecting): %v", err)
	}
	if err := m.ApplySessionConnectivity("ws", "session", "generation-1", SessionConnectivityOperational, "test_operational"); err != nil {
		t.Fatalf("ApplySessionConnectivity(operational): %v", err)
	}
	m.mu.Lock()
	registrations := len(m.controllerRegistrations["ws"])
	m.mu.Unlock()
	if registrations != 0 {
		t.Fatalf("controller registrations after operational = %d, want 0", registrations)
	}
	// The owner closure remains safe after the exact operational edge released
	// the durable reservation.
	release()
}

func TestHibernationLeaseExcludesNewPromptAndTurnStarts(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	_, _, release, err := m.AcquireHibernationLease("ws")
	if err != nil {
		t.Fatalf("AcquireHibernationLease: %v", err)
	}

	start := turnClaimEvent(true, 1, "turn-after-lease")
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", start); err == nil || !strings.Contains(err.Error(), "hibernation owns turn admission") {
		t.Fatalf("ResolveTurnLifecycle during lease = %v, want refusal", err)
	}
	if err := m.MarkPromptAccepted("ws", "daemon-session", "request", PromptAdmissionUser, func(*frontendv1.WorkspaceState) {}); err == nil || !strings.Contains(err.Error(), "hibernation owns turn admission") {
		t.Fatalf("MarkPromptAccepted during lease = %v, want refusal", err)
	}

	release()
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", start); err != nil {
		t.Fatalf("ResolveTurnLifecycle after release: %v", err)
	}
}

func TestHibernationLeaseSnapshotIncludesDurableUnappliedTurnClaim(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", turnClaimEvent(true, 1, "turn-before-lease")); err != nil {
		t.Fatalf("ResolveTurnLifecycle: %v", err)
	}

	state, found, release, err := m.AcquireHibernationLease("ws")
	if err != nil {
		t.Fatalf("AcquireHibernationLease: %v", err)
	}
	defer release()
	if !found || !state.GetTurnActive() {
		t.Fatalf("lease snapshot = (%+v, %t), want active durable claim", state, found)
	}
}

func TestHibernationLeaseAdmitsExactCompletedTurnReplayWithoutStartingATurn(t *testing.T) {
	m, _, _ := openUnwiredTest(t, fakeResolver{"vendor-session": "ws"})
	start := turnClaimEvent(true, 1, "completed-turn")
	end := turnClaimEvent(false, 2, "completed-turn")
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", start); err != nil {
		t.Fatalf("Resolve start: %v", err)
	}
	if err := applyTest(m, start); err != nil {
		t.Fatalf("Apply start: %v", err)
	}
	if _, _, _, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", end); err != nil {
		t.Fatalf("Resolve end: %v", err)
	}
	if err := applyTest(m, end); err != nil {
		t.Fatalf("Apply end: %v", err)
	}

	_, _, release, err := m.AcquireHibernationLease("ws")
	if err != nil {
		t.Fatalf("AcquireHibernationLease: %v", err)
	}
	defer release()
	before, after, replayed, err := m.ResolveTurnLifecycle("ws", "daemon-session", "", start)
	if err != nil || !replayed || len(before) != 0 || len(after) != 0 {
		t.Fatalf("completed replay during lease = before:%v after:%v replayed:%t err:%v", before, after, replayed, err)
	}
	if err := applyTest(m, start); err != nil {
		t.Fatalf("Apply completed replay during lease: %v", err)
	}
}
