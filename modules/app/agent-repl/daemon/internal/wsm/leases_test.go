package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestAcquireLeaseRoundTripsThePolicyMetadata(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)

	// Assert
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	got, held, err := s.Lease(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if !held {
		t.Fatalf("held = false after AcquireLease")
	}
	if got.ID != lease.ID || got.Holder != HolderMerge || got.Policy != PolicyRefuse {
		t.Fatalf("lease = %+v, want %+v", got, lease)
	}
}

func TestAcquireLeaseIsExclusive(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	_, err = s.AcquireLease(context.Background(), ws.ID, HolderDrain, PolicyHold)

	// Assert
	var refusal *LeaseHeldError
	if !errors.As(err, &refusal) {
		t.Fatalf("AcquireLease = %v, want a *LeaseHeldError", err)
	}
	if refusal.Lease != first.ID || refusal.Holder != HolderMerge || refusal.Policy != PolicyRefuse {
		t.Fatalf("refusal = %+v, want the standing holder", refusal)
	}
}

// TestAHeldLeaseRefusalIsRecordedAtDebug: the arbitration refusing a second
// holder is the store ANSWERING, and every caller states what it means at its
// own level. It was recorded at ERROR beside every ordinary hold and drain.
func TestAHeldLeaseRefusalIsRecordedAtDebug(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	_, _ = s.AcquireLease(context.Background(), ws.ID, HolderDrain, PolicyHold)

	// Assert
	if loggedOperation(log, "daemon.wsm.acquire_lease", "error") {
		t.Fatalf("a held-lease refusal was recorded at error: %v", log.Records())
	}
	if !loggedOperation(log, "daemon.wsm.acquire_lease", "debug") {
		t.Fatalf("the held-lease refusal was not recorded at debug: %v", log.Records())
	}
}

func TestAcquireLeaseSucceedsAfterRelease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	first, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	if err := s.ReleaseLease(context.Background(), first.ID); err != nil {
		t.Fatalf("ReleaseLease: %v", err)
	}

	// Act
	second, err := s.AcquireLease(context.Background(), ws.ID, HolderDrain, PolicyHold)

	// Assert
	if err != nil {
		t.Fatalf("AcquireLease after release: %v", err)
	}
	if second.ID == first.ID {
		t.Fatalf("the second acquisition reused lease %q", first.ID)
	}
}

func TestAcquireLeaseRefusesAnUndeclaredHolder(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.AcquireLease(context.Background(), ws.ID, LeaseHolder(99), PolicyRefuse)

	// Assert
	if err == nil {
		t.Fatalf("AcquireLease with an undeclared holder succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.acquire_lease", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestAcquireLeaseRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.AcquireLease(context.Background(), WorkspaceID("absent"), HolderMerge, PolicyRefuse)

	// Assert
	if err == nil {
		t.Fatalf("AcquireLease on an unregistered workspace succeeded")
	}
}

func TestLeaseReportsNoneWhenFree(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, held, err := s.Lease(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if held {
		t.Fatalf("held = true on a free workspace")
	}
}

func TestReleaseLeaseRefusesAnUnheldLease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.ReleaseLease(context.Background(), LeaseID("absent"))

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("ReleaseLease = %v, want ErrNotFound", err)
	}
}

func TestSetLeasePolicyMovesAMergeFromRefusingToParked(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	if err := s.SetLeasePolicy(context.Background(), lease.ID, PolicyParked); err != nil {
		t.Fatalf("SetLeasePolicy: %v", err)
	}

	// Assert
	got, _, err := s.Lease(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if got.Policy != PolicyParked {
		t.Fatalf("policy = %v, want %v", got.Policy, PolicyParked)
	}
}

func TestSetLeasePolicyRefusesAnUndeclaredPolicy(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	err = s.SetLeasePolicy(context.Background(), lease.ID, LeasePolicy(99))

	// Assert
	if err == nil {
		t.Fatalf("SetLeasePolicy with an undeclared policy succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.set_lease_policy", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestSetLeasePolicyRefusesAnUnheldLease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.SetLeasePolicy(context.Background(), LeaseID("absent"), PolicyHold)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetLeasePolicy = %v, want ErrNotFound", err)
	}
}

func TestLeaseFailsWholeOnAnUndeclaredStoredHolder(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	corrupt(t, s, `UPDATE leases SET holder = 99 WHERE id = ?`, lease.ID)

	// Act
	_, held, err := s.Lease(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "leases" || refusal.Field != "holder" {
		t.Fatalf("Lease = %v, want a *DecodeError naming leases.holder", err)
	}
	if held {
		t.Fatalf("held = true alongside the refusal")
	}
	if !loggedOperation(log, "daemon.wsm.lease", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}
