package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"

	"claude-repld/internal/dlog"
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

// twoProcesses opens TWO handles on one database file: the first stands for a
// previous daemon process, the second for the one that booted after it. The
// first is never closed through Close, which is what a SIGKILLed process
// looks like; its raw handle is closed at cleanup.
func twoProcesses(t *testing.T) (previous, current *store, ws Workspace, log *dlog.TestLogger) {
	t.Helper()
	path := filepath.Join(t.TempDir(), "wsm.db")
	previous = openStoreAt(t, path, dlog.NewTestLogger())
	t.Cleanup(func() { previous.db().Close() })
	ws = testWorkspace(t, previous)
	log = dlog.NewTestLogger()
	current = openStoreAt(t, path, log)
	t.Cleanup(func() { current.Close() })
	return previous, current, ws, log
}

// openStoreAt opens a handle on path with log.
func openStoreAt(t *testing.T, path string, log *dlog.TestLogger) *store {
	t.Helper()
	handle, err := Open(context.Background(), path, WithLogger(log))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	return handle.(*store)
}

func TestForeignLeasesListsALeaseAnotherProcessTook(t *testing.T) {
	// Arrange
	previous, current, ws, _ := twoProcesses(t)
	lease, err := previous.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	foreign, err := current.ForeignLeases(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ForeignLeases: %v", err)
	}
	if len(foreign) != 1 || foreign[0].ID != lease.ID || foreign[0].Workspace != ws.ID || foreign[0].Holder != HolderRestart {
		t.Fatalf("ForeignLeases = %+v, want exactly the previous process's %s", foreign, lease.ID)
	}
}

func TestForeignLeasesOmitsALeaseThisHandleTook(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	foreign, err := s.ForeignLeases(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("ForeignLeases: %v", err)
	}
	if len(foreign) != 0 {
		t.Fatalf("ForeignLeases = %+v, want none: the only lease is this handle's own", foreign)
	}
}

func TestCloseReleasesALeaseThisHandleStillOwns(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	log := dlog.NewTestLogger()
	s := openStoreAt(t, path, log)
	ws := testWorkspace(t, s)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	err := s.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	after := openStoreAt(t, path, dlog.NewTestLogger())
	t.Cleanup(func() { after.Close() })
	if _, held, err := after.Lease(context.Background(), ws.ID); err != nil || held {
		t.Fatalf("Lease after Close = (held %v, %v), want no lease left behind", held, err)
	}
	if !loggedOperation(log, "daemon.wsm.close", "info") {
		t.Fatalf("the release at close was not recorded at info: %v", log.Records())
	}
}

func TestCloseKeepsAMergeLeaseForTheNextBootsRecovery(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	s := openStoreAt(t, path, dlog.NewTestLogger())
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyRefuse)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	err = s.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	after := openStoreAt(t, path, dlog.NewTestLogger())
	t.Cleanup(func() { after.Close() })
	got, held, err := after.Lease(context.Background(), ws.ID)
	if err != nil || !held || got.ID != lease.ID {
		t.Fatalf("Lease after Close = (%+v, held %v, %v), want the merge lease %s kept", got, held, err, lease.ID)
	}
}

func TestCloseTreatsALeaseAnotherProcessReleasedAsGone(t *testing.T) {
	// Arrange: this handle took the lease, and the process it was handed to
	// (a successor that adopted the workspace) released it.
	path := filepath.Join(t.TempDir(), "wsm.db")
	log := dlog.NewTestLogger()
	s := openStoreAt(t, path, log)
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	adopter := openStoreAt(t, path, dlog.NewTestLogger())
	t.Cleanup(func() { adopter.Close() })
	if err := adopter.ReleaseLease(context.Background(), lease.ID); err != nil {
		t.Fatalf("ReleaseLease by the adopter: %v", err)
	}

	// Act
	err = s.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close = %v, want nil: a lease its adopter released is not a failure", err)
	}
	if !loggedOperation(log, "daemon.wsm.close", "debug") || loggedOperation(log, "daemon.wsm.close", "info") {
		t.Fatalf("the already-released lease was not recorded at debug alone: %v", log.Records())
	}
}

func TestCloseDoesNotReleaseALeaseThisHandleAlreadyReleased(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "wsm.db")
	log := dlog.NewTestLogger()
	s := openStoreAt(t, path, log)
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}
	if err := s.ReleaseLease(context.Background(), lease.ID); err != nil {
		t.Fatalf("ReleaseLease: %v", err)
	}

	// Act
	err = s.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	if loggedOperation(log, "daemon.wsm.close", "debug") || loggedOperation(log, "daemon.wsm.close", "info") {
		t.Fatalf("Close acted on a lease already released: %v", log.Records())
	}
}
