package wsm

import (
	"context"
	"testing"
)

// progressAt is one merge progress record of a workspace under a lease.
func progressAt(ws WorkspaceID, lease LeaseID, document string) MergeProgress {
	return MergeProgress{Workspace: ws, Lease: lease, UpdatedAt: instant, Document: []byte(document)}
}

func TestPutMergeProgressRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()

	// Act
	if err := s.PutMergeProgress(context.Background(), progressAt(ws.ID, lease, `{"step":"rebasing"}`)); err != nil {
		t.Fatalf("PutMergeProgress: %v", err)
	}
	got, found, err := s.MergeProgressOf(context.Background(), ws.ID)

	// Assert
	if err != nil || !found {
		t.Fatalf("MergeProgressOf = (%v, %v), want the record", found, err)
	}
	if got.Lease != lease || string(got.Document) != `{"step":"rebasing"}` || !got.UpdatedAt.Equal(instant) {
		t.Fatalf("record = %+v, want lease %s and the document as written", got, lease)
	}
}

func TestPutMergeProgressReplacesTheWorkspacesRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.PutMergeProgress(context.Background(), progressAt(ws.ID, lease, `{"step":"rebasing"}`)); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	if err := s.PutMergeProgress(context.Background(), progressAt(ws.ID, lease, `{"step":"tests"}`)); err != nil {
		t.Fatalf("PutMergeProgress: %v", err)
	}

	// Assert
	got, _, err := s.MergeProgressOf(context.Background(), ws.ID)
	if err != nil || string(got.Document) != `{"step":"tests"}` {
		t.Fatalf("record = %+v (%v), want the replacement", got, err)
	}
	if n := scalar[int](t, s, `SELECT count(*) FROM merge_progress`); n != 1 {
		t.Fatalf("%d records stand, want exactly 1", n)
	}
}

func TestPutMergeProgressRefusesARecordWithNoDocument(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.PutMergeProgress(context.Background(), progressAt(ws.ID, NewLeaseID(), ""))

	// Assert
	if err == nil {
		t.Fatal("PutMergeProgress stored a record with no document, want a refusal")
	}
}

func TestMergeProgressOfAnswersNoneWhenNoMergeRecorded(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, found, err := s.MergeProgressOf(context.Background(), ws.ID)

	// Assert
	if err != nil || found {
		t.Fatalf("MergeProgressOf = (%v, %v), want (false, nil)", found, err)
	}
}

func TestDropMergeProgressDeletesTheLeasesRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.PutMergeProgress(context.Background(), progressAt(ws.ID, lease, `{}`)); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	dropped, err := s.DropMergeProgress(context.Background(), lease)

	// Assert
	if err != nil || !dropped {
		t.Fatalf("DropMergeProgress = (%v, %v), want (true, nil)", dropped, err)
	}
	if _, found, _ := s.MergeProgressOf(context.Background(), ws.ID); found {
		t.Fatal("the record survived its drop")
	}
}

func TestDropMergeProgressLeavesAnotherLeasesRecord(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.PutMergeProgress(context.Background(), progressAt(ws.ID, NewLeaseID(), `{}`)); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	dropped, err := s.DropMergeProgress(context.Background(), NewLeaseID())

	// Assert
	if err != nil || dropped {
		t.Fatalf("DropMergeProgress = (%v, %v), want (false, nil)", dropped, err)
	}
	if _, found, _ := s.MergeProgressOf(context.Background(), ws.ID); !found {
		t.Fatal("another lease's drop deleted the record")
	}
}

func TestAdoptMergeLeaseMakesAPreviousProcesssLeaseThisHandles(t *testing.T) {
	// Arrange
	previous, current, ws, _ := twoProcesses(t)
	lease, err := previous.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	adopted, err := current.AdoptMergeLease(context.Background(), ws.ID, lease.ID)

	// Assert
	if err != nil || adopted.ID != lease.ID {
		t.Fatalf("AdoptMergeLease = (%+v, %v), want the lease %s", adopted, err, lease.ID)
	}
	foreign, err := current.ForeignLeases(context.Background())
	if err != nil || len(foreign) != 0 {
		t.Fatalf("ForeignLeases = (%+v, %v), want none once adopted", foreign, err)
	}
}

func TestAdoptMergeLeaseRefusesAnotherHoldersLease(t *testing.T) {
	// Arrange
	previous, current, ws, _ := twoProcesses(t)
	lease, err := previous.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	_, err = current.AdoptMergeLease(context.Background(), ws.ID, lease.ID)

	// Assert
	if err == nil {
		t.Fatal("AdoptMergeLease adopted a restart lease, want a refusal")
	}
}

func TestAdoptMergeLeaseRefusesALeaseThatIsNotTheOneHeld(t *testing.T) {
	// Arrange
	previous, current, ws, _ := twoProcesses(t)
	if _, err := previous.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act
	_, err := current.AdoptMergeLease(context.Background(), ws.ID, NewLeaseID())

	// Assert
	if err == nil {
		t.Fatal("AdoptMergeLease adopted a lease other than the one held, want a refusal")
	}
}

func TestAdoptMergeLeaseRefusesWhenNoLeaseIsHeld(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.AdoptMergeLease(context.Background(), ws.ID, NewLeaseID())

	// Assert
	if err == nil {
		t.Fatal("AdoptMergeLease adopted a lease nobody holds, want a refusal")
	}
}
