package wsm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestOpenMergeLedgerRecordsTheLease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()

	// Act
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got) != 1 || got[0].Lease != lease || got[0].Workspace != ws.ID {
		t.Fatalf("ledger = %+v, want one entry for lease %q", got, lease)
	}
}

func TestOpenMergeLedgerIsIdempotent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}

	// Act
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger again: %v", err)
	}

	// Assert
	got, err := s.MergeLedger(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("ledger has %d entries, want 1", len(got))
	}
}

func TestOpenMergeLedgerRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.OpenMergeLedger(context.Background(), WorkspaceID("absent"), NewLeaseID())

	// Assert
	if err == nil {
		t.Fatalf("OpenMergeLedger on an unregistered workspace succeeded")
	}
}

func TestRecordTabIntervalRoundTripsARound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	ended := instant.Add(time.Minute)
	interval := TabInterval{Round: 1, Kind: "conflicts", StartedAt: instant, EndedAt: &ended, Outcome: "resolved"}

	// Act
	if err := s.RecordTabInterval(context.Background(), lease, interval); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got[0].Intervals) != 1 {
		t.Fatalf("recorded %d intervals, want 1", len(got[0].Intervals))
	}
	restored := got[0].Intervals[0]
	if restored.Round != 1 || restored.Kind != "conflicts" || restored.Outcome != "resolved" {
		t.Fatalf("interval = %+v, want %+v", restored, interval)
	}
	if restored.EndedAt == nil || !restored.EndedAt.Equal(ended) {
		t.Fatalf("ended at = %v, want %v", restored.EndedAt, ended)
	}
}

func TestRecordTabIntervalKeepsRoundsInOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	for _, round := range []int{3, 1, 2} {
		if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: round, Kind: "tests", StartedAt: instant}); err != nil {
			t.Fatalf("RecordTabInterval: %v", err)
		}
	}

	// Act
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	for i, want := range []int{1, 2, 3} {
		if got[0].Intervals[i].Round != want {
			t.Fatalf("interval %d round = %d, want %d", i, got[0].Intervals[i].Round, want)
		}
	}
}

func TestRecordTabIntervalClosesARunningRound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	ended := instant.Add(time.Minute)

	// Act
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant, EndedAt: &ended, Outcome: "passed"}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}

	// Assert
	got, err := s.MergeLedger(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got[0].Intervals) != 1 {
		t.Fatalf("recorded %d intervals, want the one round closed in place", len(got[0].Intervals))
	}
	if got[0].Intervals[0].Outcome != "passed" {
		t.Fatalf("outcome = %q, want %q", got[0].Intervals[0].Outcome, "passed")
	}
}

func TestRecordTabIntervalRefusesRoundZero(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}

	// Act
	err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 0, Kind: "tests", StartedAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("RecordTabInterval with round 0 succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.record_tab_interval", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestRecordTabIntervalRefusesAnEmptyKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}

	// Act
	err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, StartedAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("RecordTabInterval with no kind succeeded")
	}
}

func TestRecordTabIntervalRefusesAnUnopenedLedger(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RecordTabInterval(context.Background(), LeaseID("absent"), TabInterval{Round: 1, Kind: "tests", StartedAt: instant})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RecordTabInterval = %v, want ErrNotFound", err)
	}
}

func TestMergeLedgerFailsWholeOnACorruptRound(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	corrupt(t, s, `UPDATE merge_tab_intervals SET round = 0 WHERE lease_id = ?`, lease)

	// Act
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "merge_tab_intervals" || refusal.Field != "round" {
		t.Fatalf("MergeLedger = %v, want a *DecodeError naming merge_tab_intervals.round", err)
	}
	if got != nil {
		t.Fatalf("loaded %d entries alongside the refusal, want none", len(got))
	}
	if !loggedOperation(log, "daemon.wsm.merge_ledger", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}

func TestForgetDeletesAWorkspacesLedger(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "merge", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}

	// Act
	if err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM merge_tab_intervals WHERE lease_id = ?`, lease); n != 0 {
		t.Fatalf("%d intervals survived the nuke, want none", n)
	}
}
