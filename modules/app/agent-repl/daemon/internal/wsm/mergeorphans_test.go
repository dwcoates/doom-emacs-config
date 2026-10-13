package wsm

import (
	"context"
	"testing"
	"time"
)

// openRound opens a lease's ledger and records one round on it with no end.
func openRound(t *testing.T, s *store, ws WorkspaceID, lease LeaseID) {
	t.Helper()
	if err := s.OpenMergeLedger(context.Background(), ws, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
}

// putProgress records a progress document for ws under lease.
func putProgress(t *testing.T, s *store, ws WorkspaceID, lease LeaseID) {
	t.Helper()
	if err := s.PutMergeProgress(context.Background(), MergeProgress{Workspace: ws, Lease: lease, UpdatedAt: instant, Document: []byte(`{"step":"tests"}`)}); err != nil {
		t.Fatalf("PutMergeProgress: %v", err)
	}
}

// roundEnded reports whether the lease's one round reads ended, and how.
func roundEnded(t *testing.T, s *store, ws WorkspaceID) (bool, string) {
	t.Helper()
	ledgers, err := s.MergeLedger(context.Background(), ws)
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	r := ledgers[0].Intervals[0]
	return r.EndedAt != nil, r.Outcome
}

func TestTheSweepDropsAProgressRecordNoQueueEntryNames(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	putProgress(t, s, ws.ID, NewLeaseID())

	// Act
	progress, _, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert
	if err != nil || progress != 1 {
		t.Fatalf("SweepOrphanedMergeState = %d, %v; want the orphaned record dropped", progress, err)
	}
	if _, found, _ := s.MergeProgressOf(context.Background(), ws.ID); found {
		t.Fatal("the orphaned progress record is still there")
	}
}

func TestTheSweepKeepsTheProgressOfAMergeStillQueued(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	queuedWith(t, s, RepoKey(t.TempDir()), ws.ID, MergeSource{Kind: MergeSourceOwnBranch})
	putProgress(t, s, ws.ID, NewLeaseID())

	// Act
	progress, _, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert
	if err != nil || progress != 0 {
		t.Fatalf("SweepOrphanedMergeState = %d, %v; want nothing of a queued merge dropped", progress, err)
	}
}

func TestTheSweepClosesAnOpenRoundNoMergeOwnsAsAbandoned(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	openRound(t, s, ws.ID, NewLeaseID())

	// Act
	_, rounds, err := s.SweepOrphanedMergeState(context.Background(), instant.Add(time.Hour))

	// Assert
	if err != nil || rounds != 1 {
		t.Fatalf("SweepOrphanedMergeState = %d, %v; want the orphaned round closed", rounds, err)
	}
	if ended, outcome := roundEnded(t, s, ws.ID); !ended || outcome != "abandoned" {
		t.Fatalf("round ended=%v outcome=%q, want ended as abandoned", ended, outcome)
	}
}

func TestTheSweepLeavesTheOpenRoundOfAMergeWithProgressOpen(t *testing.T) {
	// Arrange: a suspended merge -- its entry, its progress and its open round.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	queuedWith(t, s, RepoKey(t.TempDir()), ws.ID, MergeSource{Kind: MergeSourceOwnBranch})
	putProgress(t, s, ws.ID, lease)
	openRound(t, s, ws.ID, lease)

	// Act
	_, rounds, err := s.SweepOrphanedMergeState(context.Background(), instant.Add(time.Hour))

	// Assert
	if err != nil || rounds != 0 {
		t.Fatalf("SweepOrphanedMergeState = %d, %v; want a live merge's round left open", rounds, err)
	}
	if ended, _ := roundEnded(t, s, ws.ID); ended {
		t.Fatal("a live merge's open round was closed")
	}
}
