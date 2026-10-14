package wsm

import (
	"context"
	"errors"
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
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert
	if err != nil || swept.ProgressRecords != 1 {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v; want the orphaned record dropped", swept, err)
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
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert
	if err != nil || swept.ProgressRecords != 0 {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v; want nothing of a queued merge dropped", swept, err)
	}
}

func TestTheSweepClosesAnOpenRoundNoMergeOwnsAsAbandoned(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	openRound(t, s, ws.ID, NewLeaseID())

	// Act
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant.Add(time.Hour))

	// Assert
	if err != nil || swept.RoundsClosed != 1 {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v; want the orphaned round closed", swept, err)
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
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant.Add(time.Hour))

	// Assert
	if err != nil || swept.RoundsClosed != 0 {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v; want a live merge's round left open", swept, err)
	}
	if ended, _ := roundEnded(t, s, ws.ID); ended {
		t.Fatal("a live merge's open round was closed")
	}
}

func TestTheSweepReleasesAMergeLeaseNoQueueEntryNames(t *testing.T) {
	// Arrange: the lease an eviction left standing over no entry.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert.
	if err != nil || len(swept.ReleasedLeases) != 1 || swept.ReleasedLeases[0].ID != lease.ID || swept.ReleasedLeases[0].Workspace != ws.ID {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v; want the orphaned lease released, named with its workspace", swept, err)
	}
	if _, held, _ := s.Lease(context.Background(), ws.ID); held {
		t.Fatal("the orphaned merge lease still stands")
	}
	if err := s.PutHeldPrompt(context.Background(), mergeHeld(ws.ID)); !errors.Is(err, ErrMergeLeaseGone) {
		t.Fatalf("PutHeldPrompt after the sweep = %v, want the plain no-lease answer rather than the broken invariant", err)
	}
}

func TestTheSweepKeepsTheMergeLeaseOfAMergeStillQueued(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queuedWith(t, s, repo, ws.ID, ownBranch)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderMerge, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert.
	if _, held, _ := s.Lease(context.Background(), ws.ID); err != nil || len(swept.ReleasedLeases) != 0 || !held {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v (held %v); want a queued merge's lease kept", swept, err, held)
	}
}

func TestTheSweepKeepsALeaseThatIsNoMerges(t *testing.T) {
	// Arrange: a restart's lease, which no queue entry ever names.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if _, err := s.AcquireLease(context.Background(), ws.ID, HolderRestart, PolicyHold); err != nil {
		t.Fatalf("AcquireLease: %v", err)
	}

	// Act.
	swept, err := s.SweepOrphanedMergeState(context.Background(), instant)

	// Assert.
	if _, held, _ := s.Lease(context.Background(), ws.ID); err != nil || len(swept.ReleasedLeases) != 0 || !held {
		t.Fatalf("SweepOrphanedMergeState = %+v, %v (held %v); want the restart's lease kept", swept, err, held)
	}
}
