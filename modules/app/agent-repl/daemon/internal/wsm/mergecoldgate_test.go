package wsm

import (
	"context"
	"database/sql"
	"errors"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/sourcescan"
)

// entryOfQueue answers WS's entry in REPO's queue, failing the test when it
// has none.
func entryOfQueue(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) MergeQueueEntry {
	t.Helper()
	entries, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	for _, entry := range entries {
		if entry.Workspace == ws {
			return entry
		}
	}
	t.Fatalf("no entry of %s in %v", ws, entries)
	return MergeQueueEntry{}
}

// threeInLine queues three merges in order and answers their workspaces.
func threeInLine(t *testing.T, s *store, repo RepoKey) (WorkspaceID, WorkspaceID, WorkspaceID) {
	t.Helper()
	first, second, third := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	queued(t, s, repo, first.ID, instant)
	queued(t, s, repo, second.ID, instant.Add(time.Minute))
	queued(t, s, repo, third.ID, instant.Add(2*time.Minute))
	return first.ID, second.ID, third.ID
}

func TestSetMergeColdGatedMarksAWaitingMergeAtItsPlace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, second, third := threeInLine(t, s, repo)

	// Act
	changed, err := s.SetMergeColdGated(context.Background(), repo, first)

	// Assert
	if err != nil || !changed {
		t.Fatalf("SetMergeColdGated = %v, %v; want a change", changed, err)
	}
	if got := queueOrder(t, s, repo); !slices.Equal(got, []WorkspaceID{first, second, third}) {
		t.Fatalf("queue = %v, want the marked merge kept at its place", got)
	}
	if !entryOfQueue(t, s, repo, first).ColdGated {
		t.Fatalf("the marked merge reads as not at its cold gate")
	}
}

func TestSetMergeColdGatedAnswersNoChangeForAMarkedMerge(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, _, _ := threeInLine(t, s, repo)
	if _, err := s.SetMergeColdGated(context.Background(), repo, first); err != nil {
		t.Fatalf("SetMergeColdGated: %v", err)
	}

	// Act
	changed, err := s.SetMergeColdGated(context.Background(), repo, first)

	// Assert
	if err != nil || changed {
		t.Fatalf("SetMergeColdGated = %v, %v; want no change", changed, err)
	}
}

func TestSetMergeColdGatedRefusesAnAdmittedMergeAndMarksNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, _, _ := threeInLine(t, s, repo)
	if err := s.AdmitMerge(context.Background(), repo, first); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	_, err := s.SetMergeColdGated(context.Background(), repo, first)

	// Assert
	if err == nil {
		t.Fatalf("SetMergeColdGated of an admitted merge succeeded, want a refusal")
	}
	if entryOfQueue(t, s, repo, first).ColdGated {
		t.Fatalf("the refused mark was written")
	}
}

func TestSetMergeColdGatedRefusesAWorkspaceNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.SetMergeColdGated(context.Background(), RepoKey(t.TempDir()), ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("SetMergeColdGated = %v, want ErrNotFound", err)
	}
}

func TestAMarkedFrontWaitsAndTheMergeBehindItHeads(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, second, _ := threeInLine(t, s, repo)

	// Act
	if _, err := s.SetMergeColdGated(context.Background(), repo, first); err != nil {
		t.Fatalf("SetMergeColdGated: %v", err)
	}

	// Assert
	if got := entryOfQueue(t, s, repo, first).Slot; got != MergeSlotWaiting {
		t.Fatalf("the marked front's slot = %s, want waiting", got)
	}
	if got := entryOfQueue(t, s, repo, second).Slot; got != MergeSlotHead {
		t.Fatalf("the merge behind the marked front holds %s, want head", got)
	}
}

func TestMergeSlotsOfNeverHeadsAMarkedMerge(t *testing.T) {
	// Arrange
	rows := []slotRow{
		{state: MergeQueued, coldGated: true},
		{state: MergeAdmitted},
		{state: MergeQueued, coldGated: true},
		{state: MergeQueued},
	}

	// Act
	got, err := mergeSlotsOf(rows)

	// Assert
	if err != nil {
		t.Fatalf("mergeSlotsOf: %v", err)
	}
	want := []MergeSlot{MergeSlotWaiting, MergeSlotHead, MergeSlotWaiting, MergeSlotWaiting}
	if !slices.Equal(got, want) {
		t.Fatalf("mergeSlotsOf = %v, want %v", got, want)
	}
}

func TestClearMergeColdGatedResumesTheMergeAtItsPlace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, second, third := threeInLine(t, s, repo)
	if _, err := s.SetMergeColdGated(context.Background(), repo, first); err != nil {
		t.Fatalf("SetMergeColdGated: %v", err)
	}

	// Act
	changed, err := s.ClearMergeColdGated(context.Background(), repo, first, nil)

	// Assert
	if err != nil || !changed {
		t.Fatalf("ClearMergeColdGated = %v, %v; want a change", changed, err)
	}
	if got := queueOrder(t, s, repo); !slices.Equal(got, []WorkspaceID{first, second, third}) {
		t.Fatalf("queue = %v, want the merge resumed at its own place", got)
	}
	entry := entryOfQueue(t, s, repo, first)
	if entry.ColdGated || entry.Slot != MergeSlotHead {
		t.Fatalf("the resumed merge reads cold gated %v in slot %s, want unmarked at the head", entry.ColdGated, entry.Slot)
	}
}

func TestClearMergeColdGatedPutsAnOvertakenMergeBehindTheMergesWorkedOn(t *testing.T) {
	// Arrange: the marked front was overtaken by the second, now worked on.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	first, second, third := threeInLine(t, s, repo)
	if _, err := s.SetMergeColdGated(ctx, repo, first); err != nil {
		t.Fatalf("SetMergeColdGated: %v", err)
	}
	if err := s.AdmitMerge(ctx, repo, second); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	if _, err := s.ClearMergeColdGated(ctx, repo, first, []WorkspaceID{second}); err != nil {
		t.Fatalf("ClearMergeColdGated: %v", err)
	}

	// Assert
	if got := queueOrder(t, s, repo); !slices.Equal(got, []WorkspaceID{second, first, third}) {
		t.Fatalf("queue = %v, want the merge worked on, the resumed merge, then the one waiting behind it", got)
	}
}

func TestClearMergeColdGatedLeavesAMergeBehindTheMergesWorkedOnWhereItStands(t *testing.T) {
	// Arrange: the third is marked behind the merge worked on.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	first, second, third := threeInLine(t, s, repo)
	if _, err := s.SetMergeColdGated(ctx, repo, third); err != nil {
		t.Fatalf("SetMergeColdGated: %v", err)
	}
	if err := s.AdmitMerge(ctx, repo, first); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	if _, err := s.ClearMergeColdGated(ctx, repo, third, []WorkspaceID{first}); err != nil {
		t.Fatalf("ClearMergeColdGated: %v", err)
	}

	// Assert
	if got := queueOrder(t, s, repo); !slices.Equal(got, []WorkspaceID{first, second, third}) {
		t.Fatalf("queue = %v, want the order unchanged", got)
	}
}

func TestClearMergeColdGatedAnswersNoChangeForAnUnmarkedMerge(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, second, _ := threeInLine(t, s, repo)
	if err := s.AdmitMerge(context.Background(), repo, second); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	before := queueOrder(t, s, repo)

	// Act
	changed, err := s.ClearMergeColdGated(context.Background(), repo, first, []WorkspaceID{second})

	// Assert
	if err != nil || changed {
		t.Fatalf("ClearMergeColdGated = %v, %v; want no change", changed, err)
	}
	if got := queueOrder(t, s, repo); !slices.Equal(got, before) {
		t.Fatalf("queue = %v, want an unmarked merge never moved from %v", got, before)
	}
}

func TestClearMergeColdGatedRefusesAWorkspaceNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, err := s.ClearMergeColdGated(context.Background(), RepoKey(t.TempDir()), ws.ID, nil)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("ClearMergeColdGated = %v, want ErrNotFound", err)
	}
}

func TestRequeueColdGatedMergeTakesTheLeaseAndTheProgressAndKeepsThePlace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	behind := testWorkspace(t, s)
	lease := admittedWithLease(t, s, repo, ws.ID)
	queued(t, s, repo, behind.ID, instant.Add(time.Minute))
	ledger := NewLeaseID()

	// Act
	err := s.RequeueColdGatedMerge(ctx, repo, ws.ID, lease.ID, ledger)

	// Assert
	if err != nil {
		t.Fatalf("RequeueColdGatedMerge: %v", err)
	}
	if got := queueOrder(t, s, repo); !slices.Equal(got, []WorkspaceID{ws.ID, behind.ID}) {
		t.Fatalf("queue = %v, want the merge back at its own place", got)
	}
	entry := entryOfQueue(t, s, repo, ws.ID)
	if entry.State != MergeQueued || !entry.ColdGated || entry.Ledger != ledger {
		t.Fatalf("entry = %s, cold gated %v, ledger %s; want waiting, marked, under %s", entry.State, entry.ColdGated, entry.Ledger, ledger)
	}
	_, held, _ := s.Lease(ctx, ws.ID)
	_, progress, _ := s.MergeProgressOf(ctx, ws.ID)
	if held || progress {
		t.Fatalf("lease held %v, progress %v; want both gone with the requeue", held, progress)
	}
	if got := entryOfQueue(t, s, repo, behind.ID).Slot; got != MergeSlotHead {
		t.Fatalf("the merge behind holds %s, want head", got)
	}
}

func TestRequeueColdGatedMergeWithNoLeaseRequeuesTheEntryAlone(t *testing.T) {
	// Arrange: the run's teardown already released its lease.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	first, _, _ := threeInLine(t, s, repo)
	if err := s.AdmitMerge(ctx, repo, first); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	err := s.RequeueColdGatedMerge(ctx, repo, first, "", NewLeaseID())

	// Assert
	if err != nil {
		t.Fatalf("RequeueColdGatedMerge: %v", err)
	}
	if entry := entryOfQueue(t, s, repo, first); entry.State != MergeQueued || !entry.ColdGated {
		t.Fatalf("entry = %s, cold gated %v; want waiting and marked", entry.State, entry.ColdGated)
	}
}

func TestRequeueColdGatedMergeRefusesAWaitingMergeAndChangesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first, _, _ := threeInLine(t, s, repo)

	// Act
	err := s.RequeueColdGatedMerge(context.Background(), repo, first, "", NewLeaseID())

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RequeueColdGatedMerge = %v, want the refusal", err)
	}
	if entryOfQueue(t, s, repo, first).ColdGated {
		t.Fatalf("the refused requeue marked the merge")
	}
}

func TestRequeueColdGatedMergeRefusesALeaseThatIsNotTheWorkspacesAndKeepsTheEntryAdmitted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	lease := admittedWithLease(t, s, repo, ws.ID)

	// Act
	err := s.RequeueColdGatedMerge(ctx, repo, ws.ID, NewLeaseID(), NewLeaseID())

	// Assert
	if err == nil {
		t.Fatalf("RequeueColdGatedMerge with a stranger's lease succeeded, want a refusal")
	}
	if entry := entryOfQueue(t, s, repo, ws.ID); entry.State != MergeAdmitted || entry.ColdGated {
		t.Fatalf("entry = %s, cold gated %v; want the refusal to leave it admitted and unmarked", entry.State, entry.ColdGated)
	}
	if got, held, _ := s.Lease(ctx, ws.ID); !held || got.ID != lease.ID {
		t.Fatalf("lease = %v held %v, want the merge's own lease kept", got.ID, held)
	}
}

func TestRequeueColdGatedMergeRefusesAMissingLedgerAndLogsIt(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	first, _, _ := threeInLine(t, s, repo)
	if err := s.AdmitMerge(ctx, repo, first); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act
	err := s.RequeueColdGatedMerge(ctx, repo, first, "", "")

	// Assert
	if err == nil {
		t.Fatalf("RequeueColdGatedMerge with no ledger succeeded, want a refusal")
	}
	if entry := entryOfQueue(t, s, repo, first); entry.State != MergeAdmitted {
		t.Fatalf("entry = %s, want the refusal to leave it admitted", entry.State)
	}
	if !loggedOperation(log, "daemon.wsm.requeue_cold_gated_merge", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestDropMergeLeaseDeletesTheLeaseAndItsProgress(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	lease := admittedWithLease(t, s, repo, ws.ID)

	// Act
	err := s.write(ctx, "test.drop_merge_lease", nil, func(ctx context.Context, tx *sql.Tx) error {
		return dropMergeLease(ctx, tx, ws.ID, lease.ID)
	})

	// Assert
	if err != nil {
		t.Fatalf("dropMergeLease: %v", err)
	}
	_, held, _ := s.Lease(ctx, ws.ID)
	_, progress, _ := s.MergeProgressOf(ctx, ws.ID)
	if held || progress {
		t.Fatalf("lease held %v, progress %v; want both gone", held, progress)
	}
}

func TestDropMergeLeaseRefusesALeaseThatIsNotTheWorkspacesMergeLease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	lease := admittedWithLease(t, s, repo, ws.ID)

	// Act
	err := s.write(ctx, "test.drop_merge_lease", nil, func(ctx context.Context, tx *sql.Tx) error {
		return dropMergeLease(ctx, tx, ws.ID, NewLeaseID())
	})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("dropMergeLease = %v, want ErrNotFound", err)
	}
	if got, held, _ := s.Lease(ctx, ws.ID); !held || got.ID != lease.ID {
		t.Fatalf("lease = %v held %v, want the merge's own lease kept", got.ID, held)
	}
	if _, progress, _ := s.MergeProgressOf(ctx, ws.ID); !progress {
		t.Fatal("the refused drop deleted the progress record")
	}
}

func TestOnlyDropMergeLeaseDeletesAMergeLease(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "DELETE FROM leases WHERE id = ? AND workspace_id = ? AND holder = ?")

	// Assert
	if !slices.Equal(sites, []string{"mergequeue.go"}) {
		t.Fatalf("a merge lease is deleted at %v, want only dropMergeLease in mergequeue.go", sites)
	}
}
