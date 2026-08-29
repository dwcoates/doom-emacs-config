package wsm

import (
	"context"
	"errors"
	"path/filepath"
	"testing"
	"time"
)

func TestEnqueueMergeNumbersPositionsInOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)

	// Act
	firstPosition, err := s.EnqueueMerge(context.Background(), repo, first.ID, instant)
	if err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}
	secondPosition, err := s.EnqueueMerge(context.Background(), repo, second.ID, instant.Add(time.Minute))
	if err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Assert
	if firstPosition != 1 || secondPosition != 2 {
		t.Fatalf("positions = %d and %d, want 1 and 2", firstPosition, secondPosition)
	}
}

func TestEnqueueMergeRefusesADuplicate(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), repo, ws.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	_, err := s.EnqueueMerge(context.Background(), repo, ws.ID, instant)

	// Assert
	var refusal *MergeQueuedError
	if !errors.As(err, &refusal) {
		t.Fatalf("EnqueueMerge = %v, want a *MergeQueuedError", err)
	}
	if refusal.Position != 1 || refusal.State != MergeQueued {
		t.Fatalf("refusal = %+v, want the place it already holds", refusal)
	}
	if !loggedOperation(log, "daemon.wsm.enqueue_merge", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestEnqueueMergeKeysOneQueuePerRepoSpelling(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	base := t.TempDir()
	ws := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), RepoKey(base), ws.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	_, err := s.EnqueueMerge(context.Background(), RepoKey(base+string(filepath.Separator)), ws.ID, instant)

	// Assert
	var refusal *MergeQueuedError
	if !errors.As(err, &refusal) {
		t.Fatalf("EnqueueMerge with a second spelling = %v, want the duplicate refusal", err)
	}
}

func TestEnqueueMergeRefusesAnUnregisteredWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, err := s.EnqueueMerge(context.Background(), RepoKey(t.TempDir()), WorkspaceID("absent"), instant)

	// Assert
	if err == nil {
		t.Fatalf("EnqueueMerge for an unregistered workspace succeeded")
	}
}

func TestAdmitMergeMarksTheRunningEntry(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), repo, ws.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	if err := s.AdmitMerge(context.Background(), repo, ws.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 1 || got[0].State != MergeAdmitted {
		t.Fatalf("queue = %+v, want one admitted entry", got)
	}
}

func TestAdmitMergeRefusesAnEntryNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.AdmitMerge(context.Background(), RepoKey(t.TempDir()), ws.ID)

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("AdmitMerge = %v, want ErrNotFound", err)
	}
}

func TestRemoveMergeQueueEntryRenumbersTheRest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first := testWorkspace(t, s)
	second := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), repo, first.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}
	if _, err := s.EnqueueMerge(context.Background(), repo, second.ID, instant.Add(time.Minute)); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, first.ID, "evicted"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 1 || got[0].Workspace != second.ID || got[0].Position != 1 {
		t.Fatalf("queue = %+v, want the survivor at position 1", got)
	}
}

func TestRemoveMergeQueueEntryLogsTheCause(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), repo, ws.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	if err := s.RemoveMergeQueueEntry(context.Background(), repo, ws.ID, "dequeued by the user"); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}

	// Assert
	for _, record := range log.Records() {
		if record.Operation == "daemon.wsm.remove_merge_queue_entry" && record.Context["cause"] == "dequeued by the user" {
			return
		}
	}
	t.Fatalf("the removal's cause was not logged: %v", log.Records())
}

func TestRemoveMergeQueueEntryRefusesAnEntryNotInTheQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RemoveMergeQueueEntry(context.Background(), RepoKey(t.TempDir()), ws.ID, "gone")

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RemoveMergeQueueEntry = %v, want ErrNotFound", err)
	}
}

func TestMergeQueueReportsAnEmptyQueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.MergeQueue(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("queue = %+v, want none", got)
	}
}

func TestAllMergeQueuesListsEveryRepoForTheBootReenqueue(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	firstRepo := RepoKey(t.TempDir())
	secondRepo := RepoKey(t.TempDir())
	firstWS := testWorkspace(t, s)
	secondWS := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), firstRepo, firstWS.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}
	if _, err := s.EnqueueMerge(context.Background(), secondRepo, secondWS.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	got, err := s.AllMergeQueues(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("AllMergeQueues: %v", err)
	}
	if len(got) != 2 {
		t.Fatalf("loaded %d queues, want 2", len(got))
	}
	for repo, entries := range got {
		if len(entries) != 1 || entries[0].Position != 1 {
			t.Fatalf("queue %q = %+v, want one entry at position 1", repo, entries)
		}
	}
}

func TestAllMergeQueuesFailsWholeOnAnUndeclaredState(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), repo, ws.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}
	corrupt(t, s, `UPDATE merge_queue SET state = 99 WHERE workspace_id = ?`, ws.ID)

	// Act
	got, err := s.AllMergeQueues(context.Background())

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "merge_queue" || refusal.Field != "state" {
		t.Fatalf("AllMergeQueues = %v, want a *DecodeError naming merge_queue.state", err)
	}
	if got != nil {
		t.Fatalf("loaded %d queues alongside the refusal, want none", len(got))
	}
	if !loggedOperation(log, "daemon.wsm.all_merge_queues", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}

func TestMergeQueuePausedRoundTrips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, true); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	paused, err := s.MergeQueuePaused(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if !paused {
		t.Fatalf("paused = false after SetMergeQueuePaused(true)")
	}
}

func TestMergeQueuePausedResumes(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetMergeQueuePaused(context.Background(), repo, true); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if err := s.SetMergeQueuePaused(context.Background(), repo, false); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}
	paused, err := s.MergeQueuePaused(context.Background(), repo)

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if paused {
		t.Fatalf("paused = true after resuming")
	}
}

func TestMergeQueuePausedIsFalseForAnUntouchedRepo(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	paused, err := s.MergeQueuePaused(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil {
		t.Fatalf("MergeQueuePaused: %v", err)
	}
	if paused {
		t.Fatalf("paused = true for a repo that was never paused")
	}
}

func TestForgetDeletesAWorkspacesQueueEntry(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	if _, err := s.EnqueueMerge(context.Background(), repo, ws.ID, instant); err != nil {
		t.Fatalf("EnqueueMerge: %v", err)
	}

	// Act
	if err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	got, err := s.MergeQueue(context.Background(), repo)
	if err != nil {
		t.Fatalf("MergeQueue: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("queue = %+v after the nuke, want none", got)
	}
}
