package wsm

import (
	"context"
	"testing"
)

// admitted puts ws's merge in repo's line and admits it.
func admitted(t *testing.T, s *store, repo RepoKey, ws WorkspaceID) {
	t.Helper()
	queued(t, s, repo, ws, instant)
	if err := s.AdmitMerge(context.Background(), repo, ws); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
}

// owed reads whether repo owes its queue_drained scripts, failing the test on
// a read error.
func owed(t *testing.T, s *store, repo RepoKey) bool {
	t.Helper()
	got, err := s.QueueDrainOwed(context.Background(), repo)
	if err != nil {
		t.Fatalf("QueueDrainOwed: %v", err)
	}
	return got
}

func TestARepositoryThatNeverAdmittedAMergeOwesNoDrain(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got := owed(t, s, RepoKey(t.TempDir()))

	// Assert
	if got {
		t.Fatal("QueueDrainOwed = true, want false for a repository with no row")
	}
}

func TestAMergeOnlyQueuedOwesNoDrain(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	queued(t, s, repo, testWorkspace(t, s).ID, instant)

	// Act
	got := owed(t, s, repo)

	// Assert
	if got {
		t.Fatal("QueueDrainOwed = true, want false: only an admission owes the drain")
	}
}

func TestAdmittingAMergeOwesItsRepositorysDrain(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	admitted(t, s, repo, testWorkspace(t, s).ID)

	// Assert
	if !owed(t, s, repo) {
		t.Fatal("QueueDrainOwed = false, want true once a merge is admitted")
	}
}

func TestAdmittingAMergeKeepsItsRepositorysPause(t *testing.T) {
	// Arrange: the drain mark shares the pause's row.
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s)
	queued(t, s, repo, ws.ID, instant)
	if err := s.SetMergeQueuePaused(context.Background(), repo, &MergeQueuePause{By: MergeQueuePauser{Kind: MergeQueuePauserUser}, At: instant}); err != nil {
		t.Fatalf("SetMergeQueuePaused: %v", err)
	}

	// Act
	if err := s.AdmitMerge(context.Background(), repo, ws.ID); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Assert
	paused, err := s.MergeQueuePaused(context.Background(), repo)
	if err != nil || !paused {
		t.Fatalf("MergeQueuePaused = %v, %v; want the pause kept", paused, err)
	}
}

func TestARefusedAdmissionOwesNoDrain(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	err := s.AdmitMerge(context.Background(), repo, testWorkspace(t, s).ID)

	// Assert
	if err == nil {
		t.Fatal("AdmitMerge of an entry not in the queue succeeded, want a refusal")
	}
	if owed(t, s, repo) {
		t.Fatal("QueueDrainOwed = true after a refused admission, want the write rolled back whole")
	}
}

func TestClearingTheDrainOwesNothingUntilTheNextAdmission(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	admitted(t, s, repo, testWorkspace(t, s).ID)

	// Act
	if err := s.ClearQueueDrainOwed(context.Background(), repo); err != nil {
		t.Fatalf("ClearQueueDrainOwed: %v", err)
	}
	cleared := owed(t, s, repo)
	admitted(t, s, repo, testWorkspace(t, s).ID)

	// Assert
	if cleared {
		t.Fatal("QueueDrainOwed = true right after the clear, want false")
	}
	if !owed(t, s, repo) {
		t.Fatal("QueueDrainOwed = false after the next admission, want true")
	}
}

func TestClearingARepositoryWithNoRowIsANoOp(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	err := s.ClearQueueDrainOwed(context.Background(), repo)

	// Assert
	if err != nil || owed(t, s, repo) {
		t.Fatalf("ClearQueueDrainOwed = %v, want a no-op owing nothing", err)
	}
}

func TestTheDrainIsOwedPerRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo, other := RepoKey(t.TempDir()), RepoKey(t.TempDir())

	// Act
	admitted(t, s, repo, testWorkspace(t, s).ID)

	// Assert
	if owed(t, s, other) {
		t.Fatal("the other repository owes its drain, want only the admitting one")
	}
}

func TestTheDrainCallsRefuseAnEmptyRepositoryKey(t *testing.T) {
	tests := []struct {
		name string
		op   string
		call func(*store) error
	}{
		{name: "the read", op: "daemon.wsm.queue_drain_owed", call: func(s *store) error {
			_, err := s.QueueDrainOwed(context.Background(), "")
			return err
		}},
		{name: "the clear", op: "daemon.wsm.clear_queue_drain_owed", call: func(s *store) error {
			return s.ClearQueueDrainOwed(context.Background(), "")
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := tt.call(s)

			// Assert
			if err == nil {
				t.Fatal("an empty repository key was accepted, want a refusal")
			}
			for _, r := range log.Records() {
				if r.Level == "error" && r.Operation == tt.op && r.Context["error"] != nil {
					return
				}
			}
			t.Fatalf("records = %+v, want an ERROR under %s carrying the error", log.Records(), tt.op)
		})
	}
}
