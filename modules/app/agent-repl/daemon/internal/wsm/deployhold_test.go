package wsm

import (
	"context"
	"strings"
	"testing"
	"time"
)

func TestARepositoryWithNoHoldIsNotHeld(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	hold, err := s.DeployHoldOf(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil || hold != (DeployHold{}) {
		t.Fatalf("DeployHoldOf = %+v, %v; want not held", hold, err)
	}
}

func TestSetDeployHoldHoldsAndKeepsTheOriginalStart(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	first := time.Unix(100, 0)

	// Act
	if err := s.SetDeployHold(context.Background(), repo, true, first); err != nil {
		t.Fatal(err)
	}
	if err := s.SetDeployHold(context.Background(), repo, true, time.Unix(200, 0)); err != nil {
		t.Fatal(err)
	}
	hold, err := s.DeployHoldOf(context.Background(), repo)

	// Assert
	if err != nil || !hold.Held || !hold.Since.Equal(first) {
		t.Fatalf("DeployHoldOf = %+v, %v; want held since %v", hold, err, first)
	}
}

func TestAHoldIsPerRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	held, other := RepoKey(t.TempDir()), RepoKey(t.TempDir())
	if err := s.SetDeployHold(context.Background(), held, true, time.Unix(100, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	hold, err := s.DeployHoldOf(context.Background(), other)

	// Assert
	if err != nil || hold.Held {
		t.Fatalf("the other repository's hold = %+v, %v; want not held", hold, err)
	}
}

func TestRecordDeploySkipCountsTheSkipsOfAHeldRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetDeployHold(context.Background(), repo, true, time.Unix(100, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	for i := 0; i < 2; i++ {
		if err := s.RecordDeploySkip(context.Background(), repo); err != nil {
			t.Fatalf("RecordDeploySkip: %v", err)
		}
	}
	hold, err := s.DeployHoldOf(context.Background(), repo)

	// Assert
	if err != nil || hold.Skipped != 2 {
		t.Fatalf("DeployHoldOf = %+v, %v; want 2 skips", hold, err)
	}
}

func TestRecordDeploySkipRefusesARepositoryThatIsNotHeld(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	err := s.RecordDeploySkip(context.Background(), repo)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "not held") {
		t.Fatalf("RecordDeploySkip = %v, want the unheld repository refused", err)
	}
}

func TestReleasingAHoldClearsItsSkips(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetDeployHold(context.Background(), repo, true, time.Unix(100, 0)); err != nil {
		t.Fatal(err)
	}
	if err := s.RecordDeploySkip(context.Background(), repo); err != nil {
		t.Fatal(err)
	}

	// Act
	if err := s.SetDeployHold(context.Background(), repo, false, time.Unix(300, 0)); err != nil {
		t.Fatal(err)
	}
	hold, err := s.DeployHoldOf(context.Background(), repo)

	// Assert
	if err != nil || hold != (DeployHold{}) {
		t.Fatalf("DeployHoldOf after the release = %+v, %v; want not held and no skips", hold, err)
	}
}

func TestReleasingKeepsTheRepositorysQueuePause(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if err := s.SetMergeQueuePaused(context.Background(), repo, true); err != nil {
		t.Fatal(err)
	}
	if err := s.SetDeployHold(context.Background(), repo, true, time.Unix(100, 0)); err != nil {
		t.Fatal(err)
	}

	// Act
	if err := s.SetDeployHold(context.Background(), repo, false, time.Unix(300, 0)); err != nil {
		t.Fatal(err)
	}
	paused, err := s.MergeQueuePaused(context.Background(), repo)

	// Assert
	if err != nil || !paused {
		t.Fatalf("MergeQueuePaused = %v, %v; the hold and the pause are separate switches", paused, err)
	}
}

func TestTheMigrationCarriesADaemonWideHoldOntoEveryRepository(t *testing.T) {
	tests := []struct {
		name string
		held bool
	}{
		{name: "a hold standing at the upgrade", held: true},
		{name: "no hold standing", held: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange -- a layout-66 file with two repositories, their dirs
			// stored canonical as the registry stores them.
			path := fixtureAt(t, 66)
			a, err := normalizeDir(t.TempDir())
			if err != nil {
				t.Fatal(err)
			}
			b, err := normalizeDir(t.TempDir())
			if err != nil {
				t.Fatal(err)
			}
			execRaw(t, path, `INSERT INTO repositories (id, dir, name, default_branch) VALUES ('r-a', ?, 'a', 'master'), ('r-b', ?, 'b', 'master')`, a, b)
			if tt.held {
				execRaw(t, path, `INSERT INTO merge_rollout_hold (id, held_at) VALUES (1, ?)`, nanos(time.Unix(500, 0)))
			}

			// Act
			handle, err := Open(context.Background(), path, WithUnsyncedWrites())
			if err != nil {
				t.Fatalf("Open on a layout-66 database: %v", err)
			}
			defer handle.Close()

			// Assert
			for _, repo := range []string{a, b} {
				hold, err := handle.DeployHoldOf(context.Background(), RepoKey(repo))
				if err != nil || hold.Held != tt.held {
					t.Fatalf("DeployHoldOf(%s) = %+v, %v; want held %v", repo, hold, err, tt.held)
				}
				if tt.held && !hold.Since.Equal(time.Unix(500, 0)) {
					t.Fatalf("DeployHoldOf(%s).Since = %v, want the daemon-wide hold's start", repo, hold.Since)
				}
			}
		})
	}
}
