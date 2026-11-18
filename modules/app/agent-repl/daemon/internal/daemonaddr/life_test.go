package daemonaddr

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestHoldLifeHoldsOneInstanceOnce(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.addr")
	first, err := HoldLife(path, "a")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}
	defer first.Release()

	// Act
	_, err = HoldLife(path, "a")

	// Assert
	if !errors.Is(err, ErrClaimed) {
		t.Fatalf("a second HoldLife of one instance = %v, want ErrClaimed", err)
	}
}

func TestHoldLifeRefusesANameThatIsNoInstance(t *testing.T) {
	tests := []string{"", " ", "a/b"}
	for _, name := range tests {
		t.Run(name, func(t *testing.T) {
			// Act
			_, err := HoldLife(filepath.Join(t.TempDir(), "daemon.addr"), name)

			// Assert
			if err == nil {
				t.Fatalf("HoldLife(%q) = nil, want a refusal", name)
			}
		})
	}
}

func TestOtherLivesListsEveryDaemonButItself(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.addr")
	for _, instance := range []string{"b", "a", "self"} {
		life, err := HoldLife(path, instance)
		if err != nil {
			t.Fatalf("HoldLife(%s): %v", instance, err)
		}
		defer life.Release()
	}

	// Act
	others, err := OtherLives(path, "self")

	// Assert
	if err != nil || !slices.Equal(others, []string{"a", "b"}) {
		t.Fatalf("OtherLives = (%v, %v), want [a b]", others, err)
	}
}

func TestLiveOthersAnswersOnlyTheDaemonsStillRunning(t *testing.T) {
	// Arrange: one older daemon running, and one that died leaving its file.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	running, err := HoldLife(path, "running")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}
	defer running.Release()
	if err := os.WriteFile(LifePath(path, "crashed"), nil, 0o644); err != nil {
		t.Fatalf("a crashed daemon's leftover lock: %v", err)
	}

	// Act
	live, err := LiveOthers(path, "self")

	// Assert: the leftover is removed, never answered.
	if err != nil || !slices.Equal(live, []string{"running"}) {
		t.Fatalf("LiveOthers = (%v, %v), want [running]", live, err)
	}
	if _, err := os.Stat(LifePath(path, "crashed")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the leftover lock's file = %v, want it removed", err)
	}
}

func TestAwaitGoneReturnsOnceTheDaemonEnded(t *testing.T) {
	// Arrange: an older daemon that has ended since it was listed.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	older, err := HoldLife(path, "older")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}
	if err := older.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Act
	err = AwaitGone(context.Background(), path, []string{"older"})

	// Assert
	if err != nil {
		t.Fatalf("AwaitGone = %v, want it gone", err)
	}
	if _, err := os.Stat(LifePath(path, "older")); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("the freed lock's file = %v, want it removed", err)
	}
}

func TestAwaitGoneWaitsForADaemonStillRunning(t *testing.T) {
	// Arrange: an older daemon still holds its life lock.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	older, err := HoldLife(path, "older")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	err = AwaitGone(ctx, path, []string{"older"})

	// Assert: it is still waiting when its caller leaves.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("AwaitGone = %v, want the caller's cancellation while the older daemon runs", err)
	}
	if err := older.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}
}

func TestAliveAnswersWhetherTheInstanceRuns(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, path string)
		want    bool
	}{
		{name: "a daemon holding its life lock runs", want: true, arrange: func(t *testing.T, path string) {
			life, err := HoldLife(path, "elder")
			if err != nil {
				t.Fatalf("HoldLife: %v", err)
			}
			t.Cleanup(func() { life.Release() })
		}},
		{name: "a daemon that released its lock does not", arrange: func(t *testing.T, path string) {
			life, err := HoldLife(path, "elder")
			if err != nil {
				t.Fatalf("HoldLife: %v", err)
			}
			if err := life.Release(); err != nil {
				t.Fatalf("Release: %v", err)
			}
		}},
		{name: "a lock file nobody holds is a daemon that died", arrange: func(t *testing.T, path string) {
			if err := os.MkdirAll(filepath.Dir(LifePath(path, "elder")), 0o755); err != nil {
				t.Fatal(err)
			}
			if err := os.WriteFile(LifePath(path, "elder"), nil, 0o644); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "an instance that never held a lock does not", arrange: func(*testing.T, string) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "daemon.addr")
			tt.arrange(t, path)

			// Act
			alive, err := Alive(path, "elder")

			// Assert
			if err != nil || alive != tt.want {
				t.Fatalf("Alive = (%v, %v), want %v", alive, err, tt.want)
			}
		})
	}
}

func TestAnInstanceRecordsItsPrivateAddressForItsLife(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.addr")
	life, err := HoldLife(path, "a")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}

	// Act
	_, before := InstanceAddress(path, "a")
	recordErr := life.RecordAddress("127.0.0.1:5000")
	got, during := InstanceAddress(path, "a")
	releaseErr := life.Release()
	_, after := InstanceAddress(path, "a")

	// Assert
	if !errors.Is(before, ErrNoInstanceAddress) {
		t.Fatalf("before recording: %v, want ErrNoInstanceAddress", before)
	}
	if recordErr != nil || during != nil || got != "127.0.0.1:5000" {
		t.Fatalf("recorded %q (%v, %v), want 127.0.0.1:5000", got, recordErr, during)
	}
	if releaseErr != nil || !errors.Is(after, ErrNoInstanceAddress) {
		t.Fatalf("after its release: %v (%v), want no address", after, releaseErr)
	}
}

func TestLiveOthersRemovesADeadInstancesAddressWithItsLock(t *testing.T) {
	// Arrange: an instance that ended without releasing its files.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	if err := os.MkdirAll(filepath.Join(filepath.Dir(path), LifeDirName), 0o755); err != nil {
		t.Fatal(err)
	}
	for _, f := range []string{LifePath(path, "dead"), AddressPath(path, "dead")} {
		if err := os.WriteFile(f, []byte("127.0.0.1:1\n"), 0o600); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	live, err := LiveOthers(path, "self")

	// Assert
	if err != nil || len(live) != 0 {
		t.Fatalf("LiveOthers = %v, %v; want no live instance", live, err)
	}
	if _, err := InstanceAddress(path, "dead"); !errors.Is(err, ErrNoInstanceAddress) {
		t.Fatalf("the dead instance's address = %v, want it removed", err)
	}
}
