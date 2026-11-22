package daemonaddr

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
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
	err = AwaitGone(ctx, path, []string{"older"}, func(d Death) { t.Errorf("a running daemon was reaped as a death: %+v", d) })

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
		// wantNothing: the probe leaves no life locks' directory behind.
		wantNothing bool
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
		{name: "an instance that never held a lock does not", wantNothing: true, arrange: func(*testing.T, string) {}},
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
			if _, serr := os.Stat(filepath.Join(filepath.Dir(path), LifeDirName)); tt.wantNothing && !errors.Is(serr, os.ErrNotExist) {
				t.Fatalf("Alive created the life locks' directory or a file in it: %v", serr)
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

// leaveLife leaves the life lock of INSTANCE beside PATH as a daemon that
// died leaves it: standing, with CONTENT, and its private address beside it.
func leaveLife(t *testing.T, path, instance, content string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Join(filepath.Dir(path), LifeDirName), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(LifePath(path, instance), []byte(content), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(AddressPath(path, instance), []byte("127.0.0.1:1\n"), 0o600); err != nil {
		t.Fatal(err)
	}
}

// assertReaped fails unless INSTANCE's life lock and address are gone.
func assertReaped(t *testing.T, path, instance string) {
	t.Helper()
	for _, f := range []string{LifePath(path, instance), AddressPath(path, instance)} {
		if _, err := os.Stat(f); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("%s = %v, want it removed", f, err)
		}
	}
}

func TestHoldLifeWritesItsPidAndReleaseRemovesTheFile(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.addr")
	life, err := HoldLife(path, "a")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}

	// Act
	raw, readErr := life.rec.ReadAll()
	releaseErr := life.Release()

	// Assert
	if want := "pid=" + strconv.Itoa(os.Getpid()) + "\n"; readErr != nil || string(raw) != want {
		t.Fatalf("the life lock holds (%q, %v), want %q", raw, readErr, want)
	}
	if releaseErr != nil {
		t.Fatalf("Release = %v", releaseErr)
	}
	for _, f := range []string{LifePath(path, "a"), LifePath(path, "a") + pendingSuffix} {
		if _, err := os.Stat(f); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("%s after Release = %v, want it removed", f, err)
		}
	}
}

func TestLiveOthersReportsEachLeftoverLockAsADeath(t *testing.T) {
	tests := []struct {
		name       string
		arrange    func(t *testing.T, path string)
		wantLive   []string
		wantDeaths []Death
	}{
		{name: "a standing unheld lock naming its pid is a death with that pid",
			arrange:    func(t *testing.T, path string) { leaveLife(t, path, "dead", "pid=78035\n") },
			wantDeaths: []Death{{Instance: "dead", PID: 78035, PIDKnown: true}}},
		{name: "an older build's empty lock is a death with its pid unknown",
			arrange:    func(t *testing.T, path string) { leaveLife(t, path, "dead", "") },
			wantDeaths: []Death{{Instance: "dead"}}},
		{name: "a pid its holder died writing is unknown",
			arrange:    func(t *testing.T, path string) { leaveLife(t, path, "dead", "pid=780") },
			wantDeaths: []Death{{Instance: "dead"}}},
		{name: "a lock its holder marked ended orderly is no death",
			arrange: func(t *testing.T, path string) { leaveLife(t, path, "ended", "pid=78035\nended=orderly\n") }},
		{name: "a held lock is live and no death", wantLive: []string{"running"},
			arrange: func(t *testing.T, path string) {
				life, err := HoldLife(path, "running")
				if err != nil {
					t.Fatalf("HoldLife: %v", err)
				}
				t.Cleanup(func() { life.Release() })
			}},
		{name: "no life locks answer nothing", arrange: func(*testing.T, string) {}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "daemon.addr")
			tt.arrange(t, path)

			// Act
			live, deaths, err := LiveOthers(path, "self")

			// Assert
			if err != nil || !slices.Equal(live, tt.wantLive) || !slices.Equal(deaths, tt.wantDeaths) {
				t.Fatalf("LiveOthers = (%v, %+v, %v), want (%v, %+v, nil)", live, deaths, err, tt.wantLive, tt.wantDeaths)
			}
			if tt.wantLive == nil {
				for _, instance := range []string{"dead", "ended"} {
					assertReaped(t, path, instance)
				}
			}
		})
	}
}

func TestLiveOthersReportsADeathOnce(t *testing.T) {
	// Arrange: a death already reaped.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	leaveLife(t, path, "dead", "pid=78035\n")
	if _, deaths, err := LiveOthers(path, "self"); err != nil || len(deaths) != 1 {
		t.Fatalf("the first LiveOthers = (%+v, %v), want the one death", deaths, err)
	}

	// Act
	live, deaths, err := LiveOthers(path, "self")

	// Assert
	if err != nil || live != nil || deaths != nil {
		t.Fatalf("the second LiveOthers = (%v, %+v, %v), want nothing", live, deaths, err)
	}
}

// TestALockThatCannotBeProbedIsAnErrorBesideTheDeathsReaped pins that a
// lock file that cannot be opened is surfaced, never read as free or as a
// death, and that a death reaped beside it is still answered.
func TestALockThatCannotBeProbedIsAnErrorBesideTheDeathsReaped(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.addr")
	leaveLife(t, path, "dead", "pid=78035\n")
	leaveLife(t, path, "sealed", "pid=1\n")
	if err := os.Chmod(LifePath(path, "sealed"), 0); err != nil {
		t.Fatal(err)
	}

	// Act
	_, deaths, err := LiveOthers(path, "self")

	// Assert
	if err == nil || !strings.Contains(err.Error(), LifePath(path, "sealed")) {
		t.Fatalf("LiveOthers error = %v, want one naming the sealed lock", err)
	}
	if want := []Death{{Instance: "dead", PID: 78035, PIDKnown: true}}; !slices.Equal(deaths, want) {
		t.Fatalf("deaths = %+v, want %+v", deaths, want)
	}
	if _, err := os.Stat(LifePath(path, "sealed")); err != nil {
		t.Fatalf("the sealed lock = %v, want it left standing", err)
	}
}

// TestALifeLockThatCannotBeRemovedIsReadAsAnOrderlyEnd pins Release's one
// path that releases the lock with the file still standing: the file is
// marked ended orderly, so the next reaper reads no death.
func TestALifeLockThatCannotBeRemovedIsReadAsAnOrderlyEnd(t *testing.T) {
	// Arrange: a life whose directory refuses the removal.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	life, err := HoldLife(path, "a")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}
	dir := filepath.Join(filepath.Dir(path), LifeDirName)
	if err := os.Chmod(dir, 0o555); err != nil {
		t.Fatal(err)
	}
	releaseErr := life.Release()
	if err := os.Chmod(dir, 0o755); err != nil {
		t.Fatal(err)
	}

	// Act
	_, deaths, err := LiveOthers(path, "self")

	// Assert
	if releaseErr == nil || !strings.Contains(releaseErr.Error(), "remove the life lock") {
		t.Fatalf("Release = %v, want the failed removal surfaced", releaseErr)
	}
	if err != nil || deaths != nil {
		t.Fatalf("LiveOthers = (%+v, %v), want no death", deaths, err)
	}
	assertReaped(t, path, "a")
}

// TestADaemonKilledLeavesADeathItsSuccessorReaps runs the life lock's
// holder in another process, as production does: the kernel frees a killed
// holder's lock, and its file is the death.
func TestADaemonKilledLeavesADeathItsSuccessorReaps(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.addr")
	helper := startLockHelper(t, "life", path)
	pid := helper.field(t, "ready")
	live, deaths, err := LiveOthers(path, "self")
	if err != nil || !slices.Equal(live, []string{"helper"}) || deaths != nil {
		t.Fatalf("LiveOthers while it runs = (%v, %+v, %v), want it live", live, deaths, err)
	}
	if err := helper.cmd.Process.Kill(); err != nil {
		t.Fatal(err)
	}
	_ = helper.cmd.Wait()

	// Act
	live, deaths, err = LiveOthers(path, "self")

	// Assert
	if want := []Death{{Instance: "helper", PID: pid, PIDKnown: true}}; err != nil || live != nil || !slices.Equal(deaths, want) {
		t.Fatalf("LiveOthers after the kill = (%v, %+v, %v), want the death %+v", live, deaths, err, want)
	}
	assertReaped(t, path, "helper")
}

func TestAwaitGoneJudgesHowTheDaemonEnded(t *testing.T) {
	tests := []struct {
		name      string
		end       func(t *testing.T, h *lockHelper)
		wantDeath bool
	}{
		{name: "a holder that ends by Release is no death", end: func(t *testing.T, h *lockHelper) {
			h.stdin.Close()
			if err := h.cmd.Wait(); err != nil {
				t.Fatalf("the helper's orderly exit: %v", err)
			}
		}},
		{name: "a holder killed with its file standing is a death with its pid", wantDeath: true,
			end: func(t *testing.T, h *lockHelper) {
				if err := h.cmd.Process.Kill(); err != nil {
					t.Fatal(err)
				}
				_ = h.cmd.Wait()
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "daemon.addr")
			helper := startLockHelper(t, "life", path)
			pid := helper.field(t, "ready")
			var deaths []Death
			var mu sync.Mutex
			awaited := make(chan error, 1)
			go func() {
				awaited <- AwaitGone(context.Background(), path, []string{"helper"}, func(d Death) {
					mu.Lock()
					deaths = append(deaths, d)
					mu.Unlock()
				})
			}()

			// Act
			tt.end(t, helper)
			err := <-awaited

			// Assert
			var want []Death
			if tt.wantDeath {
				want = []Death{{Instance: "helper", PID: pid, PIDKnown: true}}
			}
			mu.Lock()
			defer mu.Unlock()
			if err != nil || !slices.Equal(deaths, want) {
				t.Fatalf("AwaitGone = %v with deaths %+v, want nil with %+v", err, deaths, want)
			}
			assertReaped(t, path, "helper")
		})
	}
}

func TestAwaitGoneReapsALockFreedInThisProcessWithItsFileStanding(t *testing.T) {
	// Arrange: the kernel frees the lock without Release, as at a death.
	path := filepath.Join(t.TempDir(), "daemon.addr")
	older, err := HoldLife(path, "older")
	if err != nil {
		t.Fatalf("HoldLife: %v", err)
	}
	if err := older.rec.Release(); err != nil {
		t.Fatalf("release the record: %v", err)
	}
	var deaths []Death

	// Act
	err = AwaitGone(context.Background(), path, []string{"older"}, func(d Death) { deaths = append(deaths, d) })

	// Assert
	if want := []Death{{Instance: "older", PID: os.Getpid(), PIDKnown: true}}; err != nil || !slices.Equal(deaths, want) {
		t.Fatalf("AwaitGone = %v with deaths %+v, want %+v", err, deaths, want)
	}
	assertReaped(t, path, "older")
}

func TestAwaitGoneReadsAFileAlreadyGoneAsAnOrderlyEnd(t *testing.T) {
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
	err = AwaitGone(context.Background(), path, []string{"older"}, func(d Death) { t.Errorf("an orderly end was reaped as a death: %+v", d) })

	// Assert: it is gone, and nothing was created in its place.
	if err != nil {
		t.Fatalf("AwaitGone = %v, want it gone", err)
	}
	assertReaped(t, path, "older")
}
