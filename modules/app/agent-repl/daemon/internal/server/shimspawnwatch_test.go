package server

import (
	"errors"
	"fmt"
	"os/exec"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/shimclient"
)

// spawnWatchLog collects the watch's loud lines for assertion.
type spawnWatchLog struct {
	mu    sync.Mutex
	lines []string
}

func (l *spawnWatchLog) logf(format string, args ...any) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.lines = append(l.lines, fmt.Sprintf(format, args...))
}

func (l *spawnWatchLog) contains(want string) bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	for _, line := range l.lines {
		if strings.Contains(line, want) {
			return true
		}
	}
	return false
}

// exitErr returns a real *exec.ExitError for code, so the watch is exercised
// against the same error shape os/exec produces rather than a stand-in.
func exitErr(t *testing.T, code string) error {
	t.Helper()
	err := exec.Command("/bin/sh", "-c", "exit "+code).Run()
	if err == nil {
		t.Fatalf("/bin/sh -c 'exit %s' succeeded", code)
	}
	return err
}

func TestShimSpawnWatchPublishesADeathBeforeConnectWithExitAndStderr(t *testing.T) {
	// Arrange
	log := &spawnWatchLog{}
	w := NewShimSpawnWatch(func(string) (bool, error) { return false, nil }, log.logf)
	w.Spawned("s1", func() string { return "node: cannot find module\n" })

	// Act
	failure := w.Exited("s1", exitErr(t, "3"))

	// Assert
	if !errors.Is(failure, shimclient.ErrShimDiedBeforeConnect) {
		t.Fatalf("Exited failure = %v, want %v", failure, shimclient.ErrShimDiedBeforeConnect)
	}
	if !strings.Contains(failure.Error(), "exit code 3") {
		t.Fatalf("Exited failure %v omits the exit code", failure)
	}
	if !log.contains("SHIM SPAWN FAILURE") || !log.contains("node: cannot find module") {
		t.Fatalf("watch log did not record the spawn failure with its stderr: %#v", log.lines)
	}
	select {
	case <-w.DiedBeforeConnect("s1"):
	default:
		t.Fatal("DiedBeforeConnect did not fire for a shim that never connected")
	}
}

func TestShimSpawnWatchTreatsAnExitAfterConnectingAsNoSpawnFailure(t *testing.T) {
	// Arrange
	log := &spawnWatchLog{}
	w := NewShimSpawnWatch(func(string) (bool, error) { return false, nil }, log.logf)
	w.Spawned("s1", func() string { return "" })
	w.Connected("s1")

	// Act
	failure := w.Exited("s1", exitErr(t, "1"))

	// Assert
	if failure != nil {
		t.Fatalf("Exited failure = %v, want nil for a shim that had connected", failure)
	}
	select {
	case <-w.DiedBeforeConnect("s1"):
		t.Fatal("DiedBeforeConnect fired for a shim that had already connected")
	default:
	}
}

func TestShimSpawnWatchAcceptsTheListenerProbeAsProofOfConnection(t *testing.T) {
	// Arrange: the daemon never took the connection itself, but the listener
	// still holds it parked.
	w := NewShimSpawnWatch(func(string) (bool, error) { return true, nil }, nil)
	w.Spawned("s1", func() string { return "" })

	// Act
	failure := w.Exited("s1", nil)

	// Assert
	if failure != nil {
		t.Fatalf("Exited failure = %v, want nil when the listener proved a connection", failure)
	}
}

func TestShimSpawnWatchReportsAFailedConnectionProbeAndStillRecordsTheDeath(t *testing.T) {
	// Arrange
	log := &spawnWatchLog{}
	w := NewShimSpawnWatch(func(string) (bool, error) { return false, errors.New("probe exploded") }, log.logf)
	w.Spawned("s1", func() string { return "" })

	// Act
	failure := w.Exited("s1", exitErr(t, "1"))

	// Assert: the probe failure is surfaced, and the death is not lost with it.
	if !log.contains("probe exploded") {
		t.Fatalf("watch log swallowed the connection-probe failure: %#v", log.lines)
	}
	if !errors.Is(failure, shimclient.ErrShimDiedBeforeConnect) {
		t.Fatalf("Exited failure = %v, want the death recorded despite the probe failure", failure)
	}
}

func TestShimSpawnWatchRearmsOnRespawnSoAnOldCorpseCannotFailTheNewBringUp(t *testing.T) {
	// Arrange: a first generation that died before connecting.
	w := NewShimSpawnWatch(func(string) (bool, error) { return false, nil }, nil)
	w.Spawned("s1", func() string { return "" })
	if failure := w.Exited("s1", exitErr(t, "1")); failure == nil {
		t.Fatal("first generation death was not recorded")
	}

	// Act
	w.Spawned("s1", func() string { return "" })

	// Assert
	select {
	case <-w.DiedBeforeConnect("s1"):
		t.Fatal("the respawned generation inherited the previous generation's death")
	default:
	}
	if state := w.SpawnState("s1"); !state.Alive || state.Failure != nil {
		t.Fatalf("SpawnState after respawn = %#v, want a live generation with no failure", state)
	}
}

func TestShimSpawnWatchReportsALiveProcessAndItsStderrWhileItRuns(t *testing.T) {
	// Arrange
	w := NewShimSpawnWatch(nil, nil)
	w.Spawned("s1", func() string { return "still starting\n" })

	// Act
	state := w.SpawnState("s1")

	// Assert
	if !state.Spawned || !state.Alive {
		t.Fatalf("SpawnState = %#v, want a spawned live process", state)
	}
	if state.StderrTail != "still starting\n" {
		t.Fatalf("SpawnState stderr tail = %q, want the live tail", state.StderrTail)
	}
}

func TestShimSpawnWatchReportsNothingForASessionItNeverSpawned(t *testing.T) {
	// Arrange
	w := NewShimSpawnWatch(nil, nil)

	// Act
	state := w.SpawnState("survivor")

	// Assert
	if state.Spawned || w.DiedBeforeConnect("survivor") != nil {
		t.Fatalf("SpawnState = %#v / died channel non-nil for an unspawned session", state)
	}
}

func TestShimSpawnWatchRecordsAnExitExactlyOnce(t *testing.T) {
	// Arrange
	w := NewShimSpawnWatch(nil, nil)
	w.Spawned("s1", func() string { return "" })
	first := w.Exited("s1", exitErr(t, "1"))

	// Act: a second reap report for the same process must not re-publish.
	second := w.Exited("s1", exitErr(t, "1"))

	// Assert
	if first == nil || second == nil || first.Error() != second.Error() {
		t.Fatalf("repeated Exited: first=%v second=%v, want the same recorded failure", first, second)
	}
}

func TestShimSpawnWatchReportsNoEvidenceForALauncherThatCapturedNoStderr(t *testing.T) {
	// Arrange: a launcher that cannot read the child's stderr must still be
	// able to arm the watch.
	w := NewShimSpawnWatch(nil, nil)

	// Act
	w.Spawned("s1", nil)

	// Assert
	if state := w.SpawnState("s1"); state.StderrTail != "" || !state.Spawned {
		t.Fatalf("SpawnState = %#v, want a spawned process with no stderr evidence", state)
	}
}

func TestShimSpawnWatchIgnoresAConnectionMarkForASessionItNeverSpawned(t *testing.T) {
	// Arrange
	w := NewShimSpawnWatch(nil, nil)

	// Act
	w.Connected("survivor")

	// Assert
	if state := w.SpawnState("survivor"); state.Spawned {
		t.Fatalf("SpawnState = %#v, want nothing recorded for an unspawned session", state)
	}
}

func TestShimSpawnWatchRecordsAnExitForASessionItNeverSpawnedWithoutFailingABringUp(t *testing.T) {
	// Arrange
	log := &spawnWatchLog{}
	w := NewShimSpawnWatch(nil, log.logf)

	// Act
	failure := w.Exited("survivor", exitErr(t, "1"))

	// Assert
	if failure != nil {
		t.Fatalf("Exited failure = %v, want nil for a process this daemon never spawned", failure)
	}
	if !log.contains("no spawn record for") {
		t.Fatalf("watch log did not record the unattributed exit: %#v", log.lines)
	}
}

func TestShimSpawnWatchDoesNotLetASupersededGenerationsExitFailTheNewOne(t *testing.T) {
	// Arrange: generation one's reap is held inside the connection probe until
	// generation two has armed, which is the interleaving this guards.
	log := &spawnWatchLog{}
	probing := make(chan struct{})
	respawned := make(chan struct{})
	w := NewShimSpawnWatch(func(string) (bool, error) {
		close(probing)
		<-respawned
		return false, nil
	}, log.logf)
	w.Spawned("s1", func() string { return "" })
	reaped := make(chan error, 1)
	go func() { reaped <- w.Exited("s1", exitErr(t, "1")) }()

	// Act
	<-probing
	w.Spawned("s1", func() string { return "" })
	close(respawned)

	// Assert
	if failure := <-reaped; failure != nil {
		t.Fatalf("Exited failure = %v, want nil for a superseded generation", failure)
	}
	select {
	case <-w.DiedBeforeConnect("s1"):
		t.Fatal("the superseded generation's exit fired the new generation's death")
	default:
	}
}

func TestShimConnSourceStaysInertWithNoSpawnWatchBound(t *testing.T) {
	// Arrange: the adapter must stay usable with no spawn watch at all.
	src := &ShimConnSource{}

	// Act
	state := src.SpawnState("s1")

	// Assert
	if state != (shimclient.ShimSpawnState{}) || src.DiedBeforeConnect("s1") != nil {
		t.Fatalf("unbound ShimConnSource reported %#v", state)
	}
}
