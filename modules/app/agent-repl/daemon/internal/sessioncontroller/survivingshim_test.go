package sessioncontroller

import (
	"errors"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/shimclient"
)

// ---------------------------------------------------------------------------
// THE WORKSPACE-OWNERSHIP GATE. A live shim under a session id this daemon does
// not know about must not be duplicated. See survivingshim.go.
// ---------------------------------------------------------------------------

// fakeWorkspaceLock is the kernel probe, under the test's control. answers are
// consumed in order and the last one repeats, so a test can free a workspace
// mid-wait without a sleep on the production clock.
type fakeWorkspaceLock struct {
	mu      sync.Mutex
	answers []bool
	err     error
	calls   int
}

func (l *fakeWorkspaceLock) held(string) (bool, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.calls++
	if l.err != nil {
		return false, l.err
	}
	if len(l.answers) == 0 {
		return false, nil
	}
	answer := l.answers[0]
	if len(l.answers) > 1 {
		l.answers = l.answers[1:]
	}
	return answer, nil
}

// gateHarness is one manager whose workspace-lock probe is scripted and whose
// wait bounds are short enough to run at test speed.
type gateHarness struct {
	m       *Manager
	spawner *fakeSpawner
	applier *fakeApplier
	lock    *fakeWorkspaceLock
	log     *logCapture
}

func newGateHarness(t *testing.T, lock *fakeWorkspaceLock) *gateHarness {
	t.Helper()
	h := &gateHarness{
		spawner: &fakeSpawner{resume: map[string]string{}},
		applier: &fakeApplier{},
		lock:    lock,
		log:     &logCapture{},
	}
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               h.applier,
		Spawner:           h.spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		WorkspaceLockHeld: lock.held,
		Logf:              h.log.logf,
		newClient:         func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	m.survivingShimWaitOverride = 200 * time.Millisecond
	m.survivingShimPollOverride = time.Millisecond
	t.Cleanup(m.Close)
	h.m = m
	return h
}

func (h *gateHarness) spawns() int {
	h.spawner.mu.Lock()
	defer h.spawner.mu.Unlock()
	return len(h.spawner.calls)
}

// TestSpawnProceedsWhenTheWorkspaceLockIsFree: the ordinary path must not
// acquire a new failure mode.
func TestSpawnProceedsWhenTheWorkspaceLockIsFree(t *testing.T) {
	// Arrange — nothing holds the workspace.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{false}})

	// Act
	d, created, err := h.m.bringUpTracked("ws")

	// Assert
	if err != nil {
		t.Fatalf("bringUpTracked over a free workspace = %v, want a spawn", err)
	}
	if !created || d == nil {
		t.Fatalf("created = %v, controller = %v; want a freshly created controller", created, d)
	}
	if h.spawns() != 1 {
		t.Fatalf("EnsureShim calls = %d, want 1", h.spawns())
	}
}

// TestHeldWorkspaceLockRefusesToSpawn: the defect itself. A shim holding the
// workspace that never dials in must produce a typed refusal, never a second
// shim over the same transcript.
func TestHeldWorkspaceLockRefusesToSpawn(t *testing.T) {
	// Arrange — a survivor owns the workspace for the whole wait.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})

	// Act
	_, _, err := h.m.bringUpTracked("ws")

	// Assert
	if !errors.Is(err, ErrSurvivingShim) {
		t.Fatalf("bringUpTracked error = %v, want ErrSurvivingShim", err)
	}
	if h.spawns() != 0 {
		t.Fatalf("EnsureShim calls = %d, want 0; the daemon spawned a duplicate of a live shim", h.spawns())
	}
	if !h.log.contains("SURVIVING SHIM WAIT EXPIRED") {
		t.Fatal("the expiry was not logged; a refusal nobody records cannot be explained afterwards")
	}
}

// TestAReleasedWorkspaceLockLetsTheSpawnThrough: the survivor died mid-wait, so
// the workspace really is free and the wait must not have become a refusal.
func TestAReleasedWorkspaceLockLetsTheSpawnThrough(t *testing.T) {
	// Arrange — held on the pre-check, free on the first poll.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true, false}})

	// Act
	d, created, err := h.m.bringUpTracked("ws")

	// Assert
	if err != nil {
		t.Fatalf("bringUpTracked after the holder released = %v, want a spawn", err)
	}
	if !created || d == nil {
		t.Fatalf("created = %v, controller = %v; want a freshly created controller", created, d)
	}
	if h.spawns() != 1 {
		t.Fatalf("EnsureShim calls = %d, want 1", h.spawns())
	}
}

// TestALiveControllerWinsBeforeTheProbeRuns: the existing early return is the
// hot path and must not pay for the probe.
func TestALiveControllerWinsBeforeTheProbeRuns(t *testing.T) {
	// Arrange — a controller already drives the workspace, and the probe would
	// refuse if it were consulted.
	lock := &fakeWorkspaceLock{answers: []bool{true}}
	h := newGateHarness(t, lock)
	existing := &sessionController{sessionID: "s1", workspace: "ws"}
	h.m.mu.Lock()
	h.m.byWS["ws"] = existing
	h.m.mu.Unlock()

	// Act
	d, created, err := h.m.bringUpTracked("ws")

	// Assert
	if err != nil || created || d != existing {
		t.Fatalf("bringUpTracked = (%v, %v, %v), want the live controller unchanged", d, created, err)
	}
	if lock.calls != 0 {
		t.Fatalf("workspace lock probed %d times; the live-controller early return must win first", lock.calls)
	}
}

// TestAnUnreadableWorkspaceLockRefusesRatherThanSpawning: "I could not tell" is
// never read as free.
func TestAnUnreadableWorkspaceLockRefusesRatherThanSpawning(t *testing.T) {
	// Arrange
	probeErr := errors.New("permission denied")
	h := newGateHarness(t, &fakeWorkspaceLock{err: probeErr})

	// Act
	_, _, err := h.m.bringUpTracked("ws")

	// Assert
	if !errors.Is(err, probeErr) {
		t.Fatalf("bringUpTracked error = %v, want the probe failure surfaced", err)
	}
	if h.spawns() != 0 {
		t.Fatalf("EnsureShim calls = %d, want 0", h.spawns())
	}
}

// TestASurvivorThatDialsInIsAdoptedRatherThanDuplicated: the wait's good
// outcome — a controller appears for the workspace and the caller gets it.
func TestASurvivorThatDialsInIsAdoptedRatherThanDuplicated(t *testing.T) {
	// Arrange — the lock stays held; a controller lands for the workspace while
	// the gate is waiting, exactly as a survivor's dial-in produces.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	dialledIn := &sessionController{sessionID: "s_survivor", workspace: "ws"}
	go func() {
		h.m.mu.Lock()
		h.m.byWS["ws"] = dialledIn
		h.m.mu.Unlock()
	}()

	// Act
	d, err := h.m.awaitSurvivingShim("ws")

	// Assert
	if err != nil {
		t.Fatalf("awaitSurvivingShim = %v, want the survivor adopted", err)
	}
	if d != dialledIn {
		t.Fatalf("controller = %v, want the survivor's controller", d)
	}
	if h.spawns() != 0 {
		t.Fatalf("EnsureShim calls = %d, want 0", h.spawns())
	}
}
