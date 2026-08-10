package sessioncontroller

import (
	"errors"
	"fmt"
	"strings"
	"sync"
	"syscall"
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
	// freed latches the holder's death: once the takeover's signal lands, the
	// kernel would drop the flock, and every later probe answers free.
	freed bool
}

func (l *fakeWorkspaceLock) release() {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.freed = true
}

func (l *fakeWorkspaceLock) held(string) (bool, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.calls++
	if l.err != nil {
		return false, l.err
	}
	if l.freed {
		return false, nil
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

// fakeHolder is the squatting process the expired wait escalates against: which
// pids the lock names, which signal (if any) actually kills it, and what was
// aimed at it.
type fakeHolder struct {
	mu      sync.Mutex
	pids    []int
	err     error
	diesOn  syscall.Signal // zero means nothing kills it
	sigErr  error          // returned from every delivery attempt when set
	signals []string
	lock    *fakeWorkspaceLock
}

func (h *fakeHolder) holders(string) ([]int, error) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.err != nil {
		return nil, h.err
	}
	return append([]int(nil), h.pids...), nil
}

func (h *fakeHolder) signal(pid int, sig syscall.Signal) error {
	h.mu.Lock()
	h.signals = append(h.signals, fmt.Sprintf("%d:%v", pid, sig))
	dies := h.diesOn != 0 && sig == h.diesOn
	err := h.sigErr
	h.mu.Unlock()
	if err != nil {
		return err
	}
	if dies {
		h.lock.release()
	}
	return nil
}

func (h *fakeHolder) delivered() []string {
	h.mu.Lock()
	defer h.mu.Unlock()
	return append([]string(nil), h.signals...)
}

// gateHarness is one manager whose workspace-lock probe is scripted and whose
// wait bounds are short enough to run at test speed.
type gateHarness struct {
	m       *Manager
	spawner *fakeSpawner
	applier *fakeApplier
	lock    *fakeWorkspaceLock
	holder  *fakeHolder
	log     *logCapture
}

func newGateHarness(t *testing.T, lock *fakeWorkspaceLock) *gateHarness {
	t.Helper()
	h := &gateHarness{
		spawner: &fakeSpawner{resume: map[string]string{}},
		applier: &fakeApplier{},
		lock:    lock,
		holder:  &fakeHolder{lock: lock},
		log:     &logCapture{},
	}
	m, err := New(Config{
		Push:                 &fakePusher{},
		SSM:                  h.applier,
		Spawner:              h.spawner,
		Locator:              fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:             &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore:    newFakeClearCompactStore(),
		TurnAccountings:      emptyTurnAccountingStore{},
		Registrar:            &fakeRegistrar{},
		ProtocolVersion:      "1",
		Source:               stubSource{},
		FileDiagnostics:      fakeFileDiagnosticPersister{},
		WorkspaceLockHeld:    lock.held,
		WorkspaceLockHolders: h.holder.holders,
		SignalProcess:        h.holder.signal,
		Logf:                 h.log.logf,
		newClient:            func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	m.survivingShimWaitOverride = 200 * time.Millisecond
	m.survivingShimPollOverride = time.Millisecond
	m.survivingShimKillGraceOverride = 200 * time.Millisecond
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

// TestANonDiallingHolderIsTerminatedAndTheSameAttemptSpawns: the live defect.
// A survivor of a previous daemon generation holds the workspace and will never
// dial in, so waiting it out again on every retry never converges. The expired
// wait must terminate it and continue into the spawn in the SAME attempt.
func TestANonDiallingHolderIsTerminatedAndTheSameAttemptSpawns(t *testing.T) {
	// Arrange — a holder that dies on SIGTERM and never dials in.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	h.holder.pids = []int{4242}
	h.holder.diesOn = syscall.SIGTERM

	// Act
	d, created, err := h.m.bringUpTracked("ws")

	// Assert
	if err != nil {
		t.Fatalf("bringUpTracked after the takeover = %v, want a spawn in the same attempt", err)
	}
	if !created || d == nil {
		t.Fatalf("created = %v, controller = %v; want a freshly created controller", created, d)
	}
	if h.spawns() != 1 {
		t.Fatalf("EnsureShim calls = %d, want 1", h.spawns())
	}
}

// TestTheTakeoverEscalatesToSIGKILLWhenSIGTERMIsIgnored: a holder wedged past a
// clean stop still has to lose the workspace.
func TestTheTakeoverEscalatesToSIGKILLWhenSIGTERMIsIgnored(t *testing.T) {
	// Arrange — only SIGKILL frees the lock.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	h.holder.pids = []int{99}
	h.holder.diesOn = syscall.SIGKILL

	// Act
	_, _, err := h.m.bringUpTracked("ws")

	// Assert
	if err != nil {
		t.Fatalf("bringUpTracked = %v, want the spawn after the SIGKILL escalation", err)
	}
	if got, want := h.holder.delivered(), []string{"99:terminated", "99:killed"}; len(got) != len(want) || got[0] != want[0] || got[1] != want[1] {
		t.Fatalf("signals delivered = %v, want SIGTERM then SIGKILL to pid 99", got)
	}
}

// TestTheTakeoverIsLoggedWithTheHolderAndHowLongItSquatted: a workspace taken
// from another process by force must be explainable afterwards.
func TestTheTakeoverIsLoggedWithTheHolderAndHowLongItSquatted(t *testing.T) {
	// Arrange
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	h.holder.pids = []int{7}
	h.holder.diesOn = syscall.SIGTERM

	// Act
	if _, _, err := h.m.bringUpTracked("ws"); err != nil {
		t.Fatalf("bringUpTracked = %v, want the takeover to succeed", err)
	}

	// Assert
	if !h.log.contains("SURVIVING SHIM TAKEOVER ws=\"ws\" holder_pids=[7]") {
		t.Fatal("the takeover was not logged with its holder pid; a killed process nobody recorded cannot be explained")
	}
	if !h.log.contains("SURVIVING SHIM EVICTED") {
		t.Fatal("the eviction outcome was not logged")
	}
}

// TestAnUnkillableHolderStillFailsLoudly: the takeover is an escalation, not a
// guarantee. A holder that survives both signals must keep today's typed
// refusal rather than let a duplicate spawn.
func TestAnUnkillableHolderStillFailsLoudly(t *testing.T) {
	// Arrange — signals land but nothing dies.
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	h.holder.pids = []int{31337}

	// Act
	_, _, err := h.m.bringUpTracked("ws")

	// Assert
	if !errors.Is(err, ErrSurvivingShim) {
		t.Fatalf("bringUpTracked error = %v, want ErrSurvivingShim", err)
	}
	if h.spawns() != 0 {
		t.Fatalf("EnsureShim calls = %d, want 0; the daemon spawned a duplicate of a live shim", h.spawns())
	}
	if !strings.Contains(err.Error(), "SIGKILL") {
		t.Fatalf("error = %v, want the attempted escalation recorded in it", err)
	}
}

// TestAFailedSignalIsSurfacedInTheRefusal: a kill that could not even be
// delivered (a foreign uid, say) must name that, not just "still held".
func TestAFailedSignalIsSurfacedInTheRefusal(t *testing.T) {
	// Arrange
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	h.holder.pids = []int{5}
	h.holder.sigErr = errors.New("operation not permitted")

	// Act
	_, _, err := h.m.bringUpTracked("ws")

	// Assert
	if !strings.Contains(err.Error(), "operation not permitted") {
		t.Fatalf("error = %v, want the failed signal delivery surfaced", err)
	}
}

// TestAnUnnameableHolderKeepsTheOldRefusal: the defect itself, in the case
// where escalation is impossible — the lock is held but no pid can be named,
// so there is nothing to terminate and spawning anyway is still forbidden.
func TestAnUnnameableHolderKeepsTheOldRefusal(t *testing.T) {
	// Arrange — a survivor owns the workspace for the whole wait, unnameable.
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

// TestAnUnreadableHolderProbeRefusesRatherThanKilling: "I could not tell who
// holds it" is never read as a pid to signal.
func TestAnUnreadableHolderProbeRefusesRatherThanKilling(t *testing.T) {
	// Arrange
	probeErr := errors.New("lsof exploded")
	h := newGateHarness(t, &fakeWorkspaceLock{answers: []bool{true}})
	h.holder.err = probeErr

	// Act
	_, _, err := h.m.bringUpTracked("ws")

	// Assert
	if !errors.Is(err, probeErr) {
		t.Fatalf("bringUpTracked error = %v, want the holder probe failure surfaced", err)
	}
	if got := h.holder.delivered(); len(got) != 0 {
		t.Fatalf("signals delivered = %v, want none; an unnamed holder must not be signalled", got)
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
