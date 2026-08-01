package sessioncontroller

import (
	"errors"
	"runtime"
	"strings"
	"testing"

	"claude-repld/internal/shimclient"
)

// newClosingRig builds a live session controller over a fake client that runs until
// cancelled, returning the manager and the doubles the exit tail lands in.
func newClosingRig(t *testing.T) (*Manager, *fakeSpawner, *fakeApplier, *logCapture) {
	t.Helper()
	spawner := &fakeSpawner{}
	applier := &fakeApplier{}
	cl := &logCapture{}
	cfg := Config{
		Logf:              cl.logf,
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               applier,
		Spawner:           spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		now:               func() int64 { return 1000 },
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient:         func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c} },
	}
	m, err := New(cfg)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	return m, spawner, applier, cl
}

// A Close that lands while a bring-up is still inside its spawn must ABORT
// that bring-up, not race it: Close waits on the exits WaitGroup, and a
// bring-up that reached its exits.Add only after Close began waiting is the
// WaitGroup-reuse panic. The re-check under m.mu turns that interleaving into
// a loud refusal instead.
//
// The spawner gate is what makes the interleaving deterministic: the bring-up
// is parked PAST its entry closed-check, Close completes, and only then is the
// bring-up released to discover the closed manager.
func TestCloseDuringBringUpAbortsTheBringUp(t *testing.T) {
	// Arrange — a bring-up parked inside the spawner.
	spawner := &fakeSpawner{entered: make(chan struct{}, 1), gate: make(chan struct{})}
	applier := &fakeApplier{}
	cl := &logCapture{}
	m, err := New(Config{
		Logf:              cl.logf,
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               applier,
		Spawner:           spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		now:               func() int64 { return 1000 },
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient:         func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	errs := make(chan error, 1)
	go func() { errs <- m.Ensure("ws") }()
	<-spawner.entered

	// Act — close while the bring-up is parked, then release it.
	m.Close()
	close(spawner.gate)

	// Assert — the bring-up refuses loudly instead of adding an exit
	// goroutine behind the Close that already joined them.
	err = <-errs
	if err == nil || !strings.Contains(err.Error(), "manager closed during bring-up") {
		t.Fatalf("Ensure() error = %v, want the manager-closed-during-bring-up refusal", err)
	}
	if !cl.contains("bring-up FAILED ws=\"ws\" generation=") ||
		!cl.contains("reason=manager_closed was_current=true dropped_prompts=0 decision=abort") {
		t.Fatalf("logs = %v, want canonical manager-close bring-up failure diagnostics", cl.lines)
	}
}

// Close JOINS the session-controller-exit goroutine: the tail of bringUp's `go func` —
// queue drain, empty-view publish, the registry's queued_prompts persist, the
// wired-axis close — must have completed by the time Close returns. Unjoined,
// that tail outlived Close and raced whatever tore down after it; in the e2e
// suite it recreated registry files inside a t.TempDir mid-RemoveAll, which
// was the origin of the roving "directory not empty" teardown flake.
//
// The observable is the tail's LAST act, which on a manager close is the
// shim-preservation line. It has been three different things as the tail
// changed, and each move was forced rather than chosen: it was the orphan-shim
// stop until a close stopped SIGTERMing children (see the test below), then the
// `session_controller_exit` wiring edge until that edge became conditional on a non-nil
// runErr — a manager close cancels the root ctx, which ends Run with nil, so
// there is deliberately no wiring edge left to wait on.
func TestCloseJoinsTheSessionControllerExitGoroutine(t *testing.T) {
	// Arrange.
	m, _, _, cl := newClosingRig(t)

	// Act.
	m.Close()

	// Assert.
	if !cl.contains("PRESERVING the shim") {
		t.Fatal("Close returned before the session-controller-exit goroutine finished: its last act had not run yet")
	}
}

// A MANAGER CLOSE WRITES NO WIRED ROW, and that silence is the trap this whole
// design turns on.
//
// The tail fires on the same workspace milliseconds after whatever cancelled the
// controller ctx. A tail that wrote `severed` unconditionally therefore repainted
// every hibernation blue the instant after it went teal — the entire split
// undone by one write. `client.Run` returns non-nil ONLY for a terminal protocol
// error, so a nil answer is positive evidence that nothing broke, and every
// clean cancel has already recorded a truer answer: a hibernation wrote
// `hibernated` before cancelling, a failed bring-up wrote `severed` itself, and
// a manager close's axis is rewritten wholesale by the next boot.
func TestCloseAppendsNoWiredRow(t *testing.T) {
	// Arrange.
	m, _, applier, _ := newClosingRig(t)

	// Act.
	m.Close()

	// Assert.
	for _, w := range applier.wiringsApplied() {
		if w.reason == "session_controller_exit" {
			t.Fatalf("a CLEAN controller exit wrote %s/%q; a clean exit must write nothing at all", w.wiring, w.reason)
		}
	}
}

// A MANAGER CLOSE PRESERVES THE SHIM. The daemon's shutdown decides whether
// shims live on, and the session-controller-exit tail must not quietly overrule it by
// SIGTERMing every child on its way out — which is exactly what it did while
// the stop was unconditional.
func TestCloseDoesNotStopTheShim(t *testing.T) {
	// Arrange.
	m, spawner, _, _ := newClosingRig(t)

	// Act.
	m.Close()

	// Assert.
	spawner.mu.Lock()
	stopped := append([]string(nil), spawner.stopped...)
	spawner.mu.Unlock()
	if len(stopped) != 0 {
		t.Fatalf("a manager close stopped %v; a preserved shim must survive to redial the next daemon", stopped)
	}
}

// A controller that dies on its OWN — a terminal protocol error, not a teardown —
// still stops its shim, because nothing else will and the process would be
// orphaned with its stop handle unreachable.
func TestAnUnexpectedSessionControllerExitStillStopsTheShim(t *testing.T) {
	// Arrange — a client whose Run can be made to return while the manager
	// lives on.
	spawner := &fakeSpawner{}
	runResult := make(chan error, 1)
	applier := &fakeApplier{}
	m, err := New(Config{
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               applier,
		Spawner:           spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		now:               func() int64 { return 1000 },
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient:         func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c, runResult: runResult} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}

	// Act.
	runResult <- errors.New("protocol violation")
	waitForWirings(applier, 2)
	if got := lastWiring(t, applier, "ws"); got.reason != "session_controller_exit" {
		t.Fatalf("last wiring reason = %q, want session_controller_exit", got.reason)
	}

	// Assert — the stop follows that edge in the tail's program order, so this
	// yields to it rather than waiting on a clock. A regression that never
	// stops the shim fails as a test-binary timeout, which is the same
	// rendezvous shape waitForWirings uses.
	for {
		spawner.mu.Lock()
		stopped := len(spawner.stopped)
		spawner.mu.Unlock()
		if stopped > 0 {
			return
		}
		runtime.Gosched()
	}
}
