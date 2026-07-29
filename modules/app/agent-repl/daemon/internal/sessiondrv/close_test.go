package sessiondrv

import (
	"errors"
	"runtime"
	"testing"

	"claude-repld/internal/shimclient"
)

// newClosingRig builds a live driver over a fake client that runs until
// cancelled, returning the manager and the doubles the exit tail lands in.
func newClosingRig(t *testing.T) (*Manager, *fakeSpawner, *fakeApplier) {
	t.Helper()
	spawner := &fakeSpawner{}
	applier := &fakeApplier{}
	cfg := Config{
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
	return m, spawner, applier
}

// Close JOINS the driver-exit goroutine: the tail of bringUp's `go func` —
// queue drain, empty-view publish, the registry's queued_prompts persist, the
// wired-axis close — must have completed by the time Close returns. Unjoined,
// that tail outlived Close and raced whatever tore down after it; in the e2e
// suite it recreated registry files inside a t.TempDir mid-RemoveAll, which
// was the origin of the roving "directory not empty" teardown flake.
//
// The observable is the tail's LAST act, which is the `driver_exit` wiring
// edge. It used to be the orphan-shim stop; that stop no longer runs on a
// manager close, because a close preserves the shim (see the test below).
func TestCloseJoinsTheDriverExitGoroutine(t *testing.T) {
	// Arrange.
	m, _, applier := newClosingRig(t)

	// Act.
	m.Close()

	// Assert.
	var found bool
	for _, w := range applier.wiringsApplied() {
		if w.workspace == "ws" && w.reason == "driver_exit" {
			found = true
		}
	}
	if !found {
		t.Fatal("Close returned before the driver-exit goroutine finished: its wiring edge had not landed yet")
	}
}

// A MANAGER CLOSE PRESERVES THE SHIM. The daemon's shutdown decides whether
// shims live on, and the driver-exit tail must not quietly overrule it by
// SIGTERMing every child on its way out — which is exactly what it did while
// the stop was unconditional.
func TestCloseDoesNotStopTheShim(t *testing.T) {
	// Arrange.
	m, spawner, _ := newClosingRig(t)

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

// A driver that dies on its OWN — a terminal protocol error, not a teardown —
// still stops its shim, because nothing else will and the process would be
// orphaned with its stop handle unreachable.
func TestAnUnexpectedDriverExitStillStopsTheShim(t *testing.T) {
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
	if got := lastWiring(t, applier, "ws"); got.reason != "driver_exit" {
		t.Fatalf("last wiring reason = %q, want driver_exit", got.reason)
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
