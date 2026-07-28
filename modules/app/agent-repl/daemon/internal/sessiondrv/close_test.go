package sessiondrv

import (
	"testing"

	"claude-repld/internal/shimclient"
)

// Close JOINS the driver-exit goroutine: the tail of bringUp's `go func` —
// queue drain, empty-view publish, the registry's queued_prompts persist, the
// orphan-shim stop — must have completed by the time Close returns. Unjoined,
// that tail outlived Close and raced whatever tore down after it; in the e2e
// suite it recreated registry files inside a t.TempDir mid-RemoveAll, which
// was the origin of the roving "directory not empty" teardown flake.
func TestCloseJoinsTheDriverExitGoroutine(t *testing.T) {
	// Arrange — a live driver over a fake client that runs until canceled.
	spawner := &fakeSpawner{}
	cfg := Config{
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               &fakeApplier{},
		Spawner:           spawner,
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		Registrar:         &fakeRegistrar{},
		ProtocolVersion:   "1",
		now:               func() int64 { return 1000 },
		Source:            stubSource{},
		newClient:         func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c} },
	}
	m, err := New(cfg)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}

	// Act.
	m.Close()

	// Assert — the exit tail's last act (stopping the orphaned shim) has
	// already happened, so nothing of the driver's can run after Close.
	spawner.mu.Lock()
	stopped := len(spawner.stopped)
	spawner.mu.Unlock()
	if stopped == 0 {
		t.Fatal("Close returned before the driver-exit goroutine finished: the shim stop had not run yet")
	}
}
