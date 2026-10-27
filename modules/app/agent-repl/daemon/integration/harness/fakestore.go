package harness

import (
	"sync"
	"testing"

	"agentrepl/logging/storesocket"

	"agent-repld/integration/fakestore"
)

// fakeStores names every store socket whose read-only face is served in this
// process, so a successor daemon on its predecessor's store reads the same
// books rather than binding the socket a second time.
var fakeStores sync.Map

// serveFakeStore serves the read-only face of the store on STORESOCKET
// (storesocket.ReadPath), from the books the fake shims keep beside it, for
// the rest of the test that first asks for it.
func serveFakeStore(t testing.TB, storeSocket string) {
	t.Helper()
	if _, served := fakeStores.LoadOrStore(storeSocket, true); served {
		return
	}
	stop, err := fakestore.Serve(storesocket.ReadPath(storeSocket), fakestore.Dir(storeSocket), fakestore.DefaultPageSize)
	if err != nil {
		fakeStores.Delete(storeSocket)
		t.Fatalf("serve the fake store's read-only face: %v", err)
	}
	t.Cleanup(func() {
		fakeStores.Delete(storeSocket)
		if err := stop(); err != nil {
			t.Errorf("stop the fake store's read-only face: %v", err)
		}
	})
}
