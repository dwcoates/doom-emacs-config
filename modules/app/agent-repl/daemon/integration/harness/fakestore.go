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

// KeepResumedBook puts the conversation PROFILE resumes into the store as
// workspace WS's book, as the real store holds a workspace's conversation
// before any shim of a successor daemon is up. A test that boots a successor
// onto a resumed book keeps it here before the boot, so a read made before
// the successor's shim starts finds it.
func (d *Daemon) KeepResumedBook(ws string, profile ShimProfile) {
	d.t.Helper()
	entries, err := fakestore.ResumeEntries(profile.ResumeHistory, profile.ResumeTurns)
	if err != nil {
		d.t.Fatalf("keep the resumed book: %v", err)
	}
	if err := fakestore.Write(fakestore.Dir(d.StoreSocket), fakestore.MainBook(ws), profile.HistoryPageSize, entries); err != nil {
		d.t.Fatalf("keep the resumed book: %v", err)
	}
}
