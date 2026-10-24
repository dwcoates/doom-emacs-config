package server

import (
	"agentrepl/shim-store/internal/testclose"
	"crypto/rand"
	"encoding/hex"
	"io"
	"net"
	"os"
	"path/filepath"
	"testing"

	"agentrepl/shim-store/internal/logging"
)

// shortSocketPath builds a unix socket path inside the platform's sun_path
// limit (~104 bytes on macOS), which a path under t.TempDir() would blow.
func shortSocketPath(t *testing.T) string {
	t.Helper()
	raw := make([]byte, 4)
	if _, err := rand.Read(raw); err != nil {
		t.Fatalf("rand: %v", err)
	}
	path := filepath.Join(os.TempDir(), "ar-"+hex.EncodeToString(raw)+".sock")
	t.Cleanup(func() { _ = os.Remove(path) })
	return path
}

func testLogger() *logging.Logger {
	return logging.New(&syncBuffer{}, io.Discard, true)
}

func TestListenBindsTheSocket(t *testing.T) {
	// Arrange.
	path := shortSocketPath(t)

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err != nil {
		t.Fatalf("Listen = %v, want nil", err)
	}
	defer testclose.OrFail(t, ln)
	if _, statErr := os.Stat(path); statErr != nil {
		t.Fatalf("stat %q = %v, want the socket to exist", path, statErr)
	}
}

func TestListenRestrictsTheSocketToItsOwner(t *testing.T) {
	// Arrange.
	path := shortSocketPath(t)

	// Act.
	ln, err := Listen(path, testLogger())
	if err != nil {
		t.Fatalf("Listen = %v, want nil", err)
	}
	defer testclose.OrFail(t, ln)

	// Assert.
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	if perm := info.Mode().Perm(); perm != 0o600 {
		t.Fatalf("mode = %v, want 0600", perm)
	}
}

func TestListenReclaimsAStaleSocket(t *testing.T) {
	// Arrange. A previous store died holding the path.
	path := shortSocketPath(t)
	stale, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("stage stale socket: %v", err)
	}
	stale.(*net.UnixListener).SetUnlinkOnClose(false)
	testclose.OrFail(t, stale)

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err != nil {
		t.Fatalf("Listen over a stale socket = %v, want nil", err)
	}
	testclose.OrFail(t, ln)
}

func TestListenRefusesToReplaceANonSocket(t *testing.T) {
	// Arrange. Unlinking whatever sits at an operator-supplied path is how a
	// service deletes somebody's file.
	path := shortSocketPath(t)
	if err := os.WriteFile(path, []byte("not a socket"), 0o600); err != nil {
		t.Fatalf("stage file: %v", err)
	}

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err == nil {
		testclose.OrFail(t, ln)
		t.Fatal("Listen over a regular file = nil error, want a loud refusal")
	}
}

func TestListenLeavesANonSocketAtThePathByteIdenticalAndSaysSoAtError(t *testing.T) {
	// Arrange.
	const contents = "this is somebody's file, not a socket"
	path := shortSocketPath(t)
	if err := os.WriteFile(path, []byte(contents), 0o600); err != nil {
		t.Fatalf("stage file: %v", err)
	}
	sink := &syncBuffer{}

	// Act.
	ln, err := Listen(path, logging.New(sink, io.Discard, true))

	// Assert.
	if err == nil {
		testclose.OrFail(t, ln)
		t.Fatal("Listen over a regular file = nil error, want a refusal")
	}
	if got, readErr := os.ReadFile(path); readErr != nil || string(got) != contents {
		t.Fatalf("the file at the listen path = %q (%v), want it left byte-identical", got, readErr)
	}
	if _, ok := findRecord(t, sink, "store.listen", "error"); !ok {
		t.Fatalf("records = %+v, want a store.listen error record", records(t, sink))
	}
}

func TestListenRefusesASocketALiveStoreIsServing(t *testing.T) {
	// Arrange: an ACCEPTING listener on the path. Unlinking it would leave the
	// incumbent serving a socket no client can reach, with this process
	// silently taking its callers.
	path := shortSocketPath(t)
	live, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("stage live socket: %v", err)
	}
	defer testclose.OrFail(t, live)

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err == nil {
		testclose.OrFail(t, ln)
		t.Fatal("Listen over a live socket = nil error, want a refusal to steal it")
	}
}

func TestListenLeavesALiveSocketOnDisk(t *testing.T) {
	// Arrange.
	path := shortSocketPath(t)
	live, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("stage live socket: %v", err)
	}
	defer testclose.OrFail(t, live)

	// Act.
	if _, err := Listen(path, testLogger()); err == nil {
		t.Fatal("Listen over a live socket = nil error, want a refusal")
	}

	// Assert: the incumbent's socket is untouched, so it keeps serving.
	if _, statErr := os.Stat(path); statErr != nil {
		t.Fatalf("stat %q = %v, want the incumbent's socket still on disk", path, statErr)
	}
}

func TestListenRecordsTheOccupiedSocketOnce(t *testing.T) {
	// Arrange.
	path := shortSocketPath(t)
	live, err := net.Listen("unix", path)
	if err != nil {
		t.Fatalf("stage live socket: %v", err)
	}
	defer testclose.OrFail(t, live)
	sink := &syncBuffer{}
	log := logging.New(sink, io.Discard, true)

	// Act.
	if _, err := Listen(path, log); err == nil {
		t.Fatal("Listen over a live socket = nil error, want a refusal")
	}

	// Assert.
	if _, ok := findRecord(t, sink, "store.listen.occupied", "error"); !ok {
		t.Fatalf("no store.listen.occupied error record; log was:\n%s", sink.String())
	}
}
