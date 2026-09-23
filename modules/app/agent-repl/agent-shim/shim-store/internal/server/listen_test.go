package server

import (
	"crypto/rand"
	"encoding/hex"
	"errors"
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
	defer closeOrFail(t, ln)
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
	defer closeOrFail(t, ln)

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
	closeOrFail(t, stale)

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err != nil {
		t.Fatalf("Listen over a stale socket = %v, want nil", err)
	}
	closeOrFail(t, ln)
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
		closeOrFail(t, ln)
		t.Fatal("Listen over a regular file = nil error, want a loud refusal")
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
	defer closeOrFail(t, live)

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err == nil {
		closeOrFail(t, ln)
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
	defer closeOrFail(t, live)

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
	defer closeOrFail(t, live)
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

// failingCloseListener is a net.Listener whose Close fails, which is the one
// input abandonListener's second fault needs and the kernel will not produce
// on demand.
type failingCloseListener struct {
	net.Listener
	closeErr error
}

func (l failingCloseListener) Close() error { return l.closeErr }

func TestAbandonListenerReturnsTheCauseWhenTheCloseSucceeds(t *testing.T) {
	// Arrange.
	ln, err := net.Listen("unix", shortSocketPath(t))
	if err != nil {
		t.Fatalf("stage listener: %v", err)
	}
	cause := errors.New("the setup failed")

	// Act.
	got := abandonListener(ln, "/a.sock", cause, testLogger())

	// Assert.
	if got != cause {
		t.Fatalf("abandonListener = %v, want exactly the cause", got)
	}
}

func TestAbandonListenerJoinsAFailedCloseOntoTheCause(t *testing.T) {
	// Arrange.
	cause := errors.New("the setup failed")
	closeErr := errors.New("the close failed")
	ln := failingCloseListener{closeErr: closeErr}

	// Act.
	got := abandonListener(ln, "/a.sock", cause, testLogger())

	// Assert.
	if !errors.Is(got, cause) || !errors.Is(got, closeErr) {
		t.Fatalf("abandonListener = %v, want both the cause and the close failure", got)
	}
}

func TestAbandonListenerRecordsAFailedCloseOnce(t *testing.T) {
	// Arrange.
	sink := &syncBuffer{}
	log := logging.New(sink, io.Discard, true)
	ln := failingCloseListener{closeErr: errors.New("the close failed")}

	// Act.
	_ = abandonListener(ln, "/a.sock", errors.New("the setup failed"), log)

	// Assert.
	var matches []logRecord
	for _, rec := range records(t, sink) {
		if rec.Operation == "store.listen.abandon" && rec.Level == "error" {
			matches = append(matches, rec)
		}
	}
	if len(matches) != 1 {
		t.Fatalf("store.listen.abandon error records = %d, want 1; log was:\n%s", len(matches), sink.String())
	}
	if got := matches[0].Context["socket"]; got != "/a.sock" {
		t.Fatalf("record socket = %v, want %q", got, "/a.sock")
	}
}
