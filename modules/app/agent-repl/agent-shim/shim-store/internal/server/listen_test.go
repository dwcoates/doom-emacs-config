package server

import (
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
	defer ln.Close()
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
	defer ln.Close()

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
	stale.Close()

	// Act.
	ln, err := Listen(path, testLogger())

	// Assert.
	if err != nil {
		t.Fatalf("Listen over a stale socket = %v, want nil", err)
	}
	ln.Close()
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
		ln.Close()
		t.Fatal("Listen over a regular file = nil error, want a loud refusal")
	}
}
