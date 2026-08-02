package shimlisten

import (
	"path/filepath"
	"strings"
	"testing"
)

// TestDefaultSocketPathPrefersTheEnvironmentOverride covers the INJECTION: an
// absolute override is the socket, verbatim, with no home directory consulted.
//
// It is what lets a test stand a daemon up on its own socket instead of the
// operator's live one, so the isolation is a stated fact rather than a side
// effect of also having moved $HOME.
func TestDefaultSocketPathPrefersTheEnvironmentOverride(t *testing.T) {
	// Arrange.
	want := filepath.Join(t.TempDir(), "daemon-shim.sock")
	t.Setenv(SocketEnvVar, want)

	// Act.
	got, err := DefaultSocketPath()

	// Assert.
	if err != nil {
		t.Fatalf("DefaultSocketPath with %s=%s: %v", SocketEnvVar, want, err)
	}
	if got != want {
		t.Errorf("DefaultSocketPath = %q, want the override %q verbatim", got, want)
	}
}

// TestDefaultSocketPathFallsBackToTheHomeCacheSocket covers the PRODUCTION
// path: with no override, the socket is the one well-known location every
// deployed shim dials.
func TestDefaultSocketPathFallsBackToTheHomeCacheSocket(t *testing.T) {
	// Arrange — an empty override is "unset", not "the empty path".
	home := t.TempDir()
	t.Setenv(SocketEnvVar, "")
	t.Setenv("HOME", home)

	// Act.
	got, err := DefaultSocketPath()

	// Assert.
	if err != nil {
		t.Fatalf("DefaultSocketPath with no override: %v", err)
	}
	want := filepath.Join(home, ".cache", "agent-repl", "sock", "daemon-shim.sock")
	if got != want {
		t.Errorf("DefaultSocketPath = %q, want %q", got, want)
	}
}

// TestDefaultSocketPathRefusesARelativeOverride covers the REFUSAL. The daemon
// and the shims it spawns do not share a working directory, so a relative
// override would resolve to two different sockets and present as shims that
// dial forever and never arrive. Refusing names the mistake at the point it is
// made.
func TestDefaultSocketPathRefusesARelativeOverride(t *testing.T) {
	// Arrange.
	const relative = "sock/daemon-shim.sock"
	t.Setenv(SocketEnvVar, relative)

	// Act.
	got, err := DefaultSocketPath()

	// Assert.
	if err == nil {
		t.Fatalf("DefaultSocketPath resolved the relative override %q to %q instead of refusing it", relative, got)
	}
	if !strings.Contains(err.Error(), SocketEnvVar) {
		t.Errorf("the refusal reads %q, which never names %s: the operator is not told which setting to fix", err, SocketEnvVar)
	}
	if !strings.Contains(err.Error(), relative) {
		t.Errorf("the refusal reads %q, which never quotes the offending value %q", err, relative)
	}
}
