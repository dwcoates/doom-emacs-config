package e2e

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// realHome is the operator's home directory as it stood before the first test
// ran. TestMain sets it; nothing else may write it.
var realHome string

// homeIsolationViolation reports why a harness is NOT isolated from the
// operator's real state, or "" when it is.
//
// # Why this check exists
//
// Every path this package's daemons resolve — the shim socket
// (~/.cache/agent-repl/sock/daemon-shim.sock), the session lock directory
// (~/.cache/agent-repl/run), the state store (~/.cache/agent-repl/ssm/state.db)
// — is built by joining onto os.UserHomeDir. On a developer machine those are
// the LIVE daemon's paths, and shimlisten.Server.Listen removes the socket file
// before binding it. A harness that resolved the real path therefore unlinked
// the running daemon's listener, bound its own, and left every subsequently
// spawned shim dialling a path that no longer existed. The daemon kept its
// listener fd and never noticed; the operator saw sessions hang in AwaitReady
// forever. That is a production incident caused by running the test suite, and
// it has happened — twice.
//
// The individual harnesses each already isolate themselves. The failure mode is
// not a missing concept but a FORGOTTEN LINE: mergegeometry's harness built a
// private state directory and then handed os.Getenv("HOME") — still the real
// home — to the socket helper. Checking the two conditions at the one chokepoint
// every harness passes through turns that omission into a loud failure at the
// line that commits it, rather than a silent one an operator diagnoses days
// later.
//
// Two conditions, because $HOME and the socket directory are separately
// forgettable:
//
//   - home must have moved off realHome, which covers every resolution that
//     goes through os.UserHomeDir without passing through here (the session
//     lock, the state store).
//   - sockDir must not sit under realHome, which covers a harness that moved
//     $HOME correctly but still derived its socket from the real one.
func homeIsolationViolation(realHome, home, sockDir string) string {
	switch {
	case realHome == "":
		return "the real home directory was never captured: TestMain must set realHome before any test runs"
	case home == "":
		return "$HOME is empty, so os.UserHomeDir would fall back to the operator's account state"
	case sameOrUnder(realHome, home):
		return fmt.Sprintf(
			"$HOME is still %q, inside the operator's real home %q: the session lock, the state store and the shim socket would all resolve onto the LIVE daemon's paths",
			home, realHome)
	case sameOrUnder(realHome, sockDir):
		return fmt.Sprintf(
			"the shim socket directory %q sits inside the operator's real home %q: binding it would unlink the live daemon's socket",
			sockDir, realHome)
	}
	return ""
}

// sameOrUnder reports whether p is root itself or a path beneath it. The
// separator suffix on root keeps a sibling like "/Users/dodger" from matching
// the root "/Users/dodge".
func sameOrUnder(root, p string) bool {
	root = filepath.Clean(root)
	p = filepath.Clean(p)
	return p == root || strings.HasPrefix(p, root+string(filepath.Separator))
}

// requireIsolatedHome fails the test unless the harness has moved off the
// operator's real state. It is called from isolatedShimSocket, which every
// harness in this package routes through.
func requireIsolatedHome(t *testing.T, sockDir string) {
	t.Helper()
	if why := homeIsolationViolation(realHome, os.Getenv("HOME"), sockDir); why != "" {
		t.Fatalf("e2e harness is not isolated from the operator's live state: %s", why)
	}
}

func TestHomeIsolationViolationAcceptsAFullyIsolatedHarness(t *testing.T) {
	// Arrange — $HOME and the socket dir both moved onto a private temp dir.
	const real = "/Users/operator"
	isolated := "/tmp/agent-repl-harness-1"

	// Act
	got := homeIsolationViolation(real, isolated, isolated)

	// Assert
	if got != "" {
		t.Fatalf("a harness with $HOME and the socket dir both at %q was refused: %s", isolated, got)
	}
}

func TestHomeIsolationViolationRejectsAnUnmovedHome(t *testing.T) {
	// Arrange — the mergegeometry bug: a private socket dir, but $HOME never
	// moved, so the session lock and state store still resolve to real paths.
	const real = "/Users/operator"

	// Act
	got := homeIsolationViolation(real, real, "/tmp/agent-repl-harness-1")

	// Assert
	if !strings.Contains(got, "$HOME is still") {
		t.Fatalf("an unmoved $HOME was reported as %q, which never says $HOME is the problem", got)
	}
}

func TestHomeIsolationViolationRejectsASocketDirInsideTheRealHome(t *testing.T) {
	// Arrange — $HOME moved, but the socket was still derived from the real one.
	const real = "/Users/operator"

	// Act
	got := homeIsolationViolation(real, "/tmp/agent-repl-harness-1", real+"/.cache/agent-repl/sock")

	// Assert
	if !strings.Contains(got, "unlink the live daemon's socket") {
		t.Fatalf("a socket dir under the real home was reported as %q, which never names the unlink consequence", got)
	}
}

func TestHomeIsolationViolationRejectsAnEmptyHome(t *testing.T) {
	// Arrange — an unset $HOME makes os.UserHomeDir fall back to the account.
	const real = "/Users/operator"

	// Act
	got := homeIsolationViolation(real, "", "/tmp/agent-repl-harness-1")

	// Assert
	if !strings.Contains(got, "$HOME is empty") {
		t.Fatalf("an empty $HOME was reported as %q, which never says $HOME is unset", got)
	}
}

func TestHomeIsolationViolationRejectsAnUncapturedRealHome(t *testing.T) {
	// Arrange — realHome unset means TestMain did not run, so the guard cannot
	// tell an isolated path from a live one and must refuse rather than pass.
	// Act
	got := homeIsolationViolation("", "/tmp/agent-repl-harness-1", "/tmp/agent-repl-harness-1")

	// Assert
	if !strings.Contains(got, "never captured") {
		t.Fatalf("an uncaptured real home was reported as %q, which never says the capture is missing", got)
	}
}

func TestHomeIsolationViolationAcceptsASiblingOfTheRealHome(t *testing.T) {
	// Arrange — "/Users/operator2" shares a string prefix with the real home
	// but is not inside it; a prefix test without a separator would reject it.
	const real = "/Users/operator"
	sibling := "/Users/operator2"

	// Act
	got := homeIsolationViolation(real, sibling, sibling)

	// Assert
	if got != "" {
		t.Fatalf("the sibling directory %q was treated as inside %q: %s", sibling, real, got)
	}
}

// TestTestMainCapturedTheRealHome is the live half of the guard: the pure
// function above is only meaningful if TestMain actually populated realHome.
func TestTestMainCapturedTheRealHome(t *testing.T) {
	// Arrange — TestMain has already run for this binary.
	// Act
	got := realHome

	// Assert
	if got == "" {
		t.Fatal("realHome is empty: TestMain did not capture the real home, so requireIsolatedHome would refuse every harness")
	}
}
