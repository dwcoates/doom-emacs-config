package harness

import (
	"path/filepath"
	"testing"
)

func TestShimInfoFlagFindsAValue(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js", "--listen", "/s.sock", "--log-fd", "3", "--fake"}}

	// Act
	got, ok := info.Flag("--listen")

	// Assert
	if !ok || got != "/s.sock" {
		t.Fatalf("Flag(--listen) = %q, %v, want the socket path", got, ok)
	}
}

func TestShimInfoFlagReportsAnAbsentFlag(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js"}}

	// Act
	_, ok := info.Flag("--listen")

	// Assert
	if ok {
		t.Fatal("Flag(--listen) reported present, want absent")
	}
}

func TestShimInfoHasFlagFindsABareFlag(t *testing.T) {
	// Arrange
	info := ShimInfo{Argv: []string{"fakeshim", "main.js", "--fake"}}

	// Act / Assert
	if !info.HasFlag("--fake") {
		t.Fatal("HasFlag(--fake) = false, want true")
	}
	if info.HasFlag("--real") {
		t.Fatal("HasFlag(--real) = true, want false")
	}
}

func TestWorkspaceLockPathMatchesTheShimContract(t *testing.T) {
	// Arrange / Act
	got := WorkspaceLockPath("/locks", "/w/one")

	// Assert
	name := filepath.Base(got)
	if len(name) != len("workspace-")+8+len(".lock") {
		t.Fatalf("WorkspaceLockPath = %q, want workspace-<8 hex>.lock", name)
	}
}

func TestWorkspaceLockPathIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := WorkspaceLockPath("/locks", "/w/one")
	noisy := WorkspaceLockPath("/locks", "/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("WorkspaceLockPath = %q and %q, want one key per cleaned path", plain, noisy)
	}
}

func TestProfileFileNameIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := profileFileName("/w/one")
	noisy := profileFileName("/w/two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("profileFileName = %q and %q, want one name per cleaned path", plain, noisy)
	}
}
