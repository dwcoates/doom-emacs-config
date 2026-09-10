package main

import (
	"path/filepath"
	"testing"
)

func TestLockDirPrefersTheTestOverride(t *testing.T) {
	// Arrange
	env := func(k string) string {
		if k == EnvLockDir {
			return "/tmp/locks"
		}
		return ""
	}

	// Act
	got := LockDir(env, "/Users/someone")

	// Assert
	if got != "/tmp/locks" {
		t.Fatalf("LockDir = %q, want the AGENT_REPL_LOCK_DIR override", got)
	}
}

func TestLockDirFallsBackToTheContractedCacheDir(t *testing.T) {
	// Arrange
	env := func(string) string { return "" }

	// Act
	got := LockDir(env, "/Users/someone")

	// Assert
	want := filepath.Join("/Users/someone", ".cache", "agent-repl", "run")
	if got != want {
		t.Fatalf("LockDir = %q, want %q", got, want)
	}
}

func TestWorkspaceLockPathIsTheContractedName(t *testing.T) {
	// Arrange: md5("/w/one") truncated to eight hex characters is the key.
	dir := "/locks"

	// Act
	got := WorkspaceLockPath(dir, "/w/one")

	// Assert
	if filepath.Dir(got) != dir {
		t.Fatalf("WorkspaceLockPath dir = %q, want %q", filepath.Dir(got), dir)
	}
	name := filepath.Base(got)
	if len(name) != len("workspace-")+8+len(".lock") {
		t.Fatalf("WorkspaceLockPath name = %q, want workspace-<8 hex>.lock", name)
	}
}

func TestWorkspaceLockPathIgnoresPathSpelling(t *testing.T) {
	// Arrange / Act
	plain := WorkspaceLockPath("/locks", "/w/one")
	noisy := WorkspaceLockPath("/locks", "/w/./two/../one/")

	// Assert
	if plain != noisy {
		t.Fatalf("WorkspaceLockPath(%q) = %q and (%q) = %q, want one key for one cleaned path", "/w/one", plain, "/w/./two/../one/", noisy)
	}
}

func TestWorkspaceLockPathSeparatesDistinctWorkspaces(t *testing.T) {
	// Arrange / Act
	a := WorkspaceLockPath("/locks", "/w/one")
	b := WorkspaceLockPath("/locks", "/w/two")

	// Assert
	if a == b {
		t.Fatalf("WorkspaceLockPath collided on distinct dirs: %q", a)
	}
}

func TestSessionLockPathIsKeyedOnTheVendorSessionID(t *testing.T) {
	// Arrange / Act
	got := SessionLockPath("/locks", "abc123")

	// Assert
	want := filepath.Join("/locks", "session-abc123.lock")
	if got != want {
		t.Fatalf("SessionLockPath = %q, want %q", got, want)
	}
}
