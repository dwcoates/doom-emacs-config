package sessionlock

import (
	"os"
	"path/filepath"
	"syscall"
	"testing"
)

// lockPath makes a lock file in a temp dir.
func lockPath(t *testing.T) string {
	t.Helper()
	return filepath.Join(t.TempDir(), "session-s_abc.lock")
}

// hold takes the lock the way a shim does and keeps it until cleanup, so the
// probe under test sees a genuinely held lock rather than a simulated one.
func hold(t *testing.T, path string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("flock: %v", err)
	}
	t.Cleanup(func() {
		_ = syscall.Flock(int(f.Fd()), syscall.LOCK_UN)
		f.Close()
	})
}

func TestHeldIsFalseWhenNoLockFileExists(t *testing.T) {
	// Arrange: a session no shim has ever claimed.
	path := lockPath(t)

	// Act
	held, err := HeldAt(path)

	// Assert: a free session, not an error.
	if err != nil {
		t.Fatalf("HeldAt: %v", err)
	}
	if held {
		t.Fatal("held = true for a session with no lock file")
	}
}

func TestHeldIsTrueWhileAnotherHolderHasTheLock(t *testing.T) {
	// Arrange: a live shim holding its session lock.
	path := lockPath(t)
	hold(t, path)

	// Act
	held, err := HeldAt(path)

	// Assert
	if err != nil {
		t.Fatalf("HeldAt: %v", err)
	}
	if !held {
		t.Fatal("held = false while the lock was held — the daemon would spawn a duplicate shim")
	}
}

func TestHeldIsFalseAfterTheHolderReleases(t *testing.T) {
	// Arrange: a lock file left behind by a shim that has exited. The kernel
	// releases the lock on death, so the FILE existing must not read as held —
	// otherwise a dead session could never be respawned.
	path := lockPath(t)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("flock: %v", err)
	}
	f.Close() // closing drops the lock, exactly as process death does

	// Act
	held, err := HeldAt(path)

	// Assert
	if err != nil {
		t.Fatalf("HeldAt: %v", err)
	}
	if held {
		t.Fatal("held = true for a stale lock file whose holder is gone")
	}
}

func TestProbingDoesNotRetainTheLock(t *testing.T) {
	// Arrange: the daemon probes; it must never hold a session lock itself,
	// because the lock tracks the SHIM's life, not the daemon's.
	path := lockPath(t)
	f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("open: %v", err)
	}
	f.Close()

	// Act
	if _, err := HeldAt(path); err != nil {
		t.Fatalf("HeldAt: %v", err)
	}

	// Assert: a shim can still take it afterwards.
	g, err := os.OpenFile(path, os.O_RDWR, 0o644)
	if err != nil {
		t.Fatalf("reopen: %v", err)
	}
	defer g.Close()
	if err := syscall.Flock(int(g.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		t.Fatalf("the probe left the lock held: %v", err)
	}
	_ = syscall.Flock(int(g.Fd()), syscall.LOCK_UN)
}

// TestWorkspaceLockPathIsPinned pins the exact file name a workspace derives,
// because the Node side derives it independently: agent-shim's
// test/session-lock.test.ts asserts these same literals, so a change to either
// derivation fails on both sides instead of silently handing the daemon and the
// shim two different locks over one workspace.
func TestWorkspaceLockPathIsPinned(t *testing.T) {
	tests := []struct {
		name string
		cwd  string
		want string
	}{
		{
			name: "worktree path",
			cwd:  "/Users/dodgecoates/.config/doom-worktrees/model-selection-convergence-hwx",
			want: "workspace-0b96ccc5.lock",
		},
		{
			name: "a trailing slash names the same workspace",
			cwd:  "/Users/dodgecoates/.config/doom-worktrees/model-selection-convergence-hwx/",
			want: "workspace-0b96ccc5.lock",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act
			p, err := WorkspaceLockPath(tc.cwd)
			if err != nil {
				t.Fatalf("WorkspaceLockPath: %v", err)
			}

			// Assert
			if got := filepath.Base(filepath.Dir(p)); got != "run" {
				t.Fatalf("lock dir = %q, want run", got)
			}
			if got := filepath.Base(p); got != tc.want {
				t.Fatalf("lock file = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestWorkspaceLockPathRefusesAnEmptyWorkspace: an empty cwd would hash to the
// path cleaner's "." and give every unnamed caller ONE shared lock.
func TestWorkspaceLockPathRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange / Act
	_, err := WorkspaceLockPath("")

	// Assert
	if err == nil {
		t.Fatal("WorkspaceLockPath(\"\") returned no error; an unnamed workspace must not resolve to a lock")
	}
}

// TestWorkspaceLockHeldSeesALiveHolder proves the workspace probe contends with
// the same flock a shim takes, which is the whole basis for refusing to spawn.
func TestWorkspaceLockHeldSeesALiveHolder(t *testing.T) {
	// Arrange
	cwd := t.TempDir()
	path, err := WorkspaceLockPath(cwd)
	if err != nil {
		t.Fatalf("WorkspaceLockPath: %v", err)
	}
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	t.Cleanup(func() { _ = os.Remove(path) })
	hold(t, path)

	// Act
	held, err := WorkspaceLockHeld(cwd)

	// Assert
	if err != nil {
		t.Fatalf("WorkspaceLockHeld: %v", err)
	}
	if !held {
		t.Fatal("held = false while a shim held the workspace lock — the daemon would spawn a duplicate")
	}
}

func TestPathIsUnderTheRunDirectory(t *testing.T) {
	// Arrange / Act
	p, err := Path("s_abc")
	if err != nil {
		t.Fatalf("Path: %v", err)
	}

	// Assert: locks live beside sock/ and store/, not among the sockets.
	if got := filepath.Base(filepath.Dir(p)); got != "run" {
		t.Fatalf("lock dir = %q, want run", got)
	}
	if got := filepath.Base(p); got != "session-s_abc.lock" {
		t.Fatalf("lock file = %q", got)
	}
}
