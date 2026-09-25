package livelock

import (
	"os"
	"path/filepath"
	"syscall"
	"testing"
)

func TestMain(m *testing.M) {
	if err := os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1"); err != nil {
		panic(err)
	}
	os.Exit(m.Run())
}

// lockFile creates path and takes a flock on it through its own descriptor,
// exactly as shim-lock does, releasing it at cleanup.
func lockFile(t *testing.T, path string, how int) {
	t.Helper()
	file, err := os.OpenFile(path, os.O_RDWR|os.O_CREATE, 0o600)
	if err != nil {
		t.Fatalf("creating %s: %v", path, err)
	}
	if err := syscall.Flock(int(file.Fd()), how|syscall.LOCK_NB); err != nil {
		t.Fatalf("flock %s: %v", path, err)
	}
	t.Cleanup(func() {
		if err := file.Close(); err != nil {
			t.Errorf("closing %s: %v", path, err)
		}
	})
}

func TestPathIsTheSharedWorkspaceLockSpelling(t *testing.T) {
	// Arrange.
	dir := "/run/agent-repl"

	// Act.
	got := Path(dir, "e3c68b4e")

	// Assert.
	if want := "/run/agent-repl/workspace-e3c68b4e.lock"; got != want {
		t.Fatalf("Path = %q, want %q", got, want)
	}
}

func TestHeld(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, path string)
		want    bool
	}{
		{
			name:    "an absent lock file is not held",
			arrange: func(t *testing.T, path string) {},
			want:    false,
		},
		{
			name: "a lock file nobody holds is not held",
			arrange: func(t *testing.T, path string) {
				if err := os.WriteFile(path, nil, 0o600); err != nil {
					t.Fatalf("creating %s: %v", path, err)
				}
			},
			want: false,
		},
		{
			name:    "an exclusive flock is held",
			arrange: func(t *testing.T, path string) { lockFile(t, path, syscall.LOCK_EX) },
			want:    true,
		},
		{
			name:    "a shared flock is held",
			arrange: func(t *testing.T, path string) { lockFile(t, path, syscall.LOCK_SH) },
			want:    true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := filepath.Join(t.TempDir(), "workspace-k.lock")
			tc.arrange(t, path)

			// Act.
			held, errs := Held([]string{path})

			// Assert.
			if err := errs[path]; err != nil {
				t.Fatalf("Held(%s) failed: %v", path, err)
			}
			if held[path] != tc.want {
				t.Fatalf("Held(%s) = %t, want %t", path, held[path], tc.want)
			}
		})
	}
}

func TestTheProbeDoesNotTakeTheLock(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "workspace-k.lock")
	if err := os.WriteFile(path, nil, 0o600); err != nil {
		t.Fatalf("creating %s: %v", path, err)
	}

	// Act.
	Held([]string{path})

	// Assert: a claimant that follows the probe takes the lock at once.
	lockFile(t, path, syscall.LOCK_EX)
}

func TestAnUnreadableLockFileIsAnErrorNotAnAnswer(t *testing.T) {
	// Arrange.
	if os.Geteuid() == 0 {
		t.Skip("root reads a mode-000 file, so there is no unreadable file to probe")
	}
	path := filepath.Join(t.TempDir(), "workspace-k.lock")
	if err := os.WriteFile(path, nil, 0o000); err != nil {
		t.Fatalf("creating %s: %v", path, err)
	}

	// Act.
	held, errs := Held([]string{path})

	// Assert.
	if errs[path] == nil {
		t.Fatalf("Held answered %t for a lock file it could not open", held[path])
	}
	if _, answered := held[path]; answered {
		t.Fatal("a path that could not be probed was also given an answer")
	}
}
