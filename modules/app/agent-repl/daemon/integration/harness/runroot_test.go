package harness

import (
	"errors"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
	"testing"
	"time"
)

// deadRoot lays out a run root whose owner is already gone: the lock file
// exists and nobody holds it.
func deadRoot(t *testing.T) string {
	t.Helper()
	root, lock, err := newRunRoot()
	if err != nil {
		t.Fatalf("newRunRoot: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	lock.Close() // the "owner" dies
	return root
}

func TestRunIsDead(t *testing.T) {
	cases := []struct {
		name  string
		setup func(t *testing.T) (root string)
		want  bool
	}{
		{
			name: "a held owner lock is a live run",
			setup: func(t *testing.T) string {
				root, lock, err := newRunRoot()
				if err != nil {
					t.Fatalf("newRunRoot: %v", err)
				}
				t.Cleanup(func() { lock.Close(); _ = os.RemoveAll(root) })
				return root
			},
			want: false,
		},
		{
			name:  "a released owner lock is a dead run",
			setup: deadRoot,
			want:  true,
		},
		{
			name: "a root with no owner lock at all is a dead run",
			setup: func(t *testing.T) string {
				root, err := os.MkdirTemp("/tmp", "arrun")
				if err != nil {
					t.Fatalf("mkdir: %v", err)
				}
				t.Cleanup(func() { _ = os.RemoveAll(root) })
				return root
			},
			want: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := tc.setup(t)

			// Act
			got, err := runIsDead(root)

			// Assert
			if err != nil {
				t.Fatalf("runIsDead = error %v", err)
			}
			if got != tc.want {
				t.Fatalf("runIsDead = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestReclaimDeadRunsRemovesADeadRoot(t *testing.T) {
	// Arrange
	root := deadRoot(t)

	// Act
	if err := reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	if _, err := os.Stat(root); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("dead run root %s still present (stat err %v), want it removed", root, err)
	}
}

func TestReclaimDeadRunsLeavesALiveRootAlone(t *testing.T) {
	// Arrange
	root, lock, err := newRunRoot()
	if err != nil {
		t.Fatalf("newRunRoot: %v", err)
	}
	t.Cleanup(func() { lock.Close(); _ = os.RemoveAll(root) })

	// Act
	if err := reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("live run root %s: %v, want it untouched", root, err)
	}
}

func TestReclaimDeadRunsKillsAProcessRunningOutOfTheDeadRoot(t *testing.T) {
	// Arrange: a process whose argv[0] lives under the dead root, the way a
	// leftover daemon runs out of the root's bin directory.
	root := deadRoot(t)
	bin := filepath.Join(root, "bin", "sleep")
	copyFile(t, "/bin/sleep", bin)
	cmd := exec.Command(bin, "60")
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the leftover: %v", err)
	}
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()
	t.Cleanup(func() { _ = cmd.Process.Kill() })

	// Act
	if err := reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	select {
	case err := <-exited:
		var ee *exec.ExitError
		if !errors.As(err, &ee) || ee.Sys().(syscall.WaitStatus).Signal() != syscall.SIGKILL {
			t.Fatalf("leftover exited with %v, want SIGKILL", err)
		}
	case <-time.After(DefaultTimeout):
		t.Fatalf("leftover under %s still running after the reclaim", root)
	}
}

func copyFile(t *testing.T, from, to string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(to), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	src, err := os.Open(from)
	if err != nil {
		t.Fatalf("open %s: %v", from, err)
	}
	defer src.Close()
	dst, err := os.OpenFile(to, os.O_CREATE|os.O_WRONLY, 0o755)
	if err != nil {
		t.Fatalf("create %s: %v", to, err)
	}
	defer dst.Close()
	if _, err := io.Copy(dst, src); err != nil {
		t.Fatalf("copy: %v", err)
	}
}
