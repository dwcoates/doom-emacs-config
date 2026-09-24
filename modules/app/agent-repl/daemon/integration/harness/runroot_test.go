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

// privateRunRoots is a run-root space of this test's own, so the dead roots it
// fabricates are invisible to every other run's reclaim and its own reclaim
// never reaches another run's roots.
func privateRunRoots(t *testing.T) runRootSpace {
	t.Helper()
	dir := t.TempDir()
	return runRootSpace{base: dir, creationLock: filepath.Join(dir, "creation.lock")}
}

// deadRoot lays out a run root in space whose owner is already gone: the lock
// file exists and nobody holds it.
func deadRoot(t *testing.T, space runRootSpace) string {
	t.Helper()
	root, lock, err := space.newRunRoot()
	if err != nil {
		t.Fatalf("newRunRoot: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(root) })
	lock.Close() // the "owner" dies
	return root
}

// liveRoot lays out a run root in space whose owner holds its lock until the
// test ends.
func liveRoot(t *testing.T, space runRootSpace) string {
	t.Helper()
	root, lock, err := space.newRunRoot()
	if err != nil {
		t.Fatalf("newRunRoot: %v", err)
	}
	t.Cleanup(func() { lock.Close(); _ = os.RemoveAll(root) })
	return root
}

// leftover is a process running out of a run root.
type leftover struct {
	cmd *exec.Cmd
	// exited is closed when the reaper's Wait returns, and err is its result.
	exited chan struct{}
	err    error
}

// leftoverUnder starts a process whose argv[0] lives under root, the way a
// daemon runs out of its root's bin directory.
func leftoverUnder(t *testing.T, root string) *leftover {
	t.Helper()
	bin := filepath.Join(root, "bin", "sleep")
	copyFile(t, "/bin/sleep", bin)
	l := &leftover{cmd: exec.Command(bin, "60"), exited: make(chan struct{})}
	if err := l.cmd.Start(); err != nil {
		t.Fatalf("start the leftover: %v", err)
	}
	go func() { l.err = l.cmd.Wait(); close(l.exited) }()
	t.Cleanup(func() { _ = l.cmd.Process.Kill(); <-l.exited })
	return l
}

// assertStillRunning fails if the process has exited. The exit is the
// reaper's own report, so an unexited process is a fact, not a guess.
func (l *leftover) assertStillRunning(t *testing.T) {
	t.Helper()
	select {
	case <-l.exited:
		t.Fatalf("pid %d exited (%v), want it untouched", l.cmd.Process.Pid, l.err)
	default:
	}
}

func TestRunIsDead(t *testing.T) {
	cases := []struct {
		name  string
		setup func(t *testing.T) (root string)
		want  bool
	}{
		{
			name:  "a held owner lock is a live run",
			setup: func(t *testing.T) string { return liveRoot(t, privateRunRoots(t)) },
			want:  false,
		},
		{
			name:  "a released owner lock is a dead run",
			setup: func(t *testing.T) string { return deadRoot(t, privateRunRoots(t)) },
			want:  true,
		},
		{
			name: "a root with no owner lock at all is a dead run",
			setup: func(t *testing.T) string {
				root, err := os.MkdirTemp(t.TempDir(), runRootPrefix)
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
	space := privateRunRoots(t)
	root := deadRoot(t, space)

	// Act
	if err := space.reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	if _, err := os.Stat(root); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("dead run root %s still present (stat err %v), want it removed", root, err)
	}
}

func TestReclaimDeadRunsLeavesALiveRootAlone(t *testing.T) {
	// Arrange
	space := privateRunRoots(t)
	root := liveRoot(t, space)

	// Act
	if err := space.reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("live run root %s: %v, want it untouched", root, err)
	}
}

func TestReclaimDeadRunsKillsAProcessRunningOutOfTheDeadRoot(t *testing.T) {
	// Arrange: a leftover of a run that is gone.
	space := privateRunRoots(t)
	l := leftoverUnder(t, deadRoot(t, space))

	// Act
	if err := space.reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	select {
	case <-l.exited:
		var ee *exec.ExitError
		if !errors.As(l.err, &ee) || ee.Sys().(syscall.WaitStatus).Signal() != syscall.SIGKILL {
			t.Fatalf("leftover exited with %v, want SIGKILL", l.err)
		}
	case <-time.After(DefaultTimeout):
		t.Fatalf("leftover still running after the reclaim")
	}
}

// TestReclaimDeadRunsNeverTouchesALiveRunsProcess is the cross-run guarantee
// within one space: a process running out of a root whose owner still holds
// its lock is another run's, and survives every reclaim.
func TestReclaimDeadRunsNeverTouchesALiveRunsProcess(t *testing.T) {
	// Arrange
	space := privateRunRoots(t)
	l := leftoverUnder(t, liveRoot(t, space))

	// Act
	if err := space.reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	l.assertStillRunning(t)
}

// TestReclaimDeadRunsNeverSeesAnotherSpacesRoots is what keeps a fabricated
// dead root away from every other run: a dead root in one space, with its
// leftover still running, is untouched by a reclaim of another.
func TestReclaimDeadRunsNeverSeesAnotherSpacesRoots(t *testing.T) {
	// Arrange
	theirs := privateRunRoots(t)
	root := deadRoot(t, theirs)
	l := leftoverUnder(t, root)
	ours := privateRunRoots(t)

	// Act
	if err := ours.reclaimDeadRuns(); err != nil {
		t.Fatalf("reclaimDeadRuns = %v", err)
	}

	// Assert
	l.assertStillRunning(t)
	if _, err := os.Stat(root); err != nil {
		t.Fatalf("the other space's dead root %s: %v, want it untouched", root, err)
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
