package harness

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"

	"claude-repld/internal/tempdirs/tempdirstest"
)

// A RUN ROOT IS ONE `go test` PROCESS'S WHOLE FOOTPRINT ON DISK, AND ITS
// LIFETIME IS HELD BY THE KERNEL.
//
// Every file a run writes — the built binaries, every ShortTempDir state root,
// and every t.TempDir (TMPDIR points into it for the whole run) — lives under
// one `/tmp/arrun*` directory, and the run holds an exclusive flock on
// `<root>/owner.lock` for as long as it lives.
//
// WHY. An ordinary run removes everything it made through `defer` and
// t.Cleanup. A run that does NOT end ordinarily — `go test`'s own -timeout
// panic, a SIGKILL, a host that ran out of disk mid-run — runs neither: its
// binaries, its per-test worlds and its daemons stay behind, and the daemons,
// reparented to launchd with their state roots gone, went on burning 50-95% of
// a core each until someone killed them by hand. A day of oversubscribed runs
// filled the owner's disk that way. A cleanup that only runs on the happy exit
// is not a guarantee.
//
// The flock is. The kernel drops it the instant the owning process is gone,
// however it went, so "this root's run is dead" is a fact the next run reads
// with one non-blocking flock — not a pid that may have been recycled, not an
// age guess. reclaimDeadRuns, the first thing MainAt does, kills every process
// still running out of a dead run's root and removes the root.

const ownerLockName = "owner.lock"

// runRootPrefix names every run root; reclaim considers only this prefix.
const runRootPrefix = "arrun"

// A runRootSpace is ONE directory's population of run roots and the lock that
// serializes their births against every reclaim of them. A reclaim only ever
// sees the roots of its own space.
//
// WHY THERE IS MORE THAN ONE. Every run on the host shares hostRunRoots, which
// is what lets the next run reclaim a dead one. The harness's own reclaim
// tests have to MAKE dead roots, and made in the host space those fixtures are
// indistinguishable from a real dead run: every concurrent run's reclaim --
// another worktree's suite starting up -- killed the fixture's leftover and
// removed its root while the test that owned it was still asserting, which
// was observed in a traced run. A test's fixtures live in a private space
// under its own temp dir instead, so no other run can ever see them and the
// test's own reclaim never touches another run's roots.
type runRootSpace struct {
	// base is the directory the roots are created in.
	base string
	// creationLock serializes a root's BIRTH against every reclaim.
	//
	// A root is created, then its owner lock is opened, then taken: three
	// steps, and a reclaim that looked in between saw a root with no lock
	// file, or with one nobody held yet, and read it as dead. `go test ./...`
	// starts this package's suite and the harness package's own tests at the
	// same instant, so one run's reclaim removed the other's brand-new root
	// out from under it. Holding this one fixed, never-removed lock across
	// both the birth and the scan makes that window unobservable: a reclaim
	// sees a root only before it exists or after its owner holds it.
	creationLock string
}

// hostRunRoots is the space every real run lives in: under /tmp and short, so
// the state roots beneath it carry unix sockets under a 103-byte budget. A
// scheduled run on a RAM disk moves it onto the disk's mount
// (tempdirstest.ShortBase), which is itself beneath /tmp; a set but unusable
// base is an error, never a silent /tmp.
func hostRunRoots() (runRootSpace, error) {
	base, err := tempdirstest.ShortBase(os.Getenv)
	if err != nil {
		return runRootSpace{}, err
	}
	return runRootSpace{base: base, creationLock: filepath.Join(base, "agent-repl-itest-runroot.lock")}, nil
}

// withCreationLock runs body holding the space's creation lock exclusively.
func (s runRootSpace) withCreationLock(body func() error) error {
	f, err := os.OpenFile(s.creationLock, os.O_CREATE|os.O_RDWR, 0o666)
	if err != nil {
		return fmt.Errorf("open the run-root creation lock: %w", err)
	}
	defer f.Close()
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX); err != nil {
		return fmt.Errorf("take the run-root creation lock: %w", err)
	}
	return body()
}

// runRoot is this process's run root; empty until MainAt lays it out.
var runRoot string

// newRunRoot creates a run root in the space and takes its owner lock. The
// returned file IS the lock: it must stay open for the whole run, and is never
// inherited by a child (Go opens with O_CLOEXEC).
func (s runRootSpace) newRunRoot() (dir string, lock *os.File, err error) {
	err = s.withCreationLock(func() error {
		dir, lock, err = s.newRunRootLocked()
		return err
	})
	return dir, lock, err
}

func (s runRootSpace) newRunRootLocked() (string, *os.File, error) {
	dir, err := os.MkdirTemp(s.base, runRootPrefix)
	if err != nil {
		return "", nil, fmt.Errorf("mkdir the run root: %w", err)
	}
	lock, err := os.OpenFile(filepath.Join(dir, ownerLockName), os.O_CREATE|os.O_RDWR, 0o600)
	if err != nil {
		return "", nil, fmt.Errorf("open the run root's owner lock: %w", err)
	}
	if err := syscall.Flock(int(lock.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		lock.Close()
		return "", nil, fmt.Errorf("take the run root's owner lock %s: %w", lock.Name(), err)
	}
	return dir, lock, nil
}

// reclaimDeadRuns removes every run root in the space whose owner is gone,
// after killing every process still running out of it. A root whose lock is
// HELD belongs to a live run — this suite's parallel sibling in another
// package, or another worktree's — and is left strictly alone.
//
// Every reclaim is reported on stderr, because a reclaim is evidence that an
// earlier run died without cleaning up, and that is worth seeing.
func (s runRootSpace) reclaimDeadRuns() error {
	return s.withCreationLock(s.reclaimDeadRunsLocked)
}

func (s runRootSpace) reclaimDeadRunsLocked() error {
	roots, err := filepath.Glob(filepath.Join(s.base, runRootPrefix+"*"))
	if err != nil {
		return fmt.Errorf("list the run roots: %w", err)
	}
	var errs []error
	for _, root := range roots {
		dead, err := runIsDead(root)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if !dead {
			continue
		}
		killed, err := killProcessesUnder(root)
		if err != nil {
			errs = append(errs, err)
			continue
		}
		if err := os.RemoveAll(root); err != nil {
			errs = append(errs, fmt.Errorf("remove the dead run root %s: %w", root, err))
			continue
		}
		fmt.Fprintf(os.Stderr, "harness: reclaimed the dead run root %s (killed %d leftover processes)\n", root, killed)
	}
	return errors.Join(errs...)
}

// runIsDead reports whether a run root's owner lock is free. A root with no
// lock file at all is one whose owner died between MkdirTemp and the lock,
// which is dead too.
func runIsDead(root string) (bool, error) {
	f, err := os.OpenFile(filepath.Join(root, ownerLockName), os.O_RDWR, 0)
	if errors.Is(err, os.ErrNotExist) {
		return true, nil
	}
	if err != nil {
		return false, fmt.Errorf("open the owner lock of %s: %w", root, err)
	}
	defer f.Close()
	switch err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); {
	case err == nil:
		// Released on close; nobody else can want it, its owner is gone.
		return true, nil
	case errors.Is(err, syscall.EWOULDBLOCK):
		return false, nil
	default:
		return false, fmt.Errorf("probe the owner lock of %s: %w", root, err)
	}
}

// killProcessesUnder SIGKILLs every process whose command line names a path
// under root: the daemons and fakes run out of its bin directory, and every
// one of them is handed its state root under it on argv.
func killProcessesUnder(root string) (int, error) {
	out, err := exec.Command("ps", "-axo", "pid=,args=").Output()
	if err != nil {
		return 0, fmt.Errorf("list processes to reclaim %s: %w", root, err)
	}
	prefix := root + string(filepath.Separator)
	killed := 0
	var errs []error
	for _, line := range strings.Split(string(out), "\n") {
		fields := strings.Fields(line)
		if len(fields) < 2 || !strings.Contains(line, prefix) {
			continue
		}
		pid, err := strconv.Atoi(fields[0])
		if err != nil || pid == os.Getpid() {
			continue
		}
		if err := syscall.Kill(pid, syscall.SIGKILL); err != nil && !errors.Is(err, syscall.ESRCH) {
			errs = append(errs, fmt.Errorf("kill leftover pid %d under %s: %w", pid, root, err))
			continue
		}
		killed++
	}
	return killed, errors.Join(errs...)
}
