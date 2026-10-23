package recordlock

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"sync/atomic"
	"syscall"
	"testing"
)

// A record lock's guarantees are about OTHER processes, so these cases run a
// second one: this test binary re-executed as TestLockHelperProcess, which
// takes what its mode names, prints one line, and holds until its stdin
// closes.

const lockHelperEnv = "RECORDLOCK_HELPER"

// TestLockHelperProcess is the program the cases below run. Its modes:
//
//	hold PATH     hold the exclusive lock on PATH; print "ready <pid>"
//	shared PATH   hold a shared record lock on PATH; print "ready <pid>"
//	forked PATH   hold the lock, start a child that inherits its very
//	              descriptor, print "child <pid>", and EXIT at once
//	probe PATH    print "holder <pid>" for PATH and exit
func TestLockHelperProcess(t *testing.T) {
	mode := os.Getenv(lockHelperEnv)
	if mode == "" {
		return
	}
	path := os.Getenv(lockHelperEnv + "_PATH")
	fail := func(err error) {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(3)
	}
	switch mode {
	case "hold":
		if _, _, err := TryExclusive(path); err != nil {
			fail(err)
		}
	case "shared":
		f, err := OpenFile(path)
		if err != nil {
			fail(err)
		}
		if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_RDLCK)); err != nil {
			fail(err)
		}
	case "forked":
		rec, _, err := TryExclusive(path)
		if err != nil {
			fail(err)
		}
		// THE CHILD HOLDS A DESCRIPTOR OF THE LOCK'S OPEN FILE DESCRIPTION,
		// as a child the daemon forked holds one until its exec, and its
		// Darwin shadow past it. An flock would stay held through it after
		// this process exits.
		child := exec.Command("/bin/sleep", "300")
		child.ExtraFiles = []*os.File{rec.f}
		if err := child.Start(); err != nil {
			fail(err)
		}
		fmt.Printf("child %d\n", child.Process.Pid)
		os.Exit(0)
	case "probe":
		holder, err := HolderOf(path)
		if err != nil {
			fail(err)
		}
		fmt.Printf("holder %d\n", holder.PID)
		os.Exit(0)
	default:
		fail(fmt.Errorf("unknown helper mode %q", mode))
	}
	fmt.Printf("ready %d\n", os.Getpid())
	_, _ = io.Copy(io.Discard, os.Stdin)
	os.Exit(0)
}

// lockHelper is a running TestLockHelperProcess and the line it printed.
type lockHelper struct {
	cmd   *exec.Cmd
	stdin io.WriteCloser
	line  string
}

// startLockHelper runs the helper in MODE on PATH and reads its first line.
// A holding helper is let go at the end of the test.
func startLockHelper(t *testing.T, mode, path string) *lockHelper {
	t.Helper()
	cmd := exec.Command(os.Args[0], "-test.run=^TestLockHelperProcess$")
	cmd.Env = append(os.Environ(), lockHelperEnv+"="+mode, lockHelperEnv+"_PATH="+path)
	cmd.Stderr = os.Stderr
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatalf("helper stdin: %v", err)
	}
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatalf("helper stdout: %v", err)
	}
	if err := cmd.Start(); err != nil {
		t.Fatalf("start the helper: %v", err)
	}
	h := &lockHelper{cmd: cmd, stdin: stdin}
	t.Cleanup(func() {
		h.stdin.Close()
		_ = h.cmd.Wait()
	})
	line, err := bufio.NewReader(stdout).ReadString('\n')
	if err != nil {
		t.Fatalf("the helper (%s) printed nothing: %v", mode, err)
	}
	h.line = strings.TrimSpace(line)
	return h
}

// field answers the integer after WORD in the helper's line.
func (h *lockHelper) field(t *testing.T, word string) int {
	t.Helper()
	parts := strings.Fields(h.line)
	for i := 0; i+1 < len(parts); i++ {
		if parts[i] == word {
			n, err := strconv.Atoi(parts[i+1])
			if err != nil {
				t.Fatalf("helper line %q: %v", h.line, err)
			}
			return n
		}
	}
	t.Fatalf("helper line %q names no %q", h.line, word)
	return 0
}

func lockPath(t *testing.T) string {
	return filepath.Join(t.TempDir(), "x.lock")
}

// mustTake takes path's lock in this process, released at the test's end.
func mustTake(t *testing.T, path string) *Record {
	t.Helper()
	rec, _, err := TryExclusive(path)
	if err != nil {
		t.Fatalf("TryExclusive: %v", err)
	}
	t.Cleanup(func() { rec.Release() })
	return rec
}

func TestALockIsFreeOnceItsHolderExitsWhileAChildHoldsItsDescriptor(t *testing.T) {
	// Arrange: a holder that started a child inheriting the lock's
	// descriptor, then exited; the child is still alive.
	path := lockPath(t)
	helper := startLockHelper(t, "forked", path)
	child := helper.field(t, "child")
	t.Cleanup(func() {
		if p, err := os.FindProcess(child); err == nil {
			_ = p.Kill()
			_, _ = p.Wait()
		}
	})
	if err := helper.cmd.Wait(); err != nil {
		t.Fatalf("the holder's exit: %v", err)
	}
	if err := syscall.Kill(child, 0); err != nil {
		t.Fatalf("the child holding the descriptor is gone (%v); the case needs it alive", err)
	}

	// Act.
	rec, _, err := TryExclusive(path)

	// Assert: the dead holder's lock is free; the child's descriptor holds
	// nothing.
	if err != nil {
		t.Fatalf("TryExclusive = %v, want the lock free once its holder exited", err)
	}
	rec.Release()
}

func TestTryExclusive(t *testing.T) {
	tests := []struct {
		name string
		// hold takes the lock elsewhere and answers the holder expected.
		hold       func(t *testing.T, path string) int
		wantHolder bool
	}{
		{name: "a free lock is taken", hold: func(*testing.T, string) int { return 0 }},
		{name: "a lock another process holds is refused naming it", wantHolder: true,
			hold: func(t *testing.T, path string) int { return startLockHelper(t, "hold", path).field(t, "ready") }},
		{name: "a lock this process holds is refused naming this process", wantHolder: true,
			hold: func(t *testing.T, path string) int { mustTake(t, path); return os.Getpid() }},
		{name: "a lock released is free again",
			hold: func(t *testing.T, path string) int {
				if err := mustTake(t, path).Release(); err != nil {
					t.Fatalf("Release: %v", err)
				}
				return 0
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			path := lockPath(t)
			want := tt.hold(t, path)

			// Act.
			rec, holder, err := TryExclusive(path)

			// Assert.
			if tt.wantHolder {
				if !errors.Is(err, ErrWouldBlock) || holder.PID != want {
					t.Fatalf("TryExclusive = (%+v, %v), want ErrWouldBlock held by %d", holder, err, want)
				}
				return
			}
			if err != nil {
				t.Fatalf("TryExclusive = %v, want the lock taken", err)
			}
			rec.Release()
		})
	}
}

func TestTryExclusiveNeverCreatesADirectory(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), "gone")

	// Act.
	_, _, err := TryExclusive(filepath.Join(dir, "x.lock"))

	// Assert.
	if !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("TryExclusive = %v, want os.ErrNotExist", err)
	}
	if _, err := os.Stat(dir); !os.IsNotExist(err) {
		t.Fatalf("the directory exists after TryExclusive: %v", err)
	}
}

// Every way this package asks about a lock opens and closes a descriptor of
// its file, and closing ANY descriptor drops every record lock the process
// holds on it. Asked in the process that holds the lock, none of them may let
// it go.
func TestAskingAboutALockInTheHoldingProcessKeepsItHeld(t *testing.T) {
	asks := []struct {
		name string
		ask  func(path string)
	}{
		{"a second take", func(path string) { _, _, _ = TryExclusive(path) }},
		{"a shared probe", func(path string) { _, _, _ = ProbeShared(path) }},
		{"a holder lookup", func(path string) { _, _ = HolderOf(path) }},
	}
	for _, tt := range asks {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			path := lockPath(t)
			mustTake(t, path)

			// Act.
			tt.ask(path)

			// Assert: another process still sees it held by this one.
			if got := startLockHelper(t, "probe", path).field(t, "holder"); got != os.Getpid() {
				t.Fatalf("after %s: held by %d, want this process (%d)", tt.name, got, os.Getpid())
			}
		})
	}
}

func TestProbeShared(t *testing.T) {
	tests := []struct {
		name string
		// hold takes the lock elsewhere and answers the holder expected.
		hold     func(t *testing.T, path string) int
		wantHeld bool
	}{
		{name: "a free lock is not held", hold: func(*testing.T, string) int { return 0 }},
		{name: "another process's exclusive lock is held, naming it", wantHeld: true,
			hold: func(t *testing.T, path string) int { return startLockHelper(t, "hold", path).field(t, "ready") }},
		{name: "this process's lock is held, naming this process", wantHeld: true,
			hold: func(t *testing.T, path string) int { mustTake(t, path); return os.Getpid() }},
		{name: "another process's shared probe is not held",
			hold: func(t *testing.T, path string) int { startLockHelper(t, "shared", path); return 0 }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			path := lockPath(t)
			want := tt.hold(t, path)

			// Act.
			held, holder, err := ProbeShared(path)

			// Assert.
			if err != nil || held != tt.wantHeld || holder.PID != want {
				t.Fatalf("ProbeShared = (%v, %+v, %v), want (%v, pid %d, nil)", held, holder, err, tt.wantHeld, want)
			}
		})
	}
}

func TestHolderOfAnAbsentLockNamesNoneAndCreatesNothing(t *testing.T) {
	// Arrange.
	path := lockPath(t)

	// Act.
	holder, err := HolderOf(path)

	// Assert.
	if err != nil || holder.PID != 0 {
		t.Fatalf("HolderOf = (%+v, %v), want no holder", holder, err)
	}
	if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("stat %s = %v, want it never created", path, err)
	}
}

func TestBlockExclusiveTakesTheLockOnceThisProcessLetsItGo(t *testing.T) {
	// Arrange.
	path := lockPath(t)
	first := mustTake(t, path)
	f, err := OpenFile(path)
	if err != nil {
		t.Fatalf("OpenFile: %v", err)
	}
	got := make(chan error, 1)
	var released atomic.Bool

	// Act.
	go func() {
		rec, err := BlockExclusive(f, path)
		if err == nil && !released.Load() {
			err = errors.New("the wait took the lock while this process still held it")
		}
		if err == nil {
			err = rec.Release()
		}
		got <- err
	}()
	released.Store(true)
	if err := first.Release(); err != nil {
		t.Fatalf("Release: %v", err)
	}

	// Assert.
	if err := <-got; err != nil {
		t.Fatalf("BlockExclusive: %v", err)
	}
}

func TestBlockExclusiveOnADescriptorItCannotLockAnswersTheCause(t *testing.T) {
	// Arrange: a read-only descriptor cannot carry a write lock.
	path := lockPath(t)
	if err := os.WriteFile(path, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	f, err := os.Open(path)
	if err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err = BlockExclusive(f, path)

	// Assert: the cause, and nothing left held.
	if !errors.Is(err, syscall.EBADF) {
		t.Fatalf("BlockExclusive = %v, want EBADF", err)
	}
	mustTake(t, path)
}

func TestReleasingTwiceOrNilIsSafe(t *testing.T) {
	// Arrange.
	rec := mustTake(t, lockPath(t))
	if err := rec.Release(); err != nil {
		t.Fatal(err)
	}
	var none *Record

	// Act, Assert.
	if err := rec.Release(); err != nil {
		t.Fatalf("second Release = %v, want nil", err)
	}
	if err := none.Release(); err != nil {
		t.Fatalf("Release of nil = %v, want nil", err)
	}
	if _, err := rec.Stat(); err == nil {
		t.Fatal("Stat of a released lock = nil, want an error")
	}
}

func TestOpenFileCreatesAReadWriteFile(t *testing.T) {
	// Arrange.
	path := lockPath(t)

	// Act.
	f, err := OpenFile(path)

	// Assert: created, and lockable both ways.
	if err != nil {
		t.Fatalf("OpenFile: %v", err)
	}
	defer f.Close()
	for _, typ := range []int16{syscall.F_RDLCK, syscall.F_WRLCK} {
		if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(typ)); err != nil {
			t.Fatalf("lock type %d on the opened file: %v", typ, err)
		}
	}
}

// Every lock file this package opens goes through OpenFile: a hand-rolled
// open elsewhere is a drifted copy.
func TestEveryLockFileOpenGoesThroughOpenFile(t *testing.T) {
	body, err := os.ReadFile("recordlock.go")
	if err != nil {
		t.Fatalf("read recordlock.go: %v", err)
	}
	lines := strings.Split(string(body), "\n")
	for i, line := range lines {
		if strings.Contains(line, "os.OpenFile(") && !strings.Contains(lines[max(i-1, 0)], "func OpenFile") {
			t.Errorf("recordlock.go:%d opens a file by hand; use OpenFile: %s", i+1, strings.TrimSpace(line))
		}
	}
}

func TestHold(t *testing.T) {
	tests := []struct {
		name   string
		path   func(t *testing.T) string
		hold   bool // the lock is already held in this process
		wantOK bool
	}{
		{name: "a free lock is held", path: lockPath, wantOK: true},
		{name: "a held lock answers false", path: lockPath, hold: true},
		{name: "a missing directory is created", wantOK: true,
			path: func(t *testing.T) string { return filepath.Join(t.TempDir(), "a", "b", "x.lock") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			path := tt.path(t)
			if tt.hold {
				mustTake(t, path)
			}

			// Act.
			rec, ok, err := Hold(path)

			// Assert.
			if err != nil || ok != tt.wantOK {
				t.Fatalf("Hold = (%v, %v), want (%v, nil)", ok, err, tt.wantOK)
			}
			if ok && rec.Path() != path {
				t.Fatalf("Path = %q, want %q", rec.Path(), path)
			}
			rec.Release()
		})
	}
}

func TestHoldInNeverCreatesADirectory(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), "gone")

	// Act.
	_, ok, err := HoldIn(filepath.Join(dir, "x.lock"))

	// Assert.
	if !errors.Is(err, os.ErrNotExist) || ok {
		t.Fatalf("HoldIn = (%v, %v), want os.ErrNotExist", ok, err)
	}
	if _, err := os.Stat(dir); !os.IsNotExist(err) {
		t.Fatalf("the directory exists after HoldIn: %v", err)
	}
}

func TestHoldFailsWhenTheLockCannotBeToldAbout(t *testing.T) {
	// Arrange: the lock's directory is a file.
	parent := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(parent, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, ok, err := Hold(filepath.Join(parent, "x.lock"))

	// Assert.
	if err == nil || ok {
		t.Fatalf("Hold = (%v, %v), want an error", ok, err)
	}
}

// TestEveryDaemonKernelLockGoesThroughThisPackage pins the call sites to the
// one primitive: a production file of the daemon that takes a kernel lock of
// its own fails here. The one exception is sessionlock's PROBE of the flock a
// shim's holder process takes (shim-lock), which is that process's contract
// and is only ever probed here, never held.
func TestEveryDaemonKernelLockGoesThroughThisPackage(t *testing.T) {
	// Arrange.
	allowed := map[string]bool{
		filepath.Join("internal", "recordlock", "recordlock.go"): true,
		filepath.Join("internal", "sessionlock", "api.go"):       true,
	}
	root := filepath.Join("..", "..")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "integration" || d.Name() == "testdata") {
			return filepath.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		s := string(body)
		if (strings.Contains(s, "Flock(") || strings.Contains(s, "F_SETLK")) && !allowed[rel] {
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the daemon's sources: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("kernel locks taken outside recordlock in %v; take recordlock.Hold or recordlock.TryExclusive", offenders)
	}
}
