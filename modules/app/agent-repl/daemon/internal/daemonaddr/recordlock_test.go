package daemonaddr

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
	"syscall"
	"testing"
)

// The record locks' guarantees are about OTHER processes, so these cases run
// a second one: this test binary re-executed as TestLockHelperProcess, which
// takes what its mode names, prints "ready <pid>" or "child <pid>", and holds
// until its stdin closes.

const lockHelperEnv = "DAEMONADDR_LOCK_HELPER"

// TestLockHelperProcess is the program the cases below run. Its modes:
//
//	serve PATH    hold the boot claim at PATH and its serving lock
//	shared PATH   hold a shared record lock on PATH
//	forked PATH   hold the boot claim, start a child that inherits the
//	              claim's descriptor, print the child, and EXIT at once
//	probe PATH    print who holds the boot claim and the serving lock
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
	case "serve":
		lock, err := acquireBootLock(path)
		if err != nil {
			fail(err)
		}
		if err := lock.takeServing(); err != nil {
			fail(err)
		}
	case "shared":
		f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
		if err != nil {
			fail(err)
		}
		if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(syscall.F_RDLCK)); err != nil {
			fail(err)
		}
	case "forked":
		lock, err := acquireBootLock(path)
		if err != nil {
			fail(err)
		}
		// THE CHILD HOLDS A DESCRIPTOR OF THE CLAIM'S OPEN FILE DESCRIPTION,
		// as a child the daemon forked holds one until its exec. A flock
		// would stay held through it after this process exits.
		child := exec.Command("/bin/sleep", "300")
		child.ExtraFiles = []*os.File{lock.rec.f}
		if err := child.Start(); err != nil {
			fail(err)
		}
		fmt.Printf("child %d\n", child.Process.Pid)
		os.Exit(0)
	case "probe":
		boot, err := holderOf(path)
		if err != nil {
			fail(err)
		}
		serving, err := holderOf(ServingPath(filepath.Join(filepath.Dir(path), "daemon.addr")))
		if err != nil {
			fail(err)
		}
		fmt.Printf("boot %d serving %d\n", boot.PID, serving.PID)
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

func TestAClaimIsFreeOnceItsHolderExitsWhileAChildHoldsItsDescriptor(t *testing.T) {
	// Arrange: a holder that started a child inheriting the claim's
	// descriptor, then exited; the child is still alive.
	path := filepath.Join(t.TempDir(), LockName)
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
	lock, err := acquireBootLock(path)

	// Assert: the dead holder's claim is free; the child's descriptor holds
	// nothing.
	if err != nil {
		t.Fatalf("acquireBootLock = %v, want the claim free once its holder exited", err)
	}
	lock.release()
}

func TestARefusedClaimNamesTheProcessHoldingIt(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	holder := startLockHelper(t, "serve", path).field(t, "ready")

	// Act.
	_, err := acquireBootLock(path)

	// Assert.
	var held *HeldError
	if !errors.As(err, &held) || !errors.Is(err, ErrClaimed) || held.Holder.PID != holder {
		t.Fatalf("acquireBootLock = %v, want ErrClaimed held by pid %d", err, holder)
	}
}

func TestAServingIncumbentsRefusalNamesTheServingLocksHolder(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	holder := startLockHelper(t, "serve", path).field(t, "ready")

	// Act.
	_, verdict, err := acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, nil)

	// Assert.
	var held *HeldError
	if verdict != VerdictIncumbentServing || !errors.As(err, &held) || held.Holder.PID != holder || held.Path != servingLockPath(path) {
		t.Fatalf("acquireBootLockWithin = (%q, %v), want %q naming pid %d on the serving lock", verdict, err, VerdictIncumbentServing, holder)
	}
	if !strings.Contains(err.Error(), fmt.Sprintf("held by pid %d", holder)) {
		t.Fatalf("error %q, want the holder named in it", err)
	}
}

func TestAHolderInThisProcessIsNamedAsThisProcess(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	lock, err := acquireBootLock(path)
	if err != nil {
		t.Fatalf("acquireBootLock: %v", err)
	}
	defer lock.release()

	// Act.
	_, err = acquireBootLock(path)

	// Assert.
	var held *HeldError
	if !errors.As(err, &held) || held.Holder.PID != os.Getpid() {
		t.Fatalf("second acquireBootLock = %v, want ErrClaimed held by this process", err)
	}
}

// Every way this package asks about a lock opens and closes a descriptor of
// its file, and closing ANY descriptor drops every record lock the process
// holds on it. Asked in the process that holds the claim, none of them may
// let it go.
func TestAskingAboutTheLocksInTheHoldingProcessKeepsThemHeld(t *testing.T) {
	asks := []struct {
		name string
		ask  func(path string)
	}{
		{"a second take", func(path string) { _, _ = acquireBootLock(path) }},
		{"a bounded take", func(path string) { _, _, _ = acquireBootLockWithin(path, ClaimWaitBound, BootOrdinary, nil) }},
		{"a claim probe", func(path string) { _ = ProbeBootClaim(filepath.Join(filepath.Dir(path), "daemon.addr")) }},
		{"a serving probe", func(path string) { _, _ = servingHeld(servingLockPath(path)) }},
		{"a holder lookup", func(path string) { _, _ = holderOf(path); _, _ = holderOf(servingLockPath(path)) }},
	}
	for _, tt := range asks {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: this process holds the claim and the serving lock.
			path := filepath.Join(t.TempDir(), LockName)
			servingIncumbent(t, path)

			// Act.
			tt.ask(path)

			// Assert: another process still sees both held by this one.
			probe := startLockHelper(t, "probe", path)
			if boot, serving := probe.field(t, "boot"), probe.field(t, "serving"); boot != os.Getpid() || serving != os.Getpid() {
				t.Fatalf("after %s: boot claim held by %d, serving lock by %d; want both by this process (%d)", tt.name, boot, serving, os.Getpid())
			}
		})
	}
}

func TestAProbeBesideAnotherProcessesSharedProbeReadsTheLockFree(t *testing.T) {
	// Arrange: another process's shared probe, held.
	path := servingLockPath(filepath.Join(t.TempDir(), LockName))
	startLockHelper(t, "shared", path)

	// Act.
	held, err := servingHeld(path)

	// Assert.
	if err != nil || held {
		t.Fatalf("servingHeld = (%v, %v), want (false, nil) beside another process's probe", held, err)
	}
}

func TestAFreeLockNamesNoHolder(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)

	// Act.
	holder, err := holderOf(path)

	// Assert.
	if err != nil || holder.PID != 0 {
		t.Fatalf("holderOf = (%+v, %v), want no holder", holder, err)
	}
}

func TestAHeldErrorWithNoNamedHolderSaysSo(t *testing.T) {
	// Arrange.
	err := &HeldError{Path: "/state/daemon.lock", Detail: "its holder is serving"}

	// Act.
	got := err.Error()

	// Assert.
	if got != "another daemon holds the boot claim: /state/daemon.lock (its holder is serving; the kernel named no holder)" {
		t.Fatalf("Error = %q", got)
	}
}

func TestOpenLockFileCreatesAReadWriteFile(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)

	// Act.
	f, err := openLockFile(path)

	// Assert: created, and lockable both ways.
	if err != nil {
		t.Fatalf("openLockFile: %v", err)
	}
	defer f.Close()
	for _, typ := range []int16{syscall.F_RDLCK, syscall.F_WRLCK} {
		if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, wholeFile(typ)); err != nil {
			t.Fatalf("lock type %d on the opened file: %v", typ, err)
		}
	}
}

// Every lock file this package opens goes through openLockFile: a
// hand-rolled open elsewhere is a drifted copy.
func TestEveryLockFileOpenGoesThroughOpenLockFile(t *testing.T) {
	for _, name := range []string{"lock.go", "recordlock.go", "claim.go"} {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		lines := strings.Split(string(body), "\n")
		for i, line := range lines {
			if strings.Contains(line, "os.OpenFile(") && !strings.Contains(lines[max(i-1, 0)], "func openLockFile") {
				t.Errorf("%s:%d opens a file by hand; use openLockFile: %s", name, i+1, strings.TrimSpace(line))
			}
		}
	}
}

func TestLookingUpTheHoldersOfAbsentLocksCreatesNoFile(t *testing.T) {
	// Arrange.
	addrPath := filepath.Join(t.TempDir(), "daemon.addr")

	// Act.
	boot, serving, err := LockHolders(addrPath)

	// Assert.
	if err != nil || boot.PID != 0 || serving.PID != 0 {
		t.Fatalf("LockHolders = (%+v, %+v, %v), want no holders", boot, serving, err)
	}
	for _, path := range []string{LockPath(addrPath), ServingPath(addrPath)} {
		if _, err := os.Stat(path); !errors.Is(err, os.ErrNotExist) {
			t.Fatalf("stat %s = %v, want it never created", path, err)
		}
	}
}
