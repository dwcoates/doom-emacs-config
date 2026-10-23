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

	"agent-repld/internal/recordlock"
)

// The record locks' guarantees are about OTHER processes, so these cases run
// a second one: this test binary re-executed as TestLockHelperProcess, which
// takes what its mode names, prints "ready <pid>", and holds
// until its stdin closes.

const lockHelperEnv = "DAEMONADDR_LOCK_HELPER"

// TestLockHelperProcess is the program the cases below run. Its modes:
//
//	serve PATH    hold the boot claim at PATH and its serving lock
//	shared PATH   hold a shared record lock on PATH
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
		if err := syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, &syscall.Flock_t{Type: syscall.F_RDLCK}); err != nil {
			fail(err)
		}
	case "probe":
		boot, err := recordlock.HolderOf(path)
		if err != nil {
			fail(err)
		}
		serving, err := recordlock.HolderOf(ServingPath(filepath.Join(filepath.Dir(path), "daemon.addr")))
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
		{"a holder lookup", func(path string) { _, _ = recordlock.HolderOf(path); _, _ = recordlock.HolderOf(servingLockPath(path)) }},
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
	holder, err := recordlock.HolderOf(path)

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

// Every lock file this package opens goes through recordlock.OpenFile: a
// hand-rolled open here is a drifted copy, and closing one would drop a held
// record lock.
func TestEveryLockFileOpenGoesThroughRecordlock(t *testing.T) {
	for _, name := range []string{"lock.go", "recordlock.go", "claim.go"} {
		body, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		for i, line := range strings.Split(string(body), "\n") {
			if strings.Contains(line, "os.OpenFile(") {
				t.Errorf("%s:%d opens a file by hand; use recordlock.OpenFile: %s", name, i+1, strings.TrimSpace(line))
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
