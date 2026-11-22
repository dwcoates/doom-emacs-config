package daemonaddr

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"

	"agent-repld/internal/recordlock"
	"agent-repld/internal/recordlock/locktest"
)

// The record locks' guarantees are about OTHER processes, so these cases run
// a second one (locktest): this test binary re-executed as
// TestLockHelperProcess, which takes what its mode names, prints
// "ready <pid>", and holds until its stdin closes.

const lockHelperEnv = "DAEMONADDR_LOCK_HELPER"

// TestLockHelperProcess is the program the cases below run. Its modes:
//
//	serve PATH    hold the boot claim at PATH and its serving lock
//	shared PATH   hold a shared record lock on PATH
//	probe PATH    print who holds the boot claim and the serving lock
//	life PATH     hold the life lock of instance "helper" beside the
//	              daemon.addr PATH, and Release it when stdin closes
func TestLockHelperProcess(t *testing.T) {
	locktest.Run(lockHelperEnv, map[string]locktest.Mode{
		"serve": func(path string) error {
			lock, err := acquireBootLock(path)
			if err != nil {
				return err
			}
			return lock.takeServing()
		},
		"shared": func(path string) error {
			f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
			if err != nil {
				return err
			}
			return syscall.FcntlFlock(f.Fd(), syscall.F_SETLK, &syscall.Flock_t{Type: syscall.F_RDLCK})
		},
		"probe": func(path string) error {
			boot, err := recordlock.HolderOf(path)
			if err != nil {
				return err
			}
			serving, err := recordlock.HolderOf(ServingPath(filepath.Join(filepath.Dir(path), "daemon.addr")))
			if err != nil {
				return err
			}
			fmt.Printf("boot %d serving %d\n", boot.PID, serving.PID)
			os.Exit(0)
			return nil
		},
		"life": func(path string) error {
			life, err := HoldLife(path, "helper")
			if err != nil {
				return err
			}
			locktest.Hold()
			if err := life.Release(); err != nil {
				return err
			}
			os.Exit(0)
			return nil
		},
	})
}

// startLockHelper runs TestLockHelperProcess in MODE on PATH.
func startLockHelper(t *testing.T, mode, path string) *locktest.Process {
	t.Helper()
	return locktest.Start(t, "TestLockHelperProcess", lockHelperEnv, mode, path)
}

func TestARefusedClaimNamesTheProcessHoldingIt(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), LockName)
	holder := startLockHelper(t, "serve", path).Field(t, "ready")

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
	holder := startLockHelper(t, "serve", path).Field(t, "ready")

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
			if boot, serving := probe.Field(t, "boot"), probe.Field(t, "serving"); boot != os.Getpid() || serving != os.Getpid() {
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
