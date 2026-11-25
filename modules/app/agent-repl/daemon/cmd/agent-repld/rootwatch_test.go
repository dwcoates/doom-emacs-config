package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/recordlock/locktest"
)

// fakeChanges is the watch's change source in the test's hands: each send
// is one change reported, an error one failed watch.
type fakeChanges struct {
	changes chan error
	stopped chan struct{}
}

func newFakeChanges() *fakeChanges {
	return &fakeChanges{changes: make(chan error), stopped: make(chan struct{})}
}

func (f *fakeChanges) source(context.Context) (<-chan error, func()) {
	return f.changes, func() { close(f.stopped) }
}

func TestRootWatch(t *testing.T) {
	errLoss := fmt.Errorf("%w: the state root %q is gone", daemonaddr.ErrVanished, "/tmp/gone")
	errUndecided := errors.New("stat the state root: permission denied")
	errWatch := errors.New("kqueue: too many open files")
	cases := []struct {
		name string
		// script answers each verify in turn; reports are what the change
		// source reports after the first verify, one per later verify. A
		// watch still running once the script is spent is cancelled.
		script        []error
		reports       []error
		wantStoodDown bool
		wantErrors    int
	}{
		{name: "a healthy root is verified once the watch is taken and at each change",
			script: []error{nil, nil, nil}, reports: []error{nil, nil}},
		{name: "a loss found once the watch is taken stands the daemon down at ERROR",
			script: []error{errLoss}, wantStoodDown: true, wantErrors: 1},
		{name: "a loss at a change stands the daemon down at ERROR",
			script: []error{nil, errLoss}, reports: []error{nil}, wantStoodDown: true, wantErrors: 1},
		{name: "a check that cannot tell is an ERROR, and the next change checks again",
			script: []error{errUndecided, nil}, reports: []error{nil}, wantErrors: 1},
		{name: "a failed watch over an intact root is an ERROR, and the watch ends",
			script: []error{nil, nil}, reports: []error{errWatch}, wantErrors: 1},
		{name: "a failed watch over a lost root stands the daemon down",
			script: []error{nil, errLoss}, reports: []error{kernelevent.ErrNothingToWatch}, wantStoodDown: true, wantErrors: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			src := newFakeChanges()
			log := dlog.NewTestLogger()
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			verified := make(chan struct{}, len(tc.script))
			calls := 0
			var stoodDown error
			w := rootWatch{
				verify: func() error {
					err := tc.script[calls]
					calls++
					verified <- struct{}{}
					return err
				},
				changes: src.source,
				log:     log,
				standDown: func(cause error) {
					stoodDown = cause
				},
			}
			done := make(chan error, 1)

			// Act
			go func() { done <- w.run(ctx) }()
			<-verified
			for _, report := range tc.reports {
				src.changes <- report
				<-verified
			}
			if !tc.wantStoodDown {
				cancel()
			}
			err := <-done

			// Assert
			if calls != len(tc.script) {
				t.Fatalf("verify calls = %d, want %d", calls, len(tc.script))
			}
			if tc.wantStoodDown != (stoodDown != nil) || tc.wantStoodDown != (err != nil) {
				t.Fatalf("stood down with %v, run returned %v; want a stand-down: %v", stoodDown, err, tc.wantStoodDown)
			}
			if tc.wantStoodDown && !errors.Is(stoodDown, daemonaddr.ErrVanished) {
				t.Fatalf("stand-down cause = %v, want the loss", stoodDown)
			}
			errorRecords := 0
			for _, r := range log.Records() {
				if r.Level == dlog.LevelError && r.Operation == "daemon.cmd.state_root" {
					errorRecords++
				}
			}
			if errorRecords != tc.wantErrors {
				t.Fatalf("ERROR records = %d, want %d", errorRecords, tc.wantErrors)
			}
			select {
			case <-src.stopped:
			default:
				t.Fatalf("the watch's change source was not stopped")
			}
		})
	}
}

// TestALostRootNamesWhatVanished pins that the stand-down record carries the
// loss itself, so the log says WHICH of the root, the lock or the address went.
func TestALostRootNamesWhatVanished(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	loss := fmt.Errorf("%w: daemon.lock %q is gone", daemonaddr.ErrVanished, "/tmp/root/daemon.lock")
	w := rootWatch{
		verify:    func() error { return loss },
		changes:   newFakeChanges().source,
		log:       log,
		standDown: func(error) {},
	}

	// Act
	_ = w.run(context.Background())

	// Assert
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError && r.Context["cause"] == loss.Error() {
			return
		}
	}
	t.Fatalf("records = %+v, want an ERROR whose cause is %q", log.Records(), loss.Error())
}

func TestRootWatchPathsNameTheRootItsParentAndTheAddress(t *testing.T) {
	// Act
	got := rootWatchPaths("/state/root", "/state/root/daemon.addr")

	// Assert
	want := []string{"/state", "/state/root", "/state/root/daemon.addr"}
	if !slices.Equal(got, want) {
		t.Fatalf("rootWatchPaths = %v, want %v", got, want)
	}
}

// TestRootWatchPathsNameNoLockFile pins that the watch opens no lock file: a
// watch's descriptor closing drops this process's record locks on the file
// (2026-11-22: the boot claim went to the successor before the incumbent
// exited, and its exit withdrew the successor's daemon.addr).
func TestRootWatchPathsNameNoLockFile(t *testing.T) {
	addrPath := "/state/root/daemon.addr"
	cases := []struct {
		name string
		lock string
	}{
		{name: "the boot lock", lock: daemonaddr.LockPath(addrPath)},
		{name: "the serving lock", lock: daemonaddr.ServingPath(addrPath)},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := rootWatchPaths("/state/root", addrPath)

			// Assert
			if slices.Contains(got, tc.lock) {
				t.Fatalf("rootWatchPaths = %v, want no %s", got, tc.lock)
			}
		})
	}
}

// TestFollowRootPathsRefusesEveryLockFile pins the refusal at the watch
// itself: a root watch asked to follow any of this daemon's lock files, or a
// file named as one, panics naming the path and the lock's kind, before it
// opens anything.
func TestFollowRootPathsRefusesEveryLockFile(t *testing.T) {
	const addrPath, instance = "/state/root/daemon.addr", "inst"
	cases := []struct {
		name     string
		path     string
		wantKind daemonaddr.LockKind
		wantWhy  string
	}{
		{name: "the boot lock", path: daemonaddr.LockPath(addrPath), wantKind: daemonaddr.LockBoot, wantWhy: "this daemon's"},
		{name: "the serving lock", path: daemonaddr.ServingPath(addrPath), wantKind: daemonaddr.LockServing, wantWhy: "this daemon's"},
		{name: "the life lock", path: daemonaddr.LifePath(addrPath, instance), wantKind: daemonaddr.LockLife, wantWhy: "this daemon's"},
		{name: "the life lock's pending name", path: daemonaddr.LifePendingPath(addrPath, instance), wantKind: daemonaddr.LockLifePending, wantWhy: "this daemon's"},
		{name: "the boot lock by an uncleaned path", path: "/state/root/./daemon.lock", wantKind: daemonaddr.LockBoot, wantWhy: "this daemon's"},
		{name: "another daemon's life lock, by its name", path: daemonaddr.LifePath(addrPath, "other"), wantKind: daemonaddr.LockLife, wantWhy: "named as a"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			paths := append(rootWatchPaths("/state/root", addrPath), tc.path)
			var recovered any

			// Act
			func() {
				defer func() { recovered = recover() }()
				followRootPaths(addrPath, instance, paths...)
			}()

			// Assert
			msg, ok := recovered.(string)
			if !ok {
				t.Fatalf("followRootPaths(%q) recovered %v, want a panic naming the lock", tc.path, recovered)
			}
			for _, want := range []string{"invariant violated", fmt.Sprintf("%q", tc.path), string(tc.wantKind) + " lock", tc.wantWhy} {
				if !strings.Contains(msg, want) {
					t.Fatalf("panic = %q, want it to contain %q", msg, want)
				}
			}
		})
	}
}

func TestFollowRootPathsAcceptsTheRootWatchsOwnPaths(t *testing.T) {
	// Arrange
	addrPath := "/state/root/daemon.addr"

	// Act
	src := followRootPaths(addrPath, "inst", rootWatchPaths("/state/root", addrPath)...)

	// Assert
	if src == nil {
		t.Fatal("followRootPaths answered no source for the root watch's own paths")
	}
}

// rootChangeBound bounds one reported change, the kernelevent tests' bound
// for the same kernel report (kernelevent's waitBound).
const rootChangeBound = 5 * time.Second

func TestTheRootWatchReportsTheBootLockRemovedOrReplaced(t *testing.T) {
	cases := []struct {
		name   string
		change func(t *testing.T, lock string)
	}{
		{name: "removed", change: func(t *testing.T, lock string) {
			if err := os.Remove(lock); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "replaced by rename", change: func(t *testing.T, lock string) {
			other := lock + ".new"
			if err := os.WriteFile(other, nil, 0o644); err != nil {
				t.Fatal(err)
			}
			if err := os.Rename(other, lock); err != nil {
				t.Fatal(err)
			}
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			addrPath := filepath.Join(root, "daemon.addr")
			lock := daemonaddr.LockPath(addrPath)
			if err := os.WriteFile(lock, nil, 0o644); err != nil {
				t.Fatal(err)
			}
			ctx, cancel := context.WithTimeout(context.Background(), rootChangeBound)
			defer cancel()
			changes, stop := followRootPaths(addrPath, "inst", rootWatchPaths(root, addrPath)...)(ctx)
			defer stop()

			// Act
			tc.change(t, lock)

			// Assert
			select {
			case err := <-changes:
				if err != nil {
					t.Fatalf("the watch reported %v, want the change", err)
				}
			case <-ctx.Done():
				t.Fatalf("no change reported within %s of daemon.lock %s", rootChangeBound, tc.name)
			}
		})
	}
}

// rootWatchHelperEnv runs this test binary as TestLockHelperProcess.
const rootWatchHelperEnv = "AGENT_REPLD_ROOTWATCH_HELPER"

// TestLockHelperProcess asks about a record lock from another process
// (locktest). Its one mode:
//
//	probe PATH   print "boot <pid>", the boot claim's holder beside the
//	             daemon.addr PATH, 0 for none, and exit
func TestLockHelperProcess(t *testing.T) {
	locktest.Run(rootWatchHelperEnv, map[string]locktest.Mode{
		"probe": func(path string) error {
			boot, _, err := daemonaddr.LockHolders(path)
			if err != nil {
				return err
			}
			fmt.Printf("boot %d\n", boot.PID)
			os.Exit(0)
			return nil
		},
	})
}

func TestARootWatchLeavesTheBootClaimHeld(t *testing.T) {
	// Arrange: this process holds the boot claim and publishes.
	root := t.TempDir()
	addrPath := filepath.Join(root, "daemon.addr")
	claim, err := daemonaddr.Bind(addrPath, 0)
	if err != nil {
		t.Fatal(err)
	}
	defer claim.Close()
	if err := claim.Publish(); err != nil {
		t.Fatal(err)
	}
	verified := make(chan struct{}, 2)
	w := rootWatch{
		verify: func() error {
			err := claim.Verify()
			verified <- struct{}{}
			return err
		},
		changes:   followRootPaths(addrPath, "inst", rootWatchPaths(root, addrPath)...),
		log:       dlog.NewTestLogger(),
		standDown: func(cause error) { t.Errorf("stood down: %v", cause) },
	}
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	awaitVerify := func(what string) {
		t.Helper()
		select {
		case <-verified:
		case <-time.After(rootChangeBound):
			t.Fatalf("no verify within %s %s", rootChangeBound, what)
		}
	}

	// Act: the watch runs, re-watches at a change, and stops.
	go func() { done <- w.run(ctx) }()
	awaitVerify("once the watch was taken")
	if err := os.WriteFile(filepath.Join(root, "other"), nil, 0o644); err != nil {
		t.Fatal(err)
	}
	awaitVerify("after a change of the root")
	cancel()
	if err := <-done; err != nil {
		t.Fatalf("run = %v, want nil", err)
	}

	// Assert: another process still sees this one holding the claim.
	if holder := locktest.Start(t, "TestLockHelperProcess", rootWatchHelperEnv, "probe", addrPath).Field(t, "boot"); holder != os.Getpid() {
		t.Fatalf("the boot claim's holder is pid %d, want this process's %d", holder, os.Getpid())
	}
}
