package main

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"

	"agent-repld/internal/daemonaddr"
	"agent-repld/internal/dlog"
	"agent-repld/internal/kernelevent"
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

func TestRootWatchPathsNameTheRootItsParentTheLockAndTheAddress(t *testing.T) {
	// Act
	got := rootWatchPaths("/state/root", "/state/root/daemon.addr")

	// Assert
	want := []string{"/state", "/state/root", daemonaddr.LockPath("/state/root/daemon.addr"), "/state/root/daemon.addr"}
	if !slices.Equal(got, want) {
		t.Fatalf("rootWatchPaths = %v, want %v", got, want)
	}
}
