package main

import (
	"context"
	"errors"
	"fmt"
	"testing"
	"time"

	"claude-repld/internal/daemonaddr"
	"claude-repld/internal/dlog"
)

// fakeWatchClock hands the watch a channel per wait and reports each wait it
// arms, so the test fires them one at a time without sleeping.
type fakeWatchClock struct {
	asked chan time.Duration
	fire  chan chan time.Time
}

func newFakeWatchClock() *fakeWatchClock {
	return &fakeWatchClock{asked: make(chan time.Duration), fire: make(chan chan time.Time, 1)}
}

func (c *fakeWatchClock) after(d time.Duration) <-chan time.Time {
	ch := make(chan time.Time, 1)
	c.fire <- ch
	c.asked <- d
	return ch
}

func TestRootWatch(t *testing.T) {
	errLoss := fmt.Errorf("%w: the state root %q is gone", daemonaddr.ErrVanished, "/tmp/gone")
	errUndecided := errors.New("stat the state root: permission denied")
	undecided := func(n int) []error {
		out := make([]error, n)
		for i := range out {
			out[i] = errUndecided
		}
		return out
	}
	cases := []struct {
		name string
		// script answers each verify in turn. A watch that is still running
		// once the script is spent is cancelled at its next wait.
		script        []error
		wantWaits     []time.Duration
		wantStoodDown bool
		wantErrors    int
	}{
		{
			name:      "a healthy root is watched on the cadence",
			script:    []error{nil, nil},
			wantWaits: []time.Duration{time.Second, time.Second, time.Second},
		},
		{
			name:          "a loss stands the daemon down at ERROR",
			script:        []error{nil, errLoss},
			wantWaits:     []time.Duration{time.Second, time.Second},
			wantStoodDown: true,
			wantErrors:    1,
		},
		{
			name:       "a check that cannot tell is an ERROR and backs off until one can",
			script:     []error{errUndecided, errUndecided, nil},
			wantWaits:  []time.Duration{time.Second, 2 * time.Second, 4 * time.Second, time.Second},
			wantErrors: 2,
		},
		{
			name:   "the backoff never passes its ceiling",
			script: undecided(8),
			wantWaits: []time.Duration{
				time.Second, 2 * time.Second, 4 * time.Second, 8 * time.Second, 16 * time.Second,
				32 * time.Second, stateRootCheckCeiling, stateRootCheckCeiling, stateRootCheckCeiling,
			},
			wantErrors: 8,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			clock := newFakeWatchClock()
			log := dlog.NewTestLogger()
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			calls := 0
			var stoodDown error
			w := rootWatch{
				verify: func() error {
					err := tc.script[calls]
					calls++
					return err
				},
				after: clock.after,
				log:   log,
				standDown: func(cause error) {
					stoodDown = cause
				},
			}
			done := make(chan error, 1)

			// Act
			go func() { done <- w.run(ctx) }()
			for i, want := range tc.wantWaits {
				fire := <-clock.fire
				if got := <-clock.asked; got != want {
					t.Fatalf("wait %d = %s, want %s", i, got, want)
				}
				if i < len(tc.script) {
					fire <- time.Time{}
					continue
				}
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
		})
	}
}

// TestALostRootNamesWhatVanished pins that the stand-down record carries the
// loss itself, so the log says WHICH of the root, the lock or the address went.
func TestALostRootNamesWhatVanished(t *testing.T) {
	// Arrange
	clock := newFakeWatchClock()
	log := dlog.NewTestLogger()
	loss := fmt.Errorf("%w: daemon.lock %q is gone", daemonaddr.ErrVanished, "/tmp/root/daemon.lock")
	w := rootWatch{
		verify:    func() error { return loss },
		after:     clock.after,
		log:       log,
		standDown: func(error) {},
	}
	done := make(chan error, 1)

	// Act
	go func() { done <- w.run(context.Background()) }()
	fire := <-clock.fire
	<-clock.asked
	fire <- time.Time{}
	<-done

	// Assert
	for _, r := range log.Records() {
		if r.Level == dlog.LevelError && r.Context["cause"] == loss.Error() {
			return
		}
	}
	t.Fatalf("records = %+v, want an ERROR whose cause is %q", log.Records(), loss.Error())
}
