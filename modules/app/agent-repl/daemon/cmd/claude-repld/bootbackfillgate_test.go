package main

import (
	"io"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

func testHoldLogger(t *testing.T) *dlog.Logger {
	t.Helper()
	return dlog.New(io.Discard, io.Discard, false)
}

// THE GATE, NOT A SLEEP: with the host's connect snapshot unserved and no grace
// elapsed, the held work cannot start. This is the ordering that keeps ~145
// git/stat subprocesses from landing in front of a reconnecting host.
func TestBackfillHoldBlocksUntilTheHostConnectSnapshotIsServed(t *testing.T) {
	// Arrange.
	served := make(chan struct{})
	hold := &backfillHold{served: served, grace: make(chan time.Time), log: testHoldLogger(t)}
	started := make(chan backfillStartReason, 1)

	// Act.
	go func() {
		reason, _ := hold.Wait()
		started <- reason
	}()
	select {
	case reason := <-started:
		t.Fatalf("the backfill started before the host was served (reason %q)", reason)
	default:
	}
	close(served)

	// Assert.
	if reason := <-started; reason != backfillStartHostServed {
		t.Fatalf("start reason = %q, want %q", reason, backfillStartHostServed)
	}
}

// A daemon with no Emacs attached must still repair its merge geometry: the
// hold is a yield to the host, never a dependency on one.
func TestBackfillHoldReleasesOnGraceWhenNoHostConnects(t *testing.T) {
	// Arrange.
	grace := make(chan time.Time, 1)
	hold := &backfillHold{served: make(chan struct{}), grace: grace, log: testHoldLogger(t)}

	// Act.
	grace <- time.Now()
	reason, run := hold.Wait()

	// Assert.
	if !run || reason != backfillStartGrace {
		t.Fatalf("Wait() = (%q, %v), want (%q, true)", reason, run, backfillStartGrace)
	}
}

// A daemon going down must not launch a subprocess storm on its way out.
func TestBackfillHoldRefusesToRunOnShutdown(t *testing.T) {
	// Arrange.
	stop := make(chan struct{})
	hold := &backfillHold{served: make(chan struct{}), grace: make(chan time.Time), stop: stop, log: testHoldLogger(t)}

	// Act.
	close(stop)
	reason, run := hold.Wait()

	// Assert.
	if run || reason != backfillStartShutdown {
		t.Fatalf("Wait() = (%q, %v), want (%q, false)", reason, run, backfillStartShutdown)
	}
}

// The production constructor refuses a missing host-connect signal rather than
// silently degrading to a grace-only hold.
func TestNewBackfillHoldRequiresAHostConnectSignal(t *testing.T) {
	// Arrange.
	defer func() {
		if recover() == nil {
			t.Fatal("newBackfillHold accepted a nil host-connect signal")
		}
	}()

	// Act + Assert.
	newBackfillHold(nil, nil, testHoldLogger(t))
}
