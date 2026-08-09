// The shim stderr tap's own guarantees.
//
// The tap is what lets a bounce test destroy a daemon AT a point in the shim's
// processing rather than at a point in its own statement order, so the two
// things it must never do are lose a line that arrived before the wait and
// block on one that has already arrived. Both are covered here directly,
// because a tap that silently misses either turns the tests that depend on it
// back into the races they were written to remove.
package e2e

import (
	"testing"
)

// TestShimLogTapReleasesAWaiterOnALaterLine covers the ordinary rendezvous: the
// waiter is registered first and the line arrives afterwards.
func TestShimLogTapReleasesAWaiterOnALaterLine(t *testing.T) {
	// Arrange
	tap := newShimLogTap(t)
	released := make(chan struct{})

	// Act
	go func() {
		defer close(released)
		tap.await(t, "processing daemon interrupt request", "the tap must release a waiter registered before the line")
	}()
	// The tap's own registration is what orders this: `line` only releases
	// waiters already in the map, so it is retried until the waiter is there.
	for {
		tap.mu.Lock()
		registered := len(tap.waiters) == 1
		tap.mu.Unlock()
		if registered {
			break
		}
	}
	tap.line("shim stderr: %s", `{"message":"processing daemon interrupt request"}`)

	// Assert
	<-released
}

// TestShimLogTapSatisfiesAWaitFromALineAlreadySeen covers the late arrival: a
// caller that reaches the wait after the shim already logged must not block on
// an event that is in the past.
func TestShimLogTapSatisfiesAWaitFromALineAlreadySeen(t *testing.T) {
	// Arrange
	tap := newShimLogTap(t)
	tap.line("shim stderr: %s", `{"message":"selected fake turn branch","branch":"text"}`)

	// Act
	tap.await(t, "selected fake turn branch", "a line already seen must satisfy the wait immediately")

	// Assert — reaching here without hitting the tap's deadline is the fact.
	tap.mu.Lock()
	defer tap.mu.Unlock()
	if len(tap.waiters) != 0 {
		t.Errorf("the tap holds %d waiters after a wait satisfied from history, want 0: a satisfied wait must register nothing", len(tap.waiters))
	}
}

// TestShimLogTapDropsLinesAfterFinish covers the teardown guard: the stderr
// scanners outlive the test function, and a tap that kept calling t.Logf then
// panics inside the harness rather than failing the test it belongs to.
func TestShimLogTapDropsLinesAfterFinish(t *testing.T) {
	// Arrange
	tap := newShimLogTap(t)
	tap.finish()

	// Act
	tap.line("shim stderr: %s", `{"message":"a line the scanner produced after the test returned"}`)

	// Assert
	tap.mu.Lock()
	defer tap.mu.Unlock()
	if len(tap.seen) != 0 {
		t.Errorf("the tap recorded %d lines after finish, want 0: a finished tap must not touch the test log", len(tap.seen))
	}
}
