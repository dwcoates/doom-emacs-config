package sessionwatcher

import (
	"context"
	"testing"
	"time"

	"connectrpc.com/connect"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// TestReopenDelayBacksOffToItsCeiling pins the schedule: the floor first,
// doubling per consecutive refusal, never past the ceiling.
func TestReopenDelayBacksOffToItsCeiling(t *testing.T) {
	tests := []struct {
		name     string
		refusals int
		want     time.Duration
	}{
		{name: "the first refusal waits the floor", refusals: 1, want: reopenFloor},
		{name: "the second refusal doubles it", refusals: 2, want: 2 * reopenFloor},
		{name: "the fourth refusal doubles it thrice", refusals: 4, want: 8 * reopenFloor},
		{name: "a long run of refusals waits the ceiling", refusals: 40, want: reopenCeiling},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: nothing beyond the case.

			// Act.
			got := reopenDelay(tt.refusals)

			// Assert.
			if got != tt.want {
				t.Fatalf("reopenDelay(%d) = %v, want %v", tt.refusals, got, tt.want)
			}
		})
	}
}

// refuseShell announces a shell the shim refuses to watch and waits for the
// refusal to be ruled on and its re-open armed.
func refuseShell(t *testing.T, h *harness) time.Duration {
	t.Helper()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "the store holds no rows for shell run w-1"))
	h.route(h.main, entryFrame(frameDetached("main-1", createdWork("w-1", bashWork()))))
	h.client.awaitRefusedOpen(t, "WatchBash")
	return h.clock.awaitArmed(t)
}

// TestARefusedShellWatchArmsAReopen pins the guaranteed edge: a refusal of an
// expected handle arms its re-open on the clock, at the floor.
func TestARefusedShellWatchArmsAReopen(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()

	// Act.
	delay := refuseShell(t, h)

	// Assert.
	if delay != reopenFloor {
		t.Fatalf("the armed re-open waits %v, want %v", delay, reopenFloor)
	}
}

// TestAnElapsedBackoffReopensARefusedShellWatch is the re-open itself: no
// announcement repeats, and the timer alone opens the watch again.
func TestAnElapsedBackoffReopensARefusedShellWatch(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	refuseShell(t, h)
	h.client.setBashErr(nil)

	// Act.
	h.clock.fireLast()

	// Assert.
	if open := h.client.nextBashOpen(t); open.work.GetValue() != "w-1" {
		t.Fatalf("re-opened watch = %q, want w-1", open.work.GetValue())
	}
}

// TestASecondRefusalArmsALongerReopen pins the backoff across consecutive
// refusals of one watch.
func TestASecondRefusalArmsALongerReopen(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	refuseShell(t, h)

	// Act.
	h.clock.fireLast()
	h.client.awaitRefusedOpen(t, "WatchBash")
	delay := h.clock.awaitArmed(t)

	// Assert.
	if delay != 2*reopenFloor {
		t.Fatalf("the second re-open waits %v, want %v", delay, 2*reopenFloor)
	}
}

// TestRefusalsPastTheLimitStillArmAReopen pins that the fault path does not
// end the re-opens: past openRefusalLimit the WARN and the fault stand, and
// the watch is still re-opened.
func TestRefusalsPastTheLimitStillArmAReopen(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	refuseShell(t, h)
	for i := 0; i < openRefusalLimit; i++ {
		h.clock.fireLast()
		h.client.awaitRefusedOpen(t, "WatchBash")
		h.clock.awaitArmed(t)
	}

	// Act.
	refusal := h.awaitRefusal(t)

	// Assert.
	if refusal.Operation != "watch_bash" || refusal.Handle != "w-1" {
		t.Fatalf("refusal = %+v, want watch_bash on w-1", refusal)
	}
	h.clock.fireLast()
	h.client.awaitRefusedOpen(t, "WatchBash")
	h.clock.awaitArmed(t)
}

// TestAReopenThatFiresAfterTheShellConcludedOpensNothing pins the decision on
// firing: work that concluded meanwhile is not watched again.
func TestAReopenThatFiresAfterTheShellConcludedOpensNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	refuseShell(t, h)
	h.client.setBashErr(nil)
	h.routeNow(func(w *watcher) { w.concludeLocked("w-1", concludedQueryDied) })

	// Act.
	h.clock.fireLast()
	h.client.settleOpens()
	h.quiet()

	// Assert.
	h.client.noBashOpen(t)
}

// TestAnUnannouncedRefusalArmsNoReopen pins EXPECTED: a handle nothing
// announced is a disagreement reported as a fault, not a race to wait out.
func TestAnUnannouncedRefusalArmsNoReopen(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	h.client.setBashErr(refusedOpenError("WatchBash", connect.CodeNotFound, "no rows"))
	stray := &shellWatch{work: &conversationv1.DetachedWorkId{Value: "w-stray"}}

	// Act.
	h.routeNow(func(w *watcher) { w.openShellStreamLocked(stray) })
	h.client.awaitRefusedOpen(t, "WatchBash")

	// Assert.
	h.clock.noneArmed(t)
}

// TestARefusedMainWatchIsReopenedByItsBackoff pins the main agent's watch:
// a quiet session sends no frame, so the timer is its guaranteed edge too.
func TestARefusedMainWatchIsReopenedByItsBackoff(t *testing.T) {
	// Arrange.
	h := newHarnessRefusingAgents(t, Session{Started: sessionStarted("")},
		refusedOpenError("WatchAgent", connect.CodeNotFound, "no such agent"))
	h.clock.awaitArmed(t)
	h.client.setAgentErr(nil)

	// Act.
	h.clock.fireLast()

	// Assert.
	if open := h.client.nextAgentOpen(t); open.req.GetTarget() != nil {
		t.Fatalf("the re-opened watch targeted %q, want the main agent's unset target", open.req.GetTarget().GetValue())
	}
}

// TestTheIncidentsRefusedShellFallsFreeOnItsReopenedTerminal is the
// 2026-10-08 incident, reproduced deterministically: a shell announced while
// the store held no rows for it is refused, the store then gains the run's
// rows (the sidecar's terminal), the backoff re-opens the watch, the terminal
// arrives, and the workspace falls free -- so a merge waiting on freeness
// proceeds.
func TestTheIncidentsRefusedShellFallsFreeOnItsReopenedTerminal(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	refuseShell(t, h)
	free := make(chan error, 1)
	go func() { free <- h.w.AwaitFree(context.Background()) }()
	h.client.setBashErr(nil)

	// Act.
	h.clock.fireLast()
	open := h.client.nextBashOpen(t)
	open.stream.send(t, &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{}},
	})

	// Assert.
	select {
	case err := <-free:
		if err != nil {
			t.Fatalf("AwaitFree = %v, want nil", err)
		}
	case <-time.After(waitDeadline):
		t.Fatal("the workspace never fell free after the re-opened shell's terminal")
	}
}
