package sessioncontroller

import (
	"errors"
	"testing"

	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// THE MERGE RUN'S OWN TURN, ABANDONED. The work phase's ctx.Done() arm is the
// one exit that leaves a running turn behind: the wait gives up, the run reaches
// a terminal, the lease is released, and the turn goes on holding the workspace
// `thinking` for as long as the shim lives. Observed live as a `merge-resume:`
// claim still open after a merge_failed. See mergeturnstop.go.
// ---------------------------------------------------------------------------

// abandonedMergeRig is a live session whose ledger holds the merge run's own
// turn open, which is the state the abandoned wait leaves behind.
func abandonedMergeRig(t *testing.T, openTurnIDs ...string) (*Manager, *fakeApplier, *sessionController) {
	t.Helper()
	m, applier, _ := keepAliveRig(t)
	applier.setDurableTurns(openTurnIDs...)
	m.mu.Lock()
	d := m.byWS["ws"]
	m.mu.Unlock()
	if d == nil {
		t.Fatal("the rig produced no live session controller for ws")
	}
	return m, applier, d
}

// THE HAPPY PATH: the turn is stopped through the shim, so its OWN terminal
// closes the claim through the ordinary lifecycle. Nothing is synthesized,
// because the honest close is available.
func TestAnAbandonedMergeTurnIsInterruptedThroughTheShim(t *testing.T) {
	// Arrange.
	m, applier, d := abandonedMergeRig(t, "merge-resume:q_8ddc6f0f")
	client := fakeClientFor(t, m, "ws")

	// Act.
	m.stopAbandonedMergeTurn(d, "ws", "r-merge-1", "merge-resume:q_8ddc6f0f")

	// Assert.
	if got := client.interruptCount(); got != 1 {
		t.Fatalf("interrupts sent = %d, want exactly 1; the abandoned turn has no consumer left and the lease is about to hand the shim back to the user", got)
	}
	if calls := applier.recordedOriginTurnCloses(); len(calls) != 0 {
		t.Fatalf("synthesized closes = %+v, want none; the shim's own terminal is the honest close and it was available", calls)
	}
}

// THE FALLBACK: an unreachable shim cannot produce the turn's terminal, so the
// claim is closed durably instead of left standing.
func TestAnAbandonedMergeTurnFallsBackToTheDurableCloseWhenTheShimIsUnreachable(t *testing.T) {
	// Arrange.
	m, applier, d := abandonedMergeRig(t, "merge-resume:q_8ddc6f0f")
	client := fakeClientFor(t, m, "ws")
	client.mu.Lock()
	client.interruptErr = errors.New("shim is not connected")
	client.mu.Unlock()

	// Act.
	m.stopAbandonedMergeTurn(d, "ws", "r-merge-1", "merge-resume:q_8ddc6f0f")

	// Assert.
	calls := applier.recordedOriginTurnCloses()
	if len(calls) != 1 {
		t.Fatalf("synthesized closes = %d, want exactly 1; the shim could not produce the terminal, so the daemon owes the end", len(calls))
	}
	got := calls[0]
	if len(got.turnIDs) != 1 || got.turnIDs[0] != "merge-resume:q_8ddc6f0f" {
		t.Fatalf("synthesized close = %+v, want only the run's own bound turn", got)
	}
	if got.cause != ssm.TurnCloseMergeRunTerminal {
		t.Fatalf("synthesized close cause = %q, want %q", got.cause, ssm.TurnCloseMergeRunTerminal)
	}
}

// THE BOUNDARY. A turn id this session no longer holds open is one that already
// ended, and interrupting on the strength of it would stop whatever the
// workspace is running now — which, once the lease releases, is the user's own
// work.
func TestAnAbandonedMergeStopNeverTouchesAForeignTurn(t *testing.T) {
	// Arrange — the ledger holds somebody else's turn, not the run's.
	m, applier, d := abandonedMergeRig(t, "fe-701-a1b2")
	client := fakeClientFor(t, m, "ws")

	// Act.
	m.stopAbandonedMergeTurn(d, "ws", "r-merge-1", "merge-resume:q_8ddc6f0f")

	// Assert.
	if got := client.interruptCount(); got != 0 {
		t.Fatalf("interrupts sent = %d, want 0; the run's own turn had already ended and the only turn open belongs to somebody else", got)
	}
	if calls := applier.recordedOriginTurnCloses(); len(calls) != 0 {
		t.Fatalf("synthesized closes = %+v, want none; there was no claim of this run's to close", calls)
	}
}

// AN UNREADABLE LEDGER STOPS THE WHOLE PATH. The failure it would otherwise
// admit is interrupting the user's own turn, so the stop is refused rather than
// aimed at a turn nothing could verify.
func TestAnAbandonedMergeStopIsRefusedWhenTheLedgerCannotBeRead(t *testing.T) {
	// Arrange.
	m, applier, d := abandonedMergeRig(t, "merge-resume:q_8ddc6f0f")
	applier.setActiveTurnIDsErr(errors.New("state log unreadable"))
	client := fakeClientFor(t, m, "ws")

	// Act.
	m.stopAbandonedMergeTurn(d, "ws", "r-merge-1", "merge-resume:q_8ddc6f0f")

	// Assert.
	if got := client.interruptCount(); got != 0 {
		t.Fatalf("interrupts sent = %d, want 0; nothing verified which turn was open", got)
	}
}

// A WAITER THAT BOUND NOTHING NAMES NO TURN, and a stop that cannot name its
// turn would have to interrupt whatever was running.
func TestAnAbandonedMergeStopWithNoBoundTurnInterruptsNothing(t *testing.T) {
	// Arrange.
	m, _, d := abandonedMergeRig(t, "merge-resume:q_8ddc6f0f")
	client := fakeClientFor(t, m, "ws")

	// Act.
	m.stopAbandonedMergeTurn(d, "ws", "r-merge-1", "")

	// Assert.
	if got := client.interruptCount(); got != 0 {
		t.Fatalf("interrupts sent = %d, want 0; the waiter bound no turn, so this run owns none to stop", got)
	}
}
