package server

import (
	"errors"
	"testing"
)

// The sweep is DEFERRED, not optional: an AgentShim that carries the closure
// runs it when the caller asks, and reports its failure rather than swallowing
// it. A failure is non-fatal to boot, which is exactly why it must reach the
// caller to be logged.
func TestSweepOrphanRebaseWorktreesReportsTheSweepsFailure(t *testing.T) {
	// Arrange.
	want := errors.New("leftover would not delete")
	calls := 0
	shim := &AgentShim{sweepOrphans: func() error { calls++; return want }}

	// Act.
	err := shim.SweepOrphanRebaseWorktrees()

	// Assert.
	if !errors.Is(err, want) || calls != 1 {
		t.Fatalf("SweepOrphanRebaseWorktrees() = %v after %d calls, want %v after 1", err, calls, want)
	}
}

// An AgentShim that was not built by WireAgentShim has no sweep to run, and
// says so rather than reporting a sweep that never happened as a success.
func TestSweepOrphanRebaseWorktreesRefusesAnUnboundShim(t *testing.T) {
	// Arrange.
	shim := &AgentShim{}

	// Act.
	err := shim.SweepOrphanRebaseWorktrees()

	// Assert.
	if err == nil {
		t.Fatal("an unbound AgentShim reported a successful sweep it never ran")
	}
}
