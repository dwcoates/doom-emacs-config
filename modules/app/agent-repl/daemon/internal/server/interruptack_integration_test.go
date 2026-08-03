package server

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// THE CHALLENGE'S ACK SHAPE, THROUGH THE REAL DISPATCH FUNNEL.
//
// frontend/commands_test.go pins the funnel's arm with a mock handler that
// simply RETURNS an InterruptConfirmRequired, and interruptgate_test.go pins
// the gate that decides to raise one — but nothing composes the two, and the
// composition is where the contract actually lives: the gate's typed error has
// to survive the same errors.As interception that stands between it and the
// failAck funnel every other refusal falls into. A gate error that gained a
// wrapper, or a funnel that classified before checking, would leave both unit
// tests green and hand the client a failure card.

// interruptCmd builds the frontend command a client sends to stop a workspace.
func interruptCmd(requestID string, confirmAgents bool) *frontendv1.FrontendCommand {
	return &frontendv1.FrontendCommand{
		RequestId: requestID,
		Workspace: "/ws1",
		Command: &frontendv1.FrontendCommand_Interrupt{
			Interrupt: &frontendv1.InterruptCmd{ConfirmAgents: confirmAgents},
		},
	}
}

// An unconfirmed stop with no turn but working subagents comes back as the
// contract's own arm: ok=false carrying the live-task count, with BOTH failure
// channels empty. It is a question, and a client that rendered it as a failure
// would be rendering the wrong thing.
func TestDispatchedInterruptChallengeCarriesTheCountAndNoFailure(t *testing.T) {
	// Arrange — no turn in flight, three subagent tasks still working.
	h, p := newGatedHandler(t, false, fakeLiveTasks{count: 3})

	// Act — the command goes through the real dispatch funnel.
	ack, response := frontend.DispatchWithResponse(context.Background(), nil, h, nil, interruptCmd("r1", false))

	// Assert.
	if ack.GetOk() {
		t.Fatalf("ack = %v, want ok=false for a challenge", ack)
	}
	if got := ack.GetInterruptConfirmRequired().GetLiveTasks(); got != 3 {
		t.Fatalf("interrupt_confirm_required.live_tasks = %d, want 3", got)
	}
	if ack.GetFailure() != nil {
		t.Fatalf("challenge ack carried a classified failure %v; a challenge is not a failure", ack.GetFailure())
	}
	if ack.GetError() != "" {
		t.Fatalf("challenge ack carried an error string %q; Emacs would echo it as a failure", ack.GetError())
	}
	if ack.GetRequestId() != "r1" {
		t.Fatalf("ack request_id = %q, want the command's own", ack.GetRequestId())
	}
	if response != nil {
		t.Fatalf("challenge produced a correlated response frame %v, want none", response)
	}
	if len(p.interrupts) != 0 {
		t.Fatalf("interrupts = %v, want nothing delivered behind a challenge", p.interrupts)
	}
}

// The answer to the challenge is the same command with confirm_agents set, and
// the stop then goes through: a plain successful ack and a delivery.
func TestADispatchedConfirmedInterruptReachesTheShim(t *testing.T) {
	// Arrange — the same workspace the gate just challenged.
	h, p := newGatedHandler(t, false, fakeLiveTasks{count: 3})

	// Act — the client resends with the user's explicit yes.
	ack, _ := frontend.DispatchWithResponse(context.Background(), nil, h, nil, interruptCmd("r2", true))

	// Assert.
	if !ack.GetOk() {
		t.Fatalf("ack = %v, want ok=true for a confirmed stop", ack)
	}
	if ack.GetInterruptConfirmRequired() != nil {
		t.Fatalf("confirmed stop was challenged again: %v", ack.GetInterruptConfirmRequired())
	}
	if len(p.interrupts) != 1 || p.interrupts[0] != "/ws1" {
		t.Fatalf("interrupts = %v, want the stop delivered for /ws1", p.interrupts)
	}
}
