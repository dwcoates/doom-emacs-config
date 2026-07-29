package server

import (
	"context"
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
)

// --- the interrupt confirm gate (I1) ---------------------------------------

// THE FOUR CASES, one per row: what the gate does with an interrupt that has
// not been confirmed.
func TestInterruptConfirmGate(t *testing.T) {
	tests := []struct {
		name          string
		turnActive    bool
		liveTasks     LiveTaskSource
		confirmAgents bool
		wantDelivered bool
		wantChallenge int64 // 0 = no challenge expected
	}{
		{
			name:          "a live turn is stopped without asking",
			turnActive:    true,
			liveTasks:     fakeLiveTasks{count: 4},
			wantDelivered: true,
		},
		{
			name:          "no turn but live subagents is challenged",
			liveTasks:     fakeLiveTasks{count: 3},
			wantChallenge: 3,
		},
		{
			name:          "a confirmed stop of live subagents is delivered",
			liveTasks:     fakeLiveTasks{count: 3},
			confirmAgents: true,
			wantDelivered: true,
		},
		{
			name:          "no turn and no subagents is delivered anyway",
			liveTasks:     fakeLiveTasks{count: 0},
			wantDelivered: true,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h, p := newGatedHandler(t, tc.turnActive, tc.liveTasks)
			// Act.
			err := h.Interrupt(context.Background(), "/ws1", "r1",
				&frontendv1.InterruptCmd{ConfirmAgents: tc.confirmAgents})
			// Assert.
			delivered := len(p.interrupts) == 1
			if delivered != tc.wantDelivered {
				t.Fatalf("delivered = %v (interrupts %v), want %v", delivered, p.interrupts, tc.wantDelivered)
			}
			var challenge *frontend.InterruptConfirmRequired
			if tc.wantChallenge == 0 {
				if errors.As(err, &challenge) {
					t.Fatalf("got a challenge %+v, want none", challenge)
				}
				return
			}
			if !errors.As(err, &challenge) {
				t.Fatalf("err = %v, want an InterruptConfirmRequired challenge", err)
			}
			if challenge.LiveTasks != tc.wantChallenge {
				t.Fatalf("live_tasks = %d, want %d", challenge.LiveTasks, tc.wantChallenge)
			}
		})
	}
}

// A challenged interrupt delivers NOTHING to the shim. The whole point is that
// the stop has not happened yet.
func TestAChallengedInterruptReachesNoShim(t *testing.T) {
	// Arrange.
	h, p := newGatedHandler(t, false, fakeLiveTasks{count: 2})
	// Act.
	_ = h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})
	// Assert.
	if len(p.interrupts) != 0 {
		t.Fatalf("interrupts = %v, want nothing delivered behind a challenge", p.interrupts)
	}
}

// A workspace nothing has reported a task count for has no live work ON
// RECORD, so there is nothing concrete to ask about and the stop goes through.
func TestAnUnknownWorkspaceIsNotChallenged(t *testing.T) {
	// Arrange.
	h, p := newGatedHandler(t, false, fakeLiveTasks{unknown: true})
	// Act.
	err := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})
	// Assert.
	if err != nil || len(p.interrupts) != 1 {
		t.Fatalf("Interrupt = %v with interrupts %v, want a delivered stop", err, p.interrupts)
	}
}

// An unwired gate is a construction bug, and it fails LOUDLY: silently
// skipping the check would stop working subagents with no question asked,
// which is the one thing the gate exists to prevent.
func TestAnUnwiredGateRefusesTheInterrupt(t *testing.T) {
	tests := []struct {
		name  string
		turns TurnStateSource
		tasks LiveTaskSource
	}{
		{"no turn source", nil, fakeLiveTasks{}},
		{"no live-task source", &fakePrompts{}, nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			p := &fakePrompts{}
			h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
				CommandHandlerConfig{Interrupt: InterruptGateConfig{Turns: tc.turns, LiveTasks: tc.tasks}})
			if err != nil {
				t.Fatalf("newCommandHandler: %v", err)
			}
			// Act.
			got := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})
			// Assert.
			if got == nil {
				t.Fatal("want a loud refusal when the gate is unwired")
			}
			if len(p.interrupts) != 0 {
				t.Fatalf("interrupts = %v, want nothing delivered past an unwired gate", p.interrupts)
			}
		})
	}
}

// A turn source that cannot answer surfaces its error rather than being
// treated as "no turn", which would silently turn an outage into a challenge.
func TestATurnSourceErrorSurfaces(t *testing.T) {
	// Arrange.
	p := &fakePrompts{turnErr: errors.New("no live session for workspace")}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Interrupt: InterruptGateConfig{Turns: p, LiveTasks: fakeLiveTasks{count: 2}}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}
	// Act.
	got := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})
	// Assert.
	if got == nil || !errors.Is(got, p.turnErr) {
		t.Fatalf("Interrupt = %v, want the turn source's own error", got)
	}
}
