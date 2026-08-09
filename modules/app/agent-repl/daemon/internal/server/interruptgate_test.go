package server

import (
	"context"
	"errors"
	"fmt"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
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

// A turn source that reports the workspace UNWIRED is not a refusal: the gate
// has nothing to challenge about, and the delivery path is the one that owns
// whether a turn is running behind the missing session controller. Refusing
// here made that recovery unreachable, so a stop pressed while a daemon was
// still reattaching a surviving shim was answered "no live session" about a
// turn that was still running.
func TestAnUnwiredTurnSourceStillDeliversTheStop(t *testing.T) {
	// Arrange.
	p := &fakePrompts{turnErr: fmt.Errorf("session-controller: no live session for workspace %q: %w", "/ws1", errclass.ErrNoLiveSessionController)}
	h, err := newCommandHandler(p, &fakeMerges{}, &fakeLifecycle{}, nil, &fakeSessionCmds{}, nil, nil, nil,
		CommandHandlerConfig{Interrupt: InterruptGateConfig{Turns: p, LiveTasks: fakeLiveTasks{count: 2}}})
	if err != nil {
		t.Fatalf("newCommandHandler: %v", err)
	}

	// Act.
	got := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})

	// Assert.
	if got != nil {
		t.Fatalf("Interrupt = %v, want the unwired turn source classified rather than returned", got)
	}
	if len(p.interrupts) != 1 {
		t.Fatalf("interrupts = %v, want the stop delivered so the delivery path can decide whether a turn is running", p.interrupts)
	}
}

// --- the interrupt's queue half ---------------------------------------------

// A delivered interrupt takes the workspace's WAITING merges off their queues,
// and does it BEFORE the stop reaches the shim.
func TestAnInterruptEvictsTheWorkspacesQueuedMerges(t *testing.T) {
	// Arrange — a live turn (so the gate never asks) and two queued merges.
	merges := &fakeMerges{evictCount: 2}
	h, p, _ := newGatedHandlerWithMerges(t, true, fakeLiveTasks{}, merges)
	var interruptsAtEviction int
	merges.onEvict = func() { interruptsAtEviction = len(p.interrupts) }

	// Act.
	err := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})

	// Assert — the queue was evicted for this workspace, and the shim came after.
	if err != nil {
		t.Fatalf("Interrupt() error = %v, want nil", err)
	}
	if len(merges.evicted) != 1 || merges.evicted[0] != "/ws1" {
		t.Fatalf("evicted = %v, want [/ws1]", merges.evicted)
	}
	if interruptsAtEviction != 0 {
		t.Fatalf("shim had %d interrupts at eviction time, want 0 — the queue half runs first", interruptsAtEviction)
	}
	if len(p.interrupts) != 1 {
		t.Fatalf("interrupts = %v, want the stop delivered after the eviction", p.interrupts)
	}
}

// A workspace with nothing queued is the ordinary case: the eviction reports
// zero and the interrupt is exactly what it always was.
func TestAnInterruptWithNothingQueuedIsUnchanged(t *testing.T) {
	// Arrange.
	merges := &fakeMerges{}
	h, p, _ := newGatedHandlerWithMerges(t, true, fakeLiveTasks{}, merges)

	// Act.
	err := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})

	// Assert.
	if err != nil || len(p.interrupts) != 1 {
		t.Fatalf("Interrupt = %v with interrupts %v, want a delivered stop", err, p.interrupts)
	}
}

// A CHALLENGED interrupt evicts nothing, for the same reason it delivers
// nothing: the user has not decided yet, and a merge that left the queue behind
// a question they never answered cannot be put back.
func TestAChallengedInterruptEvictsNothing(t *testing.T) {
	// Arrange — no turn in flight, live subagents, so the gate challenges.
	merges := &fakeMerges{evictCount: 1}
	h, _, _ := newGatedHandlerWithMerges(t, false, fakeLiveTasks{count: 2}, merges)

	// Act.
	_ = h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})

	// Assert.
	if len(merges.evicted) != 0 {
		t.Fatalf("evicted = %v, want nothing evicted behind a challenge", merges.evicted)
	}
}

// An eviction that FAILED surfaces on the ack, and the stop is delivered
// anyway: a queue storage failure must not swallow the interrupt the user
// asked for.
func TestAnEvictionFailureSurfacesAndStillStopsTheShim(t *testing.T) {
	// Arrange.
	evictErr := errors.New("remove queue entry: permission denied")
	merges := &fakeMerges{evictErr: evictErr}
	h, p, _ := newGatedHandlerWithMerges(t, true, fakeLiveTasks{}, merges)

	// Act.
	err := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})

	// Assert.
	if err == nil || !errors.Is(err, evictErr) {
		t.Fatalf("Interrupt() error = %v, want the eviction's own error", err)
	}
	if len(p.interrupts) != 1 {
		t.Fatalf("interrupts = %v, want the stop delivered despite the failed eviction", p.interrupts)
	}
}

// Both halves failing reports BOTH: a shim that refused the stop must not hide
// the queue failure beside it.
func TestAnInterruptReportsBothHalvesFailures(t *testing.T) {
	// Arrange.
	evictErr := errors.New("remove queue entry: permission denied")
	shimErr := errors.New("no live session for workspace")
	merges := &fakeMerges{evictErr: evictErr}
	h, p, _ := newGatedHandlerWithMerges(t, true, fakeLiveTasks{}, merges)
	p.err = shimErr

	// Act.
	err := h.Interrupt(context.Background(), "/ws1", "r1", &frontendv1.InterruptCmd{})

	// Assert.
	if !errors.Is(err, evictErr) || !errors.Is(err, shimErr) {
		t.Fatalf("Interrupt() error = %v, want both the eviction and the shim failure", err)
	}
}

// THE FRONTEND'S OWN ID SURVIVES THE HOP TO THE SESSION CONTROLLER. It is the
// only id the user's client, the daemon's command log and the shim exchange can
// be reconciled on, and dropping it here is what made a stop that WAS delivered
// and acked INTERRUPTED read exactly like one the daemon had swallowed.
func TestInterruptCarriesTheCommandRequestIDToTheSessionController(t *testing.T) {
	// Arrange.
	h, p := newGatedHandler(t, true, fakeLiveTasks{count: 0})

	// Act.
	if err := h.Interrupt(context.Background(), "/ws1", "fe-276-1074", &frontendv1.InterruptCmd{}); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	got := p.interruptRequestIDs
	if len(got) != 1 || got[0] != "fe-276-1074" {
		t.Fatalf("interrupt request ids = %v, want the frontend command's own id carried through", got)
	}
}

// A CHALLENGED STOP CARRIES NOTHING THROUGH, because it performs nothing. The
// correlation must not appear for a command the daemon deliberately did not do.
func TestAChallengedInterruptCarriesNoRequestIDThrough(t *testing.T) {
	// Arrange.
	h, p := newGatedHandler(t, false, fakeLiveTasks{count: 2})

	// Act.
	_ = h.Interrupt(context.Background(), "/ws1", "fe-276-1074", &frontendv1.InterruptCmd{})

	// Assert.
	if len(p.interruptRequestIDs) != 0 {
		t.Fatalf("interrupt request ids = %v, want none behind a challenge", p.interruptRequestIDs)
	}
}
