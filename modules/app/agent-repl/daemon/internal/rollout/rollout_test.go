package rollout

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ids"
)

func TestHandOverIsRefusedOnASuccessorThatIsStillJoining(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.c.mu.Lock()
	h.c.joiningMode = true
	h.c.mu.Unlock()

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	if !errors.Is(err, ErrJoining) {
		t.Fatalf("err = %v, want ErrJoining", err)
	}
}

func TestASuccessorStillJoining(t *testing.T) {
	ws := ids.WorkspaceID("ws-handed-over")
	cases := []struct {
		name         string
		joiningMode  bool
		manifestSeen bool
		joining      map[ids.WorkspaceID]bool
		owned        map[ids.WorkspaceID]bool
		want         bool
	}{
		{name: "a daemon that booted as the incumbent is never joining"},
		{name: "a successor that has not read the manifest is joining", joiningMode: true, want: true},
		{name: "a successor that does not yet own a handed workspace is joining", joiningMode: true, manifestSeen: true,
			joining: map[ids.WorkspaceID]bool{ws: true}, want: true},
		{name: "a successor that owns everything it was handed has finished joining", joiningMode: true, manifestSeen: true,
			joining: map[ids.WorkspaceID]bool{ws: true}, owned: map[ids.WorkspaceID]bool{ws: true}},
		{name: "a successor handed nothing has finished joining once it read the manifest", joiningMode: true, manifestSeen: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.c.mu.Lock()
			h.c.joiningMode, h.c.manifestSeen = tc.joiningMode, tc.manifestSeen
			h.c.joining, h.c.owned = tc.joining, tc.owned

			// Act
			got := h.c.stillJoiningLocked()
			h.c.mu.Unlock()

			// Assert
			if got != tc.want {
				t.Fatalf("still joining = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestASuccessorThatFinishedJoiningTakesTheNextHandover(t *testing.T) {
	// Arrange: a process that BOOTED as a successor and has since taken
	// everything it was handed — every daemon after the first handover.
	h := newHarness(t)
	h.c.mu.Lock()
	h.c.joiningMode, h.c.manifestSeen = true, true
	h.c.mu.Unlock()

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("HandOver: %v, want it accepted: the successor is simply the daemon now", err)
	}
	if h.c.Joining() {
		t.Fatalf("Joining = true for a successor that owns everything it was handed")
	}
}

func TestHandOverAnswersBeforeABusyWorkspaceFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	accepted, err := h.c.HandOver(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	if accepted.Workspaces != 1 || accepted.Busy != 1 || accepted.Forced {
		t.Fatalf("accepted = %+v, want one workspace, one busy, unforced", accepted)
	}
}

func TestHandOverNeverTransfersAWorkspaceThatIsStillBusy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	if !h.registry.Pending(ws) {
		t.Fatalf("the busy workspace's transfer is not registered")
	}
	if calls := h.pusher.Calls(); len(calls) != 0 {
		t.Fatalf("pushes = %v, want none while the workspace is busy", calls)
	}
	if kills := h.fleet.live[ws].KillRequests(); len(kills) != 0 {
		t.Fatalf("the busy workspace's session was killed %d time(s); a handover ends nothing", len(kills))
	}
}

func TestHandOverCompletesOnceTheWorkspaceFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Act
	h.registry.free(ws)
	h.clock.awaitArmed(t, adoptionWindow)
	h.successorAdopts(t)
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != ws {
		t.Fatalf("pushes = %v, want the one transfer of %q", calls, ws)
	}
}

func TestHandOverSurvivesTheCallerGivingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	ctx, cancel := context.WithCancel(context.Background())

	// Act
	if _, err := h.c.HandOver(ctx, false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	cancel()
	h.clock.awaitArmed(t, adoptionWindow)
	h.successorAdopts(t)
	h.clock.Fire(adoptionWindow)

	// Assert
	awaitExit(t, h)
}

func TestASecondHandOverIsRefusedWhileOneIsInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("first HandOver: %v", err)
	}

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	var inFlight *ErrAlreadyRollingOut
	if !errors.As(err, &inFlight) {
		t.Fatalf("err = %v, want *ErrAlreadyRollingOut", err)
	}
	if len(inFlight.WaitingOn) != 1 || inFlight.WaitingOn[0] != ws {
		t.Fatalf("waiting on %v, want exactly %q", inFlight.WaitingOn, ws)
	}
}

func TestARefusedSecondHandOverSpawnsNoSecondSuccessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("first HandOver: %v", err)
	}

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("the second HandOver was accepted")
	}

	// Assert
	if told := h.spawner.Told(); len(told) != 1 {
		t.Fatalf("successors spawned = %d, want 1", len(told))
	}
}

func TestAHandoverThatNeverSpawnedDoesNotRefuseTheNextOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.err = errors.New("the successor binary would not start")
	if _, err := h.c.HandOver(context.Background(), false); err == nil {
		t.Fatalf("the first HandOver succeeded with a spawner that fails")
	}
	h.spawner.err = nil

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("second HandOver: %v, want it accepted", err)
	}
}

func TestRollingOutNamesWhatTheHandoverInFlightWaitsOn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	_, before := h.c.RollingOut()
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Act
	waiting, rolling := h.c.RollingOut()

	// Assert
	if before {
		t.Fatalf("RollingOut reported a handover before one began")
	}
	if !rolling || len(waiting) != 1 || waiting[0] != ws {
		t.Fatalf("RollingOut = %v, %v; want the busy workspace waited on", waiting, rolling)
	}
}
