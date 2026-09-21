package rollout

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/ids"
)

// awaitExit blocks until the controller's orderly exit runs, which is the
// handover's last act and therefore proof its unbounded half completed.
func awaitExit(t *testing.T, h *harness) {
	t.Helper()
	select {
	case <-h.exits:
	case <-time.After(10 * time.Second):
		t.Fatalf("the accepted handover never reached its exit")
	}
}

func TestRollOutRefusesARequestNamingNothingRebuilt(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.c.RollOut(context.Background(), Rebuilt{})

	// Assert
	if !errors.Is(err, ErrNothingRebuilt) {
		t.Fatalf("err = %v, want ErrNothingRebuilt", err)
	}
}

func TestRollOutIsRefusedOnASuccessorThatIsStillJoining(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.c.mu.Lock()
	h.c.joiningMode = true
	h.c.mu.Unlock()

	// Act
	_, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true})

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

func TestASuccessorThatFinishedJoiningTakesTheNextRollOut(t *testing.T) {
	// Arrange: a process that BOOTED as a successor and has since taken
	// everything it was handed — every daemon after the first handover.
	h := newHarness(t)
	h.c.mu.Lock()
	h.c.joiningMode, h.c.manifestSeen = true, true
	h.c.mu.Unlock()

	// Act
	accepted, err := h.c.RollOut(context.Background(), Rebuilt{Webapp: true})

	// Assert
	if err != nil {
		t.Fatalf("RollOut: %v, want it accepted: the successor is simply the daemon now", err)
	}
	if accepted.Action != ActionWebappReload {
		t.Fatalf("action = %q, want %q", accepted.Action, ActionWebappReload)
	}
}

func TestRollOutChoosesTheAction(t *testing.T) {
	cases := []struct {
		name    string
		rebuilt Rebuilt
		want    Action
	}{
		{"a daemon rebuild hands over", Rebuilt{Daemon: true}, ActionHandover},
		{"a daemon rebuild wins over a shim and a webapp rebuild", Rebuilt{Daemon: true, Shim: true, Webapp: true}, ActionHandover},
		{"a shim rebuild relaunches", Rebuilt{Shim: true}, ActionShimRelaunch},
		{"a shim rebuild wins over a webapp rebuild", Rebuilt{Shim: true, Webapp: true}, ActionShimRelaunch},
		{"a webapp rebuild reloads", Rebuilt{Webapp: true}, ActionWebappReload},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			accepted, err := h.c.RollOut(context.Background(), tc.rebuilt)

			// Assert
			if err != nil {
				t.Fatalf("RollOut: %v", err)
			}
			if accepted.Action != tc.want {
				t.Fatalf("action = %q, want %q", accepted.Action, tc.want)
			}
		})
	}
}

func TestRollOutAnswersBeforeABusyWorkspaceFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	defer close(gate)

	// Act
	accepted, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true})

	// Assert
	if err != nil {
		t.Fatalf("RollOut: %v", err)
	}
	if accepted.Workspaces != 1 || accepted.Busy != 1 {
		t.Fatalf("accepted = %+v, want one workspace and one busy", accepted)
	}
}

func TestRollOutNeverTransfersAWorkspaceThatIsStillBusy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	defer close(gate)

	// Act
	if _, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true}); err != nil {
		t.Fatalf("RollOut: %v", err)
	}
	// The transfer is parked INSIDE the freeness wait: the fake announces the
	// wait it was asked for, so this is a rendezvous and not a delay.
	select {
	case <-h.freeness.calls:
	case <-time.After(10 * time.Second):
		t.Fatalf("the handover never began waiting on the busy workspace")
	}

	// Assert
	if calls := h.pusher.Calls(); len(calls) != 0 {
		t.Fatalf("pushes = %v, want none while the workspace is busy", calls)
	}
	if kills := h.fleet.live[ws].KillRequests(); len(kills) != 0 {
		t.Fatalf("the busy workspace's session was killed %d time(s); a rollout ends nothing", len(kills))
	}
}

func TestRollOutCompletesTheHandoverOnceTheWorkspaceFallsFree(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	if _, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true}); err != nil {
		t.Fatalf("RollOut: %v", err)
	}

	// Act
	close(gate)
	h.clock.awaitArmed(t, adoptionWindow)
	h.clock.Fire(adoptionWindow)
	awaitExit(t, h)

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != ws {
		t.Fatalf("pushes = %v, want the one transfer of %q", calls, ws)
	}
}

func TestRollOutSurvivesTheCallerGivingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	ctx, cancel := context.WithCancel(context.Background())

	// Act
	if _, err := h.c.RollOut(ctx, Rebuilt{Daemon: true}); err != nil {
		t.Fatalf("RollOut: %v", err)
	}
	cancel()
	h.clock.awaitArmed(t, adoptionWindow)
	h.clock.Fire(adoptionWindow)

	// Assert
	awaitExit(t, h)
}

func TestASecondRollOutIsRefusedWhileAHandoverIsInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	defer close(gate)
	if _, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true}); err != nil {
		t.Fatalf("first RollOut: %v", err)
	}

	// Act
	_, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true})

	// Assert
	var inFlight *ErrAlreadyRollingOut
	if !errors.As(err, &inFlight) {
		t.Fatalf("err = %v, want *ErrAlreadyRollingOut", err)
	}
	if len(inFlight.WaitingOn) != 1 || inFlight.WaitingOn[0] != ws {
		t.Fatalf("waiting on %v, want exactly %q", inFlight.WaitingOn, ws)
	}
}

func TestARefusedSecondRollOutSpawnsNoSecondSuccessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	gate := h.freeness.Gate(ws)
	defer close(gate)
	if _, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true}); err != nil {
		t.Fatalf("first RollOut: %v", err)
	}

	// Act
	_, _ = h.c.RollOut(context.Background(), Rebuilt{Daemon: true})

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
	if _, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true}); err == nil {
		t.Fatalf("the first RollOut succeeded with a spawner that fails")
	}
	h.spawner.err = nil

	// Act
	_, err := h.c.RollOut(context.Background(), Rebuilt{Daemon: true})

	// Assert
	if err != nil {
		t.Fatalf("second RollOut: %v, want it accepted", err)
	}
}

func TestAWebappRollOutReloadsOnlyTheOpenWebviews(t *testing.T) {
	// Arrange
	h := newHarness(t)
	withWeb, _ := h.workspace(t)
	h.workspace(t)
	h.participants.Set(withWeb, Participants{Host: true, Web: true})

	// Act
	accepted, err := h.c.RollOut(context.Background(), Rebuilt{Webapp: true})

	// Assert
	if err != nil {
		t.Fatalf("RollOut: %v", err)
	}
	if accepted.Workspaces != 1 {
		t.Fatalf("webviews = %d, want 1", accepted.Workspaces)
	}
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != withWeb {
		t.Fatalf("pushes = %v, want one reload of %q", calls, withWeb)
	}
}

func TestAShimRollOutCountsOnlyWorkspacesWithALiveShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	live, _ := h.workspace(t)
	parked, _ := h.workspace(t)
	delete(h.fleet.live, parked)
	h.freeness.SetFree(live, false)
	gate := h.freeness.Gate(live)
	defer close(gate)

	// Act
	accepted, err := h.c.RollOut(context.Background(), Rebuilt{Shim: true})

	// Assert
	if err != nil {
		t.Fatalf("RollOut: %v", err)
	}
	if accepted.Workspaces != 1 || accepted.Busy != 1 {
		t.Fatalf("accepted = %+v, want the one live workspace, busy", accepted)
	}
}
