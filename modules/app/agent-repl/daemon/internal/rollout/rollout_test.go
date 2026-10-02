package rollout

import (
	"context"
	"errors"
	"testing"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
)

func TestHandOverIsRefusedOnASuccessorThatIsStillJoining(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.c.mu.Lock()
	h.c.enterJoiningLocked()
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

func TestHandOverTransfersABusyWorkspaceAtOnceWithoutEndingItsWork(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	shim := h.fleet.live[ws]
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()

	// Assert
	if calls := h.pusher.Calls(); len(calls) != 1 || calls[0].WS != ws {
		t.Fatalf("pushes = %v, want the busy workspace transferred at once", calls)
	}
	if !shim.Detached() || len(shim.KillRequests()) != 0 || len(shim.ForceKills()) != 0 {
		t.Fatalf("detached %v, kills %d/%d; want the shim detached with its work running",
			shim.Detached(), len(shim.KillRequests()), len(shim.ForceKills()))
	}
}

func TestHandOverOfABusyWorkspaceCompletesWhileItsWorkRuns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Act: the successor adopts it mid-work; the work never ends.
	h.clock.awaitArmed(t, adoptionWindow)
	h.successorAdopts(t)
	awaitExit(t, h)

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].WS != ws {
		t.Fatalf("pushes = %v, want the one transfer of %q", calls, ws)
	}
	if h.freeness.Free(ws) {
		t.Fatalf("the workspace fell free; this test is of a handover that does not wait for it")
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

// TestAHandoverWaitsForTheTakeoversOwnBringUp pins the takeover-settled
// latch: a successor's takeover starts the session of a workspace it serves
// with no shim, and a handover asked while that bring-up is in flight waits
// for it, planning nothing, and proceeds once it settles. Planned beside it,
// the handover moved a workspace the bring-up then claimed, and two daemons
// served it (2026-10-02, TestASuccessorThatFinishedJoiningAcceptsADeploy).
func TestAHandoverWaitsForTheTakeoversOwnBringUp(t *testing.T) {
	// Arrange: the takeover's bring-up of a session-less workspace is held.
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	entered, release := make(chan struct{}), make(chan struct{})
	released := false
	// THE HELD START IS RELEASED EVEN WHEN THE TEST FAILS, before the
	// harness's own cleanup joins the bring-ups (registered after it, so it
	// runs first).
	t.Cleanup(func() {
		if !released {
			close(release)
		}
	})
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateFree
	h.startHold = func(ids.WorkspaceID) {
		close(entered)
		<-release
	}
	h.mu.Unlock()
	h.c.mu.Lock()
	h.c.enterJoiningLocked()
	h.c.mu.Unlock()
	h.c.becomeIncumbent(nil)
	<-entered

	// Act
	result := make(chan error, 1)
	go func() {
		_, err := h.c.HandOver(context.Background(), false)
		result <- err
	}()
	awaitRecord(t, h, opRollOut, "a handover waits for this daemon's own takeover to settle before it plans anything")
	spawnedWhileHeld := len(h.spawner.Told())
	select {
	case err := <-result:
		t.Fatalf("HandOver answered %v while the takeover's bring-up was in flight, want it waiting", err)
	default:
	}
	released = true
	close(release)

	// Assert
	if err := <-result; err != nil {
		t.Fatalf("HandOver once the takeover settled = %v, want accepted", err)
	}
	if spawnedWhileHeld != 0 {
		t.Fatalf("successors spawned while the bring-up was held = %d, want none", spawnedWhileHeld)
	}
	if got := len(h.spawner.Told()); got != 1 {
		t.Fatalf("successors spawned once the takeover settled = %d, want 1", got)
	}
}

func TestAHandoverWhoseCallerLeavesBeforeTheTakeoverSettlesStartsNothing(t *testing.T) {
	// Arrange: a takeover whose bring-up never finishes during the test.
	h := newHarness(t)
	ws := orphan(t, h)
	record, err := h.db.Workspace(context.Background(), ws)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	entered, release := make(chan struct{}), make(chan struct{})
	defer close(release)
	h.mu.Lock()
	h.lockStates[record.Dir] = sessionlock.StateFree
	h.startHold = func(ids.WorkspaceID) {
		close(entered)
		<-release
	}
	h.mu.Unlock()
	h.c.mu.Lock()
	h.c.enterJoiningLocked()
	h.c.mu.Unlock()
	h.c.becomeIncumbent(nil)
	<-entered
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	_, err = h.c.HandOver(ctx, false)

	// Assert
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("HandOver = %v, want the caller's cancellation", err)
	}
	if got := len(h.spawner.Told()); got != 0 {
		t.Fatalf("successors spawned = %d, want none", got)
	}
}

func TestADaemonThatNeverJoinedPlansAHandoverAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	_, err := h.c.HandOver(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("HandOver = %v, want accepted at once", err)
	}
	if recordWithMessage(h, opRollOut, "info", "a handover waits for this daemon's own takeover to settle before it plans anything") {
		t.Fatal("a daemon with no takeover waited for one")
	}
}
