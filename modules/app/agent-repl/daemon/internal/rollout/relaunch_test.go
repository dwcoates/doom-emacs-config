package rollout

import (
	"context"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// standDownWindow is the window the harness wires for the stand-down.
const standDownWindow = 30 * time.Second

// runRelaunch starts a relaunch, reaps the old shim once the graceful kill has
// been sent, and returns the engine's answer.
func runRelaunch(t *testing.T, h *harness, ws ids.WorkspaceID, reason RelaunchReason) error {
	t.Helper()
	old := h.fleet.live[ws]
	done := make(chan error, 1)
	go func() { done <- h.c.RelaunchShim(context.Background(), ws, reason) }()
	if old != nil {
		old.Reap()
	}
	select {
	case err := <-done:
		return err
	case <-time.After(10 * time.Second):
		t.Fatalf("RelaunchShim never returned")
		return nil
	}
}

func TestTheEngineGoesPrelaunchThenHoldThenStandDownThenReapThenResume(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	taken := h.order.Taken()
	prelaunch := indexOf(taken, "prelaunch")
	kill := indexOf(taken, "kill_session")
	install := indexOf(taken, "install")
	resume := indexOf(taken, "resume")
	if prelaunch < 0 || kill < 0 || install < 0 || resume < 0 {
		t.Fatalf("steps = %v, want the whole engine", taken)
	}
	if !(prelaunch < kill && kill < install && install < resume) {
		t.Fatalf("steps = %v, want prelaunch, stand-down, install, resume in order", taken)
	}
}

func TestThePrelaunchedShimIsBroughtUpBeforeTheOldOneIsTouched(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	if len(old.KillRequests()) != 1 {
		t.Fatalf("stand-down calls on the old shim = %d, want exactly one", len(old.KillRequests()))
	}
	taken := h.order.Taken()
	if indexOf(taken, "prelaunch") > indexOf(taken, "kill_session") {
		t.Fatalf("steps = %v, want the inert prelaunch first", taken)
	}
}

func TestTheStandDownIsGracefulAndNeverForcedFirst(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	if got := old.KillRequests(); len(got) != 1 || got[0].GetForce() {
		t.Fatalf("stand-down requests = %+v, want one KillSession{force:false}", got)
	}
	if len(old.ForceKills()) != 0 {
		t.Fatalf("force-kills = %+v, want none when the shim exits gracefully", old.ForceKills())
	}
}

func TestTheEngineTakesTheRestartPendingHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	var tookIt bool
	for _, rec := range records(h.log, opRelaunch) {
		if rec.Message == "took the restart-pending hold; the tray draws it now" {
			tookIt = true
		}
	}
	if !tookIt {
		t.Fatalf("the engine never took the restart-pending hold")
	}
}

func TestTheReapIsTheGateBeforeTheNewShimIsInstalled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.RelaunchShim(context.Background(), ws, ReasonShimChanged) }()
	// The old process is NOT reaped yet: the engine must be sitting on the gate,
	// with nothing installed and nothing resumed.
	h.clock.awaitArmed(t, standDownWindow)
	beforeReap := h.order.Taken()
	old.Reap()
	if err := <-done; err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	if indexOf(beforeReap, "install") >= 0 || indexOf(beforeReap, "resume") >= 0 {
		t.Fatalf("steps before the reap = %v, want nothing past the gate", beforeReap)
	}
}

func TestAnExpiredStandDownWindowForceKillsAndStillPassesTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.RelaunchShim(context.Background(), ws, ReasonShimChanged) }()
	h.clock.awaitArmed(t, standDownWindow)
	h.clock.Fire(standDownWindow)
	// The force-kill is what makes the process go; the fake needs telling.
	waitForForceKill(t, old)
	old.Reap()
	if err := <-done; err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	kills := old.ForceKills()
	if len(kills) != 1 || !kills[0].Force {
		t.Fatalf("force-kills = %+v, want one forced kill after the window expired", kills)
	}
	if kills[0].Actor != "rollout.relaunch" {
		t.Fatalf("kill actor = %q, want the relaunch engine named", kills[0].Actor)
	}
}

func TestAnExpiredStandDownWindowIsLoggedLoudly(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.RelaunchShim(context.Background(), ws, ReasonShimChanged) }()
	h.clock.awaitArmed(t, standDownWindow)
	h.clock.Fire(standDownWindow)
	waitForForceKill(t, old)
	old.Reap()
	<-done

	// Assert
	if errs := levelRecords(records(h.log, opRelaunch), "error"); len(errs) == 0 {
		t.Fatalf("records = %+v, want the expired window recorded as an ERROR", records(h.log, opRelaunch))
	}
}

func TestAColdResumeRaisesTheOrdinaryColdGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.resumeCold[ws] = &conversationv1.SessionCold{}

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	h.mu.Lock()
	gated := append([]ids.WorkspaceID(nil), h.coldGateCall...)
	h.mu.Unlock()
	if len(gated) != 1 || gated[0] != ws {
		t.Fatalf("cold-gate calls = %v, want one for %s", gated, ws)
	}
}

func TestAWarmResumeNeverRaisesTheColdGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	h.mu.Lock()
	gated := len(h.coldGateCall)
	h.mu.Unlock()
	if gated != 0 {
		t.Fatalf("cold-gate calls = %d, want none on a warm resume", gated)
	}
}

func TestAResumeThatFailsHardRecordsTheWorkspacesOwnFault(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.resumeErr[ws] = errFake

	// Act
	err := runRelaunch(t, h, ws, ReasonShimChanged)
	faults, faultErr := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultRelaunchFailed})

	// Assert
	if err == nil {
		t.Fatalf("RelaunchShim succeeded with a resume that failed hard")
	}
	if faultErr != nil {
		t.Fatalf("OpenFaults: %v", faultErr)
	}
	if len(faults) != 1 {
		t.Fatalf("relaunch faults = %d, want the workspace's own one", len(faults))
	}
}

func TestTheHoldIsReleasedAfterASuccessfulRelaunch(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}
	_, held, err := h.db.Lease(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if held {
		t.Fatalf("the restart-pending hold is still held; releasing it is what drains the intake")
	}
}

func TestTheHoldIsReleasedAfterAFailedResume(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.resumeErr[ws] = errFake

	// Act
	_ = runRelaunch(t, h, ws, ReasonShimChanged)
	_, held, err := h.db.Lease(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Lease: %v", err)
	}
	if held {
		t.Fatalf("the restart-pending hold survived a failed relaunch; the workspace would never accept a prompt again")
	}
}

func TestTheNewShimsPidIsRecordedForTheNextManifest(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}
	session, _, err := h.db.Session(context.Background(), ws)

	// Assert
	if err != nil {
		t.Fatalf("Session: %v", err)
	}
	if session.ShimPID == nil || *session.ShimPID != 9999 {
		t.Fatalf("shim pid = %v, want the prelaunched shim's 9999", session.ShimPID)
	}
}

func TestAPrelaunchFailureLeavesTheOldShimUntouched(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	h.fleet.prelaunchErr[ws] = errFake

	// Act
	err := h.c.RelaunchShim(context.Background(), ws, ReasonShimChanged)

	// Assert
	if err == nil {
		t.Fatalf("RelaunchShim succeeded with no shim to swap onto")
	}
	if len(old.KillRequests()) != 0 || len(old.ForceKills()) != 0 {
		t.Fatalf("the old shim was touched after a failed prelaunch")
	}
}

func TestARefusedStandDownWaitsOutTheWindowRatherThanGivingUp(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	old.killAnswer = &shimv1.KillSessionResponse{
		Result: &shimv1.KillSessionResponse_Failure{Failure: &shimv1.KillSessionFailure{
			Cause: &shimv1.KillSessionFailure_QueryRefusedToEnd{
				QueryRefusedToEnd: &shimv1.KillSessionQueryRefusedToEnd{},
			},
		}},
	}
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.RelaunchShim(context.Background(), ws, ReasonShimChanged) }()
	h.clock.awaitArmed(t, standDownWindow)
	old.Reap()
	if err := <-done; err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	if len(old.ForceKills()) != 0 {
		t.Fatalf("force-kills = %+v, want none: the shim exited inside the window after refusing", old.ForceKills())
	}
}

func TestAWorkspaceWithNoShimIsBroughtUpWithoutAStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	delete(h.fleet.live, ws)

	// Act
	if err := h.c.RelaunchShim(context.Background(), ws, ReasonShimChanged); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	taken := h.order.Taken()
	if indexOf(taken, "kill_session") >= 0 {
		t.Fatalf("steps = %v, want no stand-down for a workspace with no shim", taken)
	}
	if indexOf(taken, "resume") < 0 {
		t.Fatalf("steps = %v, want the new shim resumed", taken)
	}
}

func TestABuildStaleReasonBouncesOnlyAStaleShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.mu.Lock()
	h.sessionSHA[ws] = "deadbeef" // the deploy stamp's own sha
	h.mu.Unlock()

	// Act
	if err := h.c.RelaunchShim(context.Background(), ws, ReasonBuildStale); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	if taken := h.order.Taken(); len(taken) != 0 {
		t.Fatalf("steps = %v, want nothing: the shim is on the deployed build", taken)
	}
}

func TestABuildStaleReasonBouncesAShimOnAnOlderBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.mu.Lock()
	h.sessionSHA[ws] = "0ldbu1ld"
	h.mu.Unlock()

	// Act
	if err := runRelaunch(t, h, ws, ReasonBuildStale); err != nil {
		t.Fatalf("RelaunchShim: %v", err)
	}

	// Assert
	if indexOf(h.order.Taken(), "resume") < 0 {
		t.Fatalf("steps = %v, want the stale shim bounced", h.order.Taken())
	}
}

func TestCheckStalenessBouncesAShimWhoseReportedBuildDisagrees(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.mu.Lock()
	h.sessionSHA[ws] = "0ldbu1ld"
	h.mu.Unlock()
	done := make(chan error, 1)

	// Act
	go func() { done <- h.c.CheckStaleness(context.Background(), ws, "0ldbu1ld") }()
	h.fleet.live[ws].Reap()
	if err := <-done; err != nil {
		t.Fatalf("CheckStaleness: %v", err)
	}

	// Assert
	if indexOf(h.order.Taken(), "resume") < 0 {
		t.Fatalf("steps = %v, want the stale shim bounced", h.order.Taken())
	}
}

func TestCheckStalenessLeavesAShimOnTheDeployedBuildAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := h.c.CheckStaleness(context.Background(), ws, "deadbeef"); err != nil {
		t.Fatalf("CheckStaleness: %v", err)
	}

	// Assert
	if taken := h.order.Taken(); len(taken) != 0 {
		t.Fatalf("steps = %v, want nothing for a shim on the deployed build", taken)
	}
}

func TestCheckStalenessLeavesTheShimAloneWhenTheDeployStampCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.mu.Lock()
	h.deployErr = errFake
	h.mu.Unlock()

	// Act
	if err := h.c.CheckStaleness(context.Background(), ws, "0ldbu1ld"); err != nil {
		t.Fatalf("CheckStaleness: %v", err)
	}

	// Assert
	if taken := h.order.Taken(); len(taken) != 0 {
		t.Fatalf("steps = %v, want nothing: bouncing on a guess is worse than an older build", taken)
	}
}

func TestReloadWebappPushesTheEmptyArm(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := h.c.ReloadWebapp(context.Background(), ws); err != nil {
		t.Fatalf("ReloadWebapp: %v", err)
	}

	// Assert
	calls := h.pusher.Calls()
	if len(calls) != 1 || calls[0].Kind != "reload_webapp" || calls[0].WS != ws {
		t.Fatalf("pushes = %+v, want one reload_webapp for %s", calls, ws)
	}
	if calls[0].Address != "" {
		t.Fatalf("reload_webapp carried address %q, want none: the daemon is not changing", calls[0].Address)
	}
}
