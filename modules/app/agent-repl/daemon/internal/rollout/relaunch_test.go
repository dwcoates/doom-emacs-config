package rollout

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// standDownWindow is the window the harness wires for the stand-down.
const standDownWindow = 30 * time.Second

// runRelaunch asks for an unforced bounce of a FREE workspace (the registry
// runs it at once), reaps the old shim once the stand-down has been sent, and
// returns how the bounce ended.
func runRelaunch(t *testing.T, h *harness, ws ids.WorkspaceID, reason RelaunchReason) error {
	t.Helper()
	old := h.fleet.live[ws]
	done := make(chan error, 1)
	if _, err := h.c.BounceShim(context.Background(), ws, reason, false, func(err error) { done <- err }); err != nil {
		return err
	}
	if old != nil {
		old.Reap()
	}
	return awaitBounce(t, h, done)
}

// bounceAndWait asks for a bounce with nothing reaped on the test's side, and
// returns how it ended.
func bounceAndWait(t *testing.T, h *harness, ws ids.WorkspaceID, reason RelaunchReason, force bool) error {
	t.Helper()
	done := make(chan error, 1)
	if _, err := h.c.BounceShim(context.Background(), ws, reason, force, func(err error) { done <- err }); err != nil {
		return err
	}
	return awaitBounce(t, h, done)
}

// awaitBounce waits for a bounce's completion callback and joins the fake
// registry's goroutine.
func awaitBounce(t *testing.T, h *harness, done <-chan error) error {
	t.Helper()
	select {
	case err := <-done:
		h.registry.wait()
		return err
	case <-time.After(10 * time.Second):
		t.Fatalf("the bounce never finished")
		return nil
	}
}

// TestTheEngineGoesPrelaunchThenHoldThenStandDownThenReapThenResume pins the
// prescribed order: the inert shim comes up BESIDE the live one and waits
// there. It holds neither kernel lock -- the shim takes both inside
// StartSession -- so the two processes coexist until the swap.
func TestTheEngineGoesPrelaunchThenHoldThenStandDownThenReapThenResume(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if prelaunch >= kill || kill >= install || install >= resume {
		t.Fatalf("steps = %v, want prelaunch, stand-down, install, resume in order", taken)
	}
}

// TestThePrelaunchedShimIsBroughtUpBeforeTheOldOneIsTouched is the same order
// from the other side: nothing of the old shim is disturbed until the
// replacement is standing by.
func TestThePrelaunchedShimIsBroughtUpBeforeTheOldOneIsTouched(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]

	// Act
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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

func TestTakingAndReleasingTheRestartHoldTellTheQueue(t *testing.T) {
	// Arrange: the hold is a row the queue is not watching. Taking it is what
	// re-stamps the queued prompts under the restart (the tray draws them
	// there); releasing it is what un-stamps them — without the second call the
	// held intake would never drain after a bounce.
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
	}

	// Assert
	h.mu.Lock()
	calls := append([]ids.WorkspaceID(nil), h.leaseChanged...)
	h.mu.Unlock()
	if len(calls) != 2 || calls[0] != ws || calls[1] != ws {
		t.Fatalf("lease-changed calls = %v, want exactly two for %s (take, release)", calls, ws)
	}
	taken := h.order.Taken()
	first, kill, resume := indexOf(taken, "lease_changed"), indexOf(taken, "kill_session"), indexOf(taken, "resume")
	last := -1
	for i, step := range taken {
		if step == "lease_changed" {
			last = i
		}
	}
	if first >= kill || resume >= last {
		t.Fatalf("steps = %v, want the take before the stand-down and the release after the resume", taken)
	}
}

func TestTheReapIsTheGateBeforeTheNewShimIsInstalled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	done := make(chan error, 1)

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	// The old process is NOT reaped yet: the engine must be sitting on the gate,
	// with nothing installed and nothing resumed.
	h.clock.awaitArmed(t, standDownWindow)
	beforeReap := h.order.Taken()
	old.Reap()
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	h.clock.awaitArmed(t, standDownWindow)
	h.clock.Fire(standDownWindow)
	// The force-kill is what makes the process go; the fake needs telling.
	waitForForceKill(t, old)
	old.Reap()
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	h.clock.awaitArmed(t, standDownWindow)
	h.clock.Fire(standDownWindow)
	waitForForceKill(t, old)
	old.Reap()
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("bounce: %v", err)
	}

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
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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
	err := runRelaunch(t, h, ws, ReasonRestartVerb)
	faults, faultErr := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: FaultRelaunchFailed})

	// Assert
	if err == nil {
		t.Fatalf("the bounce succeeded with a resume that failed hard")
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
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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
	_ = runRelaunch(t, h, ws, ReasonRestartVerb)
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
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
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

// TestAPrelaunchFailureLeavesTheOldShimUntouched covers the order's whole
// point: nothing is disturbed until the replacement is standing by, so a
// prelaunch that fails costs the live session nothing.
func TestAPrelaunchFailureLeavesTheOldShimUntouched(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	h.fleet.prelaunchErr[ws] = errFake

	// Act
	err := bounceAndWait(t, h, ws, ReasonRestartVerb, false)

	// Assert
	if err == nil {
		t.Fatalf("the bounce succeeded with no shim to swap onto")
	}
	if len(old.KillRequests()) != 0 || len(old.ForceKills()) != 0 {
		t.Fatalf("the old shim was touched after a failed prelaunch")
	}
	if _, held, dbErr := h.db.Lease(context.Background(), ws); dbErr != nil || held {
		t.Fatalf("lease held = %v (err %v), want no restart hold after a failed prelaunch", held, dbErr)
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
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	h.clock.awaitArmed(t, standDownWindow)
	old.Reap()
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("bounce: %v", err)
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
	if err := bounceAndWait(t, h, ws, ReasonRestartVerb, false); err != nil {
		t.Fatalf("bounce: %v", err)
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

func TestCheckStalenessJudgesTheReportedBuildAgainstTheInstalledOne(t *testing.T) {
	tests := []struct {
		name       string
		reported   *string
		wantStale  bool
		wantBounce bool
	}{
		{name: "the installed build is left alone", reported: ptr("installed-build")},
		{name: "an older build is bounced", reported: ptr("0ldbu1ld"), wantStale: true, wantBounce: true},
		{name: "no report yet is left for the report", reported: nil},
		{name: "a report naming no build is stale", reported: ptr(""), wantStale: true, wantBounce: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			if tc.reported != nil {
				h.c.mu.Lock()
				h.c.reported[ws] = *tc.reported
				h.c.mu.Unlock()
			}
			h.fleet.live[ws].Reap()

			// Act
			got, err := h.c.CheckStaleness(context.Background(), ws, false)
			h.registry.wait()

			// Assert
			if err != nil {
				t.Fatalf("CheckStaleness: %v", err)
			}
			if got.Stale != tc.wantStale {
				t.Fatalf("stale = %v, want %v (%+v)", got.Stale, tc.wantStale, got)
			}
			bounced := indexOf(h.order.Taken(), "resume") >= 0
			if bounced != tc.wantBounce {
				t.Fatalf("steps = %v, want bounced=%v", h.order.Taken(), tc.wantBounce)
			}
		})
	}
}

// TestAStaleBuildIsBouncedOnlyOncePerReportedBuild pins the once-per-build
// gate: a relaunched shim that comes back still reporting the build it was
// bounced for is NOT bounced again, and the disagreement is said at ERROR.
// Without the gate every report would spawn a shim, forever.
func TestAStaleBuildIsBouncedOnlyOncePerReportedBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.c.mu.Lock()
	h.c.reported[ws] = "0ldbu1ld"
	h.c.mu.Unlock()
	h.fleet.live[ws].Reap()
	if _, err := h.c.CheckStaleness(context.Background(), ws, false); err != nil {
		t.Fatalf("the first CheckStaleness: %v", err)
	}
	h.registry.wait()
	first := len(h.registry.Requests())

	// Act: the relaunched shim still reports the same older build.
	h.c.mu.Lock()
	h.c.reported[ws] = "0ldbu1ld"
	h.c.mu.Unlock()
	got, err := h.c.CheckStaleness(context.Background(), ws, false)

	// Assert
	if err != nil {
		t.Fatalf("the second CheckStaleness: %v", err)
	}
	if got.Skipped == "" || len(h.registry.Requests()) != first {
		t.Fatalf("check = %+v, requests %d; want the second bounce skipped", got, len(h.registry.Requests()))
	}
	if !loggedError(h.log, opStaleness, "still reports the build it was already bounced for") {
		t.Fatalf("records = %+v, want the disagreement at ERROR", h.log.Records())
	}
}

// staleReported arranges a workspace whose live shim reported an older build.
func staleReported(t *testing.T, h *harness) ids.WorkspaceID {
	t.Helper()
	ws, _ := h.workspace(t)
	h.c.mu.Lock()
	h.c.reported[ws] = "0ldbu1ld"
	h.c.mu.Unlock()
	return ws
}

// TestAStaleBounceStillRegisteredIsNotReJudged is the takeover's re-judgement
// of the first live handover that carried sessions: the bounce its adoption
// started was still standing the shim down, and the re-check called that
// shim's report "already bounced for" at ERROR.
func TestAStaleBounceStillRegisteredIsNotReJudged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := staleReported(t, h)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.CheckStaleness(context.Background(), ws, false); err != nil {
		t.Fatalf("the first CheckStaleness: %v", err)
	}

	// Act
	got, err := h.c.CheckStaleness(context.Background(), ws, false)

	// Assert
	if err != nil {
		t.Fatalf("the second CheckStaleness: %v", err)
	}
	if got.Skipped != SkippedBounceInFlight || len(h.registry.Requests()) != 1 {
		t.Fatalf("check = %+v, requests %d; want the in-flight bounce skipped", got, len(h.registry.Requests()))
	}
	if loggedError(h.log, opStaleness, "already bounced for") {
		t.Fatalf("records = %+v, want no ERROR for a bounce still in flight", h.log.Records())
	}
}

// TestAFinishedStaleBounceForgetsTheStoodDownShimsBuild covers the
// replacement's report being the one judged: the old shim's build does not
// outlive its reap.
func TestAFinishedStaleBounceForgetsTheStoodDownShimsBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := staleReported(t, h)
	h.fleet.live[ws].Reap()

	// Act
	if _, err := h.c.CheckStaleness(context.Background(), ws, false); err != nil {
		t.Fatalf("CheckStaleness: %v", err)
	}
	h.registry.wait()
	h.c.staleChecks.Wait()

	// Assert
	h.c.mu.Lock()
	reported, known := h.c.reported[ws]
	h.c.mu.Unlock()
	if known {
		t.Fatalf("reported = %q after the bounce, want the stood-down shim's build forgotten", reported)
	}
	if loggedError(h.log, opStaleness, "already bounced for") {
		t.Fatalf("records = %+v, want no ERROR before the replacement reports", h.log.Records())
	}
}

// TestAFinishedStaleBounceReJudgesTheReplacementsReport pins the genuine
// ERROR: a replacement that reported the old build while its bounce was still
// resuming it is judged once the bounce finishes.
func TestAFinishedStaleBounceReJudgesTheReplacementsReport(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := staleReported(t, h)
	if got := h.c.claimStaleBounce(ws, "0ldbu1ld"); got != staleBounceClaimed {
		t.Fatalf("claim = %v, want claimed", got)
	}

	// Act
	h.c.settleStaleBounce(ws, nil)
	h.c.staleChecks.Wait()

	// Assert
	if !loggedError(h.log, opStaleness, "still reports the build it was already bounced for") {
		t.Fatalf("records = %+v, want the relaunched shim's old build at ERROR", h.log.Records())
	}
}

// TestAFailedStaleBounceIsNotReJudged keeps a failed bounce to its own record.
func TestAFailedStaleBounceIsNotReJudged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := staleReported(t, h)
	h.c.claimStaleBounce(ws, "0ldbu1ld")
	before := len(records(h.log, opStaleness))

	// Act
	h.c.settleStaleBounce(ws, errFake)
	h.c.staleChecks.Wait()

	// Assert
	if got := records(h.log, opStaleness)[before:]; len(got) != 0 {
		t.Fatalf("records = %+v, want no judgement after a failed bounce", got)
	}
	h.c.mu.Lock()
	inFlight := h.c.staleInFlight[ws]
	h.c.mu.Unlock()
	if inFlight {
		t.Fatal("a failed bounce was left in flight")
	}
}

// TestARefusedStaleBounceIsNotLeftInFlight covers the registry refusing the
// request: nothing runs, so nothing may stay in flight.
func TestARefusedStaleBounceIsNotLeftInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws := staleReported(t, h)
	h.registry.err = errFake

	// Act
	_, err := h.c.CheckStaleness(context.Background(), ws, false)

	// Assert
	if err == nil {
		t.Fatal("CheckStaleness = nil error, want the refusal surfaced")
	}
	h.c.mu.Lock()
	inFlight := h.c.staleInFlight[ws]
	h.c.mu.Unlock()
	if inFlight {
		t.Fatal("a refused bounce was left in flight")
	}
}

func TestCheckStalenessRefusesToGuessWhenTheInstalledBuildCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.c.mu.Lock()
	h.c.reported[ws] = "0ldbu1ld"
	h.c.mu.Unlock()
	h.mu.Lock()
	h.shimBuildErr = errFake
	h.mu.Unlock()

	// Act
	_, err := h.c.CheckStaleness(context.Background(), ws, false)

	// Assert
	if err == nil {
		t.Fatalf("CheckStaleness judged a shim against an installed build it could not read")
	}
	if len(h.registry.Requests()) != 0 {
		t.Fatalf("a shim was bounced on a guess")
	}
	if !loggedError(h.log, opStaleness, "could not read the installed shim build") {
		t.Fatalf("records = %+v, want the unreadable build at ERROR", h.log.Records())
	}
}

func TestShimReportedBouncesAStaleShimOffTheCallersGoroutine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.live[ws].Reap()

	// Act
	h.c.ShimReported(ws, "0ldbu1ld")
	h.c.staleChecks.Wait()
	h.registry.wait()

	// Assert
	requests := h.registry.Requests()
	if len(requests) != 1 || requests[0].Req.Reason != string(ReasonBuildStale) {
		t.Fatalf("requests = %+v, want one build_stale bounce", requests)
	}
}

func TestShimReportedIgnoresARepeatOfTheSameBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.c.ShimReported(ws, "installed-build")
	h.c.staleChecks.Wait()

	// Act: diagnostics restate the build on their own cadence.
	h.c.ShimReported(ws, "installed-build")
	h.c.staleChecks.Wait()

	// Assert
	checks := 0
	for _, r := range records(h.log, opStaleness) {
		if r.Message == "the shim runs the installed build" {
			checks++
		}
	}
	if checks != 1 {
		t.Fatalf("judgements = %d, want one: a restated build is not a new report", checks)
	}
}

func TestShimReportedSaysLoudlyThatAShimReportedNoBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.live[ws].Reap()

	// Act
	h.c.ShimReported(ws, "")
	h.c.staleChecks.Wait()
	h.registry.wait()

	// Assert
	if !loggedError(h.log, opStaleness, "the shim reported no build") {
		t.Fatalf("records = %+v, want the missing build at ERROR", h.log.Records())
	}
	if len(h.registry.Requests()) != 1 {
		t.Fatalf("requests = %+v, want the unproven shim bounced", h.registry.Requests())
	}
}

func TestAForcedBounceStandsTheShimDownForced(t *testing.T) {
	// Arrange: a busy workspace.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	old := h.fleet.live[ws]
	done := make(chan error, 1)

	// Act
	decision, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, true, func(err error) { done <- err })
	if err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	old.Reap()
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("bounce: %v", err)
	}

	// Assert
	if !decision.Now || !decision.Forced {
		t.Fatalf("decision = %+v, want a forced bounce now", decision)
	}
	if got := old.KillRequests(); len(got) != 1 || !got[0].GetForce() {
		t.Fatalf("stand-down requests = %+v, want one KillSession{force:true}", got)
	}
}

func TestABusyWorkspacesBounceIsRegisteredNotRun(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	decision, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, nil)

	// Assert
	if err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	if decision.Now || !h.registry.Pending(ws) {
		t.Fatalf("decision = %+v, pending %v; want it registered", decision, h.registry.Pending(ws))
	}
	if taken := h.order.Taken(); len(taken) != 0 {
		t.Fatalf("steps = %v, want nothing run for a busy workspace", taken)
	}
}

func TestARegistryRefusalIsReturnedAndLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.err = errFake

	// Act
	_, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, false, nil)

	// Assert
	if err == nil {
		t.Fatalf("BounceShim swallowed the registry's refusal")
	}
	if !loggedError(h.log, opBounce, "the bounce registry refused the shim bounce") {
		t.Fatalf("records = %+v, want the refusal at ERROR", h.log.Records())
	}
}

func TestAFailedInstallRetiresThePrelaunchedShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.installErr[ws] = errFake
	h.fleet.live[ws].Reap()

	// Act
	err := bounceAndWait(t, h, ws, ReasonRestartVerb, false)

	// Assert
	if err == nil {
		t.Fatalf("the bounce succeeded with an install that failed")
	}
	h.fleet.mu.Lock()
	fresh := h.fleet.prelaunched[ws]
	h.fleet.mu.Unlock()
	if fresh == nil || len(fresh.ForceKills()) != 1 {
		t.Fatalf("the prelaunched shim was left running after the failed install")
	}
}

func ptr(s string) *string { return &s }

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

func TestAShimBounceAsksToReplaceTheShim(t *testing.T) {
	// Arrange: the registry decides a departed shim's bounce by whether it
	// replaces the shim, so every shim bounce must say it does.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, nil); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}

	// Assert
	h.registry.mu.Lock()
	req := h.registry.pending[ws]
	h.registry.mu.Unlock()
	if !req.ReplacesShim {
		t.Fatalf("the shim bounce's request does not say it replaces the shim")
	}
}

func TestAnUnregisteredShimBounceIsAnOutcomeNotAFailure(t *testing.T) {
	// Arrange: a bounce registered behind work, whose shim then departs with
	// nothing left to replace.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	done := make(chan error, 1)
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}

	// Act
	h.registry.unregister(ws)

	// Assert
	if err := <-done; !errors.Is(err, bounce.ErrUnregistered) {
		t.Fatalf("done = %v, want ErrUnregistered handed on to the caller", err)
	}
	for _, rec := range records(h.log, opBounce) {
		if rec.Level == dlog.LevelError || rec.Level == dlog.LevelWarn {
			t.Fatalf("an unregistered bounce was recorded at %s: %q", rec.Level, rec.Message)
		}
	}
	found := false
	for _, rec := range records(h.log, opBounce) {
		if rec.Level == dlog.LevelInfo && strings.Contains(rec.Message, "unregistered") {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want the unregistration at INFO", h.log.Records())
	}
}
