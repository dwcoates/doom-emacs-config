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
	"claude-repld/internal/health"
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

// TestEveryFailedRelaunchReleasesTheRestartHold pins the hold's scope-bound
// lifetime: whichever step a bounce fails at, the restart-pending hold it took
// is released -- including a stand-down cut short by its own context, whose
// release used to be written through that same cancelled context and refused.
func TestEveryFailedRelaunchReleasesTheRestartHold(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness, ws ids.WorkspaceID)
	}{
		{name: "a replacement dead before the stand-down", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			fresh := newFakeShim(9999, h.order)
			fresh.Die()
			h.fleet.prelaunched[ws] = fresh
		}},
		{name: "a failed install", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			h.fleet.installErr[ws] = errFake
			h.fleet.live[ws].onKillSession = func() { h.fleet.live[ws].Reap() }
		}},
		{name: "a stand-down ended by its context", arrange: func(t *testing.T, h *harness, ws ids.WorkspaceID) {
			ctx, cancel := context.WithCancel(context.Background())
			h.registry.runCtx = ctx
			h.fleet.live[ws].onKillSession = cancel
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			tt.arrange(t, h, ws)

			// Act
			err := bounceAndWait(t, h, ws, ReasonRestartVerb, false)

			// Assert
			if err == nil {
				t.Fatal("the bounce succeeded; the case is a failure path")
			}
			if _, held, dbErr := h.db.Lease(context.Background(), ws); dbErr != nil || held {
				t.Fatalf("Lease after the failed bounce = (held %v, %v), want the restart hold released", held, dbErr)
			}
		})
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

// TestADeadReplacementStopsTheBounceBeforeTheStandDown covers a replacement
// that died before the point of no return: the old shim keeps serving.
func TestADeadReplacementStopsTheBounceBeforeTheStandDown(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	fresh := newFakeShim(9999, h.order)
	fresh.Die()
	h.fleet.prelaunched[ws] = fresh

	// Act
	err := bounceAndWait(t, h, ws, ReasonBuildStale, false)

	// Assert
	if err == nil {
		t.Fatal("the bounce succeeded over a dead replacement")
	}
	taken := h.order.Taken()
	if indexOf(taken, "kill_session") >= 0 || indexOf(taken, "install") >= 0 {
		t.Fatalf("steps = %v, want neither a stand-down nor an install", taken)
	}
}

// TestAReplacementThatDiesDuringTheStandDownIsNeverInstalled is the 18:27:44
// deploy: the replacement exited while the old shim stood down, and the
// relaunch installed it anyway. A fresh prelaunch is installed instead.
func TestAReplacementThatDiesDuringTheStandDownIsNeverInstalled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	dying := newFakeShim(9999, h.order)
	h.fleet.prelaunched[ws] = dying
	second := newFakeShim(9998, h.order)
	old.onKillSession = func() {
		dying.Die()
		h.fleet.mu.Lock()
		h.fleet.prelaunched[ws] = second
		h.fleet.mu.Unlock()
		old.Reap()
	}

	// Act
	err := bounceAndWait(t, h, ws, ReasonBuildStale, false)

	// Assert
	if err != nil {
		t.Fatalf("bounce: %v", err)
	}
	h.fleet.mu.Lock()
	installed := h.fleet.live[ws]
	h.fleet.mu.Unlock()
	if installed != second {
		t.Fatalf("installed pid %d, want the second prelaunch's 9998 and never the dead replacement", installed.pid)
	}
}

// TestAFailedSecondPrelaunchFailsTheBounceWithNothingInstalled covers the
// replacement for the dead replacement refusing too.
func TestAFailedSecondPrelaunchFailsTheBounceWithNothingInstalled(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	dying := newFakeShim(9999, h.order)
	h.fleet.prelaunched[ws] = dying
	old.onKillSession = func() {
		dying.Die()
		h.fleet.mu.Lock()
		h.fleet.prelaunchErr[ws] = errFake
		h.fleet.mu.Unlock()
		old.Reap()
	}

	// Act
	err := bounceAndWait(t, h, ws, ReasonBuildStale, false)

	// Assert
	if err == nil {
		t.Fatal("the bounce succeeded with no live replacement")
	}
	if indexOf(h.order.Taken(), "install") >= 0 {
		t.Fatalf("steps = %v, want nothing installed", h.order.Taken())
	}
	if !loggedError(h.log, opRelaunch, "the second prelaunch failed") {
		t.Fatalf("records = %+v, want the failed second prelaunch at ERROR", h.log.Records())
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

// TestAShimBounceEndedUnrunIsAnOutcomeNotAFailure pins the Done outcomes that
// are not failures: the bounce was unregistered (nothing is left to replace),
// or it was handed across to the daemon a move took the workspace to. Each is
// recorded at INFO, never WARN or ERROR, and handed on to the caller whole.
//
// MEASURED, deploy 2026-09-29T17:15:28: the hand-across was recorded as "the
// shim bounce failed" at ERROR for every busy workspace the deploy handed over.
func TestAShimBounceEndedUnrunIsAnOutcomeNotAFailure(t *testing.T) {
	tests := []struct {
		name string
		why  error
		// word is what the INFO record's message must name.
		word string
	}{
		{name: "the shim it would replace departed", why: bounce.ErrUnregistered, word: "unregistered"},
		{name: "a move carried it to the successor", why: bounce.ErrHandedAcross, word: "handed across"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a bounce registered behind work.
			h := newHarness(t)
			ws, _ := h.workspace(t)
			h.freeness.SetFree(ws, false)
			done := make(chan error, 1)
			if _, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, func(err error) { done <- err }); err != nil {
				t.Fatalf("BounceShim: %v", err)
			}

			// Act
			h.registry.endUnrun(ws, tt.why)

			// Assert
			if err := <-done; !errors.Is(err, tt.why) {
				t.Fatalf("done = %v, want %v handed on to the caller", err, tt.why)
			}
			found := false
			for _, rec := range records(h.log, opBounce) {
				if rec.Level == dlog.LevelError || rec.Level == dlog.LevelWarn {
					t.Fatalf("the bounce's outcome was recorded at %s: %q", rec.Level, rec.Message)
				}
				if rec.Level == dlog.LevelInfo && strings.Contains(rec.Message, tt.word) {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want the outcome at INFO naming %q", h.log.Records(), tt.word)
			}
		})
	}
}

// A RELAUNCH IS A RECOVERY EDGE (health/lifetime.go): the installed
// replacement is a healthy attach, and a warm resume a started session. A
// cold answer started nothing yet, so the session-start faults wait for the
// gate's re-open.
func TestARelaunchClosesTheFaultsWhoseLifetimeEndsThere(t *testing.T) {
	tests := []struct {
		name   string
		kind   string
		cold   bool
		closes bool
	}{
		{"a warm resume closes a refused resume", health.KindResumeFailed, false, true},
		{"a warm resume closes an undetermined bounce", health.KindBounceUnknown, false, true},
		{"a cold resume leaves a refused resume standing", health.KindResumeFailed, true, false},
		{"a cold resume still closes the attach's faults", health.KindShimDied, true, true},
		{"a relaunch leaves a fault about the conversation standing", health.KindConversationAbandoned, false, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			if tt.cold {
				h.fleet.resumeCold[ws] = &conversationv1.SessionCold{}
			}
			if _, err := h.db.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: tt.kind}); err != nil {
				t.Fatalf("OpenFault: %v", err)
			}

			// Act
			if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
				t.Fatalf("bounce: %v", err)
			}
			open, err := h.db.OpenFaults(context.Background(), wsm.FaultScope{Workspace: &ws, Kind: tt.kind})

			// Assert
			if err != nil {
				t.Fatalf("OpenFaults: %v", err)
			}
			if closed := len(open) == 0; closed != tt.closes {
				t.Fatalf("%s closed = %v, want %v", tt.kind, closed, tt.closes)
			}
		})
	}
}

// liveRefusal is the shim's refusal of an unforced stand-down: a turn the
// vendor started on its own is in flight.
func liveRefusal() *shimv1.KillSessionResponse {
	return &shimv1.KillSessionResponse{
		Result: &shimv1.KillSessionResponse_Failure{Failure: &shimv1.KillSessionFailure{
			Cause: &shimv1.KillSessionFailure_Live{Live: &conversationv1.SessionLive{
				TurnInFlight: &conversationv1.TurnId{Value: "vendor-turn"},
			}},
			Detail: "a turn is in flight",
		}},
	}
}

// deferredBounce asks for an unforced bounce of a free workspace whose shim
// refuses the stand-down as live, and joins the run that deferred it. done
// receives the requester's outcome, which a deferral never sends.
func deferredBounce(t *testing.T, h *harness, ws ids.WorkspaceID, reason RelaunchReason) (old *fakeShim, done chan error) {
	t.Helper()
	old = h.fleet.live[ws]
	old.killAnswer = liveRefusal()
	done = make(chan error, 2)
	if _, err := h.c.BounceShim(context.Background(), ws, reason, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	h.registry.wait()
	return old, done
}

// AN UNFORCED SHIM REPLACEMENT NEVER ENDS LIVE WORK. Regression, 2026-10-01
// (footer-activity-updates): the last detached subagent concluded, the
// registry took the build_stale bounce, the vendor started a turn on its own,
// the shim refused the stand-down as live, and the daemon waited out the 30s
// window and force-killed the shim -- the running turn and a resumed subagent
// with it.
func TestALiveRefusalOfAnUnforcedStandDownDefersTheBounce(t *testing.T) {
	tests := []struct {
		name   string
		assert func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error)
	}{
		{name: "the old shim is never force-killed", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			if kills := old.ForceKills(); len(kills) != 0 {
				t.Fatalf("force-kills = %+v, want none over live work", kills)
			}
		}},
		{name: "the stand-down window is never waited out", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			for _, d := range h.clock.Waits() {
				if d == standDownWindow {
					t.Fatalf("waits = %v, want no stand-down window armed after a live refusal", h.clock.Waits())
				}
			}
		}},
		{name: "the old shim keeps serving", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			if live, ok := h.fleet.Client(ws); !ok || live != old {
				t.Fatalf("the workspace's client = %v, want the old shim still serving", live)
			}
		}},
		{name: "the prelaunched shim is retired", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			h.fleet.mu.Lock()
			fresh := h.fleet.prelaunched[ws]
			h.fleet.mu.Unlock()
			if fresh == nil || len(fresh.ForceKills()) != 1 {
				t.Fatalf("the prelaunched shim was left running after the deferral")
			}
		}},
		{name: "the restart hold is released", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
				t.Fatalf("lease held = %v (err %v), want the restart hold released", held, err)
			}
		}},
		{name: "the bounce is registered again behind the work", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			deferrals := h.registry.Deferrals()
			if !h.registry.Pending(ws) || len(deferrals) != 1 || !errors.Is(deferrals[0], ErrStandDownLive) {
				t.Fatalf("pending = %v, deferrals = %v; want the bounce re-registered on ErrStandDownLive", h.registry.Pending(ws), deferrals)
			}
		}},
		{name: "the requester is not told", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			select {
			case err := <-done:
				t.Fatalf("done = %v, want the requester kept for the rerun", err)
			default:
			}
		}},
		{name: "the refusal is recorded at INFO", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			for _, rec := range records(h.log, opRelaunch) {
				if rec.Level == dlog.LevelInfo && strings.Contains(rec.Message, "refused the unforced stand-down") &&
					rec.Context["refusal"] == "live" && rec.Context["turn_in_flight"] == true {
					return
				}
			}
			t.Fatalf("records = %+v, want the live refusal at INFO with its context", records(h.log, opRelaunch))
		}},
		{name: "nothing is recorded as a fault", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim, done chan error) {
			for _, op := range []string{opRelaunch, opBounce} {
				for _, rec := range records(h.log, op) {
					if rec.Level == dlog.LevelError || rec.Level == dlog.LevelWarn {
						t.Fatalf("the deferral recorded %s: %q", rec.Level, rec.Message)
					}
				}
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)

			// Act
			old, done := deferredBounce(t, h, ws, ReasonBuildStale)

			// Assert
			tt.assert(t, h, ws, old, done)
		})
	}
}

func TestADeferredBounceRunsAtTheNextFreeness(t *testing.T) {
	// Arrange: the shim refused once; its turn then ends and it accepts.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old, done := deferredBounce(t, h, ws, ReasonBuildStale)
	old.mu.Lock()
	old.killAnswer = nil
	old.mu.Unlock()
	old.onKillSession = func() { old.Reap() }

	// Act
	if !h.registry.free(ws) {
		t.Fatalf("no bounce was registered to take at the freeness edge")
	}

	// Assert
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("done = %v, want the rerun's clean finish", err)
	}
	if kills := old.ForceKills(); len(kills) != 0 {
		t.Fatalf("force-kills = %+v, want the rerun's graceful stand-down alone", kills)
	}
	if got := old.KillRequests(); len(got) != 2 || got[1].GetForce() {
		t.Fatalf("stand-down requests = %+v, want the refused one and the accepted unforced one", got)
	}
}

// An unforced stand-down the shim never answered: the shim never vouched that
// nothing is live in it, so it is not forced either.
func TestAnUnansweredUnforcedStandDownDefersTheBounce(t *testing.T) {
	tests := []struct {
		name   string
		assert func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim)
	}{
		{name: "the old shim is never force-killed", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim) {
			if kills := old.ForceKills(); len(kills) != 0 {
				t.Fatalf("force-kills = %+v, want none on an unanswered stand-down", kills)
			}
		}},
		{name: "the bounce is registered again behind the work", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim) {
			deferrals := h.registry.Deferrals()
			if !h.registry.Pending(ws) || len(deferrals) != 1 || !errors.Is(deferrals[0], ErrStandDownUnanswered) || !errors.Is(deferrals[0], errFake) {
				t.Fatalf("pending = %v, deferrals = %v; want the bounce re-registered on ErrStandDownUnanswered naming the call's error", h.registry.Pending(ws), deferrals)
			}
		}},
		{name: "the restart hold is released", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim) {
			if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
				t.Fatalf("lease held = %v (err %v), want the restart hold released", held, err)
			}
		}},
		{name: "the unanswered stand-down is recorded at ERROR", assert: func(t *testing.T, h *harness, ws ids.WorkspaceID, old *fakeShim) {
			if !loggedErrorWith(h.log, opRelaunch, "did not answer the unforced stand-down", errFake.Error()) {
				t.Fatalf("records = %+v, want the unanswered stand-down at ERROR with its cause", records(h.log, opRelaunch))
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ws, _ := h.workspace(t)
			old := h.fleet.live[ws]
			old.killErr = errFake

			// Act
			if _, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, nil); err != nil {
				t.Fatalf("BounceShim: %v", err)
			}
			h.clock.awaitArmed(t, standDownWindow)
			h.clock.Fire(standDownWindow)
			h.registry.wait()

			// Assert
			tt.assert(t, h, ws, old)
		})
	}
}

// The call failed but the shim LEFT: it took the stand-down and only its
// answer was lost, so the gate is passed and the bounce goes on.
func TestAnUnansweredUnforcedStandDownWhoseShimLeavesInsideTheWindowPassesTheGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	old.killErr = errFake
	done := make(chan error, 1)

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	h.clock.awaitArmed(t, standDownWindow)
	old.Reap()

	// Assert
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("done = %v, want the bounce finished past the gate", err)
	}
	if kills := old.ForceKills(); len(kills) != 0 {
		t.Fatalf("force-kills = %+v, want none", kills)
	}
}

// A FORCED bounce is the user's ask: a refusal is waited out and the shim
// force-killed at the window's end, exactly as before.
func TestAForcedStandDownTheShimRefusesIsStillForceKilledAtTheWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	old.killAnswer = liveRefusal()
	done := make(chan error, 1)

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, true, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	h.clock.awaitArmed(t, standDownWindow)
	h.clock.Fire(standDownWindow)
	waitForForceKill(t, old)
	old.Reap()

	// Assert
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("done = %v, want the forced bounce finished", err)
	}
	if len(h.registry.Deferrals()) != 0 {
		t.Fatalf("deferrals = %v, want a forced bounce never deferred", h.registry.Deferrals())
	}
}

// A DEFERRED STALE-BUILD BOUNCE NEVER RAN, so it is still the workspace's
// bounce in flight: a re-report is skipped as in flight (never "already
// bounced for this build" at ERROR), and the registry's rerun replaces the
// shim.
func TestADeferredStaleBounceIsNotRefusedAsAlreadyBounced(t *testing.T) {
	// Arrange: the stale-build bounce is deferred by a live refusal.
	h := newHarness(t)
	ws := staleReported(t, h)
	old := h.fleet.live[ws]
	old.killAnswer = liveRefusal()
	if _, err := h.c.CheckStaleness(context.Background(), ws, false); err != nil {
		t.Fatalf("CheckStaleness: %v", err)
	}
	h.registry.wait()

	// Act: the shim re-reports, then falls free and accepts the rerun.
	got, err := h.c.CheckStaleness(context.Background(), ws, false)
	old.mu.Lock()
	old.killAnswer = nil
	old.mu.Unlock()
	old.onKillSession = func() { old.Reap() }
	h.registry.free(ws)
	h.registry.wait()
	h.c.staleChecks.Wait()

	// Assert
	if err != nil || got.Skipped != SkippedBounceInFlight {
		t.Fatalf("re-check = (%+v, %v), want it skipped as in flight", got, err)
	}
	if loggedError(h.log, opStaleness, "already bounced for") {
		t.Fatalf("records = %+v, want no already-bounced ERROR for a deferred bounce", h.log.Records())
	}
	h.fleet.mu.Lock()
	installs := len(h.fleet.installs)
	h.fleet.mu.Unlock()
	if installs != 1 {
		t.Fatalf("installs = %d, want the rerun to replace the shim once", installs)
	}
}

// The registry keeps a deferred bounce's Done for the rerun, so a deferral
// told to it is the registry's contract broken, said at ERROR.
func TestADeferralToldToTheShimBouncesCompletionIsRecordedAsAContractBreach(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonBuildStale, false, nil); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}

	// Act
	h.registry.endUnrun(ws, ErrStandDownLive)

	// Assert
	if !loggedError(h.log, opBounce, "told a deferral") {
		t.Fatalf("records = %+v, want the breach at ERROR", records(h.log, opBounce))
	}
}

// A SHIM HOLDING NO SESSION -- its vendor never started -- answers the
// stand-down `no_session` and does not leave; it is stopped at once rather
// than after the window.
func TestASessionlessOldShimIsStoppedAtOnceWithoutTheWindow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	old.killAnswer = &shimv1.KillSessionResponse{
		Result: &shimv1.KillSessionResponse_Failure{Failure: &shimv1.KillSessionFailure{
			Cause: &shimv1.KillSessionFailure_NoSession{NoSession: &shimv1.KillSessionNoSession{}},
		}},
	}
	done := make(chan error, 1)

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, true, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	waitForForceKill(t, old)
	old.Reap()

	// Assert
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("done = %v, want the bounce finished", err)
	}
	for _, armed := range h.clock.Waits() {
		if armed == standDownWindow {
			t.Fatalf("the stand-down window was armed for a shim that held no session")
		}
	}
}

// A RESTART THAT ENDS THE RESUME'S VENDOR-START RUN relaunches over the shim
// the bounce had just installed, forced, instead of failing the bounce.
func TestAResumeEndedByARestartRelaunchesOverTheInstalledShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	first := newFakeShim(7001, h.order)
	h.fleet.prelaunched[ws] = first
	h.fleet.resumeErrOnce[ws] = ErrResumeRestarted
	old := h.fleet.live[ws]
	old.onKillSession = func() {
		// Past the first prelaunch: the relaunch over `first` prelaunches a
		// fresh shim.
		h.fleet.mu.Lock()
		h.fleet.prelaunched[ws] = newFakeShim(7002, h.order)
		h.fleet.mu.Unlock()
	}
	done := make(chan error, 1)
	old.Reap()
	first.Reap()

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, true, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}

	// Assert
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("done = %v, want the relaunch over the restarted resume finished", err)
	}
	reqs := first.KillRequests()
	if len(reqs) != 1 || !reqs[0].GetForce() {
		t.Fatalf("stand-downs of the restarted shim = %+v, want one forced", reqs)
	}
	if got := h.fleet.live[ws].PID(); got != 7002 {
		t.Fatalf("installed pid = %d, want the relaunch's 7002", got)
	}
}

// A FORCED STAND-DOWN THE SHIM NEVER ANSWERS is bounded: the call ends at its
// bound and the shim is killed at once, never after the window.
func TestAForcedStandDownThatHangsIsKilledAtItsBound(t *testing.T) {
	// Arrange
	h := newHarness(t, func(d *Deps) { d.StandDownCallBound = time.Millisecond })
	ws, _ := h.workspace(t)
	old := h.fleet.live[ws]
	old.killHangs = true
	done := make(chan error, 1)

	// Act
	if _, err := h.c.BounceShim(context.Background(), ws, ReasonRestartVerb, true, func(err error) { done <- err }); err != nil {
		t.Fatalf("BounceShim: %v", err)
	}
	waitForForceKill(t, old)
	old.Reap()

	// Assert
	if err := awaitBounce(t, h, done); err != nil {
		t.Fatalf("done = %v, want the bounce finished past the hung stand-down", err)
	}
	for _, armed := range h.clock.Waits() {
		if armed == standDownWindow {
			t.Fatalf("the stand-down window was waited out for a hung forced stand-down")
		}
	}
}
