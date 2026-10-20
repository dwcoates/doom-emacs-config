package restarttiming_test

import (
	"errors"
	"testing"
	"time"

	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
)

func TestConstructorsLabelEachRestart(t *testing.T) {
	ws := ids.WorkspaceID("w1")
	cases := []struct {
		name      string
		got       restarttiming.Observation
		restart   restarttiming.Restart
		phase     string
		cause     string
		outcome   restarttiming.Outcome
		workspace ids.WorkspaceID
		seconds   float64
	}{
		{"deploy", restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, 2*time.Second), restarttiming.Deploy, "build", "", restarttiming.OK, "", 2},
		{"boot", restarttiming.OfBoot(restarttiming.BootServing, restarttiming.BootSuccessor, 1500*time.Millisecond), restarttiming.DaemonBoot, "serving", "successor", restarttiming.OK, "", 1.5},
		{"service", restarttiming.OfService(restarttiming.Store, restarttiming.Failed, time.Second), restarttiming.ServiceRestart, "store", "", restarttiming.Failed, "", 1},
		{"workspace start", restarttiming.OfWorkspaceStart(ws, restarttiming.StartWake, restarttiming.StartTakeover, restarttiming.OK, 3*time.Second), restarttiming.WorkspaceStart, "wake", "takeover", restarttiming.OK, ws, 3},
		{"shim restart", restarttiming.OfShimRestart(ws, restarttiming.ShimResume, "build_stale", restarttiming.OK, 4*time.Second), restarttiming.ShimRestart, "resume", "build_stale", restarttiming.OK, ws, 4},
		{"transfer", restarttiming.OfTransfer(ws, restarttiming.TransferRun, restarttiming.OK, 250*time.Millisecond), restarttiming.Transfer, "run", "", restarttiming.OK, ws, 0.25},
		{"editor start", restarttiming.OfEditorStart(restarttiming.EditorFirstTab, restarttiming.EditorRunning, 7*time.Second), restarttiming.EditorStart, "first_tab", "running", restarttiming.OK, "", 7},
		{"session start", restarttiming.OfSessionStart(ws, restarttiming.SessionVendor, restarttiming.SessionWake, restarttiming.Failed, 5*time.Second), restarttiming.SessionStart, "vendor", "wake", restarttiming.Failed, ws, 5},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			o := tc.got

			// Assert
			if o.Restart() != tc.restart || o.Phase() != tc.phase || o.Cause() != tc.cause ||
				o.Outcome() != tc.outcome || o.Workspace() != tc.workspace || o.Seconds() != tc.seconds {
				t.Fatalf("observation = %+v, want restart %s phase %s cause %q outcome %s workspace %q seconds %v",
					o, tc.restart, tc.phase, tc.cause, tc.outcome, tc.workspace, tc.seconds)
			}
		})
	}
}

func TestNegativeDurationPanics(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("a negative duration did not panic")
		}
	}()

	// Act
	restarttiming.OfDeploy(restarttiming.DeployTotal, restarttiming.OK, -time.Millisecond)
}

func TestOutcomeOf(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want restarttiming.Outcome
	}{
		{"nil is ok", nil, restarttiming.OK},
		{"an error is failed", errors.New("boom"), restarttiming.Failed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := restarttiming.OutcomeOf(tc.err); got != tc.want {
				t.Fatalf("OutcomeOf(%v) = %s, want %s", tc.err, got, tc.want)
			}
		})
	}
}

func TestABootTimerTimesEachPhaseFromTheProcessStart(t *testing.T) {
	// Arrange
	started := time.Date(2026, 10, 19, 12, 0, 0, 0, time.UTC)
	now := started
	var rec restarttimingtest.Recorder
	timer := restarttiming.NewBootTimer(&rec, restarttiming.BootSuccessor, started, func() time.Time { return now })

	// Act
	now = started.Add(2 * time.Second)
	timer.Reached(restarttiming.BootClaim)
	now = started.Add(5 * time.Second)
	timer.Reached(restarttiming.BootServing)

	// Assert
	got := rec.Observations()
	if len(got) != 2 ||
		got[0].Phase() != "claim" || got[0].Seconds() != 2 || got[0].Cause() != "successor" ||
		got[1].Phase() != "serving" || got[1].Seconds() != 5 {
		t.Fatalf("observations = %+v, want claim at 2s and serving at 5s, both successor", got)
	}
}
