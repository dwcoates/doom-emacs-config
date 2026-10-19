package restarttiming

import (
	"errors"
	"testing"
	"time"

	"agent-repld/internal/ids"
)

func TestConstructorsLabelEachRestart(t *testing.T) {
	ws := ids.WorkspaceID("w1")
	cases := []struct {
		name      string
		got       Observation
		restart   Restart
		phase     string
		cause     string
		outcome   Outcome
		workspace ids.WorkspaceID
		seconds   float64
	}{
		{"deploy", OfDeploy(DeployBuild, OK, 2*time.Second), Deploy, "build", "", OK, "", 2},
		{"boot", OfBoot(BootServing, BootSuccessor, 1500*time.Millisecond), DaemonBoot, "serving", "successor", OK, "", 1.5},
		{"service", OfService(Store, Failed, time.Second), ServiceRestart, "store", "", Failed, "", 1},
		{"workspace start", OfWorkspaceStart(ws, StartWake, StartTakeover, OK, 3*time.Second), WorkspaceStart, "wake", "takeover", OK, ws, 3},
		{"shim restart", OfShimRestart(ws, ShimResume, "build_stale", OK, 4*time.Second), ShimRestart, "resume", "build_stale", OK, ws, 4},
		{"transfer", OfTransfer(ws, TransferRun, OK, 250*time.Millisecond), Transfer, "run", "", OK, ws, 0.25},
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
	OfDeploy(DeployTotal, OK, -time.Millisecond)
}

func TestOutcomeOf(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want Outcome
	}{
		{"nil is ok", nil, OK},
		{"an error is failed", errors.New("boom"), Failed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			if got := OutcomeOf(tc.err); got != tc.want {
				t.Fatalf("OutcomeOf(%v) = %s, want %s", tc.err, got, tc.want)
			}
		})
	}
}
