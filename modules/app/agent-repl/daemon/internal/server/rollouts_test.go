package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/deploy"
	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
)

// rolloutsRequest asks UpdateRollouts to hold, or (release) to release.
func rolloutsRequest(release bool) *connect.Request[agentreplv1.UpdateRolloutsRequest] {
	if release {
		return connect.NewRequest(&agentreplv1.UpdateRolloutsRequest{Action: &agentreplv1.UpdateRolloutsRequest_Release{Release: &agentreplv1.UpdateRolloutsRelease{}}})
	}
	return connect.NewRequest(&agentreplv1.UpdateRolloutsRequest{Action: &agentreplv1.UpdateRolloutsRequest_Hold{Hold: &agentreplv1.UpdateRolloutsHold{}}})
}

func TestAHoldHoldsAndDeploysNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.UpdateRollouts(context.Background(), rolloutsRequest(false))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetHeld() == nil {
		t.Fatalf("UpdateRollouts = %v, %v; want held", resp, err)
	}
	if h.Merge.holds != 1 || len(h.Deployer.forced) != 0 {
		t.Fatalf("holds %d, deploys %d; want one hold and no deploy", h.Merge.holds, len(h.Deployer.forced))
	}
}

func TestAReleaseDeploysWhatLanded(t *testing.T) {
	tests := []struct {
		name   string
		result deploy.Result
		err    error
		want   string
	}{
		{name: "a build that carries every landing", result: deploy.Result{Outcomes: []deploy.Outcome{
			{Component: deploy.ComponentDaemon, Kind: deploy.UpToDate}}}, want: "up_to_date"},
		{name: "a build that is behind", result: deploy.Result{Outcomes: []deploy.Outcome{
			{Component: deploy.ComponentDaemon, Kind: deploy.UpToDate}, {Component: deploy.ComponentElisp, Kind: deploy.ReloadPushed}}}, want: "rolling_out"},
		{name: "a deploy that cannot start", err: errors.New("build failed"), want: "start_failed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			h.Deployer.result, h.Deployer.err = tt.result, tt.err

			// Act
			resp, err := h.Client.UpdateRollouts(context.Background(), rolloutsRequest(true))

			// Assert
			if err != nil {
				t.Fatalf("UpdateRollouts: %v", err)
			}
			released := resp.Msg.GetSuccess().GetReleased()
			if h.Merge.releases != 1 || len(h.Deployer.forced) != 1 || h.Deployer.forced[0] {
				t.Fatalf("releases %d, deploys %v; want one release and one unforced deploy", h.Merge.releases, h.Deployer.forced)
			}
			got := ""
			switch {
			case released.GetUpToDate() != nil:
				got = "up_to_date"
			case released.GetRollingOut() != nil:
				got = "rolling_out"
				if len(released.GetRollingOut().GetDeploy().GetComponents()) != 2 {
					t.Errorf("rolling out = %v, want both components' decisions", released.GetRollingOut())
				}
			case released.GetStartFailed() != nil:
				got = "start_failed"
				if released.GetStartFailed().GetDetail() != "build failed" {
					t.Errorf("detail = %q, want the deploy's account", released.GetStartFailed().GetDetail())
				}
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opUpdateRollouts}); !ok {
					t.Errorf("no error record of the failed rollout: %+v", log.Records())
				}
			}
			if got != tt.want {
				t.Fatalf("released = %v, want %s", released, tt.want)
			}
		})
	}
}

func TestTheHoldRefusalsAreAnswers(t *testing.T) {
	tests := []struct {
		name    string
		release bool
		arm     string
		got     func(*agentreplv1.UpdateRolloutsError) bool
	}{
		{name: "a hold of held rollouts", arm: merge.ArmRolloutsAlreadyHeld,
			got: func(e *agentreplv1.UpdateRolloutsError) bool { return e.GetAlreadyHeld() != nil }},
		{name: "a release of rollouts not held", release: true, arm: merge.ArmRolloutsNotHeld,
			got: func(e *agentreplv1.UpdateRolloutsError) bool { return e.GetNotHeld() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			refusal := &merge.RefusalError{Arm: tt.arm, Reason: "nothing to change"}
			h.Merge.holdErr, h.Merge.releaseErr = refusal, refusal

			// Act
			resp, err := h.Client.UpdateRollouts(context.Background(), rolloutsRequest(tt.release))

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("UpdateRollouts = %v, %v; want the %s arm", resp, err, tt.arm)
			}
			if len(h.Deployer.forced) != 0 {
				t.Errorf("a refused release deployed")
			}
		})
	}
}

func TestUpdateRolloutsRefusesARequestWithNoAction(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UpdateRollouts(context.Background(), connect.NewRequest(&agentreplv1.UpdateRolloutsRequest{}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("UpdateRollouts = %v, want InvalidArgument", err)
	}
}

func TestARolloutHoldFailureIsATransportError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.holdErr = errors.New("state database unreadable")

	// Act
	_, err := h.Client.UpdateRollouts(context.Background(), rolloutsRequest(false))

	// Assert
	if err == nil {
		t.Fatal("UpdateRollouts answered a failed hold without an error")
	}
}
