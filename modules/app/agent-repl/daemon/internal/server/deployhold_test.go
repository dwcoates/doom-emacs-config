package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
)

// theHeldRepository is the repository every deploy-hold request names.
var theHeldRepository = &workspacev1.RepositoryRef{Id: "repo-1", Dir: "/repos/one/.git"}

// deployHoldRequest asks UpdateDeployHold to hold, or (release) to release,
// theHeldRepository.
func deployHoldRequest(release bool) *connect.Request[agentreplv1.UpdateDeployHoldRequest] {
	req := &agentreplv1.UpdateDeployHoldRequest{Repository: theHeldRepository,
		Action: &agentreplv1.UpdateDeployHoldRequest_Hold{Hold: &agentreplv1.UpdateDeployHoldHold{}}}
	if release {
		req.Action = &agentreplv1.UpdateDeployHoldRequest_Release{Release: &agentreplv1.UpdateDeployHoldRelease{}}
	}
	return connect.NewRequest(req)
}

func TestAHoldHoldsTheNamedRepository(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := h.Client.UpdateDeployHold(context.Background(), deployHoldRequest(false))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetHeld() == nil {
		t.Fatalf("UpdateDeployHold = %v, %v; want held", resp, err)
	}
	want := merge.RepositoryScope{ID: "repo-1", Dir: "/repos/one/.git"}
	if h.Merge.holds != 1 || len(h.Merge.holdScopes) != 1 || h.Merge.holdScopes[0] != want {
		t.Fatalf("holds %d of %v; want one hold of %v", h.Merge.holds, h.Merge.holdScopes, want)
	}
}

func TestAReleaseAnswersWhatRanForTheSkippedDeploys(t *testing.T) {
	tests := []struct {
		name    string
		release merge.DeployRelease
		check   func(*agentreplv1.UpdateDeployHoldSuccessReleased) bool
	}{
		{name: "nothing skipped", release: merge.DeployRelease{Kind: merge.ReleaseNothingSkipped},
			check: func(r *agentreplv1.UpdateDeployHoldSuccessReleased) bool { return r.GetNothingSkipped() != nil }},
		{name: "the script ran", release: merge.DeployRelease{Kind: merge.ReleaseScriptRan, Skipped: 2, Path: "bin/deploy.sh", LogPath: "/logs/release-1.log"},
			check: func(r *agentreplv1.UpdateDeployHoldSuccessReleased) bool {
				return r.GetScriptRan().GetPath() == "bin/deploy.sh" && r.GetScriptRan().GetLogPath() == "/logs/release-1.log" && r.GetSkippedDeploys() == 2
			}},
		{name: "the script failed", release: merge.DeployRelease{Kind: merge.ReleaseScriptFailed, Skipped: 1, Path: "bin/deploy.sh", LogPath: "/logs/release-1.log", Detail: "it exited 4"},
			check: func(r *agentreplv1.UpdateDeployHoldSuccessReleased) bool {
				return r.GetScriptFailed().GetDetail() == "it exited 4" && r.GetScriptFailed().GetPath() == "bin/deploy.sh"
			}},
		{name: "a prompt deploy hook", release: merge.DeployRelease{Kind: merge.ReleasePromptNotRun, Skipped: 1},
			check: func(r *agentreplv1.UpdateDeployHoldSuccessReleased) bool { return r.GetPromptNotRun() != nil }},
		{name: "no deploy hook", release: merge.DeployRelease{Kind: merge.ReleaseNoDeployHook, Skipped: 1, Detail: "none named"},
			check: func(r *agentreplv1.UpdateDeployHoldSuccessReleased) bool { return r.GetNoDeployHook().GetDetail() == "none named" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.release = tt.release

			// Act
			resp, err := h.Client.UpdateDeployHold(context.Background(), deployHoldRequest(true))

			// Assert
			if err != nil {
				t.Fatalf("UpdateDeployHold: %v", err)
			}
			if released := resp.Msg.GetSuccess().GetReleased(); h.Merge.releases != 1 || !tt.check(released) {
				t.Fatalf("released = %v after %d release(s), want the release's own account", released, h.Merge.releases)
			}
		})
	}
}

func TestTheHoldRefusalsAreAnswers(t *testing.T) {
	tests := []struct {
		name    string
		release bool
		arm     string
		got     func(*agentreplv1.UpdateDeployHoldError) bool
	}{
		{name: "a hold of a held repository", arm: merge.ArmDeploysAlreadyHeld,
			got: func(e *agentreplv1.UpdateDeployHoldError) bool { return e.GetAlreadyHeld() != nil }},
		{name: "a release of a repository not held", release: true, arm: merge.ArmDeploysNotHeld,
			got: func(e *agentreplv1.UpdateDeployHoldError) bool { return e.GetNotHeld() != nil }},
		{name: "a hold of an unknown repository", arm: merge.ArmUnknownRepository,
			got: func(e *agentreplv1.UpdateDeployHoldError) bool { return e.GetUnknownRepository() != nil }},
		{name: "a release of an unknown repository", release: true, arm: merge.ArmUnknownRepository,
			got: func(e *agentreplv1.UpdateDeployHoldError) bool { return e.GetUnknownRepository() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			refusal := &merge.RefusalError{Arm: tt.arm, Reason: "nothing to change"}
			h.Merge.holdErr, h.Merge.releaseErr = refusal, refusal

			// Act
			resp, err := h.Client.UpdateDeployHold(context.Background(), deployHoldRequest(tt.release))

			// Assert
			if err != nil || !tt.got(resp.Msg.GetError()) {
				t.Fatalf("UpdateDeployHold = %v, %v; want the %s arm", resp, err, tt.arm)
			}
		})
	}
}

func TestUpdateDeployHoldRefusesAnIncompleteRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.UpdateDeployHoldRequest
	}{
		{name: "no repository", req: &agentreplv1.UpdateDeployHoldRequest{Action: &agentreplv1.UpdateDeployHoldRequest_Hold{Hold: &agentreplv1.UpdateDeployHoldHold{}}}},
		{name: "no action", req: &agentreplv1.UpdateDeployHoldRequest{Repository: theHeldRepository}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.UpdateDeployHold(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || h.Merge.holds+h.Merge.releases != 0 {
				t.Fatalf("UpdateDeployHold = %v after %d verb(s), want InvalidArgument and none", err, h.Merge.holds+h.Merge.releases)
			}
		})
	}
}

func TestADeployHoldStoreFailureIsATransportError(t *testing.T) {
	tests := []struct {
		name    string
		release bool
	}{
		{name: "a hold"},
		{name: "a release", release: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Merge.holdErr, h.Merge.releaseErr = errors.New("state database unreadable"), errors.New("state database unreadable")

			// Act
			_, err := h.Client.UpdateDeployHold(context.Background(), deployHoldRequest(tt.release))

			// Assert
			if err == nil {
				t.Fatal("UpdateDeployHold answered a failed store without an error")
			}
		})
	}
}

func TestRollOutLandingAnswersWhenTheRolloutRuns(t *testing.T) {
	tests := []struct {
		name       string
		req        *agentreplv1.RollOutLandingRequest
		registered bool
		wantLedger ids.LeaseID
		check      func(*agentreplv1.RollOutLandingSuccess) bool
	}{
		{name: "a deploy's ledger registers it", registered: true, wantLedger: "0011aabb",
			req:   &agentreplv1.RollOutLandingRequest{Deploy: &agentreplv1.DeployLedger{Ledger: "0011aabb"}},
			check: func(s *agentreplv1.RollOutLandingSuccess) bool { return s.GetRegistered() != nil }},
		{name: "no deploy starts it", req: &agentreplv1.RollOutLandingRequest{},
			check: func(s *agentreplv1.RollOutLandingSuccess) bool { return s.GetStarted() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
			h.Merge.rollOutRegistered = tt.registered

			// Act
			resp, err := h.Client.RollOutLanding(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil || !tt.check(resp.Msg.GetSuccess()) {
				t.Fatalf("RollOutLanding = %v, %v; want %s", resp, err, tt.name)
			}
			if len(h.Merge.rollOuts) != 1 || h.Merge.rollOuts[0] != tt.wantLedger {
				t.Fatalf("rollouts asked = %v, want one naming %q", h.Merge.rollOuts, tt.wantLedger)
			}
		})
	}
}

func TestRollOutLandingAnswersAnUnknownDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Merge.rollOutErr = &merge.RefusalError{Arm: merge.ArmUnknownDeploy, Reason: "no such deploy"}

	// Act
	resp, err := h.Client.RollOutLanding(context.Background(), connect.NewRequest(&agentreplv1.RollOutLandingRequest{
		Deploy: &agentreplv1.DeployLedger{Ledger: "0011aabb"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownDeploy() == nil {
		t.Fatalf("RollOutLanding = %v, %v; want unknown_deploy", resp, err)
	}
}

func TestRollOutLandingRefusesANamedDeployWithNoLedger(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.RollOutLanding(context.Background(), connect.NewRequest(&agentreplv1.RollOutLandingRequest{
		Deploy: &agentreplv1.DeployLedger{Ledger: "  "}}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Merge.rollOuts) != 0 {
		t.Fatalf("RollOutLanding = %v with %d ask(s), want InvalidArgument and none", err, len(h.Merge.rollOuts))
	}
}
