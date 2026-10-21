package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
)

const (
	opUpdateDeployHold = "daemon.server.update_deploy_hold"
	opRollOutLanding   = "daemon.server.roll_out_landing"
)

// UpdateDeployHold holds or releases one repository's deploy stage. A release
// runs a skipped script deploy hook once; the hold ends first, and stands
// whatever the hook then does.
func (s *server) UpdateDeployHold(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateDeployHoldRequest],
) (*connect.Response[agentreplv1.UpdateDeployHoldResponse], error) {
	const rpc = "UpdateDeployHold"
	if err := validateUpdateDeployHoldRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateDeployHoldResponse{}
	scope := *scopeOf(req.Msg.GetRepository())
	if req.Msg.GetHold() != nil {
		if err := s.deps.Merge.HoldDeploys(ctx, scope); err != nil {
			return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
		}
		resp.Result = &agentreplv1.UpdateDeployHoldResponse_Success{Success: &agentreplv1.UpdateDeployHoldSuccess{
			Applied: &agentreplv1.UpdateDeployHoldSuccess_Held{Held: &agentreplv1.UpdateDeployHoldSuccessHeld{}}}}
		return connect.NewResponse(resp), nil
	}
	release, err := s.deps.Merge.ReleaseDeploys(ctx, scope)
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	s.log.Info(opUpdateDeployHold, "released a repository's deploy stage", dlog.Context{
		"repository": scope.Dir, "skipped": release.Skipped, "kind": int(release.Kind)})
	resp.Result = &agentreplv1.UpdateDeployHoldResponse_Success{Success: &agentreplv1.UpdateDeployHoldSuccess{
		Applied: &agentreplv1.UpdateDeployHoldSuccess_Released{Released: releasedOf(release)}}}
	return connect.NewResponse(resp), nil
}

// releasedOf renders what a release ran for the deploys its hold skipped.
func releasedOf(r merge.DeployRelease) *agentreplv1.UpdateDeployHoldSuccessReleased {
	out := &agentreplv1.UpdateDeployHoldSuccessReleased{SkippedDeploys: uint32(r.Skipped)}
	switch r.Kind {
	case merge.ReleaseNothingSkipped:
		out.Skipped = &agentreplv1.UpdateDeployHoldSuccessReleased_NothingSkipped{NothingSkipped: &agentreplv1.UpdateDeployHoldSuccessReleasedNothingSkipped{}}
	case merge.ReleaseScriptRan:
		out.Skipped = &agentreplv1.UpdateDeployHoldSuccessReleased_ScriptRan{ScriptRan: &agentreplv1.UpdateDeployHoldSuccessReleasedScriptRan{Path: r.Path, LogPath: r.LogPath}}
	case merge.ReleaseScriptFailed:
		out.Skipped = &agentreplv1.UpdateDeployHoldSuccessReleased_ScriptFailed{ScriptFailed: &agentreplv1.UpdateDeployHoldSuccessReleasedScriptFailed{Path: r.Path, LogPath: r.LogPath, Detail: r.Detail}}
	case merge.ReleasePromptNotRun:
		out.Skipped = &agentreplv1.UpdateDeployHoldSuccessReleased_PromptNotRun{PromptNotRun: &agentreplv1.UpdateDeployHoldSuccessReleasedPromptNotRun{}}
	case merge.ReleaseNoDeployHook:
		out.Skipped = &agentreplv1.UpdateDeployHoldSuccessReleased_NoDeployHook{NoDeployHook: &agentreplv1.UpdateDeployHoldSuccessReleasedNoDeployHook{Detail: r.Detail}}
	default:
		panic("server: a deploy release names no kind the contract draws")
	}
	return out
}

// RollOutLanding registers the rollout of a running deploy's landing, or
// starts one at once when no deploy is named.
func (s *server) RollOutLanding(
	ctx context.Context,
	req *connect.Request[agentreplv1.RollOutLandingRequest],
) (*connect.Response[agentreplv1.RollOutLandingResponse], error) {
	const rpc = "RollOutLanding"
	if err := validateRollOutLandingRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RollOutLandingResponse{}
	registered, err := s.deps.Merge.RollOutLanding(ctx, ids.LeaseID(req.Msg.GetDeploy().GetLedger()))
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	success := &agentreplv1.RollOutLandingSuccess{When: &agentreplv1.RollOutLandingSuccess_Started{Started: &agentreplv1.RollOutLandingSuccessStarted{}}}
	if registered {
		success.When = &agentreplv1.RollOutLandingSuccess_Registered{Registered: &agentreplv1.RollOutLandingSuccessRegistered{}}
	}
	s.log.Debug(opRollOutLanding, "answered a deploy hook's rollout ask", dlog.Context{"registered": registered})
	resp.Result = &agentreplv1.RollOutLandingResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}
