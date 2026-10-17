package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/deploy"
	"agent-repld/internal/dlog"
	"agent-repld/internal/merge"
)

const opUpdateRollouts = "daemon.server.update_rollouts"

// rolloutRenames spells the merge queue's hold refusals as UpdateRollouts'
// own arms.
var rolloutRenames = map[string]string{
	merge.ArmRolloutsAlreadyHeld: "already_held",
	merge.ArmRolloutsNotHeld:     "not_held",
}

// UpdateRollouts holds or releases the runtime's rollouts. A release deploys
// what landed meanwhile: the hold ends first, and stands whatever the deploy
// then does.
func (s *server) UpdateRollouts(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateRolloutsRequest],
) (*connect.Response[agentreplv1.UpdateRolloutsResponse], error) {
	const rpc = "UpdateRollouts"
	if err := validateUpdateRolloutsRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateRolloutsResponse{}
	if req.Msg.GetHold() != nil {
		if err := s.deps.Merge.HoldRollouts(ctx); err != nil {
			return answer(resp, s.answerRefusal(s.log, rpc, resp, err, rolloutRenames))
		}
		s.log.Info(opUpdateRollouts, "held the rollouts", nil)
		resp.Result = &agentreplv1.UpdateRolloutsResponse_Success{Success: &agentreplv1.UpdateRolloutsSuccess{
			Applied: &agentreplv1.UpdateRolloutsSuccess_Held{Held: &agentreplv1.UpdateRolloutsSuccessHeld{}}}}
		return connect.NewResponse(resp), nil
	}
	if err := s.deps.Merge.ReleaseRollouts(ctx); err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, rolloutRenames))
	}
	released := s.deployReleased(ctx)
	resp.Result = &agentreplv1.UpdateRolloutsResponse_Success{Success: &agentreplv1.UpdateRolloutsSuccess{
		Applied: &agentreplv1.UpdateRolloutsSuccess_Released{Released: released}}}
	return connect.NewResponse(resp), nil
}

// deployReleased deploys what landed while the rollouts were held, and
// answers what became of it. A deploy that cannot start is an answer: the
// release stands, and the next landing or a deploy by hand rolls out what
// landed.
func (s *server) deployReleased(ctx context.Context) *agentreplv1.UpdateRolloutsSuccessReleased {
	result, err := s.deps.Deploy.Deploy(ctx, false)
	if err == nil {
		var success *agentreplv1.DeploySuccess
		success, err = deploySuccessOf(result)
		if err == nil {
			if upToDate(result) {
				s.log.Info(opUpdateRollouts, "released the rollouts; the running build carries every landing", nil)
				return &agentreplv1.UpdateRolloutsSuccessReleased{Deploy: &agentreplv1.UpdateRolloutsSuccessReleased_UpToDate{
					UpToDate: &agentreplv1.UpdateRolloutsSuccessReleasedUpToDate{}}}
			}
			s.log.Info(opUpdateRollouts, "released the rollouts and rolled out what landed during the hold", dlog.Context{"components": len(success.Components)})
			return &agentreplv1.UpdateRolloutsSuccessReleased{Deploy: &agentreplv1.UpdateRolloutsSuccessReleased_RollingOut{
				RollingOut: &agentreplv1.UpdateRolloutsSuccessReleasedRollingOut{Deploy: success}}}
		}
	}
	s.log.Error(opUpdateRollouts, "released the rollouts, but the rollout of what landed during the hold could not start", dlog.WithCause(nil, err))
	return &agentreplv1.UpdateRolloutsSuccessReleased{Deploy: &agentreplv1.UpdateRolloutsSuccessReleased_StartFailed{
		StartFailed: &agentreplv1.UpdateRolloutsSuccessReleasedStartFailed{Detail: err.Error()}}}
}

// upToDate reports that a deploy found every component already current.
func upToDate(result deploy.Result) bool {
	for _, o := range result.Outcomes {
		if o.Kind != deploy.UpToDate {
			return false
		}
	}
	return true
}
