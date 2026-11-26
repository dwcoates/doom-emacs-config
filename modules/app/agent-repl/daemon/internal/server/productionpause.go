package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/merge"
)

// UpdateProductionPause pauses or resumes one repository's production checks,
// then restates the repository's production-checks chip on each of its open
// workspaces. The chip is restated after any change that was not refused, so
// a pause that stood despite a failed interrupt is still drawn paused.
func (s *server) UpdateProductionPause(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateProductionPauseRequest],
) (*connect.Response[agentreplv1.UpdateProductionPauseResponse], error) {
	const rpc = "UpdateProductionPause"
	if err := validateUpdateProductionPauseRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateProductionPauseResponse{}
	scope := *scopeOf(req.Msg.GetRepository())
	var (
		success *agentreplv1.UpdateProductionPauseSuccess
		err     error
	)
	if req.Msg.GetPause() != nil {
		var paused merge.ProductionPaused
		paused, err = s.deps.Merge.PauseProduction(ctx, scope)
		success = &agentreplv1.UpdateProductionPauseSuccess{Applied: &agentreplv1.UpdateProductionPauseSuccess_Paused{Paused: pausedOf(paused)}}
	} else {
		err = s.deps.Merge.ResumeProduction(ctx, scope)
		success = &agentreplv1.UpdateProductionPauseSuccess{Applied: &agentreplv1.UpdateProductionPauseSuccess_Resumed{Resumed: &agentreplv1.UpdateProductionPauseSuccessResumed{}}}
	}
	if _, refused := merge.Refused(err); !refused {
		// The verbs record a restate that fails; the change itself stands.
		_ = s.deps.Verbs.RestateProductionChecks(ctx, scope)
	}
	if err != nil {
		return answer(resp, s.answerRefusal(s.log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.UpdateProductionPauseResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// pausedOf renders what a pause stopped.
func pausedOf(p merge.ProductionPaused) *agentreplv1.UpdateProductionPauseSuccessPaused {
	out := &agentreplv1.UpdateProductionPauseSuccessPaused{DroppedSpans: uint32(p.Dropped)}
	if p.Interrupted != "" {
		out.Interrupted = &workspacev1.WorkspaceRef{Id: string(p.Interrupted)}
	}
	return out
}
