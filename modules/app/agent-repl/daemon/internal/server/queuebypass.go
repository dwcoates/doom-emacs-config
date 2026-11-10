package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/workspace"
)

// UpdateQueueBypassMode takes or releases a workspace's queue bypass mode.
func (s *server) UpdateQueueBypassMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateQueueBypassModeRequest],
) (*connect.Response[agentreplv1.UpdateQueueBypassModeResponse], error) {
	const rpc = "UpdateQueueBypassMode"
	if err := validateUpdateQueueBypassModeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateQueueBypassModeResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	on, err := s.deps.Verbs.UpdateQueueBypassMode(ctx, subject.Record.ID, queueBypassChange(req.Msg))
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	// The switch swapped the conversation behind the workspace; the host view
	// is recomposed from what the swap left behind, as a bind's is.
	s.PublishHostWorkspace(ctx, subject.Record.ID)
	resp.Result = &agentreplv1.UpdateQueueBypassModeResponse_Success{
		Success: &agentreplv1.UpdateQueueBypassModeSuccess{On: on},
	}
	return connect.NewResponse(resp), nil
}

// queueBypassChange is the change a validated request's action arm asks.
func queueBypassChange(req *agentreplv1.UpdateQueueBypassModeRequest) workspace.QueueBypassChange {
	switch req.GetAction().(type) {
	case *agentreplv1.UpdateQueueBypassModeRequest_On:
		return workspace.QueueBypassOn
	case *agentreplv1.UpdateQueueBypassModeRequest_Off:
		return workspace.QueueBypassOff
	case *agentreplv1.UpdateQueueBypassModeRequest_Toggle:
		return workspace.QueueBypassToggle
	}
	// validateUpdateQueueBypassModeRequest refused every other arm, so one
	// reaching here is a contract this build does not know.
	panic(fmt.Sprintf("server: undeclared UpdateQueueBypassModeRequest.action arm %T", req.GetAction()))
}

// validateUpdateQueueBypassModeRequest is UpdateQueueBypassModeRequest's
// validation: a workspace and one action arm.
func validateUpdateQueueBypassModeRequest(req *agentreplv1.UpdateQueueBypassModeRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch req.GetAction().(type) {
	case *agentreplv1.UpdateQueueBypassModeRequest_On,
		*agentreplv1.UpdateQueueBypassModeRequest_Off,
		*agentreplv1.UpdateQueueBypassModeRequest_Toggle:
		return nil
	case nil:
		return invalid("action", "a queue-bypass action arm is required")
	}
	return invalid("action", "the queue-bypass action is not one this contract defines")
}

func (s *requestLoggingServer) UpdateQueueBypassMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateQueueBypassModeRequest],
) (*connect.Response[agentreplv1.UpdateQueueBypassModeResponse], error) {
	return serveUnary(s, ctx, "UpdateQueueBypassMode", req, s.server.UpdateQueueBypassMode)
}
