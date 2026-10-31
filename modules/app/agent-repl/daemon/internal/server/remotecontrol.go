package server

import (
	"context"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/wsm"
)

// UpdateRemoteControlMode turns a workspace's remote control (its session
// reachable from claude.ai) on, off or over. The verb records the mode and puts
// it in force; this validates, resolves the workspace, and answers the mode
// left or the refusal by its arm. A vendor refusal leaves the mode recorded,
// and answers vendor_refused with the vendor's words.
func (s *server) UpdateRemoteControlMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateRemoteControlModeRequest],
) (*connect.Response[agentreplv1.UpdateRemoteControlModeResponse], error) {
	const rpc = "UpdateRemoteControlMode"
	if err := validateUpdateRemoteControlModeRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.UpdateRemoteControlModeResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	on, err := s.deps.Verbs.UpdateRemoteControlMode(ctx, subject.Record.ID, remoteControlChange(req.Msg))
	if err != nil {
		return answer(resp, s.answerRefusal(subject.Log, rpc, resp, err, nil))
	}
	resp.Result = &agentreplv1.UpdateRemoteControlModeResponse_Success{
		Success: &agentreplv1.UpdateRemoteControlModeSuccess{On: on},
	}
	return connect.NewResponse(resp), nil
}

// remoteControlChange is the change a validated request's action arm asks.
func remoteControlChange(req *agentreplv1.UpdateRemoteControlModeRequest) wsm.RemoteControlChange {
	switch req.GetAction().(type) {
	case *agentreplv1.UpdateRemoteControlModeRequest_On:
		return wsm.RemoteControlOn
	case *agentreplv1.UpdateRemoteControlModeRequest_Off:
		return wsm.RemoteControlOff
	case *agentreplv1.UpdateRemoteControlModeRequest_Toggle:
		return wsm.RemoteControlToggle
	}
	// validateUpdateRemoteControlModeRequest refused every other arm, so one
	// reaching here is a contract this build does not know.
	panic(fmt.Sprintf("server: undeclared UpdateRemoteControlModeRequest.action arm %T", req.GetAction()))
}
