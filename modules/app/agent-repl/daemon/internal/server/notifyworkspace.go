package server

import (
	"context"
	"fmt"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
)

// opNotifyWorkspace is the operation NotifyWorkspace's records carry.
const opNotifyWorkspace = "daemon.server.notify_workspace"

// NotifyWorkspace posts a desktop banner an agent asked for on behalf of a
// workspace (endpoint_notify_workspace.proto): titled by the workspace's name,
// the request's message below it, its click selecting the workspace. It rides
// the notifier every banner rides, so a focused Emacs gets none, and the
// answer says so.
func (s *server) NotifyWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.NotifyWorkspaceRequest],
) (*connect.Response[agentreplv1.NotifyWorkspaceResponse], error) {
	const rpc = "NotifyWorkspace"
	if err := validateNotifyWorkspaceRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.NotifyWorkspaceResponse{}
	subject, refused, done := s.resolveRegisteredNaming(ctx, rpc, resp, req.Msg.GetWorkspace(), nil)
	if done {
		return answer(resp, refused)
	}
	log := subject.Log
	if subject.Record.Closed {
		return answer(resp, s.refuse(log, rpc, resp, s.fill(refusal{
			Arm: "workspace_closed", Reason: "a closed workspace has no tab for the banner's click to select", Info: true,
		})))
	}
	decision := s.deps.Banners.Notify(subject.Record.ID, req.Msg.GetMessage())
	success := &agentreplv1.NotifyWorkspaceSuccess{}
	switch decision {
	case desktopnotify.Dispatched:
		success.Delivery = &agentreplv1.NotifyWorkspaceSuccess_Posting{Posting: &agentreplv1.NotifyWorkspacePosting{}}
	case desktopnotify.EditorFocused:
		success.Delivery = &agentreplv1.NotifyWorkspaceSuccess_EditorFocused{EditorFocused: &agentreplv1.NotifyWorkspaceEditorFocused{}}
	case desktopnotify.NoProgram:
		resp.Result = &agentreplv1.NotifyWorkspaceResponse_Error{Error: &agentreplv1.NotifyWorkspaceError{
			Cause: &agentreplv1.NotifyWorkspaceError_NoBannerProgram{NoBannerProgram: &agentreplv1.NotifyWorkspaceNoBannerProgram{
				Cause: s.deps.Banners.NoProgramCause(),
			}},
		}}
		return connect.NewResponse(resp), nil
	case desktopnotify.StandingDown:
		resp.Result = &agentreplv1.NotifyWorkspaceResponse_Error{Error: &agentreplv1.NotifyWorkspaceError{
			Cause: &agentreplv1.NotifyWorkspaceError_StandingDown{StandingDown: &agentreplv1.NotifyWorkspaceStandingDown{}},
		}}
		return connect.NewResponse(resp), nil
	default:
		panic(fmt.Sprintf("server: NotifyWorkspace: the notifier answered an unknown decision %s", decision))
	}
	log.Info(opNotifyWorkspace, "an agent's desktop banner was asked for", dlog.Context{
		"rpc": rpc, "decision": decision.String(),
	})
	resp.Result = &agentreplv1.NotifyWorkspaceResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// validateNotifyWorkspaceRequest is NotifyWorkspaceRequest's base function.
func validateNotifyWorkspaceRequest(req *agentreplv1.NotifyWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if strings.TrimSpace(req.GetMessage()) == "" {
		return invalid("message", "a banner says something")
	}
	return nil
}
