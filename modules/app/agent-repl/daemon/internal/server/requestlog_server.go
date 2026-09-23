package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// requestLoggingServer owns the transport boundary around every generated RPC
// handler. The wrapped server retains policy; this layer only binds request
// identity and workspace log routing, then records entry and completion.
type requestLoggingServer struct {
	server *server
}

func (s *requestLoggingServer) SubmitPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubmitPromptRequest],
) (resp *connect.Response[agentreplv1.SubmitPromptResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SubmitPrompt", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.submit_prompt", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.submit_prompt", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SubmitPrompt(ctx, req)
}

func (s *requestLoggingServer) SelectResponse(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectResponseRequest],
) (resp *connect.Response[agentreplv1.SelectResponseResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SelectResponse", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.select_response", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.select_response", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SelectResponse(ctx, req)
}

func (s *requestLoggingServer) AdjustFeedTextScale(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdjustFeedTextScaleRequest],
) (resp *connect.Response[agentreplv1.AdjustFeedTextScaleResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AdjustFeedTextScale", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.adjust_feed_text_scale", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.adjust_feed_text_scale", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AdjustFeedTextScale(ctx, req)
}

func (s *requestLoggingServer) RequestCommandSupport(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequestCommandSupportRequest],
) (resp *connect.Response[agentreplv1.RequestCommandSupportResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "RequestCommandSupport", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.request_command_support", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.request_command_support", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.RequestCommandSupport(ctx, req)
}

func (s *requestLoggingServer) OpenFeed(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenFeedRequest],
) (resp *connect.Response[agentreplv1.OpenFeedResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "OpenFeed", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.open_feed", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.open_feed", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.OpenFeed(ctx, req)
}

func (s *requestLoggingServer) WatchFeed(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchFeedRequest],
	stream *connect.ServerStream[agentreplv1.WatchFeedResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchFeed", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_feed", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_feed", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchFeed(ctx, req, stream)
}

func (s *requestLoggingServer) GetFeedPage(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetFeedPageRequest],
) (resp *connect.Response[agentreplv1.GetFeedPageResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "GetFeedPage", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.get_feed_page", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.get_feed_page", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.GetFeedPage(ctx, req)
}

func (s *requestLoggingServer) Interrupt(
	ctx context.Context,
	req *connect.Request[agentreplv1.InterruptRequest],
) (resp *connect.Response[agentreplv1.InterruptResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "Interrupt", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.interrupt", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.interrupt", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.Interrupt(ctx, req)
}

func (s *requestLoggingServer) AnswerPermission(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerPermissionRequest],
) (resp *connect.Response[agentreplv1.AnswerPermissionResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AnswerPermission", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.answer_permission", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.answer_permission", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AnswerPermission(ctx, req)
}

func (s *requestLoggingServer) AnswerQuestion(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerQuestionRequest],
) (resp *connect.Response[agentreplv1.AnswerQuestionResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AnswerQuestion", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.answer_question", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.answer_question", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AnswerQuestion(ctx, req)
}

func (s *requestLoggingServer) AnswerColdGate(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerColdGateRequest],
) (resp *connect.Response[agentreplv1.AnswerColdGateResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AnswerColdGate", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.answer_cold_gate", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.answer_cold_gate", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AnswerColdGate(ctx, req)
}

func (s *requestLoggingServer) WatchWorkspaceRoster(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWorkspaceRosterRequest],
	stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchWorkspaceRoster", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_workspace_roster", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_workspace_roster", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchWorkspaceRoster(ctx, req, stream)
}

func (s *requestLoggingServer) CreateWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateWorkspaceRequest],
) (resp *connect.Response[agentreplv1.CreateWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "CreateWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.create_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.create_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.CreateWorkspace(ctx, req)
}

func (s *requestLoggingServer) OpenWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenWorkspaceRequest],
) (resp *connect.Response[agentreplv1.OpenWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "OpenWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.open_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.open_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.OpenWorkspace(ctx, req)
}

func (s *requestLoggingServer) CloseWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseWorkspaceRequest],
) (resp *connect.Response[agentreplv1.CloseWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "CloseWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.close_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.close_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.CloseWorkspace(ctx, req)
}

func (s *requestLoggingServer) KillWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.KillWorkspaceRequest],
) (resp *connect.Response[agentreplv1.KillWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "KillWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.kill_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.kill_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.KillWorkspace(ctx, req)
}

func (s *requestLoggingServer) NukeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.NukeWorkspaceRequest],
) (resp *connect.Response[agentreplv1.NukeWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "NukeWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.nuke_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.nuke_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.NukeWorkspace(ctx, req)
}

func (s *requestLoggingServer) ForgetWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.ForgetWorkspaceRequest],
) (resp *connect.Response[agentreplv1.ForgetWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "ForgetWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.forget_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.forget_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.ForgetWorkspace(ctx, req)
}

func (s *requestLoggingServer) MergeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.MergeWorkspaceRequest],
) (resp *connect.Response[agentreplv1.MergeWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "MergeWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.merge_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.merge_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.MergeWorkspace(ctx, req)
}

func (s *requestLoggingServer) RestartWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RestartWorkspaceRequest],
) (resp *connect.Response[agentreplv1.RestartWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "RestartWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.restart_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.restart_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.RestartWorkspace(ctx, req)
}

func (s *requestLoggingServer) SetWorkspacePriority(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspacePriorityRequest],
) (resp *connect.Response[agentreplv1.SetWorkspacePriorityResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SetWorkspacePriority", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.set_workspace_priority", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.set_workspace_priority", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SetWorkspacePriority(ctx, req)
}

func (s *requestLoggingServer) CreateTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateTaskRequest],
) (resp *connect.Response[agentreplv1.CreateTaskResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "CreateTask", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.create_task", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.create_task", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.CreateTask(ctx, req)
}

func (s *requestLoggingServer) UpdateTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateTaskRequest],
) (resp *connect.Response[agentreplv1.UpdateTaskResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "UpdateTask", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.update_task", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.update_task", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.UpdateTask(ctx, req)
}

func (s *requestLoggingServer) AssignWorkspaceTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.AssignWorkspaceTaskRequest],
) (resp *connect.Response[agentreplv1.AssignWorkspaceTaskResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AssignWorkspaceTask", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.assign_workspace_task", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.assign_workspace_task", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AssignWorkspaceTask(ctx, req)
}

func (s *requestLoggingServer) WatchTopbar(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchTopbarRequest],
	stream *connect.ServerStream[agentreplv1.WatchTopbarResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchTopbar", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_topbar", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_topbar", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchTopbar(ctx, req, stream)
}

func (s *requestLoggingServer) SetModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetModelRequest],
) (resp *connect.Response[agentreplv1.SetModelResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SetModel", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.set_model", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.set_model", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SetModel(ctx, req)
}

func (s *requestLoggingServer) SetPermissionMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetPermissionModeRequest],
) (resp *connect.Response[agentreplv1.SetPermissionModeResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SetPermissionMode", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.set_permission_mode", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.set_permission_mode", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SetPermissionMode(ctx, req)
}

func (s *requestLoggingServer) WatchFooter(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchFooterRequest],
	stream *connect.ServerStream[agentreplv1.WatchFooterResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchFooter", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_footer", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_footer", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchFooter(ctx, req, stream)
}

func (s *requestLoggingServer) WatchDaemonHolds(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchDaemonHoldsRequest],
	stream *connect.ServerStream[agentreplv1.WatchDaemonHoldsResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchDaemonHolds", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_daemon_holds", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_daemon_holds", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchDaemonHolds(ctx, req, stream)
}

func (s *requestLoggingServer) UpdateHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateHeldPromptRequest],
) (resp *connect.Response[agentreplv1.UpdateHeldPromptResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "UpdateHeldPrompt", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.update_held_prompt", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.update_held_prompt", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.UpdateHeldPrompt(ctx, req)
}

func (s *requestLoggingServer) AnswerHeldOffer(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerHeldOfferRequest],
) (resp *connect.Response[agentreplv1.AnswerHeldOfferResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AnswerHeldOffer", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.answer_held_offer", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.answer_held_offer", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AnswerHeldOffer(ctx, req)
}

func (s *requestLoggingServer) Deploy(
	ctx context.Context,
	req *connect.Request[agentreplv1.DeployRequest],
) (resp *connect.Response[agentreplv1.DeployResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "Deploy", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.deploy", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.deploy", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.Deploy(ctx, req)
}

func (s *requestLoggingServer) UpdateShutdownSchedule(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateShutdownScheduleRequest],
) (resp *connect.Response[agentreplv1.UpdateShutdownScheduleResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "UpdateShutdownSchedule", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.update_shutdown_schedule", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.update_shutdown_schedule", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.UpdateShutdownSchedule(ctx, req)
}

func (s *requestLoggingServer) UpdateMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateMergeQueueRequest],
) (resp *connect.Response[agentreplv1.UpdateMergeQueueResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "UpdateMergeQueue", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.update_merge_queue", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.update_merge_queue", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.UpdateMergeQueue(ctx, req)
}

func (s *requestLoggingServer) DaemonHealth(
	ctx context.Context,
	req *connect.Request[agentreplv1.DaemonHealthRequest],
) (resp *connect.Response[agentreplv1.DaemonHealthResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "DaemonHealth", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.daemon_health", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.daemon_health", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.DaemonHealth(ctx, req)
}

func (s *requestLoggingServer) SessionHealth(
	ctx context.Context,
	req *connect.Request[agentreplv1.SessionHealthRequest],
) (resp *connect.Response[agentreplv1.SessionHealthResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SessionHealth", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.session_health", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.session_health", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SessionHealth(ctx, req)
}

func (s *requestLoggingServer) ClientLog(
	ctx context.Context,
	req *connect.Request[agentreplv1.ClientLogRequest],
) (resp *connect.Response[agentreplv1.ClientLogResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "ClientLog", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.client_log", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.client_log", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.ClientLog(ctx, req)
}

func (s *requestLoggingServer) RegisterWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterWorkspaceRequest],
) (resp *connect.Response[agentreplv1.RegisterWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "RegisterWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.register_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.register_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.RegisterWorkspace(ctx, req)
}

func (s *requestLoggingServer) RegisterRepository(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterRepositoryRequest],
) (resp *connect.Response[agentreplv1.RegisterRepositoryResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "RegisterRepository", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.register_repository", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.register_repository", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.RegisterRepository(ctx, req)
}

func (s *requestLoggingServer) SelectWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectWorkspaceRequest],
) (resp *connect.Response[agentreplv1.SelectWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SelectWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.select_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.select_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SelectWorkspace(ctx, req)
}

func (s *requestLoggingServer) MarkWorkspaceViewed(
	ctx context.Context,
	req *connect.Request[agentreplv1.MarkWorkspaceViewedRequest],
) (resp *connect.Response[agentreplv1.MarkWorkspaceViewedResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "MarkWorkspaceViewed", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.mark_workspace_viewed", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.mark_workspace_viewed", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.MarkWorkspaceViewed(ctx, req)
}

func (s *requestLoggingServer) WatchHostWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchHostWorkspaceRequest],
	stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchHostWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_host_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_host_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchHostWorkspace(ctx, req, stream)
}

func (s *requestLoggingServer) WatchDaemon(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchDaemonRequest],
	stream *connect.ServerStream[agentreplv1.WatchDaemonResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchDaemon", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_daemon", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_daemon", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchDaemon(ctx, req, stream)
}

func (s *requestLoggingServer) AdoptHostWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdoptHostWorkspaceRequest],
) (resp *connect.Response[agentreplv1.AdoptHostWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AdoptHostWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.adopt_host_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.adopt_host_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AdoptHostWorkspace(ctx, req)
}

func (s *requestLoggingServer) WatchWebWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWebWorkspaceRequest],
	stream *connect.ServerStream[agentreplv1.WatchWebWorkspaceResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchWebWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_web_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_web_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchWebWorkspace(ctx, req, stream)
}

func (s *requestLoggingServer) SelectAccount(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectAccountRequest],
) (resp *connect.Response[agentreplv1.SelectAccountResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SelectAccount", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.select_account", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.select_account", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SelectAccount(ctx, req)
}

func (s *requestLoggingServer) OpenLogin(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenLoginRequest],
) (resp *connect.Response[agentreplv1.OpenLoginResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "OpenLogin", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.open_login", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.open_login", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.OpenLogin(ctx, req)
}

func (s *requestLoggingServer) WatchLoginTerminal(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchLoginTerminalRequest],
	stream *connect.ServerStream[agentreplv1.LoginTerminalOutput],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchLoginTerminal", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_login_terminal", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_login_terminal", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchLoginTerminal(ctx, req, stream)
}

func (s *requestLoggingServer) SendLoginInput(
	ctx context.Context,
	req *connect.Request[agentreplv1.SendLoginInputRequest],
) (resp *connect.Response[agentreplv1.SendLoginInputResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SendLoginInput", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.send_login_input", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.send_login_input", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SendLoginInput(ctx, req)
}

func (s *requestLoggingServer) CloseLogin(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseLoginRequest],
) (resp *connect.Response[agentreplv1.CloseLoginResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "CloseLogin", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.close_login", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.close_login", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.CloseLogin(ctx, req)
}

func (s *requestLoggingServer) OpenExternal(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenExternalRequest],
) (resp *connect.Response[agentreplv1.OpenExternalResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "OpenExternal", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.open_external", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.open_external", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.OpenExternal(ctx, req)
}

func (s *requestLoggingServer) OpenInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenInEditorRequest],
) (resp *connect.Response[agentreplv1.OpenInEditorResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "OpenInEditor", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.open_in_editor", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.open_in_editor", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.OpenInEditor(ctx, req)
}

func (s *requestLoggingServer) AdoptWebWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdoptWebWorkspaceRequest],
) (resp *connect.Response[agentreplv1.AdoptWebWorkspaceResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "AdoptWebWorkspace", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.adopt_web_workspace", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.adopt_web_workspace", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.AdoptWebWorkspace(ctx, req)
}

func (s *requestLoggingServer) WatchPage(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchPageRequest],
	stream *connect.ServerStream[agentreplv1.WatchPageResponse],
) (err error) {
	boundary, err := s.server.beginRequest(ctx, "WatchPage", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.watch_page", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.watch_page", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.WatchPage(ctx, req, stream)
}

func (s *requestLoggingServer) SubscribePage(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribePageRequest],
) (resp *connect.Response[agentreplv1.SubscribePageResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "SubscribePage", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.subscribe_page", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.subscribe_page", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.SubscribePage(ctx, req)
}

func (s *requestLoggingServer) UnsubscribePage(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribePageRequest],
) (resp *connect.Response[agentreplv1.UnsubscribePageResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "UnsubscribePage", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.unsubscribe_page", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.unsubscribe_page", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.UnsubscribePage(ctx, req)
}

func (s *requestLoggingServer) ListWorkspaceTranscripts(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListWorkspaceTranscriptsRequest],
) (resp *connect.Response[agentreplv1.ListWorkspaceTranscriptsResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "ListWorkspaceTranscripts", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.list_workspace_transcripts", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.list_workspace_transcripts", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.ListWorkspaceTranscripts(ctx, req)
}

func (s *requestLoggingServer) BindWorkspaceSession(
	ctx context.Context,
	req *connect.Request[agentreplv1.BindWorkspaceSessionRequest],
) (resp *connect.Response[agentreplv1.BindWorkspaceSessionResponse], err error) {
	boundary, err := s.server.beginRequest(ctx, "BindWorkspaceSession", req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, connect.NewError(connect.CodeInternal, err)
	}
	boundary.log.Debug("daemon.server.bind_workspace_session", "entered the rpc handler", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "resolved the rpc request scope", boundary.entryContext())
	boundary.log.Debug(boundaryOperation(boundary.rpc), "delegated the rpc request", boundary.entryContext())
	defer func() {
		boundary.log.Debug("daemon.server.bind_workspace_session", "completed the rpc handler", boundary.completionContext(err))
	}()
	return s.server.BindWorkspaceSession(ctx, req)
}
