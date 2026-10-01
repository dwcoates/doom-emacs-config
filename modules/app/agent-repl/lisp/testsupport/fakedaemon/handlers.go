package main

// Code in this file mirrors agentreplv1connect.AgentReplHandler one method
// per rpc.  Unary methods all delegate to handleUnary (record, then answer
// from the scripted table or the default synthesis).  The three streams
// Emacs holds are implemented against the subscriber registry; every other
// stream belongs to the webapp, which this fake deliberately does not mock,
// and answers CodeUnimplemented so a wrong caller fails loudly.

import (
	"context"
	"fmt"

	v1 "agentrepl/proto/agentrepl/v1"
	"connectrpc.com/connect"
)

func (s *fakeServer) SubmitPrompt(ctx context.Context, req *connect.Request[v1.SubmitPromptRequest]) (*connect.Response[v1.SubmitPromptResponse], error) {
	return handleUnary[v1.SubmitPromptRequest, v1.SubmitPromptResponse](ctx, s, "SubmitPrompt", req.Msg)
}

func (s *fakeServer) SelectFeedRow(ctx context.Context, req *connect.Request[v1.SelectFeedRowRequest]) (*connect.Response[v1.SelectFeedRowResponse], error) {
	return handleUnary[v1.SelectFeedRowRequest, v1.SelectFeedRowResponse](ctx, s, "SelectFeedRow", req.Msg)
}

func (s *fakeServer) PlanRollback(ctx context.Context, req *connect.Request[v1.PlanRollbackRequest]) (*connect.Response[v1.PlanRollbackResponse], error) {
	return handleUnary[v1.PlanRollbackRequest, v1.PlanRollbackResponse](ctx, s, "PlanRollback", req.Msg)
}

func (s *fakeServer) RollBack(ctx context.Context, req *connect.Request[v1.RollBackRequest]) (*connect.Response[v1.RollBackResponse], error) {
	return handleUnary[v1.RollBackRequest, v1.RollBackResponse](ctx, s, "RollBack", req.Msg)
}

func (s *fakeServer) AdjustFeedTextScale(ctx context.Context, req *connect.Request[v1.AdjustFeedTextScaleRequest]) (*connect.Response[v1.AdjustFeedTextScaleResponse], error) {
	return handleUnary[v1.AdjustFeedTextScaleRequest, v1.AdjustFeedTextScaleResponse](ctx, s, "AdjustFeedTextScale", req.Msg)
}

func (s *fakeServer) RequestCommandSupport(ctx context.Context, req *connect.Request[v1.RequestCommandSupportRequest]) (*connect.Response[v1.RequestCommandSupportResponse], error) {
	return handleUnary[v1.RequestCommandSupportRequest, v1.RequestCommandSupportResponse](ctx, s, "RequestCommandSupport", req.Msg)
}

func (s *fakeServer) OpenFeed(ctx context.Context, req *connect.Request[v1.OpenFeedRequest]) (*connect.Response[v1.OpenFeedResponse], error) {
	return handleUnary[v1.OpenFeedRequest, v1.OpenFeedResponse](ctx, s, "OpenFeed", req.Msg)
}

func (s *fakeServer) WatchFeed(ctx context.Context, req *connect.Request[v1.WatchFeedRequest], stream *connect.ServerStream[v1.WatchFeedResponse]) error {
	s.record(ctx, "WatchFeed", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchFeed"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchFeed is a webapp stream"))
}

func (s *fakeServer) GetFeedPage(ctx context.Context, req *connect.Request[v1.GetFeedPageRequest]) (*connect.Response[v1.GetFeedPageResponse], error) {
	return handleUnary[v1.GetFeedPageRequest, v1.GetFeedPageResponse](ctx, s, "GetFeedPage", req.Msg)
}

func (s *fakeServer) Interrupt(ctx context.Context, req *connect.Request[v1.InterruptRequest]) (*connect.Response[v1.InterruptResponse], error) {
	return handleUnary[v1.InterruptRequest, v1.InterruptResponse](ctx, s, "Interrupt", req.Msg)
}

func (s *fakeServer) AnswerPermission(ctx context.Context, req *connect.Request[v1.AnswerPermissionRequest]) (*connect.Response[v1.AnswerPermissionResponse], error) {
	return handleUnary[v1.AnswerPermissionRequest, v1.AnswerPermissionResponse](ctx, s, "AnswerPermission", req.Msg)
}

func (s *fakeServer) AnswerQuestion(ctx context.Context, req *connect.Request[v1.AnswerQuestionRequest]) (*connect.Response[v1.AnswerQuestionResponse], error) {
	return handleUnary[v1.AnswerQuestionRequest, v1.AnswerQuestionResponse](ctx, s, "AnswerQuestion", req.Msg)
}

func (s *fakeServer) AnswerColdGate(ctx context.Context, req *connect.Request[v1.AnswerColdGateRequest]) (*connect.Response[v1.AnswerColdGateResponse], error) {
	return handleUnary[v1.AnswerColdGateRequest, v1.AnswerColdGateResponse](ctx, s, "AnswerColdGate", req.Msg)
}

func (s *fakeServer) WatchWorkspaceRoster(ctx context.Context, req *connect.Request[v1.WatchWorkspaceRosterRequest], stream *connect.ServerStream[v1.WatchWorkspaceRosterResponse]) error {
	s.record(ctx, "WatchWorkspaceRoster", req.Msg)
	if err := awaitAcceptanceGate(ctx, s, "WatchWorkspaceRoster"); err != nil {
		return err
	}
	return serveStream[v1.WatchWorkspaceRosterResponse](ctx, s, streamRoster, "", stream)
}

func (s *fakeServer) CreateWorkspace(ctx context.Context, req *connect.Request[v1.CreateWorkspaceRequest]) (*connect.Response[v1.CreateWorkspaceResponse], error) {
	return handleUnary[v1.CreateWorkspaceRequest, v1.CreateWorkspaceResponse](ctx, s, "CreateWorkspace", req.Msg)
}

func (s *fakeServer) OpenWorkspace(ctx context.Context, req *connect.Request[v1.OpenWorkspaceRequest]) (*connect.Response[v1.OpenWorkspaceResponse], error) {
	return handleUnary[v1.OpenWorkspaceRequest, v1.OpenWorkspaceResponse](ctx, s, "OpenWorkspace", req.Msg)
}

func (s *fakeServer) CloseWorkspace(ctx context.Context, req *connect.Request[v1.CloseWorkspaceRequest]) (*connect.Response[v1.CloseWorkspaceResponse], error) {
	return handleUnary[v1.CloseWorkspaceRequest, v1.CloseWorkspaceResponse](ctx, s, "CloseWorkspace", req.Msg)
}

func (s *fakeServer) KillWorkspace(ctx context.Context, req *connect.Request[v1.KillWorkspaceRequest]) (*connect.Response[v1.KillWorkspaceResponse], error) {
	return handleUnary[v1.KillWorkspaceRequest, v1.KillWorkspaceResponse](ctx, s, "KillWorkspace", req.Msg)
}

func (s *fakeServer) NukeWorkspace(ctx context.Context, req *connect.Request[v1.NukeWorkspaceRequest]) (*connect.Response[v1.NukeWorkspaceResponse], error) {
	return handleUnary[v1.NukeWorkspaceRequest, v1.NukeWorkspaceResponse](ctx, s, "NukeWorkspace", req.Msg)
}

func (s *fakeServer) ForgetWorkspace(ctx context.Context, req *connect.Request[v1.ForgetWorkspaceRequest]) (*connect.Response[v1.ForgetWorkspaceResponse], error) {
	return handleUnary[v1.ForgetWorkspaceRequest, v1.ForgetWorkspaceResponse](ctx, s, "ForgetWorkspace", req.Msg)
}

func (s *fakeServer) MergeWorkspace(ctx context.Context, req *connect.Request[v1.MergeWorkspaceRequest]) (*connect.Response[v1.MergeWorkspaceResponse], error) {
	return handleUnary[v1.MergeWorkspaceRequest, v1.MergeWorkspaceResponse](ctx, s, "MergeWorkspace", req.Msg)
}

func (s *fakeServer) RestartWorkspace(ctx context.Context, req *connect.Request[v1.RestartWorkspaceRequest]) (*connect.Response[v1.RestartWorkspaceResponse], error) {
	return handleUnary[v1.RestartWorkspaceRequest, v1.RestartWorkspaceResponse](ctx, s, "RestartWorkspace", req.Msg)
}

func (s *fakeServer) SetWorkspacePriority(ctx context.Context, req *connect.Request[v1.SetWorkspacePriorityRequest]) (*connect.Response[v1.SetWorkspacePriorityResponse], error) {
	return handleUnary[v1.SetWorkspacePriorityRequest, v1.SetWorkspacePriorityResponse](ctx, s, "SetWorkspacePriority", req.Msg)
}

func (s *fakeServer) FoldRepository(ctx context.Context, req *connect.Request[v1.FoldRepositoryRequest]) (*connect.Response[v1.FoldRepositoryResponse], error) {
	return handleUnary[v1.FoldRepositoryRequest, v1.FoldRepositoryResponse](ctx, s, "FoldRepository", req.Msg)
}

func (s *fakeServer) CreateTask(ctx context.Context, req *connect.Request[v1.CreateTaskRequest]) (*connect.Response[v1.CreateTaskResponse], error) {
	return handleUnary[v1.CreateTaskRequest, v1.CreateTaskResponse](ctx, s, "CreateTask", req.Msg)
}

func (s *fakeServer) UpdateTask(ctx context.Context, req *connect.Request[v1.UpdateTaskRequest]) (*connect.Response[v1.UpdateTaskResponse], error) {
	return handleUnary[v1.UpdateTaskRequest, v1.UpdateTaskResponse](ctx, s, "UpdateTask", req.Msg)
}

func (s *fakeServer) AssignWorkspaceTask(ctx context.Context, req *connect.Request[v1.AssignWorkspaceTaskRequest]) (*connect.Response[v1.AssignWorkspaceTaskResponse], error) {
	return handleUnary[v1.AssignWorkspaceTaskRequest, v1.AssignWorkspaceTaskResponse](ctx, s, "AssignWorkspaceTask", req.Msg)
}

func (s *fakeServer) WatchTopbar(ctx context.Context, req *connect.Request[v1.WatchTopbarRequest], stream *connect.ServerStream[v1.WatchTopbarResponse]) error {
	s.record(ctx, "WatchTopbar", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchTopbar"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchTopbar is a webapp stream"))
}

func (s *fakeServer) SetModel(ctx context.Context, req *connect.Request[v1.SetModelRequest]) (*connect.Response[v1.SetModelResponse], error) {
	return handleUnary[v1.SetModelRequest, v1.SetModelResponse](ctx, s, "SetModel", req.Msg)
}

func (s *fakeServer) SetPermissionMode(ctx context.Context, req *connect.Request[v1.SetPermissionModeRequest]) (*connect.Response[v1.SetPermissionModeResponse], error) {
	return handleUnary[v1.SetPermissionModeRequest, v1.SetPermissionModeResponse](ctx, s, "SetPermissionMode", req.Msg)
}

func (s *fakeServer) SelectAccount(ctx context.Context, req *connect.Request[v1.SelectAccountRequest]) (*connect.Response[v1.SelectAccountResponse], error) {
	return handleUnary[v1.SelectAccountRequest, v1.SelectAccountResponse](ctx, s, "SelectAccount", req.Msg)
}

func (s *fakeServer) WatchFooter(ctx context.Context, req *connect.Request[v1.WatchFooterRequest], stream *connect.ServerStream[v1.WatchFooterResponse]) error {
	s.record(ctx, "WatchFooter", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchFooter"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchFooter is a webapp stream"))
}

func (s *fakeServer) WatchDaemonHolds(ctx context.Context, req *connect.Request[v1.WatchDaemonHoldsRequest], stream *connect.ServerStream[v1.WatchDaemonHoldsResponse]) error {
	s.record(ctx, "WatchDaemonHolds", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchDaemonHolds"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchDaemonHolds is a webapp stream"))
}

func (s *fakeServer) UpdateHeldPrompt(ctx context.Context, req *connect.Request[v1.UpdateHeldPromptRequest]) (*connect.Response[v1.UpdateHeldPromptResponse], error) {
	return handleUnary[v1.UpdateHeldPromptRequest, v1.UpdateHeldPromptResponse](ctx, s, "UpdateHeldPrompt", req.Msg)
}

func (s *fakeServer) EditHeldPrompt(ctx context.Context, req *connect.Request[v1.EditHeldPromptRequest]) (*connect.Response[v1.EditHeldPromptResponse], error) {
	return handleUnary[v1.EditHeldPromptRequest, v1.EditHeldPromptResponse](ctx, s, "EditHeldPrompt", req.Msg)
}

func (s *fakeServer) FoldHeldPrompt(ctx context.Context, req *connect.Request[v1.FoldHeldPromptRequest]) (*connect.Response[v1.FoldHeldPromptResponse], error) {
	return handleUnary[v1.FoldHeldPromptRequest, v1.FoldHeldPromptResponse](ctx, s, "FoldHeldPrompt", req.Msg)
}

func (s *fakeServer) AnswerHeldOffer(ctx context.Context, req *connect.Request[v1.AnswerHeldOfferRequest]) (*connect.Response[v1.AnswerHeldOfferResponse], error) {
	return handleUnary[v1.AnswerHeldOfferRequest, v1.AnswerHeldOfferResponse](ctx, s, "AnswerHeldOffer", req.Msg)
}

func (s *fakeServer) UpdateShutdownSchedule(ctx context.Context, req *connect.Request[v1.UpdateShutdownScheduleRequest]) (*connect.Response[v1.UpdateShutdownScheduleResponse], error) {
	return handleUnary[v1.UpdateShutdownScheduleRequest, v1.UpdateShutdownScheduleResponse](ctx, s, "UpdateShutdownSchedule", req.Msg)
}

func (s *fakeServer) UpdateMergeQueue(ctx context.Context, req *connect.Request[v1.UpdateMergeQueueRequest]) (*connect.Response[v1.UpdateMergeQueueResponse], error) {
	return handleUnary[v1.UpdateMergeQueueRequest, v1.UpdateMergeQueueResponse](ctx, s, "UpdateMergeQueue", req.Msg)
}

func (s *fakeServer) DaemonHealth(ctx context.Context, req *connect.Request[v1.DaemonHealthRequest]) (*connect.Response[v1.DaemonHealthResponse], error) {
	return handleUnary[v1.DaemonHealthRequest, v1.DaemonHealthResponse](ctx, s, "DaemonHealth", req.Msg)
}

func (s *fakeServer) SessionHealth(ctx context.Context, req *connect.Request[v1.SessionHealthRequest]) (*connect.Response[v1.SessionHealthResponse], error) {
	return handleUnary[v1.SessionHealthRequest, v1.SessionHealthResponse](ctx, s, "SessionHealth", req.Msg)
}

func (s *fakeServer) ClientLog(ctx context.Context, req *connect.Request[v1.ClientLogRequest]) (*connect.Response[v1.ClientLogResponse], error) {
	return handleUnary[v1.ClientLogRequest, v1.ClientLogResponse](ctx, s, "ClientLog", req.Msg)
}

func (s *fakeServer) RegisterWorkspace(ctx context.Context, req *connect.Request[v1.RegisterWorkspaceRequest]) (*connect.Response[v1.RegisterWorkspaceResponse], error) {
	return handleUnary[v1.RegisterWorkspaceRequest, v1.RegisterWorkspaceResponse](ctx, s, "RegisterWorkspace", req.Msg)
}

func (s *fakeServer) RegisterRepository(ctx context.Context, req *connect.Request[v1.RegisterRepositoryRequest]) (*connect.Response[v1.RegisterRepositoryResponse], error) {
	return handleUnary[v1.RegisterRepositoryRequest, v1.RegisterRepositoryResponse](ctx, s, "RegisterRepository", req.Msg)
}

func (s *fakeServer) SelectWorkspace(ctx context.Context, req *connect.Request[v1.SelectWorkspaceRequest]) (*connect.Response[v1.SelectWorkspaceResponse], error) {
	return handleUnary[v1.SelectWorkspaceRequest, v1.SelectWorkspaceResponse](ctx, s, "SelectWorkspace", req.Msg)
}

func (s *fakeServer) MarkWorkspaceViewed(ctx context.Context, req *connect.Request[v1.MarkWorkspaceViewedRequest]) (*connect.Response[v1.MarkWorkspaceViewedResponse], error) {
	return handleUnary[v1.MarkWorkspaceViewedRequest, v1.MarkWorkspaceViewedResponse](ctx, s, "MarkWorkspaceViewed", req.Msg)
}

func (s *fakeServer) ReportEditorFocus(ctx context.Context, req *connect.Request[v1.ReportEditorFocusRequest]) (*connect.Response[v1.ReportEditorFocusResponse], error) {
	return handleUnary[v1.ReportEditorFocusRequest, v1.ReportEditorFocusResponse](ctx, s, "ReportEditorFocus", req.Msg)
}

func (s *fakeServer) WatchHostWorkspace(ctx context.Context, req *connect.Request[v1.WatchHostWorkspaceRequest], stream *connect.ServerStream[v1.WatchHostWorkspaceResponse]) error {
	s.record(ctx, "WatchHostWorkspace", req.Msg)
	if err := validateRequest(req.Msg); err != nil {
		// An unset non-optional field is illegal, immediately — the stream is
		// refused rather than standing on a request nobody filled in.
		logError("fakedaemon.stream.invalid-request", "refused a stream request that breaches the validation invariant",
			map[string]any{"method": "WatchHostWorkspace", "error": err.Error()})
		return connect.NewError(connect.CodeInvalidArgument, err)
	}
	if err := awaitAcceptanceGate(ctx, s, "WatchHostWorkspace"); err != nil {
		return err
	}
	ws := req.Msg.GetWorkspace()
	return serveStream[v1.WatchHostWorkspaceResponse](ctx, s, streamHost, ws.GetId(), stream)
}

func (s *fakeServer) WatchDaemon(ctx context.Context, req *connect.Request[v1.WatchDaemonRequest], stream *connect.ServerStream[v1.WatchDaemonResponse]) error {
	s.record(ctx, "WatchDaemon", req.Msg)
	if err := validateWatchDaemonRequest(req.Msg); err != nil {
		// The real daemon refuses a watch that names no client, and an Emacs
		// watch that does not state the elisp it has loaded: a deploy could
		// not tell whether that Emacs runs the checkout's elisp.
		logError("fakedaemon.stream.invalid-request", "refused a stream request that breaches the validation invariant",
			map[string]any{"method": "WatchDaemon", "error": err.Error()})
		return connect.NewError(connect.CodeInvalidArgument, err)
	}
	if err := awaitAcceptanceGate(ctx, s, "WatchDaemon"); err != nil {
		return err
	}
	return serveStream[v1.WatchDaemonResponse](ctx, s, streamDaemon, "", stream)
}

func (s *fakeServer) AdoptHostWorkspace(ctx context.Context, req *connect.Request[v1.AdoptHostWorkspaceRequest]) (*connect.Response[v1.AdoptHostWorkspaceResponse], error) {
	return handleUnary[v1.AdoptHostWorkspaceRequest, v1.AdoptHostWorkspaceResponse](ctx, s, "AdoptHostWorkspace", req.Msg)
}

func (s *fakeServer) WatchWebWorkspace(ctx context.Context, req *connect.Request[v1.WatchWebWorkspaceRequest], stream *connect.ServerStream[v1.WatchWebWorkspaceResponse]) error {
	s.record(ctx, "WatchWebWorkspace", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchWebWorkspace"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchWebWorkspace is a webapp stream"))
}

func (s *fakeServer) OpenLogin(ctx context.Context, req *connect.Request[v1.OpenLoginRequest]) (*connect.Response[v1.OpenLoginResponse], error) {
	return handleUnary[v1.OpenLoginRequest, v1.OpenLoginResponse](ctx, s, "OpenLogin", req.Msg)
}

func (s *fakeServer) WatchLoginTerminal(ctx context.Context, req *connect.Request[v1.WatchLoginTerminalRequest], stream *connect.ServerStream[v1.LoginTerminalOutput]) error {
	s.record(ctx, "WatchLoginTerminal", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchLoginTerminal"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchLoginTerminal is a webapp stream"))
}

func (s *fakeServer) SendLoginInput(ctx context.Context, req *connect.Request[v1.SendLoginInputRequest]) (*connect.Response[v1.SendLoginInputResponse], error) {
	return handleUnary[v1.SendLoginInputRequest, v1.SendLoginInputResponse](ctx, s, "SendLoginInput", req.Msg)
}

func (s *fakeServer) CloseLogin(ctx context.Context, req *connect.Request[v1.CloseLoginRequest]) (*connect.Response[v1.CloseLoginResponse], error) {
	return handleUnary[v1.CloseLoginRequest, v1.CloseLoginResponse](ctx, s, "CloseLogin", req.Msg)
}

func (s *fakeServer) OpenExternal(ctx context.Context, req *connect.Request[v1.OpenExternalRequest]) (*connect.Response[v1.OpenExternalResponse], error) {
	return handleUnary[v1.OpenExternalRequest, v1.OpenExternalResponse](ctx, s, "OpenExternal", req.Msg)
}

func (s *fakeServer) OpenInEditor(ctx context.Context, req *connect.Request[v1.OpenInEditorRequest]) (*connect.Response[v1.OpenInEditorResponse], error) {
	return handleUnary[v1.OpenInEditorRequest, v1.OpenInEditorResponse](ctx, s, "OpenInEditor", req.Msg)
}

func (s *fakeServer) AdoptWebWorkspace(ctx context.Context, req *connect.Request[v1.AdoptWebWorkspaceRequest]) (*connect.Response[v1.AdoptWebWorkspaceResponse], error) {
	return handleUnary[v1.AdoptWebWorkspaceRequest, v1.AdoptWebWorkspaceResponse](ctx, s, "AdoptWebWorkspace", req.Msg)
}

// The page mux. `WatchPage`, `SubscribePage` and `UnsubscribePage` are how a
// BROWSER holds its standing subscriptions on one connection
// (endpoint_watch_page.proto); Emacs holds its three streams directly and never
// attaches a page, so this fake refuses all three loudly rather than mocking a
// surface no Emacs scenario can reach.

func (s *fakeServer) WatchPage(ctx context.Context, req *connect.Request[v1.WatchPageRequest], stream *connect.ServerStream[v1.WatchPageResponse]) error {
	s.record(ctx, "WatchPage", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a stream this fake does not mock was called",
		map[string]any{"method": "WatchPage"})
	return connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; WatchPage is the webapp's page mux"))
}

func (s *fakeServer) SubscribePage(ctx context.Context, req *connect.Request[v1.SubscribePageRequest]) (*connect.Response[v1.SubscribePageResponse], error) {
	s.record(ctx, "SubscribePage", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a page-mux verb this fake does not mock was called",
		map[string]any{"method": "SubscribePage"})
	return nil, connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; SubscribePage is the webapp's page mux"))
}

func (s *fakeServer) UnsubscribePage(ctx context.Context, req *connect.Request[v1.UnsubscribePageRequest]) (*connect.Response[v1.UnsubscribePageResponse], error) {
	s.record(ctx, "UnsubscribePage", req.Msg)
	logWarn("fakedaemon.stream.not-mocked", "a page-mux verb this fake does not mock was called",
		map[string]any{"method": "UnsubscribePage"})
	return nil, connect.NewError(connect.CodeUnimplemented,
		fmt.Errorf("fakedaemon mocks only Emacs's streams; UnsubscribePage is the webapp's page mux"))
}

func (s *fakeServer) BindWorkspaceSession(ctx context.Context, req *connect.Request[v1.BindWorkspaceSessionRequest]) (*connect.Response[v1.BindWorkspaceSessionResponse], error) {
	return handleUnary[v1.BindWorkspaceSessionRequest, v1.BindWorkspaceSessionResponse](ctx, s, "BindWorkspaceSession", req.Msg)
}

func (s *fakeServer) ListWorkspaceTranscripts(ctx context.Context, req *connect.Request[v1.ListWorkspaceTranscriptsRequest]) (*connect.Response[v1.ListWorkspaceTranscriptsResponse], error) {
	return handleUnary[v1.ListWorkspaceTranscriptsRequest, v1.ListWorkspaceTranscriptsResponse](ctx, s, "ListWorkspaceTranscripts", req.Msg)
}

func (s *fakeServer) Deploy(ctx context.Context, req *connect.Request[v1.DeployRequest]) (*connect.Response[v1.DeployResponse], error) {
	return handleUnary[v1.DeployRequest, v1.DeployResponse](ctx, s, "Deploy", req.Msg)
}
