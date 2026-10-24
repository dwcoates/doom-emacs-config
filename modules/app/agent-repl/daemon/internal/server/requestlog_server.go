package server

import (
	"context"
	"errors"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// requestLoggingServer owns the transport boundary around every generated RPC
// handler. The wrapped server retains policy; this layer only binds request
// identity and workspace log routing, then records entry and completion.
//
// EVERY WRAPPER IS ONE CALL to serveUnary or serveStream, so the boundary's
// records, its failure answer and its operation name are written once and no
// rpc can drift from the others.
type requestLoggingServer struct {
	server *server
}

// serveUnary runs one unary rpc inside its request boundary: it binds the
// request's identity and log routing, records entry and completion under the
// rpc's operation, and answers a boundary that could not be opened through
// boundaryFailure.
//
// EVERY UNARY RPC IS TIMED, from before its boundary opens (the boundary's
// workspace read is the daemon's time too) to its handler's return, by
// method and outcome (Deps.RPCs). A stream is not: a watch lasts as long as
// its client does.
func serveUnary[Req, Resp any](
	s *requestLoggingServer,
	ctx context.Context,
	rpc string,
	req *connect.Request[Req],
	handle func(context.Context, *connect.Request[Req]) (*connect.Response[Resp], error),
) (resp *connect.Response[Resp], err error) {
	began := time.Now()
	defer func() { s.server.deps.RPCs.ObserveRPC(rpc, rpcOutcome(err), time.Since(began)) }()
	boundary, err := s.server.beginRequest(ctx, rpc, req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return nil, boundaryFailure(err)
	}
	defer boundary.enter()(&err)
	return handle(ctx, req)
}

// RPCRecorder times the unary rpcs the daemon answers
// (runtimemetrics.Surface).
type RPCRecorder interface {
	ObserveRPC(rpc, outcome string, took time.Duration)
}

// rpcOutcome names how a unary rpc ended for its timing: "ok", or the
// Connect code it answered, a closed set.
func rpcOutcome(err error) string {
	if err == nil {
		return "ok"
	}
	var cerr *connect.Error
	if errors.As(err, &cerr) {
		return cerr.Code().String()
	}
	return connect.CodeUnknown.String()
}

// serveStream is serveUnary for a server-streaming rpc.
func serveStream[Req, Resp any](
	s *requestLoggingServer,
	ctx context.Context,
	rpc string,
	req *connect.Request[Req],
	stream *connect.ServerStream[Resp],
	handle func(context.Context, *connect.Request[Req], *connect.ServerStream[Resp]) error,
) (err error) {
	boundary, err := s.server.beginRequest(ctx, rpc, req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return boundaryFailure(err)
	}
	defer boundary.enter()(&err)
	return handle(ctx, req, stream)
}

func (s *requestLoggingServer) SubmitPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubmitPromptRequest],
) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	return serveUnary(s, ctx, "SubmitPrompt", req, s.server.SubmitPrompt)
}

func (s *requestLoggingServer) FoldMergeBubble(
	ctx context.Context,
	req *connect.Request[agentreplv1.FoldMergeBubbleRequest],
) (*connect.Response[agentreplv1.FoldMergeBubbleResponse], error) {
	return serveUnary(s, ctx, "FoldMergeBubble", req, s.server.FoldMergeBubble)
}

func (s *requestLoggingServer) SelectFeedRow(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectFeedRowRequest],
) (*connect.Response[agentreplv1.SelectFeedRowResponse], error) {
	return serveUnary(s, ctx, "SelectFeedRow", req, s.server.SelectFeedRow)
}

func (s *requestLoggingServer) PlanRollback(
	ctx context.Context,
	req *connect.Request[agentreplv1.PlanRollbackRequest],
) (*connect.Response[agentreplv1.PlanRollbackResponse], error) {
	return serveUnary(s, ctx, "PlanRollback", req, s.server.PlanRollback)
}

func (s *requestLoggingServer) ListRollbackPrompts(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListRollbackPromptsRequest],
) (*connect.Response[agentreplv1.ListRollbackPromptsResponse], error) {
	return serveUnary(s, ctx, "ListRollbackPrompts", req, s.server.ListRollbackPrompts)
}

func (s *requestLoggingServer) RollBack(
	ctx context.Context,
	req *connect.Request[agentreplv1.RollBackRequest],
) (*connect.Response[agentreplv1.RollBackResponse], error) {
	return serveUnary(s, ctx, "RollBack", req, s.server.RollBack)
}

func (s *requestLoggingServer) AdjustFeedTextScale(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdjustFeedTextScaleRequest],
) (*connect.Response[agentreplv1.AdjustFeedTextScaleResponse], error) {
	return serveUnary(s, ctx, "AdjustFeedTextScale", req, s.server.AdjustFeedTextScale)
}

func (s *requestLoggingServer) RequestCommandSupport(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequestCommandSupportRequest],
) (*connect.Response[agentreplv1.RequestCommandSupportResponse], error) {
	return serveUnary(s, ctx, "RequestCommandSupport", req, s.server.RequestCommandSupport)
}

func (s *requestLoggingServer) OpenFeed(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenFeedRequest],
) (*connect.Response[agentreplv1.OpenFeedResponse], error) {
	return serveUnary(s, ctx, "OpenFeed", req, s.server.OpenFeed)
}

func (s *requestLoggingServer) WatchFeed(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchFeedRequest],
	stream *connect.ServerStream[agentreplv1.WatchFeedResponse],
) error {
	return serveStream(s, ctx, "WatchFeed", req, stream, s.server.WatchFeed)
}

func (s *requestLoggingServer) LoadFeedThrough(
	ctx context.Context,
	req *connect.Request[agentreplv1.LoadFeedThroughRequest],
	stream *connect.ServerStream[agentreplv1.LoadFeedThroughResponse],
) error {
	return serveStream(s, ctx, "LoadFeedThrough", req, stream, s.server.LoadFeedThrough)
}

func (s *requestLoggingServer) GetFeedPage(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetFeedPageRequest],
) (*connect.Response[agentreplv1.GetFeedPageResponse], error) {
	return serveUnary(s, ctx, "GetFeedPage", req, s.server.GetFeedPage)
}

func (s *requestLoggingServer) Interrupt(
	ctx context.Context,
	req *connect.Request[agentreplv1.InterruptRequest],
) (*connect.Response[agentreplv1.InterruptResponse], error) {
	return serveUnary(s, ctx, "Interrupt", req, s.server.Interrupt)
}

func (s *requestLoggingServer) AnswerPermission(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerPermissionRequest],
) (*connect.Response[agentreplv1.AnswerPermissionResponse], error) {
	return serveUnary(s, ctx, "AnswerPermission", req, s.server.AnswerPermission)
}

func (s *requestLoggingServer) AnswerQuestion(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerQuestionRequest],
) (*connect.Response[agentreplv1.AnswerQuestionResponse], error) {
	return serveUnary(s, ctx, "AnswerQuestion", req, s.server.AnswerQuestion)
}

func (s *requestLoggingServer) UpdateQuestionDraft(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateQuestionDraftRequest],
) (*connect.Response[agentreplv1.UpdateQuestionDraftResponse], error) {
	return serveUnary(s, ctx, "UpdateQuestionDraft", req, s.server.UpdateQuestionDraft)
}

func (s *requestLoggingServer) AnswerColdGate(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerColdGateRequest],
) (*connect.Response[agentreplv1.AnswerColdGateResponse], error) {
	return serveUnary(s, ctx, "AnswerColdGate", req, s.server.AnswerColdGate)
}

func (s *requestLoggingServer) WatchWorkspaceRoster(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWorkspaceRosterRequest],
	stream *connect.ServerStream[agentreplv1.WatchWorkspaceRosterResponse],
) error {
	return serveStream(s, ctx, "WatchWorkspaceRoster", req, stream, s.server.WatchWorkspaceRoster)
}

func (s *requestLoggingServer) CreateWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateWorkspaceRequest],
) (*connect.Response[agentreplv1.CreateWorkspaceResponse], error) {
	return serveUnary(s, ctx, "CreateWorkspace", req, s.server.CreateWorkspace)
}

func (s *requestLoggingServer) OpenWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenWorkspaceRequest],
) (*connect.Response[agentreplv1.OpenWorkspaceResponse], error) {
	return serveUnary(s, ctx, "OpenWorkspace", req, s.server.OpenWorkspace)
}

func (s *requestLoggingServer) CloseWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseWorkspaceRequest],
) (*connect.Response[agentreplv1.CloseWorkspaceResponse], error) {
	return serveUnary(s, ctx, "CloseWorkspace", req, s.server.CloseWorkspace)
}

func (s *requestLoggingServer) KillWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.KillWorkspaceRequest],
) (*connect.Response[agentreplv1.KillWorkspaceResponse], error) {
	return serveUnary(s, ctx, "KillWorkspace", req, s.server.KillWorkspace)
}

func (s *requestLoggingServer) NukeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.NukeWorkspaceRequest],
) (*connect.Response[agentreplv1.NukeWorkspaceResponse], error) {
	return serveUnary(s, ctx, "NukeWorkspace", req, s.server.NukeWorkspace)
}

func (s *requestLoggingServer) ForgetWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.ForgetWorkspaceRequest],
) (*connect.Response[agentreplv1.ForgetWorkspaceResponse], error) {
	return serveUnary(s, ctx, "ForgetWorkspace", req, s.server.ForgetWorkspace)
}

func (s *requestLoggingServer) FinishWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.FinishWorkspaceRequest],
) (*connect.Response[agentreplv1.FinishWorkspaceResponse], error) {
	return serveUnary(s, ctx, "FinishWorkspace", req, s.server.FinishWorkspace)
}

func (s *requestLoggingServer) RestartWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RestartWorkspaceRequest],
) (*connect.Response[agentreplv1.RestartWorkspaceResponse], error) {
	return serveUnary(s, ctx, "RestartWorkspace", req, s.server.RestartWorkspace)
}

func (s *requestLoggingServer) FoldRepository(
	ctx context.Context,
	req *connect.Request[agentreplv1.FoldRepositoryRequest],
) (*connect.Response[agentreplv1.FoldRepositoryResponse], error) {
	return serveUnary(s, ctx, "FoldRepository", req, s.server.FoldRepository)
}

func (s *requestLoggingServer) UpdateSidebarView(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateSidebarViewRequest],
) (*connect.Response[agentreplv1.UpdateSidebarViewResponse], error) {
	return serveUnary(s, ctx, "UpdateSidebarView", req, s.server.UpdateSidebarView)
}

func (s *requestLoggingServer) SetWorkspacePriority(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspacePriorityRequest],
) (*connect.Response[agentreplv1.SetWorkspacePriorityResponse], error) {
	return serveUnary(s, ctx, "SetWorkspacePriority", req, s.server.SetWorkspacePriority)
}

func (s *requestLoggingServer) SetWorkspacePrincipalLink(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspacePrincipalLinkRequest],
) (*connect.Response[agentreplv1.SetWorkspacePrincipalLinkResponse], error) {
	return serveUnary(s, ctx, "SetWorkspacePrincipalLink", req, s.server.SetWorkspacePrincipalLink)
}

func (s *requestLoggingServer) SetWorkspaceParent(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceParentRequest],
) (*connect.Response[agentreplv1.SetWorkspaceParentResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceParent", req, s.server.SetWorkspaceParent)
}

func (s *requestLoggingServer) SetWorkspaceHidden(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceHiddenRequest],
) (*connect.Response[agentreplv1.SetWorkspaceHiddenResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceHidden", req, s.server.SetWorkspaceHidden)
}

func (s *requestLoggingServer) MoveWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.MoveWorkspaceRequest],
) (*connect.Response[agentreplv1.MoveWorkspaceResponse], error) {
	return serveUnary(s, ctx, "MoveWorkspace", req, s.server.MoveWorkspace)
}

func (s *requestLoggingServer) SwapWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.SwapWorkspacesRequest],
) (*connect.Response[agentreplv1.SwapWorkspacesResponse], error) {
	return serveUnary(s, ctx, "SwapWorkspaces", req, s.server.SwapWorkspaces)
}

func (s *requestLoggingServer) ArrangeSidebar(
	ctx context.Context,
	req *connect.Request[agentreplv1.ArrangeSidebarRequest],
) (*connect.Response[agentreplv1.ArrangeSidebarResponse], error) {
	return serveUnary(s, ctx, "ArrangeSidebar", req, s.server.ArrangeSidebar)
}

func (s *requestLoggingServer) GetSidebarArrangement(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetSidebarArrangementRequest],
) (*connect.Response[agentreplv1.GetSidebarArrangementResponse], error) {
	return serveUnary(s, ctx, "GetSidebarArrangement", req, s.server.GetSidebarArrangement)
}

func (s *requestLoggingServer) CreateTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateTaskRequest],
) (*connect.Response[agentreplv1.CreateTaskResponse], error) {
	return serveUnary(s, ctx, "CreateTask", req, s.server.CreateTask)
}

func (s *requestLoggingServer) UpdateTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateTaskRequest],
) (*connect.Response[agentreplv1.UpdateTaskResponse], error) {
	return serveUnary(s, ctx, "UpdateTask", req, s.server.UpdateTask)
}

func (s *requestLoggingServer) AssignWorkspaceTask(
	ctx context.Context,
	req *connect.Request[agentreplv1.AssignWorkspaceTaskRequest],
) (*connect.Response[agentreplv1.AssignWorkspaceTaskResponse], error) {
	return serveUnary(s, ctx, "AssignWorkspaceTask", req, s.server.AssignWorkspaceTask)
}

func (s *requestLoggingServer) WatchTopbar(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchTopbarRequest],
	stream *connect.ServerStream[agentreplv1.WatchTopbarResponse],
) error {
	return serveStream(s, ctx, "WatchTopbar", req, stream, s.server.WatchTopbar)
}

func (s *requestLoggingServer) SetModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetModelRequest],
) (*connect.Response[agentreplv1.SetModelResponse], error) {
	return serveUnary(s, ctx, "SetModel", req, s.server.SetModel)
}

func (s *requestLoggingServer) SetEffort(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetEffortRequest],
) (*connect.Response[agentreplv1.SetEffortResponse], error) {
	return serveUnary(s, ctx, "SetEffort", req, s.server.SetEffort)
}

func (s *requestLoggingServer) RetryVendorError(
	ctx context.Context,
	req *connect.Request[agentreplv1.RetryVendorErrorRequest],
) (*connect.Response[agentreplv1.RetryVendorErrorResponse], error) {
	return serveUnary(s, ctx, "RetryVendorError", req, s.server.RetryVendorError)
}

func (s *requestLoggingServer) SetPermissionMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetPermissionModeRequest],
) (*connect.Response[agentreplv1.SetPermissionModeResponse], error) {
	return serveUnary(s, ctx, "SetPermissionMode", req, s.server.SetPermissionMode)
}

func (s *requestLoggingServer) WatchFooter(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchFooterRequest],
	stream *connect.ServerStream[agentreplv1.WatchFooterResponse],
) error {
	return serveStream(s, ctx, "WatchFooter", req, stream, s.server.WatchFooter)
}

func (s *requestLoggingServer) UpdateHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateHeldPromptRequest],
) (*connect.Response[agentreplv1.UpdateHeldPromptResponse], error) {
	return serveUnary(s, ctx, "UpdateHeldPrompt", req, s.server.UpdateHeldPrompt)
}

func (s *requestLoggingServer) EditHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.EditHeldPromptRequest],
) (*connect.Response[agentreplv1.EditHeldPromptResponse], error) {
	return serveUnary(s, ctx, "EditHeldPrompt", req, s.server.EditHeldPrompt)
}

func (s *requestLoggingServer) FoldHeldPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.FoldHeldPromptRequest],
) (*connect.Response[agentreplv1.FoldHeldPromptResponse], error) {
	return serveUnary(s, ctx, "FoldHeldPrompt", req, s.server.FoldHeldPrompt)
}

func (s *requestLoggingServer) UpdateClassifierPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateClassifierPromptRequest],
) (*connect.Response[agentreplv1.UpdateClassifierPromptResponse], error) {
	return serveUnary(s, ctx, "UpdateClassifierPrompt", req, s.server.UpdateClassifierPrompt)
}

func (s *requestLoggingServer) AnswerMergeQueueOffer(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerMergeQueueOfferRequest],
) (*connect.Response[agentreplv1.AnswerMergeQueueOfferResponse], error) {
	return serveUnary(s, ctx, "AnswerMergeQueueOffer", req, s.server.AnswerMergeQueueOffer)
}

func (s *requestLoggingServer) Deploy(
	ctx context.Context,
	req *connect.Request[agentreplv1.DeployRequest],
) (*connect.Response[agentreplv1.DeployResponse], error) {
	return serveUnary(s, ctx, "Deploy", req, s.server.Deploy)
}

func (s *requestLoggingServer) UpdateShutdownSchedule(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateShutdownScheduleRequest],
) (*connect.Response[agentreplv1.UpdateShutdownScheduleResponse], error) {
	return serveUnary(s, ctx, "UpdateShutdownSchedule", req, s.server.UpdateShutdownSchedule)
}

func (s *requestLoggingServer) UpdateMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateMergeQueueRequest],
) (*connect.Response[agentreplv1.UpdateMergeQueueResponse], error) {
	return serveUnary(s, ctx, "UpdateMergeQueue", req, s.server.UpdateMergeQueue)
}

func (s *requestLoggingServer) DaemonHealth(
	ctx context.Context,
	req *connect.Request[agentreplv1.DaemonHealthRequest],
) (*connect.Response[agentreplv1.DaemonHealthResponse], error) {
	return serveUnary(s, ctx, "DaemonHealth", req, s.server.DaemonHealth)
}

func (s *requestLoggingServer) SessionHealth(
	ctx context.Context,
	req *connect.Request[agentreplv1.SessionHealthRequest],
) (*connect.Response[agentreplv1.SessionHealthResponse], error) {
	return serveUnary(s, ctx, "SessionHealth", req, s.server.SessionHealth)
}

func (s *requestLoggingServer) ReportLatencies(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportLatenciesRequest],
) (*connect.Response[agentreplv1.ReportLatenciesResponse], error) {
	return serveUnary(s, ctx, "ReportLatencies", req, s.server.ReportLatencies)
}

func (s *requestLoggingServer) ClientLog(
	ctx context.Context,
	req *connect.Request[agentreplv1.ClientLogRequest],
) (*connect.Response[agentreplv1.ClientLogResponse], error) {
	return serveUnary(s, ctx, "ClientLog", req, s.server.ClientLog)
}

func (s *requestLoggingServer) SendHiddenKeepalives(
	ctx context.Context,
	req *connect.Request[agentreplv1.SendHiddenKeepalivesRequest],
) (*connect.Response[agentreplv1.SendHiddenKeepalivesResponse], error) {
	return serveUnary(s, ctx, "SendHiddenKeepalives", req, s.server.SendHiddenKeepalives)
}

func (s *requestLoggingServer) UpdatePersistentWifiMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdatePersistentWifiModeRequest],
) (*connect.Response[agentreplv1.UpdatePersistentWifiModeResponse], error) {
	return serveUnary(s, ctx, "UpdatePersistentWifiMode", req, s.server.UpdatePersistentWifiMode)
}

func (s *requestLoggingServer) RegisterWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterWorkspaceRequest],
) (*connect.Response[agentreplv1.RegisterWorkspaceResponse], error) {
	return serveUnary(s, ctx, "RegisterWorkspace", req, s.server.RegisterWorkspace)
}

func (s *requestLoggingServer) RegisterRepository(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterRepositoryRequest],
) (*connect.Response[agentreplv1.RegisterRepositoryResponse], error) {
	return serveUnary(s, ctx, "RegisterRepository", req, s.server.RegisterRepository)
}

func (s *requestLoggingServer) SelectWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectWorkspaceRequest],
) (*connect.Response[agentreplv1.SelectWorkspaceResponse], error) {
	return serveUnary(s, ctx, "SelectWorkspace", req, s.server.SelectWorkspace)
}

func (s *requestLoggingServer) SelectInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectInEditorRequest],
) (*connect.Response[agentreplv1.SelectInEditorResponse], error) {
	return serveUnary(s, ctx, "SelectInEditor", req, s.server.SelectInEditor)
}

func (s *requestLoggingServer) ReportPageUnloading(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportPageUnloadingRequest],
) (*connect.Response[agentreplv1.ReportPageUnloadingResponse], error) {
	return serveUnary(s, ctx, "ReportPageUnloading", req, s.server.ReportPageUnloading)
}

func (s *requestLoggingServer) MarkWorkspaceViewed(
	ctx context.Context,
	req *connect.Request[agentreplv1.MarkWorkspaceViewedRequest],
) (*connect.Response[agentreplv1.MarkWorkspaceViewedResponse], error) {
	return serveUnary(s, ctx, "MarkWorkspaceViewed", req, s.server.MarkWorkspaceViewed)
}

func (s *requestLoggingServer) ReportEditorFocus(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportEditorFocusRequest],
) (*connect.Response[agentreplv1.ReportEditorFocusResponse], error) {
	return serveUnary(s, ctx, "ReportEditorFocus", req, s.server.ReportEditorFocus)
}

func (s *requestLoggingServer) WatchHostWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchHostWorkspaceRequest],
	stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse],
) error {
	return serveStream(s, ctx, "WatchHostWorkspace", req, stream, s.server.WatchHostWorkspace)
}

func (s *requestLoggingServer) WatchDaemon(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchDaemonRequest],
	stream *connect.ServerStream[agentreplv1.WatchDaemonResponse],
) error {
	return serveStream(s, ctx, "WatchDaemon", req, stream, s.server.WatchDaemon)
}

func (s *requestLoggingServer) AdoptHostWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdoptHostWorkspaceRequest],
) (*connect.Response[agentreplv1.AdoptHostWorkspaceResponse], error) {
	return serveUnary(s, ctx, "AdoptHostWorkspace", req, s.server.AdoptHostWorkspace)
}

func (s *requestLoggingServer) WatchWebWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWebWorkspaceRequest],
	stream *connect.ServerStream[agentreplv1.WatchWebWorkspaceResponse],
) error {
	return serveStream(s, ctx, "WatchWebWorkspace", req, stream, s.server.WatchWebWorkspace)
}

func (s *requestLoggingServer) SelectAccount(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectAccountRequest],
) (*connect.Response[agentreplv1.SelectAccountResponse], error) {
	return serveUnary(s, ctx, "SelectAccount", req, s.server.SelectAccount)
}

func (s *requestLoggingServer) OpenLogin(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenLoginRequest],
) (*connect.Response[agentreplv1.OpenLoginResponse], error) {
	return serveUnary(s, ctx, "OpenLogin", req, s.server.OpenLogin)
}

func (s *requestLoggingServer) WatchLoginTerminal(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchLoginTerminalRequest],
	stream *connect.ServerStream[agentreplv1.LoginTerminalOutput],
) error {
	return serveStream(s, ctx, "WatchLoginTerminal", req, stream, s.server.WatchLoginTerminal)
}

func (s *requestLoggingServer) SendLoginInput(
	ctx context.Context,
	req *connect.Request[agentreplv1.SendLoginInputRequest],
) (*connect.Response[agentreplv1.SendLoginInputResponse], error) {
	return serveUnary(s, ctx, "SendLoginInput", req, s.server.SendLoginInput)
}

func (s *requestLoggingServer) CloseLogin(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseLoginRequest],
) (*connect.Response[agentreplv1.CloseLoginResponse], error) {
	return serveUnary(s, ctx, "CloseLogin", req, s.server.CloseLogin)
}

func (s *requestLoggingServer) OpenExternal(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenExternalRequest],
) (*connect.Response[agentreplv1.OpenExternalResponse], error) {
	return serveUnary(s, ctx, "OpenExternal", req, s.server.OpenExternal)
}

func (s *requestLoggingServer) InspectChessBoardSquare(
	ctx context.Context,
	req *connect.Request[agentreplv1.InspectChessBoardSquareRequest],
) (*connect.Response[agentreplv1.InspectChessBoardSquareResponse], error) {
	return serveUnary(s, ctx, "InspectChessBoardSquare", req, s.server.InspectChessBoardSquare)
}

func (s *requestLoggingServer) OpenInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenInEditorRequest],
) (*connect.Response[agentreplv1.OpenInEditorResponse], error) {
	return serveUnary(s, ctx, "OpenInEditor", req, s.server.OpenInEditor)
}

func (s *requestLoggingServer) AdoptWebWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AdoptWebWorkspaceRequest],
) (*connect.Response[agentreplv1.AdoptWebWorkspaceResponse], error) {
	return serveUnary(s, ctx, "AdoptWebWorkspace", req, s.server.AdoptWebWorkspace)
}

func (s *requestLoggingServer) WatchPage(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchPageRequest],
	stream *connect.ServerStream[agentreplv1.WatchPageResponse],
) error {
	return serveStream(s, ctx, "WatchPage", req, stream, s.server.WatchPage)
}

func (s *requestLoggingServer) OpenTabs(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenTabsRequest],
	stream *connect.ServerStream[agentreplv1.OpenTabsResponse],
) error {
	return serveStream(s, ctx, "OpenTabs", req, stream, s.server.OpenTabs)
}

func (s *requestLoggingServer) ReportPageDrawn(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportPageDrawnRequest],
) (*connect.Response[agentreplv1.ReportPageDrawnResponse], error) {
	return serveUnary(s, ctx, "ReportPageDrawn", req, s.server.ReportPageDrawn)
}

func (s *requestLoggingServer) SubscribePage(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribePageRequest],
) (*connect.Response[agentreplv1.SubscribePageResponse], error) {
	return serveUnary(s, ctx, "SubscribePage", req, s.server.SubscribePage)
}

func (s *requestLoggingServer) UnsubscribePage(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribePageRequest],
) (*connect.Response[agentreplv1.UnsubscribePageResponse], error) {
	return serveUnary(s, ctx, "UnsubscribePage", req, s.server.UnsubscribePage)
}

func (s *requestLoggingServer) ListWorkspaceTranscripts(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListWorkspaceTranscriptsRequest],
) (*connect.Response[agentreplv1.ListWorkspaceTranscriptsResponse], error) {
	return serveUnary(s, ctx, "ListWorkspaceTranscripts", req, s.server.ListWorkspaceTranscripts)
}

func (s *requestLoggingServer) BindWorkspaceSession(
	ctx context.Context,
	req *connect.Request[agentreplv1.BindWorkspaceSessionRequest],
) (*connect.Response[agentreplv1.BindWorkspaceSessionResponse], error) {
	return serveUnary(s, ctx, "BindWorkspaceSession", req, s.server.BindWorkspaceSession)
}

func (s *requestLoggingServer) DismissNewsDigest(
	ctx context.Context,
	req *connect.Request[agentreplv1.DismissNewsDigestRequest],
) (*connect.Response[agentreplv1.DismissNewsDigestResponse], error) {
	return serveUnary(s, ctx, "DismissNewsDigest", req, s.server.DismissNewsDigest)
}

func (s *requestLoggingServer) RefreshNewsDigest(
	ctx context.Context,
	req *connect.Request[agentreplv1.RefreshNewsDigestRequest],
) (*connect.Response[agentreplv1.RefreshNewsDigestResponse], error) {
	return serveUnary(s, ctx, "RefreshNewsDigest", req, s.server.RefreshNewsDigest)
}

func (s *requestLoggingServer) ListSlashCommands(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListSlashCommandsRequest],
) (*connect.Response[agentreplv1.ListSlashCommandsResponse], error) {
	return serveUnary(s, ctx, "ListSlashCommands", req, s.server.ListSlashCommands)
}

func (s *requestLoggingServer) AnswerEditorEvaluation(
	ctx context.Context,
	req *connect.Request[agentreplv1.AnswerEditorEvaluationRequest],
) (*connect.Response[agentreplv1.AnswerEditorEvaluationResponse], error) {
	return serveUnary(s, ctx, "AnswerEditorEvaluation", req, s.server.AnswerEditorEvaluation)
}

func (s *requestLoggingServer) CompactWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.CompactWorkspaceRequest],
) (*connect.Response[agentreplv1.CompactWorkspaceResponse], error) {
	return serveUnary(s, ctx, "CompactWorkspace", req, s.server.CompactWorkspace)
}

func (s *requestLoggingServer) RunTests(
	ctx context.Context,
	req *connect.Request[agentreplv1.RunTestsRequest],
	stream *connect.ServerStream[agentreplv1.RunTestsResponse],
) error {
	return serveStream(s, ctx, "RunTests", req, stream, s.server.RunTests)
}

func (s *requestLoggingServer) RunRedundancy(
	ctx context.Context,
	req *connect.Request[agentreplv1.RunRedundancyRequest],
	stream *connect.ServerStream[agentreplv1.RunRedundancyResponse],
) error {
	return serveStream(s, ctx, "RunRedundancy", req, stream, s.server.RunRedundancy)
}

func (s *requestLoggingServer) EvaluateInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.EvaluateInEditorRequest],
) (*connect.Response[agentreplv1.EvaluateInEditorResponse], error) {
	return serveUnary(s, ctx, "EvaluateInEditor", req, s.server.EvaluateInEditor)
}

func (s *requestLoggingServer) GetMergeQueueStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetMergeQueueStatusRequest],
) (*connect.Response[agentreplv1.GetMergeQueueStatusResponse], error) {
	return serveUnary(s, ctx, "GetMergeQueueStatus", req, s.server.GetMergeQueueStatus)
}

func (s *requestLoggingServer) GetWorkspaceStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetWorkspaceStatusRequest],
) (*connect.Response[agentreplv1.GetWorkspaceStatusResponse], error) {
	return serveUnary(s, ctx, "GetWorkspaceStatus", req, s.server.GetWorkspaceStatus)
}

func (s *requestLoggingServer) SubscribeMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeMergeQueueRequest],
) (*connect.Response[agentreplv1.SubscribeMergeQueueResponse], error) {
	return serveUnary(s, ctx, "SubscribeMergeQueue", req, s.server.SubscribeMergeQueue)
}

func (s *requestLoggingServer) SubscribeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeWorkspaceRequest],
) (*connect.Response[agentreplv1.SubscribeWorkspaceResponse], error) {
	return serveUnary(s, ctx, "SubscribeWorkspace", req, s.server.SubscribeWorkspace)
}

func (s *requestLoggingServer) UnsubscribeMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeMergeQueueRequest],
) (*connect.Response[agentreplv1.UnsubscribeMergeQueueResponse], error) {
	return serveUnary(s, ctx, "UnsubscribeMergeQueue", req, s.server.UnsubscribeMergeQueue)
}

func (s *requestLoggingServer) UnsubscribeWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeWorkspaceRequest],
) (*connect.Response[agentreplv1.UnsubscribeWorkspaceResponse], error) {
	return serveUnary(s, ctx, "UnsubscribeWorkspace", req, s.server.UnsubscribeWorkspace)
}

func (s *requestLoggingServer) UpdateDeployHold(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateDeployHoldRequest],
) (*connect.Response[agentreplv1.UpdateDeployHoldResponse], error) {
	return serveUnary(s, ctx, "UpdateDeployHold", req, s.server.UpdateDeployHold)
}

func (s *requestLoggingServer) RollOutLanding(
	ctx context.Context,
	req *connect.Request[agentreplv1.RollOutLandingRequest],
) (*connect.Response[agentreplv1.RollOutLandingResponse], error) {
	return serveUnary(s, ctx, "RollOutLanding", req, s.server.RollOutLanding)
}

func (s *requestLoggingServer) SetWorkspaceOneShot(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceOneShotRequest],
) (*connect.Response[agentreplv1.SetWorkspaceOneShotResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceOneShot", req, s.server.SetWorkspaceOneShot)
}

func (s *requestLoggingServer) SetWorkspaceCorrespondence(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceCorrespondenceRequest],
) (*connect.Response[agentreplv1.SetWorkspaceCorrespondenceResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceCorrespondence", req, s.server.SetWorkspaceCorrespondence)
}

func (s *requestLoggingServer) OpenFeedImage(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenFeedImageRequest],
) (*connect.Response[agentreplv1.OpenFeedImageResponse], error) {
	return serveUnary(s, ctx, "OpenFeedImage", req, s.server.OpenFeedImage)
}

func (s *requestLoggingServer) CloseFeedImage(
	ctx context.Context,
	req *connect.Request[agentreplv1.CloseFeedImageRequest],
) (*connect.Response[agentreplv1.CloseFeedImageResponse], error) {
	return serveUnary(s, ctx, "CloseFeedImage", req, s.server.CloseFeedImage)
}
