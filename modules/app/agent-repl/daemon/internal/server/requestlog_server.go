package server

import (
	"context"
	"errors"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/proflabel"
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
	requestID := req.Header().Get(requestIDHeader)
	phase := "boundary"
	defer func() {
		took := time.Since(began)
		s.server.deps.RPCs.ObserveRPC(rpc, rpcOutcome(err), took)
		recordAbandoned(s.server.log, ctx, rpc, requestID, phase, took)
	}()
	if target, ok, err := s.server.forwardTarget(ctx, rpc, req.Header(), requestMessage(req)); err != nil {
		return nil, boundaryFailure(err)
	} else if ok {
		phase = "forward"
		resp, err := forwardUnary[Req, Resp](ctx, s.server, rpc, target, req)
		if !errors.Is(err, errTargetGone) {
			return resp, err
		}
		// THE DAEMON IT WAS SENT TO IS GONE: the call is answered here, as
		// one for a daemon that no longer runs is (targetGone).
		phase = "boundary"
	}
	boundary, err := s.server.beginRequest(ctx, rpc, requestID, requestMessage(req))
	if err != nil {
		return nil, boundaryFailure(err)
	}
	phase = "handler"
	defer boundary.enter()(&err)
	ctx = boundary.handOver(ctx)
	// THE HANDLER'S WORK IS LABELED BY ITS WORKSPACE (internal/proflabel), so
	// a CPU profile splits by workspace.
	proflabel.Do(ctx, ids.WorkspaceID(boundary.workspace), func(ctx context.Context) { resp, err = handle(ctx, req) })
	return resp, err
}

// recordAbandoned records, at INFO, a unary rpc whose caller left before it
// answered: the caller timed out or went away while the daemon still held the
// request in its boundary (resolving the workspace) or its handler.
//
// A CLIENT TIMEOUT NAMES ITS REQUEST ON BOTH SIDES. Emacs logs a unary timeout
// by its request id; this record carries the same id, the phase the request
// was held in and how long it waited, so a burst of client timeouts reads as
// requests the daemon held (and where), or, with no such record, as requests
// that never reached a handler (2026-11-18, the handover's 30s of timeouts).
func recordAbandoned(log dlog.Logger, ctx context.Context, rpc, requestID, phase string, took time.Duration) {
	if ctx.Err() == nil {
		return
	}
	log.Info("daemon.server.request_abandoned", "the rpc's caller left before the daemon answered", dlog.Context{
		"rpc":        rpc,
		"request_id": requestID,
		"phase":      phase,
		"waited_ms":  took.Milliseconds(),
		"cause":      context.Cause(ctx).Error(),
	})
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
	if target, ok, err := s.server.forwardTarget(ctx, rpc, req.Header(), requestMessage(req)); err != nil {
		return boundaryFailure(err)
	} else if ok {
		err := forwardStream[Req, Resp](ctx, s.server, rpc, target, req, stream)
		if !errors.Is(err, errTargetGone) {
			return err
		}
		// THE DAEMON IT WAS SENT TO IS GONE: the stream is served here.
	}
	boundary, err := s.server.beginRequest(ctx, rpc, req.Header().Get(requestIDHeader), requestMessage(req))
	if err != nil {
		return boundaryFailure(err)
	}
	defer boundary.enter()(&err)
	ctx = boundary.handOver(ctx)
	// A STREAM ANOTHER DAEMON RELAYS FOR A PAGE IT HOLDS (relayWatch) connects
	// that page to the stream's workspace here, from the stream's acceptance
	// until it ends.
	if page := req.Header().Get(RelayedPageHeader); page != "" {
		if ws := requestWorkspaceID(requestMessage(req)); ws != "" {
			accepted, release := s.server.holdRelayedPage(page, ids.WorkspaceID(ws))
			defer release()
			ctx = withAcceptHook(ctx, accepted)
		}
	}
	proflabel.Do(ctx, ids.WorkspaceID(boundary.workspace), func(ctx context.Context) { err = handle(ctx, req, stream) })
	return err
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

func (s *requestLoggingServer) ReportComposerEntries(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportComposerEntriesRequest],
) (*connect.Response[agentreplv1.ReportComposerEntriesResponse], error) {
	return serveUnary(s, ctx, "ReportComposerEntries", req, s.server.ReportComposerEntries)
}

func (s *requestLoggingServer) FindEntryNeighbor(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindEntryNeighborRequest],
) (*connect.Response[agentreplv1.FindEntryNeighborResponse], error) {
	return serveUnary(s, ctx, "FindEntryNeighbor", req, s.server.FindEntryNeighbor)
}

func (s *requestLoggingServer) SelectFeedRow(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectFeedRowRequest],
) (*connect.Response[agentreplv1.SelectFeedRowResponse], error) {
	return serveUnary(s, ctx, "SelectFeedRow", req, s.server.SelectFeedRow)
}

func (s *requestLoggingServer) AskAboutResponse(
	ctx context.Context,
	req *connect.Request[agentreplv1.AskAboutResponseRequest],
) (*connect.Response[agentreplv1.AskAboutResponseResponse], error) {
	return serveUnary(s, ctx, "AskAboutResponse", req, s.server.AskAboutResponse)
}

func (s *requestLoggingServer) SelectResponseText(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectResponseTextRequest],
) (*connect.Response[agentreplv1.SelectResponseTextResponse], error) {
	return serveUnary(s, ctx, "SelectResponseText", req, s.server.SelectResponseText)
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

func (s *requestLoggingServer) KeepLiveWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.KeepLiveWorkspaceRequest],
) (*connect.Response[agentreplv1.KeepLiveWorkspaceResponse], error) {
	return serveUnary(s, ctx, "KeepLiveWorkspace", req, s.server.KeepLiveWorkspace)
}

func (s *requestLoggingServer) PassColdGate(
	ctx context.Context,
	req *connect.Request[agentreplv1.PassColdGateRequest],
) (*connect.Response[agentreplv1.PassColdGateResponse], error) {
	return serveUnary(s, ctx, "PassColdGate", req, s.server.PassColdGate)
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

func (s *requestLoggingServer) WithdrawWorkspaceClose(
	ctx context.Context,
	req *connect.Request[agentreplv1.WithdrawWorkspaceCloseRequest],
) (*connect.Response[agentreplv1.WithdrawWorkspaceCloseResponse], error) {
	return serveUnary(s, ctx, "WithdrawWorkspaceClose", req, s.server.WithdrawWorkspaceClose)
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

func (s *requestLoggingServer) ParkWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.ParkWorkspaceRequest],
) (*connect.Response[agentreplv1.ParkWorkspaceResponse], error) {
	return serveUnary(s, ctx, "ParkWorkspace", req, s.server.ParkWorkspace)
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

func (s *requestLoggingServer) ChangeWorkspaceLinks(
	ctx context.Context,
	req *connect.Request[agentreplv1.ChangeWorkspaceLinksRequest],
) (*connect.Response[agentreplv1.ChangeWorkspaceLinksResponse], error) {
	return serveUnary(s, ctx, "ChangeWorkspaceLinks", req, s.server.ChangeWorkspaceLinks)
}

func (s *requestLoggingServer) SetWorkspaceSummary(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceSummaryRequest],
) (*connect.Response[agentreplv1.SetWorkspaceSummaryResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceSummary", req, s.server.SetWorkspaceSummary)
}

func (s *requestLoggingServer) SetWorkspaceName(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceNameRequest],
) (*connect.Response[agentreplv1.SetWorkspaceNameResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceName", req, s.server.SetWorkspaceName)
}

func (s *requestLoggingServer) ListCreationModels(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListCreationModelsRequest],
) (*connect.Response[agentreplv1.ListCreationModelsResponse], error) {
	return serveUnary(s, ctx, "ListCreationModels", req, s.server.ListCreationModels)
}

func (s *requestLoggingServer) ValidateWorkspaceName(
	ctx context.Context,
	req *connect.Request[agentreplv1.ValidateWorkspaceNameRequest],
) (*connect.Response[agentreplv1.ValidateWorkspaceNameResponse], error) {
	return serveUnary(s, ctx, "ValidateWorkspaceName", req, s.server.ValidateWorkspaceName)
}

func (s *requestLoggingServer) SetWorkspaceParent(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceParentRequest],
) (*connect.Response[agentreplv1.SetWorkspaceParentResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceParent", req, s.server.SetWorkspaceParent)
}

func (s *requestLoggingServer) SetWorkspaceBacklogged(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceBackloggedRequest],
) (*connect.Response[agentreplv1.SetWorkspaceBackloggedResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceBacklogged", req, s.server.SetWorkspaceBacklogged)
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

func (s *requestLoggingServer) WatchWorkspaceMentions(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchWorkspaceMentionsRequest],
	stream *connect.ServerStream[agentreplv1.WatchWorkspaceMentionsResponse],
) error {
	return serveStream(s, ctx, "WatchWorkspaceMentions", req, stream, s.server.WatchWorkspaceMentions)
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

func (s *requestLoggingServer) SetDefaultModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetDefaultModelRequest],
) (*connect.Response[agentreplv1.SetDefaultModelResponse], error) {
	return serveUnary(s, ctx, "SetDefaultModel", req, s.server.SetDefaultModel)
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

func (s *requestLoggingServer) AwaitIdle(
	ctx context.Context,
	req *connect.Request[agentreplv1.AwaitIdleRequest],
) (*connect.Response[agentreplv1.AwaitIdleResponse], error) {
	return serveUnary(s, ctx, "AwaitIdle", req, s.server.AwaitIdle)
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

func (s *requestLoggingServer) ReportVendorSpend(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportVendorSpendRequest],
) (*connect.Response[agentreplv1.ReportVendorSpendResponse], error) {
	return serveUnary(s, ctx, "ReportVendorSpend", req, s.server.ReportVendorSpend)
}

func (s *requestLoggingServer) ClientLog(
	ctx context.Context,
	req *connect.Request[agentreplv1.ClientLogRequest],
) (*connect.Response[agentreplv1.ClientLogResponse], error) {
	return serveUnary(s, ctx, "ClientLog", req, s.server.ClientLog)
}

func (s *requestLoggingServer) SendBackloggedKeepalives(
	ctx context.Context,
	req *connect.Request[agentreplv1.SendBackloggedKeepalivesRequest],
) (*connect.Response[agentreplv1.SendBackloggedKeepalivesResponse], error) {
	return serveUnary(s, ctx, "SendBackloggedKeepalives", req, s.server.SendBackloggedKeepalives)
}

func (s *requestLoggingServer) UpdatePersistentWifiMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdatePersistentWifiModeRequest],
) (*connect.Response[agentreplv1.UpdatePersistentWifiModeResponse], error) {
	return serveUnary(s, ctx, "UpdatePersistentWifiMode", req, s.server.UpdatePersistentWifiMode)
}

func (s *requestLoggingServer) UpdateRemoteControlMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateRemoteControlModeRequest],
) (*connect.Response[agentreplv1.UpdateRemoteControlModeResponse], error) {
	return serveUnary(s, ctx, "UpdateRemoteControlMode", req, s.server.UpdateRemoteControlMode)
}

func (s *requestLoggingServer) RegisterWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterWorkspaceRequest],
) (*connect.Response[agentreplv1.RegisterWorkspaceResponse], error) {
	return serveUnary(s, ctx, "RegisterWorkspace", req, s.server.RegisterWorkspace)
}

func (s *requestLoggingServer) RegisterWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.RegisterWorkspacesRequest],
) (*connect.Response[agentreplv1.RegisterWorkspacesResponse], error) {
	return serveUnary(s, ctx, "RegisterWorkspaces", req, s.server.RegisterWorkspaces)
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

func (s *requestLoggingServer) FindPullRequestOwner(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindPullRequestOwnerRequest],
) (*connect.Response[agentreplv1.FindPullRequestOwnerResponse], error) {
	return serveUnary(s, ctx, "FindPullRequestOwner", req, s.server.FindPullRequestOwner)
}

func (s *requestLoggingServer) LoadPageInEditor(
	ctx context.Context,
	req *connect.Request[agentreplv1.LoadPageInEditorRequest],
) (*connect.Response[agentreplv1.LoadPageInEditorResponse], error) {
	return serveUnary(s, ctx, "LoadPageInEditor", req, s.server.LoadPageInEditor)
}

func (s *requestLoggingServer) ReportPageState(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportPageStateRequest],
) (*connect.Response[agentreplv1.ReportPageStateResponse], error) {
	return serveUnary(s, ctx, "ReportPageState", req, s.server.ReportPageState)
}

func (s *requestLoggingServer) ReportPageUnloading(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportPageUnloadingRequest],
) (*connect.Response[agentreplv1.ReportPageUnloadingResponse], error) {
	return serveUnary(s, ctx, "ReportPageUnloading", req, s.server.ReportPageUnloading)
}

func (s *requestLoggingServer) GetMemoryOverview(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetMemoryOverviewRequest],
) (*connect.Response[agentreplv1.GetMemoryOverviewResponse], error) {
	return serveUnary(s, ctx, "GetMemoryOverview", req, s.server.GetMemoryOverview)
}

func (s *requestLoggingServer) GetWorkspaceMemory(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetWorkspaceMemoryRequest],
) (*connect.Response[agentreplv1.GetWorkspaceMemoryResponse], error) {
	return serveUnary(s, ctx, "GetWorkspaceMemory", req, s.server.GetWorkspaceMemory)
}

func (s *requestLoggingServer) ReportUserInteraction(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportUserInteractionRequest],
) (*connect.Response[agentreplv1.ReportUserInteractionResponse], error) {
	return serveUnary(s, ctx, "ReportUserInteraction", req, s.server.ReportUserInteraction)
}

func (s *requestLoggingServer) ReportRecencyReach(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportRecencyReachRequest],
) (*connect.Response[agentreplv1.ReportRecencyReachResponse], error) {
	return serveUnary(s, ctx, "ReportRecencyReach", req, s.server.ReportRecencyReach)
}

func (s *requestLoggingServer) ReportEditorFocus(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportEditorFocusRequest],
) (*connect.Response[agentreplv1.ReportEditorFocusResponse], error) {
	return serveUnary(s, ctx, "ReportEditorFocus", req, s.server.ReportEditorFocus)
}

func (s *requestLoggingServer) ReportDesktopBannerClicked(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportDesktopBannerClickedRequest],
) (*connect.Response[agentreplv1.ReportDesktopBannerClickedResponse], error) {
	return serveUnary(s, ctx, "ReportDesktopBannerClicked", req, s.server.ReportDesktopBannerClicked)
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

func (s *requestLoggingServer) AcceptHandedWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.AcceptHandedWorkspaceRequest],
) (*connect.Response[agentreplv1.AcceptHandedWorkspaceResponse], error) {
	return serveUnary(s, ctx, "AcceptHandedWorkspace", req, s.server.AcceptHandedWorkspace)
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

func (s *requestLoggingServer) ListSlackChannels(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListSlackChannelsRequest],
) (*connect.Response[agentreplv1.ListSlackChannelsResponse], error) {
	return serveUnary(s, ctx, "ListSlackChannels", req, s.server.ListSlackChannels)
}

func (s *requestLoggingServer) ReportComposerSelection(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportComposerSelectionRequest],
) (*connect.Response[agentreplv1.ReportComposerSelectionResponse], error) {
	return serveUnary(s, ctx, "ReportComposerSelection", req, s.server.ReportComposerSelection)
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

func (s *requestLoggingServer) SetWorkspaceModel(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetWorkspaceModelRequest],
) (*connect.Response[agentreplv1.SetWorkspaceModelResponse], error) {
	return serveUnary(s, ctx, "SetWorkspaceModel", req, s.server.SetWorkspaceModel)
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

func (s *requestLoggingServer) ListMergeLandings(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListMergeLandingsRequest],
) (*connect.Response[agentreplv1.ListMergeLandingsResponse], error) {
	return serveUnary(s, ctx, "ListMergeLandings", req, s.server.ListMergeLandings)
}

func (s *requestLoggingServer) RecordDirectLanding(
	ctx context.Context,
	req *connect.Request[agentreplv1.RecordDirectLandingRequest],
) (*connect.Response[agentreplv1.RecordDirectLandingResponse], error) {
	return serveUnary(s, ctx, "RecordDirectLanding", req, s.server.RecordDirectLanding)
}

func (s *requestLoggingServer) FindWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindWorkspacesRequest],
) (*connect.Response[agentreplv1.FindWorkspacesResponse], error) {
	return serveUnary(s, ctx, "FindWorkspaces", req, s.server.FindWorkspaces)
}

func (s *requestLoggingServer) ListMergedWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListMergedWorkspacesRequest],
) (*connect.Response[agentreplv1.ListMergedWorkspacesResponse], error) {
	return serveUnary(s, ctx, "ListMergedWorkspaces", req, s.server.ListMergedWorkspaces)
}

func (s *requestLoggingServer) LocateWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.LocateWorkspaceRequest],
) (*connect.Response[agentreplv1.LocateWorkspaceResponse], error) {
	return serveUnary(s, ctx, "LocateWorkspace", req, s.server.LocateWorkspace)
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

func (s *requestLoggingServer) GatherWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.GatherWorkspacesRequest],
) (*connect.Response[agentreplv1.GatherWorkspacesResponse], error) {
	return serveUnary(s, ctx, "GatherWorkspaces", req, s.server.GatherWorkspaces)
}

func (s *requestLoggingServer) GetGatherStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetGatherStatusRequest],
) (*connect.Response[agentreplv1.GetGatherStatusResponse], error) {
	return serveUnary(s, ctx, "GetGatherStatus", req, s.server.GetGatherStatus)
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

func (s *requestLoggingServer) RequeueMerge(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequeueMergeRequest],
) (*connect.Response[agentreplv1.RequeueMergeResponse], error) {
	return serveUnary(s, ctx, "RequeueMerge", req, s.server.RequeueMerge)
}

func (s *requestLoggingServer) DiscardMerge(
	ctx context.Context,
	req *connect.Request[agentreplv1.DiscardMergeRequest],
) (*connect.Response[agentreplv1.DiscardMergeResponse], error) {
	return serveUnary(s, ctx, "DiscardMerge", req, s.server.DiscardMerge)
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

func (s *requestLoggingServer) ReapWorktrees(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReapWorktreesRequest],
) (*connect.Response[agentreplv1.ReapWorktreesResponse], error) {
	return serveUnary(s, ctx, "ReapWorktrees", req, s.server.ReapWorktrees)
}

func (s *requestLoggingServer) PruneStorage(
	ctx context.Context,
	req *connect.Request[agentreplv1.PruneStorageRequest],
) (*connect.Response[agentreplv1.PruneStorageResponse], error) {
	return serveUnary(s, ctx, "PruneStorage", req, s.server.PruneStorage)
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

func (s *requestLoggingServer) CreateThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.CreateThreadRequest],
) (*connect.Response[agentreplv1.CreateThreadResponse], error) {
	return serveUnary(s, ctx, "CreateThread", req, s.server.CreateThread)
}

func (s *requestLoggingServer) RequestNewThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequestNewThreadRequest],
) (*connect.Response[agentreplv1.RequestNewThreadResponse], error) {
	return serveUnary(s, ctx, "RequestNewThread", req, s.server.RequestNewThread)
}

func (s *requestLoggingServer) WatchChat(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchChatRequest],
	stream *connect.ServerStream[agentreplv1.WatchChatResponse],
) error {
	return serveStream(s, ctx, "WatchChat", req, stream, s.server.WatchChat)
}

func (s *requestLoggingServer) SwitchChatSibling(
	ctx context.Context,
	req *connect.Request[agentreplv1.SwitchChatSiblingRequest],
) (*connect.Response[agentreplv1.SwitchChatSiblingResponse], error) {
	return serveUnary(s, ctx, "SwitchChatSibling", req, s.server.SwitchChatSibling)
}

func (s *requestLoggingServer) ShowChatNode(
	ctx context.Context,
	req *connect.Request[agentreplv1.ShowChatNodeRequest],
) (*connect.Response[agentreplv1.ShowChatNodeResponse], error) {
	return serveUnary(s, ctx, "ShowChatNode", req, s.server.ShowChatNode)
}

func (s *requestLoggingServer) ReportChatSightings(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportChatSightingsRequest],
) (*connect.Response[agentreplv1.ReportChatSightingsResponse], error) {
	return serveUnary(s, ctx, "ReportChatSightings", req, s.server.ReportChatSightings)
}

func (s *requestLoggingServer) SearchChats(
	ctx context.Context,
	req *connect.Request[agentreplv1.SearchChatsRequest],
) (*connect.Response[agentreplv1.SearchChatsResponse], error) {
	return serveUnary(s, ctx, "SearchChats", req, s.server.SearchChats)
}

func (s *requestLoggingServer) SelectChatText(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectChatTextRequest],
) (*connect.Response[agentreplv1.SelectChatTextResponse], error) {
	return serveUnary(s, ctx, "SelectChatText", req, s.server.SelectChatText)
}

func (s *requestLoggingServer) ShowSidebarApp(
	ctx context.Context,
	req *connect.Request[agentreplv1.ShowSidebarAppRequest],
) (*connect.Response[agentreplv1.ShowSidebarAppResponse], error) {
	return serveUnary(s, ctx, "ShowSidebarApp", req, s.server.ShowSidebarApp)
}

func (s *requestLoggingServer) SubscribePullRequest(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribePullRequestRequest],
) (*connect.Response[agentreplv1.SubscribePullRequestResponse], error) {
	return serveUnary(s, ctx, "SubscribePullRequest", req, s.server.SubscribePullRequest)
}

func (s *requestLoggingServer) UnsubscribePullRequest(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribePullRequestRequest],
) (*connect.Response[agentreplv1.UnsubscribePullRequestResponse], error) {
	return serveUnary(s, ctx, "UnsubscribePullRequest", req, s.server.UnsubscribePullRequest)
}

func (s *requestLoggingServer) ListPullRequestSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListPullRequestSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListPullRequestSubscriptionsResponse], error) {
	return serveUnary(s, ctx, "ListPullRequestSubscriptions", req, s.server.ListPullRequestSubscriptions)
}

func (s *requestLoggingServer) CommentOnPullRequest(
	ctx context.Context,
	req *connect.Request[agentreplv1.CommentOnPullRequestRequest],
) (*connect.Response[agentreplv1.CommentOnPullRequestResponse], error) {
	return serveUnary(s, ctx, "CommentOnPullRequest", req, s.server.CommentOnPullRequest)
}

func (s *requestLoggingServer) SubscribeGithubMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeGithubMergeQueueRequest],
) (*connect.Response[agentreplv1.SubscribeGithubMergeQueueResponse], error) {
	return serveUnary(s, ctx, "SubscribeGithubMergeQueue", req, s.server.SubscribeGithubMergeQueue)
}

func (s *requestLoggingServer) UnsubscribeGithubMergeQueue(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeGithubMergeQueueRequest],
) (*connect.Response[agentreplv1.UnsubscribeGithubMergeQueueResponse], error) {
	return serveUnary(s, ctx, "UnsubscribeGithubMergeQueue", req, s.server.UnsubscribeGithubMergeQueue)
}

func (s *requestLoggingServer) ListGithubMergeQueueSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListGithubMergeQueueSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListGithubMergeQueueSubscriptionsResponse], error) {
	return serveUnary(s, ctx, "ListGithubMergeQueueSubscriptions", req, s.server.ListGithubMergeQueueSubscriptions)
}

func (s *requestLoggingServer) SubscribeGithubWorkflowRun(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeGithubWorkflowRunRequest],
) (*connect.Response[agentreplv1.SubscribeGithubWorkflowRunResponse], error) {
	return serveUnary(s, ctx, "SubscribeGithubWorkflowRun", req, s.server.SubscribeGithubWorkflowRun)
}

func (s *requestLoggingServer) UnsubscribeGithubWorkflowRun(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeGithubWorkflowRunRequest],
) (*connect.Response[agentreplv1.UnsubscribeGithubWorkflowRunResponse], error) {
	return serveUnary(s, ctx, "UnsubscribeGithubWorkflowRun", req, s.server.UnsubscribeGithubWorkflowRun)
}

func (s *requestLoggingServer) ListGithubWorkflowRunSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListGithubWorkflowRunSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListGithubWorkflowRunSubscriptionsResponse], error) {
	return serveUnary(s, ctx, "ListGithubWorkflowRunSubscriptions", req, s.server.ListGithubWorkflowRunSubscriptions)
}

func (s *requestLoggingServer) SubscribeSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubscribeSlackThreadRequest],
) (*connect.Response[agentreplv1.SubscribeSlackThreadResponse], error) {
	return serveUnary(s, ctx, "SubscribeSlackThread", req, s.server.SubscribeSlackThread)
}

func (s *requestLoggingServer) UnsubscribeSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.UnsubscribeSlackThreadRequest],
) (*connect.Response[agentreplv1.UnsubscribeSlackThreadResponse], error) {
	return serveUnary(s, ctx, "UnsubscribeSlackThread", req, s.server.UnsubscribeSlackThread)
}

func (s *requestLoggingServer) SetSlackThreadMode(
	ctx context.Context,
	req *connect.Request[agentreplv1.SetSlackThreadModeRequest],
) (*connect.Response[agentreplv1.SetSlackThreadModeResponse], error) {
	return serveUnary(s, ctx, "SetSlackThreadMode", req, s.server.SetSlackThreadMode)
}

func (s *requestLoggingServer) ListSlackThreadSubscriptions(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListSlackThreadSubscriptionsRequest],
) (*connect.Response[agentreplv1.ListSlackThreadSubscriptionsResponse], error) {
	return serveUnary(s, ctx, "ListSlackThreadSubscriptions", req, s.server.ListSlackThreadSubscriptions)
}

func (s *requestLoggingServer) ReadSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReadSlackThreadRequest],
) (*connect.Response[agentreplv1.ReadSlackThreadResponse], error) {
	return serveUnary(s, ctx, "ReadSlackThread", req, s.server.ReadSlackThread)
}

func (s *requestLoggingServer) PostToSlackThread(
	ctx context.Context,
	req *connect.Request[agentreplv1.PostToSlackThreadRequest],
) (*connect.Response[agentreplv1.PostToSlackThreadResponse], error) {
	return serveUnary(s, ctx, "PostToSlackThread", req, s.server.PostToSlackThread)
}

func (s *requestLoggingServer) GetSlackStatus(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetSlackStatusRequest],
) (*connect.Response[agentreplv1.GetSlackStatusResponse], error) {
	return serveUnary(s, ctx, "GetSlackStatus", req, s.server.GetSlackStatus)
}

func (s *requestLoggingServer) UpdateSlackReactionSettings(
	ctx context.Context,
	req *connect.Request[agentreplv1.UpdateSlackReactionSettingsRequest],
) (*connect.Response[agentreplv1.UpdateSlackReactionSettingsResponse], error) {
	return serveUnary(s, ctx, "UpdateSlackReactionSettings", req, s.server.UpdateSlackReactionSettings)
}

func (s *requestLoggingServer) NotifyWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.NotifyWorkspaceRequest],
) (*connect.Response[agentreplv1.NotifyWorkspaceResponse], error) {
	return serveUnary(s, ctx, "NotifyWorkspace", req, s.server.NotifyWorkspace)
}
