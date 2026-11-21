package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const opComposerDraft = "daemon.server.report_composer_draft"

// ReportComposerDraft keeps, or clears, the text a workspace's composer held
// when the editor closed the workspace (endpoint_report_composer_draft.proto),
// and republishes the workspace's host view, whose `composer_draft` carries it
// to the editor's next open.
//
// A STORE THAT REFUSES THE WRITE keeps the draft it kept before: the refusal
// is recorded at ERROR and answered `store_failed`, so the editor surfaces it
// rather than believing the text kept.
func (s *server) ReportComposerDraft(
	ctx context.Context,
	req *connect.Request[agentreplv1.ReportComposerDraftRequest],
) (*connect.Response[agentreplv1.ReportComposerDraftResponse], error) {
	const rpc = "ReportComposerDraft"
	if err := validateReportComposerDraftRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.ReportComposerDraftResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	var err error
	fields := dlog.Context{}
	switch draft := req.Msg.GetDraft().(type) {
	case *agentreplv1.ReportComposerDraftRequest_Kept:
		fields["draft"], fields["chars"] = "kept", len(draft.Kept.GetText())
		err = s.deps.DB.PutComposerDraft(ctx, ws, draft.Kept.GetText())
	case *agentreplv1.ReportComposerDraftRequest_Cleared:
		fields["draft"] = "cleared"
		err = s.deps.DB.ClearComposerDraft(ctx, ws)
	}
	if err != nil {
		subject.Log.Error(opComposerDraft, "could not write the composer draft; the one kept before stands", dlog.WithCause(fields, err))
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(refusal{Arm: "store_failed", Reason: err.Error(),
			Fields: map[string]any{"reason": err.Error()}})))
	}
	subject.Log.Info(opComposerDraft, "the editor reported the composer draft", fields)
	s.PublishHostWorkspace(ctx, ws)
	resp.Result = &agentreplv1.ReportComposerDraftResponse_Success{Success: &agentreplv1.ReportComposerDraftSuccess{}}
	return connect.NewResponse(resp), nil
}

// validateReportComposerDraftRequest refuses a request naming no workspace,
// stating no draft arm, or keeping an empty draft, as InvalidArgument.
func validateReportComposerDraftRequest(req *agentreplv1.ReportComposerDraftRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch draft := req.GetDraft().(type) {
	case *agentreplv1.ReportComposerDraftRequest_Kept:
		if draft.Kept.GetText() == "" {
			return invalid("draft.kept.text", "a kept draft is never empty; an empty composer is reported cleared")
		}
	case *agentreplv1.ReportComposerDraftRequest_Cleared:
	default:
		return invalid("draft", "whether a draft is kept or cleared is required")
	}
	return nil
}

// hostComposerDraft answers the composer draft the daemon keeps for WS, for
// the host view, and false when the store could not be read: a view is never
// published without it, since an editor told no draft would lose the one kept.
func (s *server) hostComposerDraft(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) (*agentreplv1.HostComposerDraft, bool) {
	text, found, err := s.deps.DB.ComposerDraft(ctx, ws)
	if err != nil {
		logHostStreamRead(log, "daemon.server.compose_host_workspace", "the host view was not composed",
			"could not read the composer draft", err, nil)
		return nil, false
	}
	if !found {
		return nil, true
	}
	return &agentreplv1.HostComposerDraft{Text: text}, true
}
