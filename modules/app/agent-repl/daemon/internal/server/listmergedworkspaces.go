package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/sidebar"
)

// ListMergedWorkspaces answers one page of merged workspaces' rows, resolved
// on demand by the sidebar from the merged set its last push was resolved
// from, with that set's token (agentrepl.v1.ListMergedWorkspaces). A merged
// workspace is in no roster stream: this is how its row is read.
func (s *server) ListMergedWorkspaces(
	_ context.Context,
	req *connect.Request[agentreplv1.ListMergedWorkspacesRequest],
) (*connect.Response[agentreplv1.ListMergedWorkspacesResponse], error) {
	const rpc = "ListMergedWorkspaces"
	order, cerr := validateListMergedWorkspacesRequest(req.Msg)
	if cerr != nil {
		return nil, cerr
	}
	ctx := dlog.Context{
		"repository": req.Msg.GetRepository().GetId() + req.Msg.GetRepository().GetDir(),
		"order":      order.String(), "page": req.Msg.GetPage(),
	}
	page, err := s.deps.Sidebar.MergedPage(sidebar.MergedPageQuery{Repository: req.Msg.Repository, Order: order, Page: req.Msg.GetPage()})
	var unknown *sidebar.UnknownRepositoryError
	switch {
	case errors.Is(err, sidebar.ErrMergedNotReady):
		s.log.Info(rpc, "answered not_ready: the roster has published no registry yet", dlog.WithCause(ctx, err))
		return connect.NewResponse(&agentreplv1.ListMergedWorkspacesResponse{Result: &agentreplv1.ListMergedWorkspacesResponse_Error{
			Error: &agentreplv1.ListMergedWorkspacesError{Cause: &agentreplv1.ListMergedWorkspacesError_NotReady{
				NotReady: &agentreplv1.ListMergedWorkspacesNotReady{}}}}}), nil
	case errors.As(err, &unknown):
		s.log.Info(rpc, "answered unknown_repository: the registry holds no repository the request names", dlog.WithCause(ctx, err))
		return connect.NewResponse(&agentreplv1.ListMergedWorkspacesResponse{Result: &agentreplv1.ListMergedWorkspacesResponse_Error{
			Error: &agentreplv1.ListMergedWorkspacesError{Cause: &agentreplv1.ListMergedWorkspacesError_UnknownRepository{
				UnknownRepository: &agentreplv1.ListMergedWorkspacesUnknownRepository{Repository: req.Msg.GetRepository()}}}}}), nil
	case err != nil:
		return nil, fail(s.log.With(ctx), rpc, fmt.Errorf("resolve the merged page: %w", err))
	}
	rows := &frontendv1.RosterMergedRows{Rows: page.Rows}
	success := &agentreplv1.ListMergedWorkspacesSuccess{Revision: &frontendv1.RosterMergedRevision{Revision: page.Revision}}
	if page.More {
		success.Extent = &agentreplv1.ListMergedWorkspacesSuccess_More{More: &agentreplv1.ListMergedWorkspacesSuccessMore{Rows: rows, Omitted: page.Omitted}}
	} else {
		success.Extent = &agentreplv1.ListMergedWorkspacesSuccess_Last{Last: &agentreplv1.ListMergedWorkspacesSuccessLast{Rows: rows}}
	}
	s.log.Debug("daemon.server.list_merged_workspaces", "answered a page of merged workspaces", dlog.Merge(ctx, dlog.Context{
		"rows": len(page.Rows), "more": page.More, "omitted": page.Omitted, "merged_revision": page.Revision}))
	return connect.NewResponse(&agentreplv1.ListMergedWorkspacesResponse{Result: &agentreplv1.ListMergedWorkspacesResponse_Success{Success: success}}), nil
}

// validateListMergedWorkspacesRequest is ListMergedWorkspacesRequest's base
// function: it answers the order the request names.
func validateListMergedWorkspacesRequest(req *agentreplv1.ListMergedWorkspacesRequest) (sidebar.MergedOrder, *connect.Error) {
	if req.Repository != nil {
		if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
			return 0, err
		}
	}
	switch req.GetOrder().GetOrder().(type) {
	case *agentreplv1.ListMergedWorkspacesOrder_MostRecentlyMerged:
		return sidebar.MergedByMerge, nil
	case *agentreplv1.ListMergedWorkspacesOrder_MostRecentlyCreated:
		return sidebar.MergedByCreation, nil
	default:
		return 0, invalid("order", "an order names its arm")
	}
}
