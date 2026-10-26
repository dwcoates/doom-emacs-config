package server

import (
	"context"
	"fmt"
	"strings"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspacefind"
	"agent-repld/internal/wsm"
)

// FindWorkspaces answers the registry's agent workspaces, open or closed,
// narrowed as the request asks (internal/workspacefind), each with its name
// and its stored summary.
func (s *server) FindWorkspaces(
	ctx context.Context,
	req *connect.Request[agentreplv1.FindWorkspacesRequest],
) (*connect.Response[agentreplv1.FindWorkspacesResponse], error) {
	const rpc = "FindWorkspaces"
	if err := validateFindWorkspacesRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.FindWorkspacesResponse{}
	repositories, err := s.deps.DB.ListRepositories(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the repositories: %w", err))
	}
	query := findQueryOf(req.Msg)
	if ref := req.Msg.Repository; ref != nil {
		repo, ok := workspacefind.ResolveRepository(repositories, wsm.RepoID(ref.GetId()), ref.GetDir())
		if !ok {
			return answer(resp, s.refuse(s.log, rpc, resp, s.fill(refusal{
				Arm: "unknown_repository", Reason: fmt.Sprintf("no registered repository is named by id %q dir %q", ref.GetId(), ref.GetDir()), Info: true,
			})))
		}
		query.Repository = &repo.ID
	}
	records, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the workspaces: %w", err))
	}
	summaries, err := s.deps.DB.ListWorkspaceSummaries(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the workspace summaries: %w", err))
	}
	result, err := workspacefind.Find(records, repositories, summaries, query)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	found := make([]*agentreplv1.FoundWorkspace, 0, len(result.Found))
	for _, f := range result.Found {
		found = append(found, foundWorkspaceOf(f))
	}
	success := &agentreplv1.FindWorkspacesSuccess{}
	if result.Omitted > 0 {
		success.Extent = &agentreplv1.FindWorkspacesSuccess_Partial{Partial: &agentreplv1.FindWorkspacesSuccessPartial{
			Workspaces: found, Omitted: uint32(result.Omitted)}}
	} else {
		success.Extent = &agentreplv1.FindWorkspacesSuccess_Complete{Complete: &agentreplv1.FindWorkspacesSuccessComplete{Workspaces: found}}
	}
	s.log.Debug("daemon.server.find_workspaces", "answered a workspace find", dlog.Context{
		"found": len(found), "omitted": result.Omitted, "repository": req.Msg.GetRepository().GetId() + req.Msg.GetRepository().GetDir(),
		"text": req.Msg.GetText(), "standing": fmt.Sprint(query.Standing)})
	resp.Result = &agentreplv1.FindWorkspacesResponse_Success{Success: success}
	return connect.NewResponse(resp), nil
}

// validateFindWorkspacesRequest is FindWorkspacesRequest's base function.
func validateFindWorkspacesRequest(req *agentreplv1.FindWorkspacesRequest) *connect.Error {
	if req.Repository != nil {
		if err := validateRepositoryRef("repository", req.GetRepository()); err != nil {
			return err
		}
	}
	if w := req.Window; w != nil && w.SinceMs != nil && w.UntilMs != nil && w.GetUntilMs() < w.GetSinceMs() {
		return invalid("window", "a window's end precedes its start")
	}
	if req.Standing != nil && req.GetStanding().GetStanding() == nil {
		return invalid("standing", "a standing names its arm")
	}
	if req.Text != nil && strings.TrimSpace(req.GetText()) == "" {
		return invalid("text", "a text filter is not blank")
	}
	if req.Limit != nil && req.GetLimit() == 0 {
		return invalid("limit", "a limit is at least 1")
	}
	return nil
}

// findQueryOf reads a validated request's narrowing, all but its repository,
// which the handler resolves against the registry.
func findQueryOf(req *agentreplv1.FindWorkspacesRequest) workspacefind.Query {
	var q workspacefind.Query
	if w := req.Window; w != nil {
		if w.SinceMs != nil {
			since := time.UnixMilli(w.GetSinceMs())
			q.Since = &since
		}
		if w.UntilMs != nil {
			until := time.UnixMilli(w.GetUntilMs())
			q.Until = &until
		}
	}
	if req.Standing != nil {
		switch req.GetStanding().GetStanding().(type) {
		case *agentreplv1.FindWorkspacesStanding_Open:
			q.Standing = workspacefind.OpenStanding
		case *agentreplv1.FindWorkspacesStanding_Closed:
			q.Standing = workspacefind.ClosedStanding
		case *agentreplv1.FindWorkspacesStanding_Landed:
			q.Standing = workspacefind.LandedStanding
		default:
			panic(fmt.Sprintf("server: find standing arm %T has no query standing", req.GetStanding().GetStanding()))
		}
	}
	if req.Text != nil {
		text := req.GetText()
		q.Text = &text
	}
	if req.Limit != nil {
		limit := int(req.GetLimit())
		q.Limit = &limit
	}
	return q
}

// foundWorkspaceOf renders one found workspace.
func foundWorkspaceOf(f workspacefind.Found) *agentreplv1.FoundWorkspace {
	ws := f.Workspace
	standing := &agentreplv1.FoundWorkspaceStanding{}
	switch {
	case ws.MergedAt != nil:
		standing.Standing = &agentreplv1.FoundWorkspaceStanding_Landed{Landed: &agentreplv1.FoundWorkspaceStandingLanded{LandedAtMs: ws.MergedAt.UnixMilli()}}
	case ws.Closed:
		closed := &agentreplv1.FoundWorkspaceStandingClosed{}
		if ws.Closure != nil {
			at := ws.Closure.At.UnixMilli()
			closed.ClosedAtMs = &at
		}
		standing.Standing = &agentreplv1.FoundWorkspaceStanding_Closed{Closed: closed}
	default:
		standing.Standing = &agentreplv1.FoundWorkspaceStanding_Open{Open: &agentreplv1.FoundWorkspaceStandingOpen{}}
	}
	return &agentreplv1.FoundWorkspace{
		Workspace:      ws.Ref(),
		Name:           ws.Name,
		Branch:         ws.Branch,
		Repository:     &workspacev1.RepositoryRef{Id: string(f.Repository.ID), Dir: f.Repository.Dir},
		Summary:        f.Summary,
		CreatedAtMs:    ws.CreatedAt.UnixMilli(),
		LastActiveAtMs: f.LastActiveAt.UnixMilli(),
		Standing:       standing,
	}
}
