package server

import (
	"context"
	"fmt"
	"path/filepath"
	"strings"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/workspacefind"
)

// LocateWorkspace answers which workspace a name, a directory, or a directory
// inside a worktree names, off the registry, so a hidden merge workspace and
// an open workspace the sidebar does not draw are found
// (workspacefind.Locate).
func (s *server) LocateWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.LocateWorkspaceRequest],
) (*connect.Response[agentreplv1.LocateWorkspaceResponse], error) {
	const rpc = "LocateWorkspace"
	by, err := locateByOf(req.Msg)
	if err != nil {
		return nil, err
	}
	records, lerr := s.deps.DB.ListWorkspaces(ctx)
	if lerr != nil {
		return nil, fail(s.log, rpc, fmt.Errorf("list the workspaces: %w", lerr))
	}
	roster, ok := s.deps.Sidebar.Topic().Latest()
	if !ok {
		return nil, fail(s.log, rpc, fmt.Errorf("read the roster: the daemon has published no roster yet"))
	}
	located := workspacefind.Locate(records, roster, by)
	refs := make([]*workspacev1.WorkspaceRef, 0, len(located))
	ids := make([]string, 0, len(located))
	for _, ws := range located {
		refs = append(refs, ws.Ref())
		ids = append(ids, string(ws.ID))
	}
	s.log.Debug("daemon.server.locate_workspace", "located the workspaces a name or directory names", dlog.Context{
		"name": by.Name, "at": by.At, "containing": by.Containing, "located": ids, "roster_revision": roster.GetRevision()})
	return connect.NewResponse(&agentreplv1.LocateWorkspaceResponse{Result: &agentreplv1.LocateWorkspaceResponse_Success{
		Success: &agentreplv1.LocateWorkspaceSuccess{Workspaces: refs}}}), nil
}

// locateByOf validates a LocateWorkspaceRequest and answers what it matches on.
func locateByOf(req *agentreplv1.LocateWorkspaceRequest) (workspacefind.LocateBy, *connect.Error) {
	switch by := req.GetBy().(type) {
	case *agentreplv1.LocateWorkspaceRequest_Named:
		if strings.TrimSpace(by.Named.GetName()) == "" {
			return workspacefind.LocateBy{}, invalid("named.name", "a name is not blank")
		}
		return workspacefind.LocateBy{Name: by.Named.GetName()}, nil
	case *agentreplv1.LocateWorkspaceRequest_At:
		if !filepath.IsAbs(by.At.GetDir()) {
			return workspacefind.LocateBy{}, invalid("at.dir", "a directory is absolute")
		}
		return workspacefind.LocateBy{At: by.At.GetDir()}, nil
	case *agentreplv1.LocateWorkspaceRequest_Containing:
		if !filepath.IsAbs(by.Containing.GetDir()) {
			return workspacefind.LocateBy{}, invalid("containing.dir", "a directory is absolute")
		}
		return workspacefind.LocateBy{Containing: by.Containing.GetDir()}, nil
	}
	return workspacefind.LocateBy{}, invalid("by", "a locate names a name, a directory or a containing directory")
}
