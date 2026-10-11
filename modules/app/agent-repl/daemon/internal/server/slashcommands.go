package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// SlashCommandCatalogs answers a workspace's standing slash-command catalog,
// reporting false when no session of the workspace has stated one.
// Implemented by commandcatalog.Catalogs, which the session watchers feed.
type SlashCommandCatalogs interface {
	Catalog(ws ids.WorkspaceID) (*conversationv1.SessionCommandCatalog, bool)
}

// ListSlashCommands answers the workspace's standing slash-command catalog
// for the composer's completion. An unstated catalog is an ANSWER, never an
// error: a workspace whose session has not read its runtime yet is ordinary.
func (s *server) ListSlashCommands(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListSlashCommandsRequest],
) (*connect.Response[agentreplv1.ListSlashCommandsResponse], error) {
	const rpc = "ListSlashCommands"
	resp := &agentreplv1.ListSlashCommandsResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	catalog, stated := s.deps.SlashCommands.Catalog(subject.Record.ID)
	if !stated {
		subject.Log.Debug("daemon.server.list_slash_commands", "answered that no catalog has been stated", dlog.Context{
			"branch": "unstated",
		})
		resp.Result = &agentreplv1.ListSlashCommandsResponse_Success{Success: &agentreplv1.ListSlashCommandsSuccess{
			Catalog: &agentreplv1.ListSlashCommandsSuccess_Unstated{Unstated: &agentreplv1.ListSlashCommandsUnstated{}},
		}}
		return connect.NewResponse(resp), nil
	}
	subject.Log.Debug("daemon.server.list_slash_commands", "answered the standing catalog", dlog.Context{
		"branch":   "stated",
		"commands": len(catalog.GetCommands()),
	})
	resp.Result = &agentreplv1.ListSlashCommandsResponse_Success{Success: &agentreplv1.ListSlashCommandsSuccess{
		Catalog: &agentreplv1.ListSlashCommandsSuccess_Stated{Stated: &agentreplv1.ListSlashCommandsStated{Catalog: catalog}},
	}}
	return connect.NewResponse(resp), nil
}
