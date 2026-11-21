package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// KeepLives is the server's view of the cache keeper's keep-live
// (internal/cachelapse.Keeper.KeepLive): the keeper's one keep-alive
// subroutine, run on one workspace now.
type KeepLives interface {
	// KeepLive runs the subroutine on WS and answers how it ended. An error
	// is the call's context ending before the run did.
	KeepLive(ctx context.Context, ws ids.WorkspaceID) (*agentreplv1.KeepLiveWorkspaceResponse, error)
}

// KeepLiveWorkspace warms one workspace's cache by the keeper's own
// keep-alive (`agent-repld workspace keep-live WS`). The registry resolves
// the workspace here; the keeper answers everything else, and records every
// step under daemon.cachelapse.drive.
func (s *server) KeepLiveWorkspace(
	ctx context.Context,
	req *connect.Request[agentreplv1.KeepLiveWorkspaceRequest],
) (*connect.Response[agentreplv1.KeepLiveWorkspaceResponse], error) {
	const rpc = "KeepLiveWorkspace"
	resp := &agentreplv1.KeepLiveWorkspaceResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	answered, err := s.deps.KeepLives.KeepLive(ctx, subject.Record.ID)
	if err != nil {
		// THE CALLER LEFT, NOT A FAULT: the run goes on under its own bounds
		// and records how it ends.
		subject.Log.Info("daemon.server.keep_live_workspace", "the keep-live call ended before its run did", dlog.Context{"cause": err.Error()})
		return nil, connect.NewError(connect.CodeCanceled, err)
	}
	return connect.NewResponse(answered), nil
}
