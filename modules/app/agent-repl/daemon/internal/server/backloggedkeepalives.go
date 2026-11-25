package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// BackloggedKeepalives is the server's view of the backlogged workspaces' cache
// keeper (internal/cachelapse): one keep-alive to every backlogged workspace.
type BackloggedKeepalives interface {
	// SendBackloggedKeepalives asks every backlogged workspace's shim for one
	// keep-alive and answers the response whole.
	SendBackloggedKeepalives(ctx context.Context) *agentreplv1.SendBackloggedKeepalivesResponse
}

// SendBackloggedKeepalives sends one keep-alive to every backlogged workspace's
// session (`SPC j R`). The keeper owns the fan-out and records each outcome;
// this only delegates. The request is empty by design, so there is nothing to
// validate.
func (s *server) SendBackloggedKeepalives(
	ctx context.Context,
	_ *connect.Request[agentreplv1.SendBackloggedKeepalivesRequest],
) (*connect.Response[agentreplv1.SendBackloggedKeepalivesResponse], error) {
	return connect.NewResponse(s.deps.BackloggedKeepalives.SendBackloggedKeepalives(ctx)), nil
}
