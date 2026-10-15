package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// HiddenKeepalives is the server's view of the hidden workspaces' cache
// keeper (internal/cachelapse): one keep-alive to every hidden workspace.
type HiddenKeepalives interface {
	// SendHiddenKeepalives asks every hidden workspace's shim for one
	// keep-alive and answers the response whole.
	SendHiddenKeepalives(ctx context.Context) *agentreplv1.SendHiddenKeepalivesResponse
}

// SendHiddenKeepalives sends one keep-alive to every hidden workspace's
// session (`SPC j R`). The keeper owns the fan-out and records each outcome;
// this only delegates. The request is empty by design, so there is nothing to
// validate.
func (s *server) SendHiddenKeepalives(
	ctx context.Context,
	_ *connect.Request[agentreplv1.SendHiddenKeepalivesRequest],
) (*connect.Response[agentreplv1.SendHiddenKeepalivesResponse], error) {
	return connect.NewResponse(s.deps.HiddenKeepalives.SendHiddenKeepalives(ctx)), nil
}
