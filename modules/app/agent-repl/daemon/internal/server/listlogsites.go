package server

import (
	"context"
	"errors"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"connectrpc.com/connect"
)

// errListLogSitesUnbuilt is ListLogSites's answer until its tally is built
// (docs/protobuf-design/log-site-keys.md "ListLogSites"): the rpc is in the
// contract, and no daemon store keeps the sites that fired yet.
var errListLogSitesUnbuilt = errors.New("ListLogSites is not built yet: no store keeps the log sites that fired")

// ListLogSites answers CodeUnimplemented, loudly, until the per-deploy tally
// of fired sites exists; it is never a silent empty answer, which would read
// as a window in which nothing fired.
func (s *requestLoggingServer) ListLogSites(
	ctx context.Context,
	req *connect.Request[agentreplv1.ListLogSitesRequest],
) (*connect.Response[agentreplv1.ListLogSitesResponse], error) {
	return nil, connect.NewError(connect.CodeUnimplemented, errListLogSitesUnbuilt)
}
