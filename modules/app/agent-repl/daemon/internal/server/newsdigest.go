package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/publish"
)

// NewsDigest is the server's view of the daily news digest
// (internal/newsdigest): the dismiss, the refresh, and the standing's
// publication.
type NewsDigest interface {
	// Dismiss takes the standing digest down in every webview, or answers
	// unknown_digest; an error is the store failing.
	Dismiss(ctx context.Context, req *agentreplv1.DismissNewsDigestRequest) (*agentreplv1.DismissNewsDigestResponse, error)
	// Refresh makes a digest now; an error is a failure outside the
	// contract's arms.
	Refresh(ctx context.Context) (*agentreplv1.RefreshNewsDigestResponse, error)
	// Topic is the standing, pushed on every webview WatchDaemon stream.
	Topic() *publish.Topic[*agentreplv1.NewsDigestStanding]
}

// DismissNewsDigest takes the standing news digest down in every webview. The
// digest owns the change and its record; this only validates and delegates.
func (s *server) DismissNewsDigest(
	ctx context.Context,
	req *connect.Request[agentreplv1.DismissNewsDigestRequest],
) (*connect.Response[agentreplv1.DismissNewsDigestResponse], error) {
	const rpc = "DismissNewsDigest"
	if err := validateDismissNewsDigestRequest(req.Msg); err != nil {
		return nil, err
	}
	resp, err := s.deps.NewsDigest.Dismiss(ctx, req.Msg)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	return connect.NewResponse(resp), nil
}

// RefreshNewsDigest makes a news digest now instead of at the daily cadence.
func (s *server) RefreshNewsDigest(
	ctx context.Context,
	_ *connect.Request[agentreplv1.RefreshNewsDigestRequest],
) (*connect.Response[agentreplv1.RefreshNewsDigestResponse], error) {
	const rpc = "RefreshNewsDigest"
	resp, err := s.deps.NewsDigest.Refresh(ctx)
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	return connect.NewResponse(resp), nil
}
