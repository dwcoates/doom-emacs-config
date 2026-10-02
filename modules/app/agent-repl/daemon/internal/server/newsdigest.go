package server

import (
	"context"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
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
	// Redisplay stands the day's dismissed digest again for a full Emacs
	// restart; an error is the store failing, already recorded at ERROR.
	Redisplay(ctx context.Context) error
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

// EditorInstances tells a full Emacs restart apart from a reconnect
// (internal/editorinstance).
type EditorInstances interface {
	// Connected records the instance an Emacs WatchDaemon carried and answers
	// whether it is a new Emacs process; an error is the store failing,
	// already recorded at ERROR.
	Connected(ctx context.Context, instance string) (bool, error)
}

// Startup brings the editor's workspaces up for a new Emacs process and tells
// its stream each step (internal/startup).
type Startup interface {
	// Run brings every open workspace up and hands emit the events in order
	// until the last go-ahead and the finish, or until ctx ends.
	Run(ctx context.Context, emit func(*agentreplv1.DaemonStartupEvent))
}

// editorConnected judges an Emacs WatchDaemon's instance and does what a NEW
// Emacs process is owed: the day's digest stands again. A store that cannot
// record the instance refuses the stream, loudly, rather than serve an Emacs
// whose restart the daemon could not judge; the editor reconnects.
func (s *server) editorConnected(ctx context.Context, emacs *agentreplv1.WatchDaemonEmacs) (bool, error) {
	isNew, err := s.deps.EditorInstances.Connected(ctx, emacs.GetInstance().GetValue())
	if err != nil {
		return false, fail(s.log, "WatchDaemon", err)
	}
	if !isNew {
		return false, nil
	}
	// THE DIGEST'S OWN ERROR RECORD IS THE RECORD: a redisplay that failed is
	// stated at ERROR by the digest, and the Emacs stream serves on.
	if err := s.deps.NewsDigest.Redisplay(ctx); err != nil {
		s.log.Debug("WatchDaemon", "the day's digest was not redisplayed; the digest recorded why", dlog.Context{"cause": err.Error()})
	}
	return true, nil
}
