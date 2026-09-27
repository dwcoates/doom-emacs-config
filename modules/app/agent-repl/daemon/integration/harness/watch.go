package harness

import (
	"context"
	"errors"
	"net/http"
	"os"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
)

// Stream is one open server stream, delivered as a channel of pushes. The
// channel closes when the stream ends; Err reports why.
type Stream[T any] struct {
	// C carries every push in order.
	C <-chan T

	t       *testing.T
	cancel  context.CancelFunc
	errCh   chan error
	headers chan http.Header
}

// AwaitHeaders answers the stream's response headers, which Connect delivers
// as soon as the server FLUSHES them — before any push.
//
// It is the suite's FLUSH-ON-ACCEPT probe. A handler that builds its first
// view before writing anything leaves the client with no response at all until
// something happens to publish; a handler that flushes at accept hands the
// client its headers immediately, so a stream with no view yet is still
// visibly OPEN. Nothing else on the wire can tell those two apart.
func (s *Stream[T]) AwaitHeaders(t *testing.T, ctx context.Context, what string) http.Header {
	t.Helper()
	if s.headers == nil {
		t.Fatalf("stream %s was not opened with header capture", what)
		return nil
	}
	select {
	case h := <-s.headers:
		return h
	case <-ctx.Done():
		t.Fatalf("waiting for %s to flush its response headers: %v", what, ctx.Err())
		return nil
	}
}

// Err answers the stream's terminal error once its channel has closed.
func (s *Stream[T]) Err() error {
	select {
	case err := <-s.errCh:
		return err
	default:
		return nil
	}
}

// Close ends the stream.
func (s *Stream[T]) Close() { s.cancel() }

// Drain reads and DISCARDS every push for the rest of the stream's life.
//
// It exists for the streams a test opens for their SERVER-SIDE EFFECT rather
// than for their pushes — a participant hold is the whole example. Nothing
// reads such a stream, so the pump's buffered channel fills, the pump wedges
// on the send, and the daemon's own writer blocks behind it. Draining keeps
// the stream genuinely open for as long as its context lives.
//
// A drained stream's C must not also be read by the caller: the two would
// race for the same pushes.
func (s *Stream[T]) Drain() {
	go func() {
		for range s.C {
		}
	}()
}

// AwaitView reads pushes until one satisfies the predicate, and answers it.
// The wait is bounded by the daemon's context; nothing sleeps.
func AwaitView[T any](t *testing.T, ctx context.Context, s *Stream[T], what string, pred func(T) bool) T {
	t.Helper()
	var zero T
	for {
		select {
		case v, ok := <-s.C:
			if !ok {
				t.Fatalf("stream ended before %s: %v", what, s.Err())
				return zero
			}
			if pred(v) {
				return v
			}
		case <-ctx.Done():
			t.Fatalf("waiting for %s: %v", what, ctx.Err())
			return zero
		}
	}
}

// AwaitNext answers the next push, whatever it is.
func AwaitNext[T any](t *testing.T, ctx context.Context, s *Stream[T], what string) T {
	t.Helper()
	return AwaitView(t, ctx, s, what, func(T) bool { return true })
}

// ExpectNoPush asserts that nothing arrives before the probe deadline. It is a
// negative assertion, so it necessarily waits out a bound rather than
// synchronizing on an event.
func ExpectNoPush[T any](t *testing.T, s *Stream[T], probe time.Duration, what string) {
	t.Helper()
	timer := time.NewTimer(probe)
	defer timer.Stop()
	select {
	case v, ok := <-s.C:
		if ok {
			t.Fatalf("got a push %v, want none: %s", v, what)
		}
	case <-timer.C:
	}
}

// ProbeWindow is how long a negative assertion waits before concluding that
// nothing is coming.
const ProbeWindow = 500 * time.Millisecond

// runStream pumps a Connect server stream onto a channel, carrying every
// frame the stream delivers.
func runStream[Resp any, Push any](t *testing.T, ctx context.Context, open func(context.Context) (*connect.ServerStreamForClient[Resp], error), pick func(*Resp) Push) *Stream[Push] {
	t.Helper()
	return runStreamSelecting(t, ctx, open, func(r *Resp) (Push, bool) { return pick(r), true })
}

// runStreamSelecting is runStream for a response message that carries MORE
// THAN ONE KIND OF FRAME on one wire, where a picker alone cannot say "this
// frame is not the one this stream is about".
//
// WatchFeed is the case that forced it. Its response carries three independent
// arms -- `row`, `selection` and `feed_text_scale` (endpoint_watch_feed.proto)
// -- and the feed_text_scale frame is pushed on EVERY open feed's watch, root
// and sub-feed alike. A picker of `r.GetRow()` turned each of those into a nil
// row on the row channel, so every negative probe in the suite read a zoom
// frame as "a row was pushed" and reported `got a push <nil>, want none`.
// SELECTING is the fix rather than dropping nils inside the picker: a stream
// says which frames it is about, and a frame it is not about never reaches its
// channel at all.
func runStreamSelecting[Resp any, Push any](t *testing.T, ctx context.Context, open func(context.Context) (*connect.ServerStreamForClient[Resp], error), pick func(*Resp) (Push, bool)) *Stream[Push] {
	t.Helper()
	streamCtx, cancel := context.WithCancel(ctx)
	t.Cleanup(cancel)

	stream, err := open(streamCtx)
	if err != nil {
		cancel()
		t.Fatalf("opening a stream: %v", err)
	}
	ch := make(chan Push, 256)
	errCh := make(chan error, 1)
	headers := make(chan http.Header, 1)
	go func() {
		defer close(ch)
		// Connect's ResponseHeader blocks until the server's headers arrive,
		// and the request is already in flight on its own goroutine, so this
		// costs nothing and never depends on a push.
		headers <- stream.ResponseHeader()
		for stream.Receive() {
			v, ok := pick(stream.Msg())
			if !ok {
				continue
			}
			select {
			case ch <- v:
			case <-streamCtx.Done():
				errCh <- streamCtx.Err()
				return
			}
		}
		errCh <- stream.Err()
	}()
	return &Stream[Push]{C: ch, t: t, cancel: cancel, errCh: errCh, headers: headers}
}

// WatchFooter opens the workspace's footer stream.
func (d *Daemon) WatchFooter(ws *workspacev1.WorkspaceRef) *Stream[*frontendv1.FooterView] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchFooterResponse], error) {
			return d.Client().WatchFooter(ctx, connect.NewRequest(&agentreplv1.WatchFooterRequest{Workspace: ws}))
		},
		func(r *agentreplv1.WatchFooterResponse) *frontendv1.FooterView { return r.GetFooter() })
}

// WatchTopbar opens the workspace's topbar stream.
func (d *Daemon) WatchTopbar(ws *workspacev1.WorkspaceRef) *Stream[*frontendv1.TopbarView] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchTopbarResponse], error) {
			return d.Client().WatchTopbar(ctx, connect.NewRequest(&agentreplv1.WatchTopbarRequest{Workspace: ws}))
		},
		func(r *agentreplv1.WatchTopbarResponse) *frontendv1.TopbarView { return r.GetTopbar() })
}

// WatchRoster opens the editor-global workspace roster stream.
func (d *Daemon) WatchRoster() *Stream[*frontendv1.WorkspaceRoster] {
	d.t.Helper()
	return d.WatchRosterOn(d.Client())
}

// WatchRosterOn opens a roster stream on an explicit client, for the tests
// whose subject is two independent subscribers.
func (d *Daemon) WatchRosterOn(client interface {
	WatchWorkspaceRoster(context.Context, *connect.Request[agentreplv1.WatchWorkspaceRosterRequest]) (*connect.ServerStreamForClient[agentreplv1.WatchWorkspaceRosterResponse], error)
}) *Stream[*frontendv1.WorkspaceRoster] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchWorkspaceRosterResponse], error) {
			return client.WatchWorkspaceRoster(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceRosterRequest{}))
		},
		func(r *agentreplv1.WatchWorkspaceRosterResponse) *frontendv1.WorkspaceRoster { return r.GetRoster() })
}

// WatchHost opens the workspace's host stream (Emacs's view) on the daemon's
// own context, which DefaultTimeout bounds.
func (d *Daemon) WatchHost(ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.WatchHostWorkspaceResponse] {
	d.t.Helper()
	return d.WatchHostFor(d.ctx, ws)
}

// DialHTTP1 builds a client that speaks HTTP/1.1 and the Connect JSON codec,
// the transport Emacs speaks: one connection per exchange, so a standing
// stream is an ordinary (never hijacked) HTTP/1.1 response.
func (d *Daemon) DialHTTP1() agentreplv1connect.AgentReplClient {
	d.t.Helper()
	return agentreplv1connect.NewAgentReplClient(&http.Client{}, "http://"+d.Addr, connect.WithProtoJSON())
}

// WatchHostFor opens the workspace's host stream on a context THE CALLER OWNS.
//
// d.ctx expires at DefaultTimeout, which is right for a wait and wrong for a
// HOLD: the footer's connectivity truth is the pair of participant streams
// (internal/resolve/footer/status.go, `!s.hostStream || !s.webStream`), so a
// host hold that expires mid-test drops the footer to `disconnected` under a
// test that is still running — and a real webapp then closes its composer
// gate and silently swallows every later submission. A hold therefore runs on
// the bound of the thing it is holding FOR, never on the harness's wait bound.
func (d *Daemon) WatchHostFor(
	ctx context.Context,
	ws *workspacev1.WorkspaceRef,
) *Stream[*agentreplv1.WatchHostWorkspaceResponse] {
	d.t.Helper()
	return runStream(d.t, ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse], error) {
			return d.Client().WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ws}))
		},
		func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.WatchHostWorkspaceResponse { return r })
}

// WatchHostOn opens the workspace's host stream on an explicit client, for
// the tests whose subject is the transport a client speaks.
func (d *Daemon) WatchHostOn(client interface {
	WatchHostWorkspace(context.Context, *connect.Request[agentreplv1.WatchHostWorkspaceRequest]) (*connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse], error)
}, ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.WatchHostWorkspaceResponse] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse], error) {
			return client.WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ws}))
		},
		func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.WatchHostWorkspaceResponse { return r })
}

// WatchWeb opens the workspace's webview stream.
func (d *Daemon) WatchWeb(ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.WatchWebWorkspaceResponse] {
	d.t.Helper()
	return d.WatchWebOn(d.Client(), ws)
}

// WatchWebOn opens the workspace's webview stream on an explicit client.
func (d *Daemon) WatchWebOn(client interface {
	WatchWebWorkspace(context.Context, *connect.Request[agentreplv1.WatchWebWorkspaceRequest]) (*connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse], error)
}, ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.WatchWebWorkspaceResponse] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse], error) {
			return client.WatchWebWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ws, WebappBuild: FakeWebappEntry}))
		},
		func(r *agentreplv1.WatchWebWorkspaceResponse) *agentreplv1.WatchWebWorkspaceResponse { return r })
}

// WatchHolds opens the workspace's hold-tray stream.
func (d *Daemon) WatchHolds(ws *workspacev1.WorkspaceRef) *Stream[*frontendv1.DaemonHoldTray] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchDaemonHoldsResponse], error) {
			return d.Client().WatchDaemonHolds(ctx, connect.NewRequest(&agentreplv1.WatchDaemonHoldsRequest{Workspace: ws}))
		},
		func(r *agentreplv1.WatchDaemonHoldsResponse) *frontendv1.DaemonHoldTray { return r.GetTray() })
}

// WatchDaemonStream opens the daemon-wide announcement stream.
func (d *Daemon) WatchDaemonStream() *Stream[*agentreplv1.WatchDaemonResponse] {
	d.t.Helper()
	return d.WatchDaemonStreamOn(d.Client())
}

// WatchDaemonStreamOn opens the daemon stream on an explicit client, so a test
// can hold the several subscribers a drain announcement must reach.
func (d *Daemon) WatchDaemonStreamOn(client interface {
	WatchDaemon(context.Context, *connect.Request[agentreplv1.WatchDaemonRequest]) (*connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse], error)
}) *Stream[*agentreplv1.WatchDaemonResponse] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse], error) {
			return client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: PinnedElispBuild}}}))
		},
		func(r *agentreplv1.WatchDaemonResponse) *agentreplv1.WatchDaemonResponse { return r })
}

// WatchFeed tails a feed from a token minted by OpenFeed.
func (d *Daemon) WatchFeed(token *agentreplv1.FeedWatchToken) *Stream[*frontendv1.FeedRow] {
	d.t.Helper()
	return d.WatchFeedOn(d.Client(), token)
}

// WatchFeedOn tails a feed on an explicit client.
func (d *Daemon) WatchFeedOn(client interface {
	WatchFeed(context.Context, *connect.Request[agentreplv1.WatchFeedRequest]) (*connect.ServerStreamForClient[agentreplv1.WatchFeedResponse], error)
}, token *agentreplv1.FeedWatchToken) *Stream[*frontendv1.FeedRow] {
	d.t.Helper()
	// ROW FRAMES ONLY. The response also carries `selection` and
	// `feed_text_scale` frames, neither of which is a row; see
	// runStreamSelecting.
	return runStreamSelecting(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchFeedResponse], error) {
			return client.WatchFeed(ctx, connect.NewRequest(&agentreplv1.WatchFeedRequest{Watch: token}))
		},
		func(r *agentreplv1.WatchFeedResponse) (*frontendv1.FeedRow, bool) {
			return r.GetRow(), r.GetRow() != nil
		})
}

// WatchLogin opens the login pty's terminal stream.
func (d *Daemon) WatchLogin(ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.LoginTerminalOutput] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.LoginTerminalOutput], error) {
			return d.Client().WatchLoginTerminal(ctx, connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ws}))
		},
		func(r *agentreplv1.LoginTerminalOutput) *agentreplv1.LoginTerminalOutput { return r })
}

// AwaitProcessGone waits for a pid to leave, bounded by the context.
func AwaitProcessGone(t *testing.T, ctx context.Context, pid int) {
	t.Helper()
	ticker := time.NewTicker(pollInterval)
	defer ticker.Stop()
	for {
		if err := syscall.Kill(pid, 0); err != nil {
			if errors.Is(err, syscall.ESRCH) || errors.Is(err, os.ErrProcessDone) {
				return
			}
		}
		select {
		case <-ticker.C:
		case <-ctx.Done():
			t.Fatalf("process %d did not exit: %v", pid, ctx.Err())
		}
	}
}
