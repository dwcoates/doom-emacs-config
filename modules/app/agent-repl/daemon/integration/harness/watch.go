package harness

import (
	"context"
	"errors"
	"os"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
)

// Stream is one open server stream, delivered as a channel of pushes. The
// channel closes when the stream ends; Err reports why.
type Stream[T any] struct {
	// C carries every push in order.
	C <-chan T

	t      *testing.T
	cancel context.CancelFunc
	errCh  chan error
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

// runStream pumps a Connect server stream onto a channel.
func runStream[Resp any, Push any](t *testing.T, ctx context.Context, open func(context.Context) (*connect.ServerStreamForClient[Resp], error), pick func(*Resp) Push) *Stream[Push] {
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
	go func() {
		defer close(ch)
		for stream.Receive() {
			select {
			case ch <- pick(stream.Msg()):
			case <-streamCtx.Done():
				errCh <- streamCtx.Err()
				return
			}
		}
		errCh <- stream.Err()
	}()
	return &Stream[Push]{C: ch, t: t, cancel: cancel, errCh: errCh}
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

// WatchHost opens the workspace's host stream (Emacs's view).
func (d *Daemon) WatchHost(ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.WatchHostWorkspaceResponse] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse], error) {
			return d.Client().WatchHostWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ws}))
		},
		func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.WatchHostWorkspaceResponse { return r })
}

// WatchWeb opens the workspace's webview stream.
func (d *Daemon) WatchWeb(ws *workspacev1.WorkspaceRef) *Stream[*agentreplv1.WatchWebWorkspaceResponse] {
	d.t.Helper()
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse], error) {
			return d.Client().WatchWebWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ws}))
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
			return client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{}))
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
	return runStream(d.t, d.ctx,
		func(ctx context.Context) (*connect.ServerStreamForClient[agentreplv1.WatchFeedResponse], error) {
			return client.WatchFeed(ctx, connect.NewRequest(&agentreplv1.WatchFeedRequest{Watch: token}))
		},
		func(r *agentreplv1.WatchFeedResponse) *frontendv1.FeedRow { return r.GetRow() })
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
