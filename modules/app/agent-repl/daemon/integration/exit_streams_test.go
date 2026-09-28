//go:build integration

package integration

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// awaitStreamEnd reads a stream to its end and answers its last push and how
// it ended: nil for the end frame, the transport's error for a cut.
func awaitStreamEnd[T any](t *testing.T, d *harness.Daemon, s *harness.Stream[T], what string) (T, error) {
	t.Helper()
	ctx, cancel := d.WaitCtx()
	defer cancel()
	var last T
	for {
		select {
		case v, ok := <-s.C:
			if !ok {
				return last, s.Err()
			}
			last = v
		case <-ctx.Done():
			t.Fatalf("the %s stream was still open after the daemon exited: %v", what, ctx.Err())
			return last, nil
		}
	}
}

// streamEnd is how one stream ended: whether its last push was the planned
// `DaemonStreamEnding`, and the end's error.
type streamEnd struct {
	ending bool
	err    error
}

// TestEveryStandingStreamEndsWithItsEndFrameWhenTheDaemonExits pins the
// 2026-09-27 regression end to end, against a real daemon process: the exit
// left its standing streams to `http.Server.Shutdown`, which waits out its
// grace on an HTTP/1.1 stream that never goes idle and then CLOSES it -- and
// closes nothing at all on a hijacked h2c connection, leaving it to the
// process exit. Every client still watching read "producer closed without an
// end frame": Emacs, which speaks HTTP/1.1, on a workspace no transfer notice
// had reached and on its roster. Each stream now ends with its end frame
// before the process goes, on both transports -- and the three streams whose
// contract carries `DaemonStreamEnding` (host, roster, daemon) send it as
// their last push first, so a client reads the end as planned.
func TestEveryStandingStreamEndsWithItsEndFrameWhenTheDaemonExits(t *testing.T) {
	t.Parallel()
	transports := []struct {
		name string
		dial func(d *harness.Daemon) agentreplv1connect.AgentReplClient
	}{
		{name: "h2c", dial: func(d *harness.Daemon) agentreplv1connect.AgentReplClient { return d.Client() }},
		{name: "HTTP/1.1, as Emacs speaks", dial: func(d *harness.Daemon) agentreplv1connect.AgentReplClient { return d.DialHTTP1() }},
	}
	streams := []struct {
		name string
		open func(f *fixture, client agentreplv1connect.AgentReplClient) func() streamEnd
		// wantEnding is whether the stream's contract carries the planned
		// ending arm, which a planned exit must send as its last frame.
		wantEnding bool
	}{
		{
			name: "WatchHostWorkspace",
			open: func(f *fixture, client agentreplv1connect.AgentReplClient) func() streamEnd {
				s := f.d.WatchHostOn(client, f.ws)
				s.AwaitHeaders(f.t, f.d.Ctx(), "the host stream")
				return func() streamEnd {
					last, err := awaitStreamEnd(f.t, f.d, s, "host")
					return streamEnd{ending: last.GetEnding() != nil, err: err}
				}
			},
			wantEnding: true,
		},
		{
			name: "WatchWorkspaceRoster",
			open: func(f *fixture, client agentreplv1connect.AgentReplClient) func() streamEnd {
				s := f.d.WatchRosterFramesOn(client)
				s.AwaitHeaders(f.t, f.d.Ctx(), "the roster stream")
				return func() streamEnd {
					last, err := awaitStreamEnd(f.t, f.d, s, "roster")
					return streamEnd{ending: last.GetEnding() != nil, err: err}
				}
			},
			wantEnding: true,
		},
		{
			name: "WatchDaemon",
			open: func(f *fixture, client agentreplv1connect.AgentReplClient) func() streamEnd {
				s := f.d.WatchDaemonStreamOn(client)
				s.AwaitHeaders(f.t, f.d.Ctx(), "the daemon stream")
				return func() streamEnd {
					last, err := awaitStreamEnd(f.t, f.d, s, "daemon")
					return streamEnd{ending: last.GetEnding() != nil, err: err}
				}
			},
			wantEnding: true,
		},
		{
			name: "WatchWebWorkspace",
			open: func(f *fixture, client agentreplv1connect.AgentReplClient) func() streamEnd {
				s := f.d.WatchWebOn(client, f.ws)
				s.AwaitHeaders(f.t, f.d.Ctx(), "the web stream")
				return func() streamEnd {
					_, err := awaitStreamEnd(f.t, f.d, s, "web")
					return streamEnd{err: err}
				}
			},
		},
	}
	for _, transport := range transports {
		for _, stream := range streams {
			t.Run(stream.name+" over "+transport.name, func(t *testing.T) {
				t.Parallel()
				// Arrange
				f := newRegistered(t, harness.Opts{})
				ended := stream.open(f, transport.dial(f.d))

				// Act
				if _, err := f.d.Client().UpdateShutdownSchedule(f.d.Ctx(), connect.NewRequest(&agentreplv1.UpdateShutdownScheduleRequest{
					Action: &agentreplv1.UpdateShutdownScheduleRequest_Now{Now: &agentreplv1.UpdateShutdownScheduleNow{
						Reason: drainReasonOperator("the exit under test"),
					}},
				})); err != nil {
					t.Fatalf("UpdateShutdownSchedule{now} = %v, want the immediate shutdown accepted", err)
				}
				if code := f.d.AwaitExit(); code != 0 {
					t.Fatalf("the daemon's exit code = %d, want an orderly 0", code)
				}

				// Assert
				end := ended()
				if end.err != nil {
					t.Fatalf("the %s stream ended with %v, want its end frame", stream.name, end.err)
				}
				if end.ending != stream.wantEnding {
					t.Fatalf("the %s stream's last push was the planned ending = %v, want %v",
						stream.name, end.ending, stream.wantEnding)
				}
			})
		}
	}
}
