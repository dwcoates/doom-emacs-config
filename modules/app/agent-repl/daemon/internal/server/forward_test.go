package server

import (
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"net/url"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/h2cclient"
	"agent-repld/internal/workspace"
)

// hostOf answers the host:port of a harness's server.
func hostOf(t *testing.T, h *harness) string {
	t.Helper()
	u, err := url.Parse(h.HTTP.URL)
	if err != nil {
		t.Fatal(err)
	}
	return u.Host
}

func TestACallIsServedByTheDaemonThatServesItsWorkspace(t *testing.T) {
	cases := []struct {
		name      string
		standing  workspace.Standing
		successor bool
		incumbent bool
		forwarded bool
		want      string
	}{
		{name: "a workspace this daemon serves is served here", standing: workspace.StandingOwned, successor: true, want: "here"},
		{name: "a transferred workspace is served by the successor", standing: workspace.StandingTransferringAway, successor: true, want: "there"},
		{name: "a workspace not yet adopted is served by the incumbent", standing: workspace.StandingNotYetAdopted, incumbent: true, want: "there"},
		{name: "a workspace no other daemon serves is served here", standing: workspace.StandingNotYetAdopted, want: "here"},
		{name: "a call already forwarded is never forwarded again", standing: workspace.StandingTransferringAway, successor: true, forwarded: true, want: "here"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the serving daemon, and this one.
			there := newHarness(t)
			there.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: ref(), Name: "there"}
			target := hostOf(t, there)
			here := newHarness(t, func(d *Deps) {
				d.Ownership = &fakeOwnership{standing: tc.standing}
				d.SuccessorAddress = func() string {
					if tc.successor {
						return target
					}
					return ""
				}
				d.IncumbentAddress = func() string {
					if tc.incumbent {
						return target
					}
					return ""
				}
			})
			here.Verbs.api.status = &agentreplv1.WorkspaceStatus{Workspace: ref(), Name: "here"}
			req := connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()})
			if tc.forwarded {
				req.Header().Set(ForwardedHeader, "1")
			}

			// Act
			resp, err := here.Unmarked.GetWorkspaceStatus(context.Background(), req)

			// Assert
			if err != nil {
				t.Fatalf("GetWorkspaceStatus: %v", err)
			}
			if got := resp.Msg.GetSuccess().GetStatus().GetName(); got != tc.want {
				t.Fatalf("served by %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAForwardedStreamRelaysEveryMessageFromTheServingDaemon(t *testing.T) {
	// Arrange: the serving daemon's stream sends two messages and ends; this
	// daemon's forwards it.
	const procedure = "/agentrepl.v1.AgentRepl/WatchHostWorkspace"
	sawForwarded := make(chan bool, 1)
	thereMux := http.NewServeMux()
	thereMux.Handle(procedure, connect.NewServerStreamHandler(procedure,
		func(_ context.Context, req *connect.Request[agentreplv1.WatchHostWorkspaceRequest], stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse]) error {
			sawForwarded <- req.Header().Get(ForwardedHeader) != ""
			for range 2 {
				if err := stream.Send(&agentreplv1.WatchHostWorkspaceResponse{}); err != nil {
					return err
				}
			}
			return nil
		}))
	there := httptest.NewServer(h2c.NewHandler(thereMux, &http2.Server{}))
	defer there.Close()
	target, _ := url.Parse(there.URL)
	s := &server{log: dlog.NewTestLogger(), deps: Deps{Forward: h2cclient.Client()}}
	hereMux := http.NewServeMux()
	hereMux.Handle(procedure, connect.NewServerStreamHandler(procedure,
		func(ctx context.Context, req *connect.Request[agentreplv1.WatchHostWorkspaceRequest], stream *connect.ServerStream[agentreplv1.WatchHostWorkspaceResponse]) error {
			return forwardStream[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](ctx, s, "WatchHostWorkspace", target.Host, req, stream)
		}))
	here := httptest.NewServer(h2c.NewHandler(hereMux, &http2.Server{}))
	defer here.Close()
	client := connect.NewClient[agentreplv1.WatchHostWorkspaceRequest, agentreplv1.WatchHostWorkspaceResponse](h2cclient.Client(), here.URL+procedure)

	// Act
	stream, err := client.CallServerStream(context.Background(), connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("CallServerStream: %v", err)
	}
	defer stream.Close()
	received := 0
	for stream.Receive() {
		received++
	}

	// Assert
	if stream.Err() != nil || received != 2 {
		t.Fatalf("relayed %d messages (%v), want both", received, stream.Err())
	}
	if !<-sawForwarded {
		t.Fatal("the serving daemon saw no forwarded mark on the call")
	}
}

func TestACallWhoseStandingCannotBeReadIsAFailureNotAForward(t *testing.T) {
	// Arrange
	here := newHarness(t, func(d *Deps) {
		d.Ownership = &fakeOwnership{err: errors.New("the rollout is gone")}
	})

	// Act
	_, err := here.Unmarked.GetWorkspaceStatus(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{Workspace: ref()}))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "the rollout is gone") {
		t.Fatalf("GetWorkspaceStatus = %v, want the standing's failure", err)
	}
}
