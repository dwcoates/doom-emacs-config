package main

import (
	"bytes"
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync/atomic"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"
	"golang.org/x/net/http2/h2c"
	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/descriptorpb"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// countingDaemon counts the calls that reach it.
type countingDaemon struct {
	agentreplv1connect.UnimplementedAgentReplHandler
	kills, statuses, footers atomic.Int32
}

func (d *countingDaemon) KillWorkspace(context.Context, *connect.Request[agentreplv1.KillWorkspaceRequest]) (*connect.Response[agentreplv1.KillWorkspaceResponse], error) {
	d.kills.Add(1)
	return connect.NewResponse(&agentreplv1.KillWorkspaceResponse{}), nil
}

func (d *countingDaemon) GetWorkspaceStatus(context.Context, *connect.Request[agentreplv1.GetWorkspaceStatusRequest]) (*connect.Response[agentreplv1.GetWorkspaceStatusResponse], error) {
	d.statuses.Add(1)
	return connect.NewResponse(&agentreplv1.GetWorkspaceStatusResponse{}), nil
}

func (d *countingDaemon) WatchFooter(context.Context, *connect.Request[agentreplv1.WatchFooterRequest], *connect.ServerStream[agentreplv1.WatchFooterResponse]) error {
	d.footers.Add(1)
	return nil
}

// dryRunClient serves a counting daemon and answers a client to it whose
// dry run prints to the answered buffer.
func dryRunClient(t *testing.T) (*countingDaemon, agentreplv1connect.AgentReplClient, *bytes.Buffer) {
	t.Helper()
	daemon := &countingDaemon{}
	path, handler := agentreplv1connect.NewAgentReplHandler(daemon)
	mux := http.NewServeMux()
	mux.Handle(path, handler)
	server := httptest.NewServer(h2c.NewHandler(mux, &http2.Server{}))
	t.Cleanup(server.Close)
	var out bytes.Buffer
	return daemon, newDaemonClient(server.Listener.Addr().String(), connect.WithInterceptors(dryRunInterceptor(&out))), &out
}

func TestADryRunPrintsAChangeInsteadOfSendingIt(t *testing.T) {
	// Arrange
	daemon, client, out := dryRunClient(t)

	// Act
	_, err := client.KillWorkspace(context.Background(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: &workspacev1.WorkspaceRef{Id: "ws-peer"}}))

	// Assert
	if !errors.Is(err, errDryRun) || daemon.kills.Load() != 0 {
		t.Fatalf("err %v, kills %d; want the dry run's answer and nothing sent", err, daemon.kills.Load())
	}
	if !strings.Contains(out.String(), "would call /agentrepl.v1.AgentRepl/KillWorkspace") || !strings.Contains(out.String(), `"ws-peer"`) {
		t.Fatalf("printed %q, want the procedure and the request", out.String())
	}
}

func TestADryRunLetsAReadThrough(t *testing.T) {
	// Arrange
	daemon, client, out := dryRunClient(t)

	// Act
	_, err := client.GetWorkspaceStatus(context.Background(), connect.NewRequest(&agentreplv1.GetWorkspaceStatusRequest{}))

	// Assert
	if err != nil || daemon.statuses.Load() != 1 || out.Len() != 0 {
		t.Fatalf("err %v, statuses %d, printed %q; want the read sent and nothing printed", err, daemon.statuses.Load(), out.String())
	}
}

func TestADryRunStopsAStreamThatIsNoRead(t *testing.T) {
	// Arrange
	daemon, client, out := dryRunClient(t)

	// Act
	stream, err := client.WatchFooter(context.Background(), connect.NewRequest(&agentreplv1.WatchFooterRequest{}))
	if err == nil {
		stream.Receive()
		err = stream.Err()
	}

	// Assert
	if err == nil || daemon.footers.Load() != 0 || !strings.Contains(out.String(), "WatchFooter") {
		t.Fatalf("err %v, footers %d, printed %q; want the stream stopped and printed", err, daemon.footers.Load(), out.String())
	}
}

func TestTheReadsTheCLIReliesOnAreMarkedReads(t *testing.T) {
	// The roster read and the status reads resolve a dry run's arguments, so
	// a dry run that stopped them could not resolve anything. `workspace
	// place` resolves its subsection off the sidebar arrangement.
	service := agentreplv1.File_agentrepl_v1_service_proto.Services().ByName("AgentRepl")
	for _, name := range []protoreflect.Name{"WatchWorkspaceRoster", "GetWorkspaceStatus", "GetMergeQueueStatus", "DaemonHealth", "GetSidebarArrangement"} {
		t.Run(string(name), func(t *testing.T) {
			// Act
			options := service.Methods().ByName(name).Options().(*descriptorpb.MethodOptions)

			// Assert
			if options.GetIdempotencyLevel() != descriptorpb.MethodOptions_NO_SIDE_EFFECTS {
				t.Fatalf("%s is not marked NO_SIDE_EFFECTS", name)
			}
		})
	}
}

func TestDryRunAsksForTheInterceptor(t *testing.T) {
	// Arrange
	f := newCLIFixture()

	// Act
	code := f.run(workspaceNoun, "kill", "peer", "-dry-run")

	// Assert
	if code != exitSuccess || f.dialOptions != 1 {
		t.Fatalf("exit %d, dial options %d; want the dry run's interceptor installed", code, f.dialOptions)
	}
}

func TestAnswerTreatsAStoppedRequestAsSuccess(t *testing.T) {
	// Arrange
	f := newCLIFixture()
	c := &cliCall{out: &f.out, errOut: &f.errOut, name: "workspace kill"}

	// Act
	code := answer[agentreplv1.KillWorkspaceResponse](c, nil, errDryRun)

	// Assert
	if code != exitSuccess || f.errOut.Len() != 0 {
		t.Fatalf("exit %d, stderr %q; want success and no failure", code, f.errOut.String())
	}
}
