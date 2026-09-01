package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
)

// TestNewRefusesAMissingDependency pins that a surface never starts half-wired:
// a handler with a nil collaborator would answer success for work it never did.
func TestNewRefusesAMissingDependency(t *testing.T) {
	// Arrange.
	deps := Deps{}

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil {
		t.Fatal("New accepted a Deps with no state client")
	}
}

// TestNewRefusesAMissingWebappDist pins that the asset origin is required: a
// surface with no dist directory would serve a blank webview.
func TestNewRefusesAMissingWebappDist(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := Deps{
		DB: h.DB, Prompts: h.Prompts, Queue: h.Queue, Verbs: h.Verbs, Merge: h.Merge,
		Drain: h.Drain, Rollout: h.Rollout, Health: h.Health, Login: h.Login,
		Ownership: h.Ownership, SuccessorAddress: func() string { return "" },
		Feed: h.Feed, Footer: h.Footer, Topbar: h.Topbar, Sidebar: h.Sidebar,
		Holds: h.Holds, Log: h.Surfaces,
	}

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil {
		t.Fatal("New accepted a Deps with no webapp dist directory")
	}
}

// TestBinaryCodecIsServed pins that the default binary Connect codec is served.
func TestBinaryCodecIsServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Health.daemon = &agentreplv1.DaemonHealthResponse{
		Result: &agentreplv1.DaemonHealthResponse_Success{
			Success: &agentreplv1.DaemonHealthSuccess{
				Health: &agentreplv1.DaemonHealthSuccess_Healthy{Healthy: &agentreplv1.DaemonHealthy{}},
			},
		},
	}

	// Act.
	resp, err := h.Client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("DaemonHealth over the binary codec: %v", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatal("the binary codec answered no healthy arm")
	}
}

// TestJSONCodecIsServed pins that the JSON codec is served on the same origin.
func TestJSONCodecIsServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Health.daemon = &agentreplv1.DaemonHealthResponse{
		Result: &agentreplv1.DaemonHealthResponse_Success{
			Success: &agentreplv1.DaemonHealthSuccess{
				Health: &agentreplv1.DaemonHealthSuccess_Healthy{Healthy: &agentreplv1.DaemonHealthy{}},
			},
		},
	}
	client := agentreplv1connect.NewAgentReplClient(h.HTTP.Client(), h.HTTP.URL, connect.WithProtoJSON())

	// Act.
	resp, err := client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("DaemonHealth over the JSON codec: %v", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatal("the JSON codec answered no healthy arm")
	}
}

// TestHTTP11IsServed pins the HTTP/1.1 half of the one-listener contract.
func TestHTTP11IsServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Health.daemon = &agentreplv1.DaemonHealthResponse{}

	// Act.
	_, err := h.Client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("DaemonHealth over HTTP/1.1: %v", err)
	}
}

// TestH2CIsServed pins the cleartext HTTP/2 half of the one-listener contract.
func TestH2CIsServed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Health.daemon = &agentreplv1.DaemonHealthResponse{}
	client := agentreplv1connect.NewAgentReplClient(h2cClient(), h.HTTP.URL)

	// Act.
	_, err := client.DaemonHealth(context.Background(),
		connect.NewRequest(&agentreplv1.DaemonHealthRequest{}))

	// Assert.
	if err != nil {
		t.Fatalf("DaemonHealth over h2c: %v", err)
	}
}

// TestConnectRoutesTakePrecedenceOverAssets pins that an rpc path is served by
// the Connect handler rather than by the asset origin beneath it.
func TestConnectRoutesTakePrecedenceOverAssets(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: a GET on the rpc path reaches Connect, which refuses the method.
	resp, err := h.HTTP.Client().Get(h.HTTP.URL + agentreplv1connect.AgentReplDaemonHealthProcedure)
	if err != nil {
		t.Fatalf("get the rpc path: %v", err)
	}
	defer resp.Body.Close()

	// Assert: the asset origin would have answered 404; Connect answers 415
	// or 405 for a bare GET, never a not-found.
	if resp.StatusCode == 404 {
		t.Fatal("the asset origin answered an rpc path; Connect must take precedence")
	}
}

// TestRelayNotifyPushesTheTypedKind pins that a notification's kind name is
// rendered as the typed arm rather than guessed at.
func TestRelayNotifyPushesTheTypedKind(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchHostWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("open the host stream: %v", err)
	}

	// Act.
	h.Server.Relay().Notify(testWorkspaceID, "consent needed", "permission_requested", "Bash")
	if !stream.Receive() {
		t.Fatalf("receive the notification: %v", stream.Err())
	}

	// Assert.
	got := stream.Msg().GetNotification().GetKind().GetPermissionRequested()
	if got == nil || got.GetToolName() != "Bash" {
		t.Fatalf("notification kind = %v, want permission_requested{Bash}", stream.Msg().GetNotification().GetKind())
	}
}

// TestPushTransferredReachesTheWebStreamWithTheAddress pins that the web arm
// carries the successor's address: a webview has no daemon-level stream to have
// learned it from.
func TestPushTransferredReachesTheWebStreamWithTheAddress(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchWebWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("open the web stream: %v", err)
	}

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")
	if !stream.Receive() {
		t.Fatalf("receive the transfer: %v", stream.Err())
	}

	// Assert.
	if got := stream.Msg().GetTransferred().GetAddress(); got != "127.0.0.1:4242" {
		t.Fatalf("address = %q, want the successor's", got)
	}
}
