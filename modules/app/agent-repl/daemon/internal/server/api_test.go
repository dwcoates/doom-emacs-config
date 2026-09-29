package server

import (
	"context"
	"net/http"
	"strings"
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

// TestNewRefusesMissingLoudFaults pins that the standing loud faults are
// required: an Emacs stream with nothing to subscribe to would never be told
// a failed deploy.
func TestNewRefusesMissingLoudFaults(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := Deps{
		DB: h.DB, Prompts: h.Prompts, Queue: h.Queue, Verbs: h.Verbs, Merge: h.Merge,
		Drain: h.Drain, Rollout: h.Rollout, Deploy: h.Deployer, Health: h.Health, Login: h.Login,
		Ownership: h.Ownership, SuccessorAddress: func() string { return "" },
		Feed: h.Feed, Footer: h.Footer, Topbar: h.Topbar, Sidebar: h.Sidebar,
		Holds: h.Holds, WebappDist: h.WebappDist, ImageOrigin: http.NotFoundHandler(), Log: h.Surfaces,
	}

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the standing loud faults") {
		t.Fatalf("New = %v, want the refusal naming the standing loud faults", err)
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
		Holds: h.Holds, LoudFaults: &h.LoudFaults, Focus: h.Focus, Log: h.Surfaces,
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

// TestPushTransferredReachesTheWebStreamWithTheAddress pins that the web arm
// carries the successor's address: a webview has no daemon-level stream to have
// learned it from.
func TestPushTransferredReachesTheWebStreamWithTheAddress(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchWebWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "webapp-test"}))
	if err != nil {
		t.Fatalf("open the web stream: %v", err)
	}

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")
	push := receiveWebEvent(t, stream)

	// Assert.
	if got := push.GetTransferred().GetAddress(); got != "127.0.0.1:4242" {
		t.Fatalf("address = %q, want the successor's", got)
	}
}

// TestTransportClosedRecordsTheInfoShape pins the record a refused stream open
// makes: INFO, operation daemon.refusal.transport_closed, structured rpc and
// cause.
func TestTransportClosedRecordsTheInfoShape(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}

	// Act.
	TransportClosed(log, "WatchFooter", "unknown_workspace", "no workspace \"w1\" is registered", true)

	// Assert.
	info := log.at("INFO")
	if len(info) != 1 {
		t.Fatalf("INFO records = %d, want exactly one", len(info))
	}
	if info[0].Operation != "daemon.refusal.transport_closed" {
		t.Fatalf("operation = %q, want daemon.refusal.transport_closed", info[0].Operation)
	}
	if info[0].Context["rpc"] != "WatchFooter" || info[0].Context["cause"] != "unknown_workspace" {
		t.Fatalf("context = %v, want structured rpc and cause", info[0].Context)
	}
}

// TestServerCloseRecordsTheLifecycleEdgeAtInfo pins that the serving surface's
// shutdown remains visible at the default production threshold.
func TestServerCloseRecordsTheLifecycleEdgeAtInfo(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{global: log}
	})

	// Act.
	if err := h.Server.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert.
	info := log.at("INFO")
	if len(info) != 1 {
		t.Fatalf("INFO records = %v, want exactly one", info)
	}
	if info[0].Operation != "daemon.server.close" {
		t.Fatalf("operation = %q, want daemon.server.close", info[0].Operation)
	}
}

// TestTransportClosedEmitsNoWarning pins the ruling's negative half: a
// by-design transport-closed refusal is never warned as an unlanded arm.
func TestTransportClosedEmitsNoWarning(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}

	// Act.
	TransportClosed(log, "WatchFeed", "unknown_token", "the token was never minted", true)

	// Assert.
	if warnings := log.at("WARN"); len(warnings) != 0 {
		t.Fatalf("WARN records = %v, want none", warnings)
	}
}

// TestTransportClosedDropsTheIntendedArmSpelling pins that the client-facing
// message names the cause WITHOUT the unlanded-arm spelling.
func TestTransportClosedDropsTheIntendedArmSpelling(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}

	// Act.
	cerr := TransportClosed(log, "WatchTopbar", "not_yet_adopted", "not adopted yet", false)

	// Assert.
	if got := cerr.Message(); got != "WatchTopbar closed the stream: not_yet_adopted: not adopted yet" {
		t.Fatalf("message = %q, want the cause named without \"intended arm:\"", got)
	}
}

// TestTransportClosedUsesNotFoundForAnUnknownID pins the code split.
func TestTransportClosedUsesNotFoundForAnUnknownID(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}

	// Act.
	cerr := TransportClosed(log, "WatchFeed", "unknown_token", "no such token", true)

	// Assert.
	if cerr.Code() != connect.CodeNotFound {
		t.Fatalf("code = %v, want CodeNotFound", cerr.Code())
	}
}
