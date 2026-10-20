package server

import (
	"context"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

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

// TestNewRefusesAMissingRetarget pins that the retarget operation is
// required: SetWorkspaceParent has no other way to change a merge target.
func TestNewRefusesAMissingRetarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.Retarget = nil })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the retarget operation") {
		t.Fatalf("New = %v, want the refusal naming the retarget operation", err)
	}
}

// TestNewRefusesMissingLoudFaults pins that the standing loud faults are
// required: an Emacs stream with nothing to subscribe to would never be told
// a failed deploy.
func TestNewRefusesMissingLoudFaults(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	deps := h.withoutDep(func(d *Deps) { d.LoudFaults = nil })

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
	deps := h.withoutDep(func(d *Deps) { d.WebappDist = "" })

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the webapp dist directory") {
		t.Fatalf("New = %v, want the refusal naming the webapp dist directory", err)
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

// TestPushTransferredRecordsWhoHeldTheStreams pins that the transfer notice's
// record names the live holders of both streams, the only evidence of whether
// any participant was there to be told.
func TestPushTransferredRecordsWhoHeldTheStreams(t *testing.T) {
	// Arrange.
	log := &recordingLogger{}
	h := newHarness(t, func(deps *Deps) {
		deps.Log = &fakeSurfaces{global: log}
	})

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")

	// Assert.
	for _, rec := range log.at("INFO") {
		if rec.Operation != "daemon.server.transferred" {
			continue
		}
		if rec.Context["host_stream"] != false || rec.Context["web_stream"] != false {
			t.Fatalf("context = %v, want host_stream and web_stream both false with no stream held", rec.Context)
		}
		return
	}
	t.Fatalf("no daemon.server.transferred record among %v", log.records)
}

// TestRetractTransferredStopsReplayingTheNoticeToALateWebStream pins the
// take-back's first half: a page attaching after the workspace came back is
// not handed the notice that it moved.
func TestRetractTransferredStopsReplayingTheNoticeToALateWebStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")
	h.Server.RetractTransferred(testWorkspaceID)
	stream, err := h.Client.WatchWebWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "webapp-test"}))
	if err != nil {
		t.Fatalf("open the web stream: %v", err)
	}

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:5353")
	push := receiveWebEvent(t, stream)

	// Assert.
	if got := push.GetTransferred().GetAddress(); got != "127.0.0.1:5353" {
		t.Fatalf("first event's address = %q, want the later transfer's and no replay of the retracted one", got)
	}
}

// TestRetractTransferredLetsTheNextIdenticalNoticeReachTheHostStream pins the
// take-back's second half: the next transfer's notice is identical to the
// retracted one, and a host stream that heard the first must hear it too.
func TestRetractTransferredLetsTheNextIdenticalNoticeReachTheHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream := dialHostStream(t, h, ctx)
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")
	if receiveHostEvent(t, stream).GetTransferred() == nil {
		t.Fatal("the first transfer notice did not arrive")
	}
	h.Server.RetractTransferred(testWorkspaceID)

	// Act.
	h.Server.PushTransferred(testWorkspaceID, "127.0.0.1:4242")
	push := receiveHostEvent(t, stream)

	// Assert.
	if push.GetTransferred() == nil {
		t.Fatalf("event = %v, want the second transfer notice", push)
	}
}

// TestAHostEventIsNotReplayedToALateHostStream pins that reload_webapp is an
// event, not a standing notice: a host stream opened after it is never handed
// it, so a reconnecting Emacs does not reload its webview a second time.
func TestAHostEventIsNotReplayedToALateHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	h.Server.PushReloadWebapp(testWorkspaceID)
	stream := dialHostStream(t, h, ctx)

	// Act: a later event marks where the stream's first event must be.
	h.Server.NotificationClicked(testWorkspaceID)
	push := receiveHostEvent(t, stream)

	// Assert.
	if push.GetNotificationClicked() == nil {
		t.Fatalf("first event = %v, want notification_clicked (reload_webapp is never replayed)", push)
	}
}

// TestTheWebappReloadNamesItsBuildOnlyForADeploy pins the two shapes of the
// reload_webapp arm: a deploy's names the build it brings, so Emacs reloads its
// host once per build; a restart's names none, so Emacs reloads one page.
func TestTheWebappReloadNamesItsBuildOnlyForADeploy(t *testing.T) {
	tests := []struct {
		name string
		push func(s Server)
		want *string
	}{
		{name: "deploy", push: func(s Server) { s.PushReloadWebappBuild(testWorkspaceID, "Web2") }, want: proto.String("Web2")},
		{name: "restart", push: func(s Server) { s.Relay().ReloadWebapp(testWorkspaceID) }, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			stream := dialHostStream(t, h, ctx)

			// Act.
			tt.push(h.Server)
			push := receiveHostEvent(t, stream)

			// Assert.
			reload := push.GetReloadWebapp()
			if reload == nil {
				t.Fatalf("event = %v, want reload_webapp", push)
			}
			if got := reload.WebappBuild; (got == nil) != (tt.want == nil) || (got != nil && *got != *tt.want) {
				t.Fatalf("webapp_build = %v, want %v", got, tt.want)
			}
		})
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

// TestNewRefusesAMissingClassifierPrompt pins that the routing brief's
// updater is required: UpdateClassifierPrompt would otherwise answer nothing.
func TestNewRefusesAMissingClassifierPrompt(t *testing.T) {
	// Arrange: every other dependency is the harness's own.
	var deps Deps
	newHarness(t, func(d *Deps) { deps = *d })
	deps.ClassifierPrompt = nil

	// Act.
	_, err := New(deps)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the classifier prompt updater") {
		t.Fatalf("New = %v, want the refusal naming the classifier prompt updater", err)
	}
}

// TestWithoutDepStripsOnlyTheOneDependency pins the helper every New refusal
// test shares: the stripped copy differs from the complete set in exactly the
// one field, and the harness's own set is untouched.
func TestWithoutDepStripsOnlyTheOneDependency(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	deps := h.withoutDep(func(d *Deps) { d.Retarget = nil })

	// Assert.
	if deps.Retarget != nil || h.Deps.Retarget == nil {
		t.Fatalf("stripped retarget = %v, harness retarget = %v; want only the copy stripped", deps.Retarget, h.Deps.Retarget)
	}
	if deps.DB == nil || deps.WebappDist != h.Deps.WebappDist {
		t.Fatal("withoutDep stripped more than the one dependency")
	}
}

// TestNoTestHandBuildsTheSurfacesDeps fails a test that spells its own Deps
// literal instead of stripping the harness's complete set: a hand-built set
// passes or fails on New's check ORDER rather than on the dependency it names.
func TestNoTestHandBuildsTheSurfacesDeps(t *testing.T) {
	// Arrange.
	handBuilt := regexp.MustCompile(`(:= |New\()Deps\{[^}]`)
	files, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var offenders []string

	// Act.
	for _, file := range files {
		if file == "fakes_test.go" {
			continue
		}
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}
		if handBuilt.MatchString(string(raw)) {
			offenders = append(offenders, file)
		}
	}

	// Assert.
	if len(offenders) != 0 {
		t.Fatalf("hand-built Deps literals in %v; strip the harness's set with withoutDep", offenders)
	}
}

func TestNewRefusesMissingTimingDependencies(t *testing.T) {
	tests := []struct {
		name  string
		strip func(*Deps)
		want  string
	}{
		{"no rpc recorder", func(d *Deps) { d.RPCs = nil }, "an rpc recorder"},
		{"no reconnect tracker", func(d *Deps) { d.Reconnects = nil }, "a reconnect tracker"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			deps := h.withoutDep(tt.strip)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("New = %v, want the refusal naming %s", err, tt.want)
			}
		})
	}
}
