package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/bounce"
	"claude-repld/internal/deploy"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

func TestDeployPassesTheForceThrough(t *testing.T) {
	tests := []struct {
		name  string
		force bool
	}{
		{name: "unforced"},
		{name: "forced", force: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.Deploy(context.Background(), connect.NewRequest(&agentreplv1.DeployRequest{Force: tc.force}))

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if got := h.Deployer.forced; len(got) != 1 || got[0] != tc.force {
				t.Fatalf("forced = %v, want [%v]", got, tc.force)
			}
		})
	}
}

func TestDeployAnswersEveryDecision(t *testing.T) {
	tests := []struct {
		name    string
		outcome deploy.Outcome
		check   func(*agentreplv1.DeployComponentOutcome) bool
	}{
		{
			name:    "up to date",
			outcome: deploy.Outcome{Component: deploy.ComponentStore, Build: "b", Kind: deploy.UpToDate},
			check: func(o *agentreplv1.DeployComponentOutcome) bool {
				return o.GetUpToDate() != nil && o.GetComponent() == agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE && o.GetBuild() == "b"
			},
		},
		{
			name:    "restarted",
			outcome: deploy.Outcome{Component: deploy.ComponentSidecar, Build: "b", Kind: deploy.Restarted},
			check:   func(o *agentreplv1.DeployComponentOutcome) bool { return o.GetRestarted() != nil },
		},
		{
			name: "handing over",
			outcome: deploy.Outcome{Component: deploy.ComponentDaemon, Build: "b", Kind: deploy.HandingOver,
				Handover: rollout.HandoverAcceptance{Workspaces: 3, Busy: 1, Forced: true}},
			check: func(o *agentreplv1.DeployComponentOutcome) bool {
				h := o.GetHandingOver()
				return h.GetWorkspaces() == 3 && h.GetBusy() == 1 && h.GetForced()
			},
		},
		{
			name: "shims bouncing now and registered",
			outcome: deploy.Outcome{Component: deploy.ComponentShim, Build: "b", Kind: deploy.ShimsBouncing, Shims: []deploy.ShimBounce{
				{Workspace: "ws-now", Decision: bounce.Decision{Now: true, Forced: true}},
				{Workspace: "ws-later", Decision: bounce.Decision{TurnInFlight: true, DetachedWork: 2}},
			}},
			check: func(o *agentreplv1.DeployComponentOutcome) bool {
				b := o.GetShims().GetBounces()
				return len(b) == 2 && b[0].GetWorkspace() == "ws-now" && b[0].GetBouncedNow().GetForced() &&
					b[1].GetRegistered().GetTurnInFlight() && b[1].GetRegistered().GetDetachedWork() == 2
			},
		},
		{
			name:    "reload pushed",
			outcome: deploy.Outcome{Component: deploy.ComponentElisp, Build: "b", Kind: deploy.ReloadPushed, Recipients: 2},
			check:   func(o *agentreplv1.DeployComponentOutcome) bool { return o.GetReloadPushed().GetRecipients() == 2 },
		},
		{
			name:    "deferred to the successor",
			outcome: deploy.Outcome{Component: deploy.ComponentWebapp, Build: "b", Kind: deploy.DeferredToSuccessor},
			check:   func(o *agentreplv1.DeployComponentOutcome) bool { return o.GetDeferredToSuccessor() != nil },
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Deployer.result = deploy.Result{Outcomes: []deploy.Outcome{tc.outcome}}

			// Act
			resp, err := h.Client.Deploy(context.Background(), connect.NewRequest(&agentreplv1.DeployRequest{}))

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			components := resp.Msg.GetSuccess().GetComponents()
			if len(components) != 1 || !tc.check(components[0]) {
				t.Fatalf("components = %v, want the %s arm", components, tc.name)
			}
		})
	}
}

func TestDeployAnswersEveryRefusalAsItsArm(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		check func(*agentreplv1.DeployError) bool
	}{
		{
			name: "a build failure",
			err:  &deploy.BuildFailed{Step: "webapp", Detail: "tsc failed", Log: "/l"},
			check: func(e *agentreplv1.DeployError) bool {
				b := e.GetBuildFailed()
				return b.GetStep() == "webapp" && b.GetDetail() == "tsc failed" && b.GetLog() == "/l"
			},
		},
		{
			name:  "a deploy already running",
			err:   deploy.ErrAlreadyDeploying,
			check: func(e *agentreplv1.DeployError) bool { return e.GetAlreadyDeploying() != nil },
		},
		{
			name: "a handover in flight",
			err:  &rollout.ErrAlreadyRollingOut{WaitingOn: []ids.WorkspaceID{"ws-busy"}},
			check: func(e *agentreplv1.DeployError) bool {
				w := e.GetAlreadyRollingOut().GetWaitingOn()
				return len(w) == 1 && w[0] == "ws-busy"
			},
		},
		{
			name:  "a successor still joining",
			err:   rollout.ErrJoining,
			check: func(e *agentreplv1.DeployError) bool { return e.GetJoining() != nil },
		},
		{
			name: "a service that did not come back",
			err:  &deploy.ServiceRestartFailed{Component: deploy.ComponentStore, Detail: "died"},
			check: func(e *agentreplv1.DeployError) bool {
				s := e.GetServiceRestartFailed()
				return s.GetComponent() == agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE && s.GetDetail() == "died"
			},
		},
		{
			name: "an install that failed",
			err:  &deploy.InstallFailed{Component: deploy.ComponentShim, Detail: "rename failed"},
			check: func(e *agentreplv1.DeployError) bool {
				return e.GetInstallFailed().GetComponent() == agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM
			},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Deployer.err = tc.err

			// Act
			resp, err := h.Client.Deploy(context.Background(), connect.NewRequest(&agentreplv1.DeployRequest{}))

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v, want the refusal as an answer", err)
			}
			if !tc.check(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the %s arm", resp.Msg.GetResult(), tc.name)
			}
		})
	}
}

// deployErrorLogged reports whether the Deploy handler recorded its failure.
func deployErrorLogged(log *recordingLogger, cause string) bool {
	for _, rec := range log.at("ERROR") {
		if rec.Operation == "Deploy" && strings.Contains(fmt.Sprint(rec.Context["cause"]), cause) {
			return true
		}
	}
	return false
}

func TestDeployAnswersAnUntypedFailureAsAFailure(t *testing.T) {
	// Arrange
	log := &recordingLogger{}
	h := newHarness(t, func(d *Deps) { d.Log = &fakeSurfaces{global: log} })
	h.Deployer.err = errors.New("the staging root is gone")

	// Act
	_, err := h.Client.Deploy(context.Background(), connect.NewRequest(&agentreplv1.DeployRequest{}))

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal {
		t.Fatalf("Deploy = %v, want an internal failure", err)
	}
	if !deployErrorLogged(log, "the staging root is gone") {
		t.Fatalf("records = %+v, want the failure at ERROR", log.at("ERROR"))
	}
}

func TestDeployRefusesADecisionTheContractDoesNotName(t *testing.T) {
	tests := []struct {
		name    string
		outcome deploy.Outcome
		cause   string
	}{
		{name: "a component", outcome: deploy.Outcome{Component: "lint", Kind: deploy.UpToDate}, cause: "a component the contract does not name"},
		{name: "a decision", outcome: deploy.Outcome{Component: deploy.ComponentShim, Kind: "vanished"}, cause: "a decision the contract does not name"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := &recordingLogger{}
			h := newHarness(t, func(d *Deps) { d.Log = &fakeSurfaces{global: log} })
			h.Deployer.result = deploy.Result{Outcomes: []deploy.Outcome{tc.outcome}}

			// Act
			_, err := h.Client.Deploy(context.Background(), connect.NewRequest(&agentreplv1.DeployRequest{}))

			// Assert
			if connect.CodeOf(err) != connect.CodeInternal {
				t.Fatalf("Deploy = %v, want an internal failure, never a defaulted arm", err)
			}
			if !deployErrorLogged(log, tc.cause) {
				t.Fatalf("records = %+v, want the violation at ERROR", log.at("ERROR"))
			}
		})
	}
}

func TestWatchDaemonRefusesARequestNamingNoBuild(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.WatchDaemonRequest
	}{
		{name: "no client", req: &agentreplv1.WatchDaemonRequest{}},
		{name: "an Emacs naming no elisp build", req: &agentreplv1.WatchDaemonRequest{
			Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{}},
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			// Act
			stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(tc.req))
			if err == nil {
				stream.Receive()
				err = stream.Err()
			}

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("WatchDaemon = %v, want invalid_argument", err)
			}
		})
	}
}

func TestWatchWebWorkspaceRefusesAPageNamingNoBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	stream, err := h.Client.WatchWebWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref()}))
	if err == nil {
		stream.Receive()
		err = stream.Err()
	}

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("WatchWebWorkspace = %v, want invalid_argument", err)
	}
}

// openDaemonWatch opens one WatchDaemon stream as the named client and
// returns once its handler is serving: a progress event published after the
// open is received, and the handler registers its stream before it serves any.
func openDaemonWatch(t *testing.T, h *harness, req *agentreplv1.WatchDaemonRequest, sync string) *connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse] {
	t.Helper()
	ctx, cancel := context.WithCancel(context.Background())
	t.Cleanup(cancel)
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(req))
	if err != nil {
		t.Fatalf("WatchDaemon: %v", err)
	}
	h.Server.(*server).MutationProgress(&agentreplv1.WorkspaceMutationProgress{OpId: sync})
	for stream.Receive() {
		if stream.Msg().GetMutationProgress().GetOpId() == sync {
			return stream
		}
	}
	t.Fatalf("the stream ended before it served: %v", stream.Err())
	return nil
}

func emacsWatch(build string) *agentreplv1.WatchDaemonRequest {
	return &agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: build}}}
}

func webviewWatch() *agentreplv1.WatchDaemonRequest {
	return &agentreplv1.WatchDaemonRequest{Client: &agentreplv1.WatchDaemonRequest_Webview{Webview: &agentreplv1.WatchDaemonWebview{}}}
}

func TestTheElispReloadReachesOnlyTheNamedEmacsStream(t *testing.T) {
	// Arrange: one Emacs and one webview hold the daemon stream.
	h := newHarness(t)
	emacs := openDaemonWatch(t, h, emacsWatch("older"), "sync-emacs")
	openDaemonWatch(t, h, webviewWatch(), "sync-webview")
	s := h.Server.(*server)
	clients := s.EmacsBuilds()
	if len(clients) != 1 || clients[0].Build != "older" {
		t.Fatalf("EmacsBuilds = %+v, want the one Emacs and its build", clients)
	}

	// Act
	reached := s.PushReloadElisp([]string{clients[0].ID}, "/root", "fresh")

	// Assert
	if reached != 1 {
		t.Fatalf("reached = %d, want the one Emacs", reached)
	}
	for emacs.Receive() {
		if reload := emacs.Msg().GetReloadElisp(); reload != nil {
			if reload.GetModuleRoot() != "/root" || reload.GetBuild() != "fresh" {
				t.Fatalf("reload = %v, want the root and the fresh build", reload)
			}
			return
		}
	}
	t.Fatalf("the Emacs stream ended without the reload: %v", emacs.Err())
}

func TestAnOpenWebStreamReportsItsBuild(t *testing.T) {
	// Arrange
	h := newHarness(t)
	s := h.Server.(*server)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchWebWorkspace(ctx, connect.NewRequest(&agentreplv1.WatchWebWorkspaceRequest{Workspace: ref(), WebappBuild: "Web1"}))
	if err != nil {
		t.Fatalf("WatchWebWorkspace: %v", err)
	}
	// The handler records the build before it serves its first frame (the
	// session identity it composes before subscribing).
	if !stream.Receive() {
		t.Fatalf("the web stream ended before its first frame: %v", stream.Err())
	}

	// Act
	open := s.WebviewBuilds()

	// Assert
	if got := open[testWorkspaceID]; len(got) != 1 || got[0] != "Web1" {
		t.Fatalf("open builds = %v, want the page's Web1", open)
	}
}

func TestAReleasedWebBuildIsForgotten(t *testing.T) {
	// Arrange
	h := newHarness(t)
	s := h.Server.(*server)
	release := s.holdWebBuild(testWorkspaceID, "Web1")
	other := s.holdWebBuild(testWorkspaceID, "Web2")

	// Act
	release()

	// Assert
	if got := s.WebviewBuilds()[testWorkspaceID]; len(got) != 1 || got[0] != "Web2" {
		t.Fatalf("builds = %v, want only the stream still open", got)
	}
	other()
	if got := s.WebviewBuilds(); len(got) != 0 {
		t.Fatalf("builds = %v, want none once every stream closed", got)
	}
}
