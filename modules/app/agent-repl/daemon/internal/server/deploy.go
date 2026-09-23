package server

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"strconv"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/deploy"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
)

// Deployer is the daemon's deploy, as the Deploy rpc asks it.
type Deployer interface {
	Deploy(ctx context.Context, force bool) (deploy.Result, error)
}

const opDeployRPC = "daemon.server.deploy"

// Deploy builds the checkout and puts what is out of date into service (see
// endpoint_deploy.proto and internal/deploy). The answer is the deploy's
// DECISIONS: a registered shim bounce and a handover still run on the daemon's
// lifetime after it.
func (s *server) Deploy(
	ctx context.Context,
	req *connect.Request[agentreplv1.DeployRequest],
) (*connect.Response[agentreplv1.DeployResponse], error) {
	force := req.Msg.GetForce()
	fields := dlog.Context{"forced": force}
	result, err := s.deps.Deploy.Deploy(ctx, force)
	if err != nil {
		cause, ok := deployError(err)
		if !ok {
			return nil, fail(s.log, "Deploy", err)
		}
		s.log.Info(opDeployRPC, "answered a typed deploy refusal", withFields(fields, dlog.Context{"cause": err.Error()}))
		return connect.NewResponse(&agentreplv1.DeployResponse{
			Result: &agentreplv1.DeployResponse_Error{Error: cause},
		}), nil
	}
	success := &agentreplv1.DeploySuccess{}
	for _, o := range result.Outcomes {
		out, err := deployOutcome(o)
		if err != nil {
			return nil, fail(s.log, "Deploy", err)
		}
		success.Components = append(success.Components, out)
	}
	s.log.Info(opDeployRPC, "answered the deploy's decisions", withFields(fields, dlog.Context{"components": len(success.Components)}))
	return connect.NewResponse(&agentreplv1.DeployResponse{
		Result: &agentreplv1.DeployResponse_Success{Success: success},
	}), nil
}

// deployError maps the deploy's typed refusals onto their arms. An error with
// no arm is not a refusal and is answered as a failure.
func deployError(err error) (*agentreplv1.DeployError, bool) {
	var (
		build    *deploy.BuildFailed
		service  *deploy.ServiceRestartFailed
		install  *deploy.InstallFailed
		inFlight *rollout.ErrAlreadyRollingOut
	)
	switch {
	case errors.As(err, &build):
		return &agentreplv1.DeployError{Cause: &agentreplv1.DeployError_BuildFailed{
			BuildFailed: &agentreplv1.DeployBuildFailed{Step: build.Step, Detail: build.Detail, Log: build.Log},
		}}, true
	case errors.Is(err, deploy.ErrAlreadyDeploying):
		return &agentreplv1.DeployError{Cause: &agentreplv1.DeployError_AlreadyDeploying{
			AlreadyDeploying: &agentreplv1.DeployAlreadyDeploying{},
		}}, true
	case errors.As(err, &inFlight):
		waiting := make([]string, 0, len(inFlight.WaitingOn))
		for _, ws := range inFlight.WaitingOn {
			waiting = append(waiting, string(ws))
		}
		return &agentreplv1.DeployError{Cause: &agentreplv1.DeployError_AlreadyRollingOut{
			AlreadyRollingOut: &agentreplv1.DeployAlreadyRollingOut{WaitingOn: waiting},
		}}, true
	case errors.Is(err, rollout.ErrJoining):
		return &agentreplv1.DeployError{Cause: &agentreplv1.DeployError_Joining{
			Joining: &agentreplv1.DeployJoining{},
		}}, true
	case errors.As(err, &service):
		return &agentreplv1.DeployError{Cause: &agentreplv1.DeployError_ServiceRestartFailed{
			ServiceRestartFailed: &agentreplv1.DeployServiceRestartFailed{Component: componentArm(service.Component), Detail: service.Detail},
		}}, true
	case errors.As(err, &install):
		return &agentreplv1.DeployError{Cause: &agentreplv1.DeployError_InstallFailed{
			InstallFailed: &agentreplv1.DeployInstallFailed{Component: componentArm(install.Component), Detail: install.Detail},
		}}, true
	}
	return nil, false
}

// componentArm names a component on the wire.
func componentArm(c deploy.Component) agentreplv1.DeployComponent {
	switch c {
	case deploy.ComponentDaemon:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON
	case deploy.ComponentShim:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM
	case deploy.ComponentWebapp:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP
	case deploy.ComponentStore:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE
	case deploy.ComponentSidecar:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR
	case deploy.ComponentElisp:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP
	default:
		return agentreplv1.DeployComponent_DEPLOY_COMPONENT_UNSPECIFIED
	}
}

// deployOutcome renders one decision. A component or a decision this handler
// cannot name is an invariant violation, never a defaulted arm.
func deployOutcome(o deploy.Outcome) (*agentreplv1.DeployComponentOutcome, error) {
	component := componentArm(o.Component)
	if component == agentreplv1.DeployComponent_DEPLOY_COMPONENT_UNSPECIFIED {
		return nil, fmt.Errorf("server: the deploy decided for a component the contract does not name: %q", o.Component)
	}
	out := &agentreplv1.DeployComponentOutcome{Component: component, Build: o.Build}
	switch o.Kind {
	case deploy.UpToDate:
		out.Outcome = &agentreplv1.DeployComponentOutcome_UpToDate{UpToDate: &agentreplv1.DeployUpToDate{}}
	case deploy.Restarted:
		out.Outcome = &agentreplv1.DeployComponentOutcome_Restarted{Restarted: &agentreplv1.DeployServiceRestarted{}}
	case deploy.HandingOver:
		out.Outcome = &agentreplv1.DeployComponentOutcome_HandingOver{HandingOver: &agentreplv1.DeployHandingOver{
			Workspaces: uint32(o.Handover.Workspaces), Busy: uint32(o.Handover.Busy), Forced: o.Handover.Forced,
		}}
	case deploy.ShimsBouncing:
		bounces := &agentreplv1.DeployShimBounces{}
		for _, b := range o.Shims {
			one := &agentreplv1.DeployShimBounce{Workspace: string(b.Workspace)}
			if b.Decision.Now {
				one.When = &agentreplv1.DeployShimBounce_BouncedNow{BouncedNow: &agentreplv1.DeployBouncedNow{Forced: b.Decision.Forced}}
			} else {
				one.When = &agentreplv1.DeployShimBounce_Registered{Registered: &agentreplv1.DeployBounceRegistered{
					TurnInFlight: b.Decision.TurnInFlight, DetachedWork: uint32(b.Decision.DetachedWork),
				}}
			}
			bounces.Bounces = append(bounces.Bounces, one)
		}
		out.Outcome = &agentreplv1.DeployComponentOutcome_Shims{Shims: bounces}
	case deploy.ReloadPushed:
		out.Outcome = &agentreplv1.DeployComponentOutcome_ReloadPushed{ReloadPushed: &agentreplv1.DeployReloadPushed{
			Recipients: uint32(o.Recipients),
		}}
	case deploy.DeferredToSuccessor:
		out.Outcome = &agentreplv1.DeployComponentOutcome_DeferredToSuccessor{DeferredToSuccessor: &agentreplv1.DeployDeferredToSuccessor{}}
	default:
		return nil, fmt.Errorf("server: the deploy made a decision the contract does not name: %q", o.Kind)
	}
	return out, nil
}

// ---- the connected clients' builds (deploy.Clients) ----

// EmacsBuilds answers every open Emacs WatchDaemon stream's elisp build.
func (s *server) EmacsBuilds() []deploy.EmacsClient {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]deploy.EmacsClient, 0, len(s.daemonWatchers))
	for w := range s.daemonWatchers {
		if w.emacs {
			out = append(out, deploy.EmacsClient{ID: w.id, Build: w.elispBuild})
		}
	}
	sort.Slice(out, func(i, j int) bool { return out[i].ID < out[j].ID })
	return out
}

// PushReloadElisp pushes reload_elisp to the named Emacs streams. It is an
// EVENT addressed to those streams alone — a webview's WatchDaemon never
// carries it — and it answers how many it reached. A stream whose queue is
// full is reported loudly rather than blocked on.
func (s *server) PushReloadElisp(streams []string, moduleRoot, build string) int {
	want := make(map[string]bool, len(streams))
	for _, id := range streams {
		want[id] = true
	}
	push := &agentreplv1.WatchDaemonResponse{Push: &agentreplv1.WatchDaemonResponse_ReloadElisp{
		ReloadElisp: &agentreplv1.DaemonReloadElisp{ModuleRoot: moduleRoot, Build: build},
	}}
	s.mu.Lock()
	defer s.mu.Unlock()
	reached := 0
	for w := range s.daemonWatchers {
		if !w.emacs || !want[w.id] {
			continue
		}
		select {
		case w.elisp <- push:
			reached++
			s.log.Info("daemon.server.reload_elisp", "pushed the elisp reload to an Emacs", dlog.Context{
				"stream": w.id, "build": build, "module_root": moduleRoot,
			})
		default:
			s.log.Error("daemon.server.reload_elisp", "an Emacs stream's push queue is full; the elisp reload did not reach it", dlog.Context{
				"stream": w.id, "build": build,
			})
		}
	}
	return reached
}

// WebviewBuilds answers, per workspace, the webapp build every open webview
// of it reported on WatchWebWorkspace.
func (s *server) WebviewBuilds() map[ids.WorkspaceID][]string {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make(map[ids.WorkspaceID][]string, len(s.webBuilds))
	for ws, streams := range s.webBuilds {
		for _, build := range streams {
			out[ws] = append(out[ws], build)
		}
		sort.Strings(out[ws])
	}
	return out
}

// holdWebBuild records one web stream's reported build for as long as the
// stream stands, and answers the release.
func (s *server) holdWebBuild(ws ids.WorkspaceID, build string) func() {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.webStreamSeq++
	key := strconv.FormatUint(s.webStreamSeq, 10)
	if s.webBuilds[ws] == nil {
		s.webBuilds[ws] = map[string]string{}
	}
	s.webBuilds[ws][key] = build
	return func() {
		s.mu.Lock()
		defer s.mu.Unlock()
		delete(s.webBuilds[ws], key)
		if len(s.webBuilds[ws]) == 0 {
			delete(s.webBuilds, ws)
		}
	}
}

// validateWatchDaemonRequest is WatchDaemonRequest's base function: the client
// is REQUIRED, and an Emacs states the elisp it loaded.
func validateWatchDaemonRequest(req *agentreplv1.WatchDaemonRequest) *connect.Error {
	switch client := req.GetClient().(type) {
	case *agentreplv1.WatchDaemonRequest_Emacs:
		if client.Emacs.GetElispBuild() == "" {
			return invalid("client.emacs.elisp_build", "the elisp build this Emacs loaded is required")
		}
		return nil
	case *agentreplv1.WatchDaemonRequest_Webview:
		return nil
	default:
		return invalid("client", "the connecting client (emacs or webview) is required")
	}
}

// validateWatchWebWorkspaceRequest is WatchWebWorkspaceRequest's base
// function: the workspace and the page's own webapp build are REQUIRED.
func validateWatchWebWorkspaceRequest(req *agentreplv1.WatchWebWorkspaceRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	if req.GetWebappBuild() == "" {
		return invalid("webapp_build", "the webapp build this page runs is required")
	}
	return nil
}

// withFields copies base and overlays extra.
func withFields(base, extra dlog.Context) dlog.Context {
	out := make(dlog.Context, len(base)+len(extra))
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}
