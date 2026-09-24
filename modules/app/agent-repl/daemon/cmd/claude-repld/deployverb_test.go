package main

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// fakeDeployCaller answers the verb's one rpc with a scripted response.
type fakeDeployCaller struct {
	address string
	forced  []bool
	resp    *agentreplv1.DeployResponse
	err     error
}

func (f *fakeDeployCaller) Deploy(_ context.Context, req *connect.Request[agentreplv1.DeployRequest]) (*connect.Response[agentreplv1.DeployResponse], error) {
	f.forced = append(f.forced, req.Msg.GetForce())
	if f.err != nil {
		return nil, f.err
	}
	return connect.NewResponse(f.resp), nil
}

// verbFixture is a state root with a daemon.addr, and the fake it dials.
type verbFixture struct {
	stateDir string
	caller   *fakeDeployCaller
	out      bytes.Buffer
	errOut   bytes.Buffer
}

func newVerbFixture(t *testing.T, addr string, resp *agentreplv1.DeployResponse, err error) *verbFixture {
	t.Helper()
	f := &verbFixture{stateDir: t.TempDir(), caller: &fakeDeployCaller{resp: resp, err: err}}
	if addr != "" {
		if werr := os.WriteFile(filepath.Join(f.stateDir, "daemon.addr"), []byte(addr), 0o644); werr != nil {
			t.Fatalf("write daemon.addr: %v", werr)
		}
	}
	return f
}

func (f *verbFixture) run(args ...string) int {
	dial := func(address string) deployCaller {
		f.caller.address = address
		return f.caller
	}
	return runDeployVerb(context.Background(), append([]string{"-state-dir", f.stateDir}, args...), dial, &f.out, &f.errOut)
}

func success(components ...*agentreplv1.DeployComponentOutcome) *agentreplv1.DeployResponse {
	return &agentreplv1.DeployResponse{Result: &agentreplv1.DeployResponse_Success{
		Success: &agentreplv1.DeploySuccess{Components: components},
	}}
}

func refusal(e *agentreplv1.DeployError) *agentreplv1.DeployResponse {
	return &agentreplv1.DeployResponse{Result: &agentreplv1.DeployResponse_Error{Error: e}}
}

func TestTheDeployVerbAsksTheServingDaemon(t *testing.T) {
	tests := []struct {
		name      string
		args      []string
		wantForce bool
	}{
		{name: "unforced by default"},
		{name: "forced on request", args: []string{"-force"}, wantForce: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newVerbFixture(t, "127.0.0.1:4242\n", success(), nil)

			// Act
			code := f.run(tc.args...)

			// Assert
			if code != exitSuccess {
				t.Fatalf("exit = %d, stderr %q", code, f.errOut.String())
			}
			if f.caller.address != "127.0.0.1:4242" {
				t.Fatalf("dialed %q, want the advertised address", f.caller.address)
			}
			if len(f.caller.forced) != 1 || f.caller.forced[0] != tc.wantForce {
				t.Fatalf("forced = %v, want [%v]", f.caller.forced, tc.wantForce)
			}
		})
	}
}

func TestTheDeployVerbPrintsEveryDecision(t *testing.T) {
	tests := []struct {
		name    string
		outcome *agentreplv1.DeployComponentOutcome
		want    string
	}{
		{name: "up to date", outcome: &agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Build: "b",
			Outcome: &agentreplv1.DeployComponentOutcome_UpToDate{UpToDate: &agentreplv1.DeployUpToDate{}}}, want: "store build=b up-to-date"},
		{name: "restarted", outcome: &agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR, Build: "b",
			Outcome: &agentreplv1.DeployComponentOutcome_Restarted{Restarted: &agentreplv1.DeployServiceRestarted{}}}, want: "sidecar build=b restarted"},
		{name: "handing over", outcome: &agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Build: "b",
			Outcome: &agentreplv1.DeployComponentOutcome_HandingOver{HandingOver: &agentreplv1.DeployHandingOver{Workspaces: 3, Busy: 1}}},
			want: "daemon build=b handing-over workspaces=3 busy=1 forced=false"},
		{name: "shims", outcome: &agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM, Build: "b",
			Outcome: &agentreplv1.DeployComponentOutcome_Shims{Shims: &agentreplv1.DeployShimBounces{Bounces: []*agentreplv1.DeployShimBounce{
				{Workspace: "w1", When: &agentreplv1.DeployShimBounce_BouncedNow{BouncedNow: &agentreplv1.DeployBouncedNow{}}},
				{Workspace: "w2", When: &agentreplv1.DeployShimBounce_Registered{Registered: &agentreplv1.DeployBounceRegistered{TurnInFlight: true, DetachedWork: 2}}},
			}}}}, want: "shim build=b shims w1:bounced-now(forced=false) w2:registered(turn=true,detached=2)"},
		{name: "reload pushed", outcome: &agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP, Build: "b",
			Outcome: &agentreplv1.DeployComponentOutcome_ReloadPushed{ReloadPushed: &agentreplv1.DeployReloadPushed{Recipients: 1}}},
			want: "elisp build=b reload-pushed recipients=1"},
		{name: "deferred", outcome: &agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP, Build: "b",
			Outcome: &agentreplv1.DeployComponentOutcome_DeferredToSuccessor{DeferredToSuccessor: &agentreplv1.DeployDeferredToSuccessor{}}},
			want: "webapp build=b deferred-to-successor"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newVerbFixture(t, "127.0.0.1:1\n", success(tc.outcome), nil)

			// Act
			code := f.run()

			// Assert
			if code != exitSuccess || strings.TrimSpace(f.out.String()) != tc.want {
				t.Fatalf("exit %d, out %q, want %q", code, f.out.String(), tc.want)
			}
		})
	}
}

func TestTheDeployVerbFailsLoudly(t *testing.T) {
	tests := []struct {
		name     string
		addr     string
		args     []string
		resp     *agentreplv1.DeployResponse
		err      error
		wantText string
	}{
		{name: "no daemon.addr", wantText: "no daemon is serving"},
		{name: "a daemon.addr naming no address", addr: "\n", wantText: "names no address"},
		{name: "a stray argument", addr: "127.0.0.1:1\n", args: []string{"now"}, wantText: "unexpected arguments"},
		{name: "a transport failure", addr: "127.0.0.1:1\n", err: errors.New("connection refused"), wantText: "connection refused"},
		{name: "a response with no arm", addr: "127.0.0.1:1\n", resp: &agentreplv1.DeployResponse{}, wantText: "no result arm"},
		{name: "an outcome with no arm", addr: "127.0.0.1:1\n",
			resp: success(&agentreplv1.DeployComponentOutcome{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM}), wantText: "no arm"},
		{name: "a build failure", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{Cause: &agentreplv1.DeployError_BuildFailed{
			BuildFailed: &agentreplv1.DeployBuildFailed{Step: "webapp", Detail: "error TS2322", Log: "/l"}}}), wantText: "the webapp build failed (output in /l):\nerror TS2322"},
		{name: "already deploying", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{Cause: &agentreplv1.DeployError_AlreadyDeploying{
			AlreadyDeploying: &agentreplv1.DeployAlreadyDeploying{}}}), wantText: "already running"},
		{name: "already rolling out", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{Cause: &agentreplv1.DeployError_AlreadyRollingOut{
			AlreadyRollingOut: &agentreplv1.DeployAlreadyRollingOut{WaitingOn: []string{"w1"}}}}), wantText: "waiting on w1"},
		{name: "joining", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{Cause: &agentreplv1.DeployError_Joining{
			Joining: &agentreplv1.DeployJoining{}}}), wantText: "still joining"},
		{name: "a service restart failure", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{Cause: &agentreplv1.DeployError_ServiceRestartFailed{
			ServiceRestartFailed: &agentreplv1.DeployServiceRestartFailed{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, Detail: "socket never bound"}}}),
			wantText: "the store did not come back onto the fresh build: socket never bound"},
		{name: "an install failure", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{Cause: &agentreplv1.DeployError_InstallFailed{
			InstallFailed: &agentreplv1.DeployInstallFailed{Component: agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON, Detail: "read-only"}}}),
			wantText: "the daemon build could not be installed: read-only"},
		{name: "a refusal with no cause", addr: "127.0.0.1:1\n", resp: refusal(&agentreplv1.DeployError{}), wantText: "no cause arm"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newVerbFixture(t, tc.addr, tc.resp, tc.err)

			// Act
			code := f.run(tc.args...)

			// Assert
			if code != exitFailure {
				t.Fatalf("exit = %d, want %d", code, exitFailure)
			}
			if !strings.Contains(f.errOut.String(), tc.wantText) {
				t.Fatalf("stderr = %q, want it to contain %q", f.errOut.String(), tc.wantText)
			}
		})
	}
}
