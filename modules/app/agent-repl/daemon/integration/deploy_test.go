//go:build integration

package integration

import (
	"context"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// deploy_test.go drives the daemon's own deploy through the Deploy rpc. The
// harness's fake build (harness/deploybuild.go) stages exactly what runs
// unless a test stages a change, so no test builds for real, and the
// services' build reports are stated so no test reaches launchd.

// deployOutcomes asks the daemon to deploy and answers its decisions by
// component.
func deployOutcomes(t *testing.T, d *harness.Daemon, force bool) map[agentreplv1.DeployComponent]*agentreplv1.DeployComponentOutcome {
	t.Helper()
	resp, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{Force: force}))
	if err != nil {
		t.Fatalf("Deploy = error %v, want an answer", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("Deploy = %v, want its decisions", resp.Msg)
	}
	out := map[agentreplv1.DeployComponent]*agentreplv1.DeployComponentOutcome{}
	for _, o := range success.GetComponents() {
		out[o.GetComponent()] = o
	}
	return out
}

func TestADeployOfWhatAlreadyRunsDecidesEveryComponentUpToDate(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})

	// Act
	outcomes := deployOutcomes(t, d, false)

	// Assert
	for _, component := range []agentreplv1.DeployComponent{
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR,
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP, agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON,
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM, agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP,
	} {
		o := outcomes[component]
		if o.GetUpToDate() == nil || o.GetBuild() == "" {
			t.Fatalf("%s = %v, want up to date and naming the fresh build", component, o)
		}
	}
	if got := len(d.Deploy.Invocations()); got != 1 {
		t.Fatalf("builds = %d, want one", got)
	}
	if got := len(d.Launchctl.Invocations()); got != 0 {
		t.Fatalf("launchctl invocations = %d, want none: nothing was out of date", got)
	}
}

func TestADeployWhoseBuildFailsDeploysNothingAndSaysWhy(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})
	d.StageDeployBuild(harness.DeployFails)
	d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run")

	// Act
	resp, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{}))

	// Assert
	if err != nil {
		t.Fatalf("Deploy = error %v, want the typed refusal", err)
	}
	failed := resp.Msg.GetError().GetBuildFailed()
	if failed.GetStep() != "build" || !strings.Contains(failed.GetDetail(), harness.FakeDeployBuildRefusal) || failed.GetLog() == "" {
		t.Fatalf("Deploy = %v, want build_failed naming the step, the build's own words and the archived log", resp.Msg)
	}
	if got := len(d.Launchctl.Invocations()); got != 0 {
		t.Fatalf("launchctl invocations = %d, want none: a failed build restarts nothing", got)
	}
}

func TestADeployPushesTheWebappReloadToAStaleWebview(t *testing.T) {
	t.Parallel()
	// Arrange: an open webview on the served build, and a build whose webapp
	// is newer.
	f := newOpened(t, harness.Opts{})
	host := f.d.WatchHost(f.ws)
	web := f.d.WatchWeb(f.ws)
	defer web.Close()
	harness.AwaitNext(t, f.d.Ctx(), host, "the fresh host push")
	f.d.StageDeployBuild(harness.DeployStaleWebapp)

	// Act
	outcomes := deployOutcomes(t, f.d, false)

	// Assert
	if got := outcomes[agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP].GetReloadPushed().GetRecipients(); got != 1 {
		t.Fatalf("webapp = %v, want the reload pushed to the one workspace with a stale webview",
			outcomes[agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP])
	}
	harness.AwaitView(t, f.d.Ctx(), host, "reload_webapp", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetReloadWebapp() != nil
	})
}

func TestADeployPushesTheElispReloadToAStaleEmacsAlone(t *testing.T) {
	t.Parallel()
	// Arrange: an Emacs on older elisp holds the daemon stream.
	d := harness.StartDaemon(t, harness.Opts{})
	ctx, cancel := context.WithCancel(d.Ctx())
	defer cancel()
	stream, err := d.Client().WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "older-elisp"}},
	}))
	if err != nil {
		t.Fatalf("WatchDaemon: %v", err)
	}
	d.AwaitLogRecord(d.RunLogPath(), "the stale Emacs's stream accepted", func(r harness.LogRecord) bool {
		return r.Operation == "WatchDaemon" && r.Context["elisp_build"] == "older-elisp"
	})

	// Act
	outcomes := deployOutcomes(t, d, false)

	// Assert
	elisp := outcomes[agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP]
	if elisp.GetReloadPushed().GetRecipients() != 1 || elisp.GetBuild() != harness.PinnedElispBuild {
		t.Fatalf("elisp = %v, want the reload pushed to the one stale Emacs, naming the checkout's build", elisp)
	}
	for stream.Receive() {
		if reload := stream.Msg().GetReloadElisp(); reload != nil {
			if reload.GetBuild() != harness.PinnedElispBuild || reload.GetModuleRoot() != harness.PinnedCheckout(t) {
				t.Fatalf("reload_elisp = %v, want the checkout's root and build", reload)
			}
			return
		}
	}
	t.Fatalf("the stale Emacs's stream ended without the reload: %v", stream.Err())
}
