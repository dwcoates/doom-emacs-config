//go:build integration

package integration

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"strings"
	"sync"
	"testing"

	"agentrepl/logging/buildreport"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

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

// ---- the update line: a deploy's progress on the footer --------------------

// footerUpdatePhase names the update line's phase arm on a pushed footer, ""
// when the strip carries none.
func footerUpdatePhase(v *frontendv1.FooterView) string {
	var update *frontendv1.FooterStatusActivityUpdate
	switch arm := v.GetStrip().GetStatus().GetStatus().(type) {
	case *frontendv1.FooterStatus_Idle:
		update = arm.Idle.GetActivity().GetUpdate()
	case *frontendv1.FooterStatus_Thinking:
		update = arm.Thinking.GetActivity().GetUpdate()
	}
	switch update.GetPhase().(type) {
	case *frontendv1.FooterStatusActivityUpdate_Building:
		return "building"
	case *frontendv1.FooterStatusActivityUpdate_Installing:
		return "installing"
	case *frontendv1.FooterStatusActivityUpdate_RestartingServices:
		return "restarting_services"
	case *frontendv1.FooterStatusActivityUpdate_HandingOver:
		return "handing_over"
	case *frontendv1.FooterStatusActivityUpdate_Waiting:
		return "waiting"
	case *frontendv1.FooterStatusActivityUpdate_Updated:
		return "updated"
	default:
		return ""
	}
}

func TestADeployShowsItsPhasesOnTheFooterAndRetiresUpdated(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer after readiness", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act
	deployOutcomes(t, f.d, false)

	// Assert: every phase arrives in order, then the momentary updated goes.
	for _, want := range []string{"building", "installing", "updated"} {
		awaitFooter(t, f, footer, "the "+want+" line", func(v *frontendv1.FooterView) bool {
			return footerUpdatePhase(v) == want
		})
	}
	awaitFooter(t, f, footer, "the updated line retired", func(v *frontendv1.FooterView) bool {
		return footerUpdatePhase(v) == ""
	})
}

func TestADeployHandoverShowsTheWaitingWorkspaceAndItsSuccessorSaysUpdated(t *testing.T) {
	t.Parallel()
	// Arrange: a turn in flight, with both participants open so the successor
	// adopts only when they ask it to.
	d := harness.StartDaemon(t, harness.Opts{Timeout: harness.HandoverChainTimeout})
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	host := d.WatchHost(f.ws)
	f.web = d.WatchWeb(f.ws)
	harness.AwaitNext(t, d.Ctx(), host, "the fresh host push")
	footer := d.WatchFooter(f.ws)
	openTurnOn(t, f, "k-deploy-progress")
	daemonStream := d.WatchDaemonStream()

	// Act: an UNFORCED deploy of a newer daemon hands over at freeness.
	d.StageDeployBuild(harness.DeployStaleDaemon)
	if _, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{})); err != nil {
		t.Fatalf("Deploy = error %v, want the handover accepted", err)
	}

	// Assert: the busy workspace says what its move waits on.
	waiting := awaitFooter(t, f, footer, "the waiting line", func(v *frontendv1.FooterView) bool {
		return footerUpdatePhase(v) == "waiting"
	})
	counts := waiting.GetStrip().GetStatus().GetThinking().GetActivity().GetUpdate().GetWaiting()
	if counts.GetTurns() != 1 || counts.GetBackground() != 0 {
		t.Fatalf("waiting = %+v, want the one turn in flight", counts)
	}

	// Act: the turn ends, so the workspace falls free and transfers.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
	successor := drainDial(announced.GetAddress())
	var wg sync.WaitGroup
	var hostErr, webErr error
	wg.Add(2)
	go func() {
		defer wg.Done()
		_, hostErr = successor.AdoptHostWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptHostWorkspaceRequest{Workspace: f.ws}))
	}()
	go func() {
		defer wg.Done()
		_, webErr = successor.AdoptWebWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.AdoptWebWorkspaceRequest{Workspace: f.ws}))
	}()
	wg.Wait()
	if hostErr != nil || webErr != nil {
		t.Fatalf("AdoptHostWorkspace = %v, AdoptWebWorkspace = %v; want both to succeed", hostErr, webErr)
	}

	// Assert: the SUCCESSOR ends the story once the incumbent is gone: its
	// footer takes the momentary updated line onto every strip.
	d.AwaitRunLogRecordFromAnyProcess("the successor's updated line", func(r harness.LogRecord) bool {
		return r.PID != d.PID() && r.Operation == "daemon.footer.deploy_progress" && r.Context["phase"] == "updated"
	})
	d.AwaitWorkspaceLogRecord(f.repo.Dir, "the updated line on this workspace's strip", func(r harness.LogRecord) bool {
		text, _ := r.Context["text"].(string)
		return r.PID != d.PID() && r.Operation == "daemon.footer.activity_line_changed" &&
			r.Context["kind"] == "update" && strings.Contains(text, "updated")
	})
}

// ---- a failed deploy: the deploy_failed fault on the footer -----------------

// footerFault answers the fault line on an idle strip, nil when none stands.
func footerFault(v *frontendv1.FooterView) *frontendv1.FooterStatusActivityFault {
	return v.GetStrip().GetStatus().GetIdle().GetActivity().GetFault()
}

func TestADeployWhoseBuildFailsStandsAsAFaultOnTheFooter(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer after readiness", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
	f.d.StageDeployBuild(harness.DeployFails)
	f.d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run")

	// Act
	resp, err := f.d.Client().Deploy(f.d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{}))
	if err != nil || resp.Msg.GetError().GetBuildFailed() == nil {
		t.Fatalf("Deploy = (%v, %v), want the build_failed refusal", resp, err)
	}

	// Assert: the failure stands on the strip, naming its step and its words.
	view := awaitFooter(t, f, footer, "the deploy_failed fault line", func(v *frontendv1.FooterView) bool {
		return footerFault(v).GetKind() == "deploy_failed"
	})
	if detail := footerFault(view).GetDetail(); detail != "build: "+harness.FakeDeployBuildRefusal {
		t.Fatalf("fault detail = %q, want the step and the build's own words", detail)
	}
}

func TestALaterDeployThatBuildsTakesTheFaultDown(t *testing.T) {
	t.Parallel()
	// Arrange: a failed deploy's fault stands on the strip.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	f.d.StageDeployBuild(harness.DeployFails)
	f.d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run")
	if _, err := f.d.Client().Deploy(f.d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{})); err != nil {
		t.Fatalf("Deploy = error %v, want the typed refusal", err)
	}
	awaitFooter(t, f, footer, "the deploy_failed fault line", func(v *frontendv1.FooterView) bool {
		return footerFault(v).GetKind() == "deploy_failed"
	})
	f.d.StageDeployBuild(harness.DeployCurrent)

	// Act
	deployOutcomes(t, f.d, false)

	// Assert: the fault comes down and the deploy ends its own story.
	awaitFooter(t, f, footer, "the fault retracted", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil && footerFault(v) == nil
	})
}

// ---- a failed deploy: on the topbar, and rolled back -----------------------

// topbarHasLine reports whether the topbar's warning strip carries line.
func topbarHasLine(v *frontendv1.TopbarView, line string) bool {
	for _, w := range v.GetWarnings().GetWarnings() {
		if w.GetLine().GetText() == line {
			return true
		}
	}
	return false
}

// buildFailedTopbarLine is the topbar line the harness's failing build stands as.
var buildFailedTopbarLine = "deploy failed: build: " + harness.FakeDeployBuildRefusal

func TestADeployWhoseBuildFailsStandsOnTheTopbar(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	f.d.StageDeployBuild(harness.DeployFails)
	f.d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run")

	// Act
	resp, err := f.d.Client().Deploy(f.d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{}))
	if err != nil || resp.Msg.GetError().GetBuildFailed() == nil {
		t.Fatalf("Deploy = (%v, %v), want the build_failed refusal", resp, err)
	}

	// Assert: the failure stands in the topbar's error section, naming its
	// step and its words.
	awaitTopbar(t, f, topbar, "the deploy_failed warning line", func(v *frontendv1.TopbarView) bool {
		return topbarHasLine(v, buildFailedTopbarLine)
	})
}

func TestALaterDeployThatBuildsTakesTheFaultOffTheTopbar(t *testing.T) {
	t.Parallel()
	// Arrange: a failed deploy's fault stands on the topbar.
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	f.d.StageDeployBuild(harness.DeployFails)
	f.d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run")
	if _, err := f.d.Client().Deploy(f.d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{})); err != nil {
		t.Fatalf("Deploy = error %v, want the typed refusal", err)
	}
	awaitTopbar(t, f, topbar, "the deploy_failed warning line", func(v *frontendv1.TopbarView) bool {
		return topbarHasLine(v, buildFailedTopbarLine)
	})
	f.d.StageDeployBuild(harness.DeployCurrent)

	// Act
	deployOutcomes(t, f.d, false)

	// Assert
	awaitTopbar(t, f, topbar, "the warning line retracted", func(v *frontendv1.TopbarView) bool {
		return !topbarHasLine(v, buildFailedTopbarLine)
	})
}

// failSidecarRestart makes the next deploy judge the sidecar stale and its
// launchd kickstart fail, before AND after the rollback: the cache bin under
// the test's HOME is empty, so the deploy installs into it and then fails.
func failSidecarRestart(t *testing.T, d *harness.Daemon) {
	t.Helper()
	d.StageDeployBuild(harness.DeployCurrent)
	if err := buildreport.Write(d.LockDir, buildreport.ServiceSidecar, buildreport.Report{PID: d.PID(), Build: "an older sidecar"}); err != nil {
		t.Fatalf("state the sidecar's stale build report: %v", err)
	}
	d.Launchctl.SetExitCode(1)
	d.ExpectWarnings("daemon.scriptrunner.run", "daemon.deploy.services", "daemon.deploy.decide",
		"daemon.deploy.rollback", "daemon.deploy.run")
}

func TestADeployWhoseServiceRestartFailsRollsTheCacheBinBack(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{})
	failSidecarRestart(t, d)

	// Act
	resp, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{}))
	if err != nil || resp.Msg.GetError().GetServiceRestartFailed() == nil {
		t.Fatalf("Deploy = (%v, %v), want the service_restart_failed refusal", resp, err)
	}

	// Assert: what the install put where nothing stood is gone again.
	removed := d.AwaitLogRecord(d.RunLogPath(), "the rollback's removal of the installed sidecar", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.deploy.rollback" && r.Context["artifact"] == buildreport.ServiceSidecar &&
			strings.HasSuffix(fmt.Sprint(r.Context["live"]), "/"+buildreport.ServiceSidecar) &&
			strings.Contains(r.Message, "removed what the install put where nothing stood before")
	})
	if _, err := os.Stat(fmt.Sprint(removed.Context["live"])); !errors.Is(err, fs.ErrNotExist) {
		t.Fatalf("the installed sidecar after the rollback: stat = %v, want it absent as it was", err)
	}
}

func TestADeployWhoseRollbackFailsStandsAsItsOwnFaultOnTheTopbar(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	topbar := f.d.WatchTopbar(f.ws)
	failSidecarRestart(t, f.d)

	// Act
	if _, err := f.d.Client().Deploy(f.d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{})); err != nil {
		t.Fatalf("Deploy = error %v, want the typed refusal", err)
	}

	// Assert: the step's fault and the rollback's own both stand.
	awaitTopbar(t, f, topbar, "the step's and the rollback's warning lines", func(v *frontendv1.TopbarView) bool {
		var step, rollback bool
		for _, w := range v.GetWarnings().GetWarnings() {
			text := w.GetLine().GetText()
			step = step || strings.HasPrefix(text, "deploy failed: restart services sidecar, rollback failed: ")
			rollback = rollback || strings.HasPrefix(text, "deploy failed: rollback sidecar: ")
		}
		return step && rollback
	})
}
