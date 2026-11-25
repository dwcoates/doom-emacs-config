//go:build integration

package integration

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"

	"agentrepl/logging/buildreport"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/userenv"

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
	// A daemon of its own: a deploy is the daemon's one, and another case's
	// running beside it answers already_deploying.
	d := harness.StartDaemon(t, harness.Opts{})

	// Act
	outcomes := deployOutcomes(t, d, false)

	// Assert
	components := []agentreplv1.DeployComponent{
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_STORE, agentreplv1.DeployComponent_DEPLOY_COMPONENT_SIDECAR,
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_ELISP, agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON,
		agentreplv1.DeployComponent_DEPLOY_COMPONENT_SHIM, agentreplv1.DeployComponent_DEPLOY_COMPONENT_WEBAPP,
	}
	// On macOS the fake launcher's build report names the build the deploy
	// stages, so it too is up to date and nothing kickstarts it.
	if runtime.GOOS == "darwin" {
		components = append(components, agentreplv1.DeployComponent_DEPLOY_COMPONENT_LAUNCHER)
	}
	for _, component := range components {
		o := outcomes[component]
		if o.GetUpToDate() == nil || o.GetBuild() == "" {
			t.Fatalf("%s = %v, want up to date and naming the fresh build", component, o)
		}
	}
	if got := len(d.Deploy.Invocations()); got != 1 {
		t.Fatalf("builds = %d, want one", got)
	}
	if got := len(d.Services.InvocationsExcept(harness.PlatformServiceVerbs.Status)); got != 0 {
		t.Fatalf("service manager invocations = %d, want none: nothing was out of date", got)
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
	harness.AwaitHostView(t, f.d.Ctx(), host, "the fresh host push")
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

func TestAnEmacsConnectingOnOlderElispIsPushedTheReloadAsItsVerdict(t *testing.T) {
	t.Parallel()
	// Arrange: the daemon serves, and only then does an Emacs on older elisp
	// connect -- the order in which a successor's settle once missed it.
	d := harness.StartDaemon(t, harness.Opts{})
	ctx, cancel := context.WithCancel(d.Ctx())
	defer cancel()

	// Act
	stream, err := d.Client().WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "older-elisp", Focus: harness.UnfocusedEditor(), Instance: harness.EditorInstance()}},
	}))
	if err != nil {
		t.Fatalf("WatchDaemon: %v", err)
	}

	// Assert: the reload is the stream's one frame, and the stream ends.
	if !stream.Receive() {
		t.Fatalf("the stale Emacs's stream ended without its verdict: %v", stream.Err())
	}
	reload := stream.Msg().GetReloadElisp()
	if reload == nil || reload.GetBuild() != harness.PinnedElispBuild || reload.GetModuleRoot() != harness.PinnedCheckout(t) {
		t.Fatalf("first frame = %v, want reload_elisp naming the checkout's root and build", stream.Msg())
	}
	if stream.Receive() {
		t.Fatalf("frame after the reload = %v, want the stream ended", stream.Msg())
	}
	if err := stream.Err(); err != nil {
		t.Fatalf("the stale Emacs's stream ended with %v, want a clean end", err)
	}
	d.AwaitLogRecord(d.RunLogPath(), "the connect-time decision", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.deploy.decide" && r.Context["trigger"] == "connect" &&
			r.Context["running"] == "older-elisp" && r.Context["fresh"] == harness.PinnedElispBuild
	})
}

// ---- the update line: a deploy's progress on the footer --------------------

// footerUpdatePhase names the update line's phase arm on a pushed footer, ""
// when the strip carries none.
func footerUpdatePhase(v *frontendv1.FooterView) string {
	var update *frontendv1.FooterStatusActivityUpdate
	switch arm := v.GetStrip().GetStatus().GetStatus().(type) {
	case *frontendv1.FooterStatus_Idle:
		update = arm.Idle.GetActivity().GetSalient().GetUpdate()
	case *frontendv1.FooterStatus_Working:
		update = arm.Working.GetActivity().GetSalient().GetUpdate()
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
	default:
		return ""
	}
}

// footerUpdated answers the finished deploy's transient on an idle strip, nil
// when none is live.
func footerUpdated(v *frontendv1.FooterView) *frontendv1.FooterActivityTransientUpdated {
	return v.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetTransient().GetUpdated()
}

func TestADeployShowsItsPhasesOnTheFooterAndAnnouncesUpdated(t *testing.T) {
	t.Parallel()
	// Arrange
	// A daemon of its own: a deploy is the daemon's one, and another case's
	// running beside it answers already_deploying.
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	awaitFooter(t, f, footer, "the footer after readiness", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})

	// Act
	deployOutcomes(t, f.d, false)

	// Assert: every phase arrives in order, then the finished deploy takes the
	// salient line down and is announced as the transient updated.
	for _, want := range []string{"building", "installing"} {
		awaitFooter(t, f, footer, "the "+want+" line", func(v *frontendv1.FooterView) bool {
			return footerUpdatePhase(v) == want
		})
	}
	awaitFooter(t, f, footer, "the updated transient", func(v *frontendv1.FooterView) bool {
		return footerUpdatePhase(v) == "" && footerUpdated(v) != nil
	})
}

func TestADeployHandoverShowsTheWaitingWorkspaceAndItsSuccessorSaysUpdated(t *testing.T) {
	t.Parallel()
	// Arrange: a turn in flight, with both participants open so the successor
	// adopts only when they ask it to.
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.HandoverChainTimeout})
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	host := d.WatchHost(f.ws)
	f.web = d.WatchWeb(f.ws)
	harness.AwaitHostView(t, d.Ctx(), host, "the fresh host push")
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
	counts := waiting.GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetUpdate().GetWaiting()
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
	// footer announces the transient updated on every strip.
	d.AwaitRunLogRecordFromAnyProcess("the successor's updated line", func(r harness.LogRecord) bool {
		return r.PID != d.PID() && r.Operation == "daemon.footer.deploy_progress" && r.Context["phase"] == "updated"
	})
	d.AwaitWorkspaceLogRecord(f.repo.Dir, "the updated transient on this workspace's strip", func(r harness.LogRecord) bool {
		return r.PID != d.PID() && r.Operation == "daemon.footer.transient_raised" && r.Context["kind"] == "updated"
	})
}

// A MID-WORK HOLDOUT'S ROW ON THE SUCCESSOR READS THE INCUMBENT'S STATUS.
// Emacs takes its whole roster from the successor during a handover, and the
// successor's own footer state for a workspace the incumbent still serves is
// frozen: on 2026-11-13 a holdout stayed `idle_async` in the sidebar while
// the incumbent published `thinking` six times. The successor mirrors the
// incumbent's own row until it adopts the workspace (rollout.Join's one
// exception, internal/rostermirror). Seam: the successor's WatchWorkspaceRoster
// subscription on the incumbent, between two real daemons.
func TestADeployHandoverDrawsAMidWorkHoldoutsRowFromTheIncumbent(t *testing.T) {
	t.Parallel()
	// Arrange: a turn in flight, with both participants open so the
	// workspace transfers only once it falls free and both have called.
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.HandoverChainTimeout})
	f := drainOpenWorkspace(t, d)
	f.shim.ExpectStartSession()
	host := d.WatchHost(f.ws)
	f.web = d.WatchWeb(f.ws)
	harness.AwaitHostView(t, d.Ctx(), host, "the fresh host push")
	openTurnOn(t, f, "k-holdout-mirror")
	daemonStream := d.WatchDaemonStream()

	// Act: an UNFORCED deploy announces its successor while the turn runs.
	d.StageDeployBuild(harness.DeployStaleDaemon)
	if _, err := d.Client().Deploy(d.Ctx(), connect.NewRequest(&agentreplv1.DeployRequest{})); err != nil {
		t.Fatalf("Deploy = error %v, want the handover accepted", err)
	}
	announced := harness.AwaitView(t, d.Ctx(), daemonStream, "shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced()
	successor := drainDial(announced.GetAddress())
	roster := d.WatchRosterOn(successor)

	// Assert: the successor's row is the incumbent's thinking, not its own.
	harness.AwaitView(t, d.Ctx(), roster, "the holdout's row thinking on the successor", func(r *frontendv1.RosterPush) bool {
		_, thinking := rosterRow(r, f.ws.GetId()).GetStatus().(*frontendv1.RosterRow_Thinking)
		return thinking
	})

	// Act: the turn ends, so the workspace falls free and transfers.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	harness.AwaitView(t, d.Ctx(), host, "transferred", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetTransferred() != nil
	})
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

	// Assert: the adopted row is the successor's own, and the mirror ended
	// with the incumbent.
	if hostErr != nil || webErr != nil {
		t.Fatalf("AdoptHostWorkspace = %v, AdoptWebWorkspace = %v; want both to succeed", hostErr, webErr)
	}
	d.AwaitRunLogRecordFromAnyProcess("the successor's mirror ending with the handover", func(r harness.LogRecord) bool {
		return r.PID != d.PID() && r.Operation == "daemon.rostermirror" &&
			(r.Message == "the incumbent ended its roster stream; every row is this daemon's own" ||
				r.Message == "the mirror stopped; every row is this daemon's own")
	})
}

// deployHandsOver asks CLIENT for an unforced deploy and fails unless its
// daemon decision is a handover. Found live on 2026-09-21: the first deploy
// after a handover was refused as `joining` by a daemon that had finished
// joining minutes before.
func deployHandsOver(t *testing.T, ctx context.Context, client interface {
	Deploy(context.Context, *connect.Request[agentreplv1.DeployRequest]) (*connect.Response[agentreplv1.DeployResponse], error)
}, who string) {
	t.Helper()
	resp, err := client.Deploy(ctx, connect.NewRequest(&agentreplv1.DeployRequest{}))
	if err != nil {
		t.Fatalf("Deploy on %s = error %v, want an answer", who, err)
	}
	if resp.Msg.GetError().GetJoining() != nil {
		t.Fatalf("Deploy on %s = %v, want a handover, not joining", who, resp.Msg)
	}
	for _, o := range resp.Msg.GetSuccess().GetComponents() {
		if o.GetComponent() == agentreplv1.DeployComponent_DEPLOY_COMPONENT_DAEMON && o.GetHandingOver() != nil {
			return
		}
	}
	t.Fatalf("Deploy on %s = %v, want the daemon handing over", who, resp.Msg)
}

// A SUCCESSOR THAT FINISHED JOINING ACCEPTS THE NEXT DEPLOY. Every daemon a
// second deploy talks to booted as a successor, so this is every deploy after
// the first. The staged build stays the stale-daemon one, so the successor,
// running the same binary the incumbent did, finds itself stale in turn and
// hands over to a third daemon, which then serves the workspace.
//
// THE FIRST DAEMON IS STARTED THE WAY EMACS STARTS ONE: naming Doom's env file
// (emacsUserEnv), so every daemon in the chain refreshes the one it spawns
// from it, and each spawned daemon proves, before it serves, that its own
// spawner can spawn the next on the environment it was given (rollout's
// PreflightSpawn). 0.425.14 read that environment's manifest from a path that
// did not exist; it deployed, and the deploy after it could not roll out at
// all, because no test daemon had ever been given an env file.
func TestASuccessorThatFinishedJoiningAcceptsADeploy(t *testing.T) {
	t.Parallel()
	// Arrange: one completed handover of a headless workspace.
	d := harness.StartDaemon(t, harness.Opts{DurableState: true, Timeout: harness.HandoverChainTimeout, ExtraEnv: emacsUserEnv(t)})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	daemonStream := d.WatchDaemonStream()
	d.StageDeployBuild(harness.DeployStaleDaemon)
	deployHandsOver(t, d.Ctx(), d.Client(), "the first incumbent")
	first := harness.AwaitView(t, d.Ctx(), daemonStream, "the first shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced().GetAddress()
	if code := d.AwaitExit(); code != 0 {
		t.Fatalf("the first incumbent's exit code = %d, want an orderly 0", code)
	}
	drainAwaitAddrFileChange(t, d, first)
	handover := d.AwaitRunLogRecordFromAnyProcess("the first handover naming its successor", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.rollout.handover" && r.Context["successor_pid"] != nil
	})
	d.StateServiceBuilds(int(handover.Context["successor_pid"].(float64)))
	successor := drainDial(first)
	announcements := d.WatchDaemonStreamOn(successor)

	// Act
	deployHandsOver(t, d.Ctx(), successor, "the successor")

	// Assert: a THIRD daemon is announced, and it serves the workspace.
	second := harness.AwaitView(t, d.Ctx(), announcements, "the successor's shutdown_announced", func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetShutdownAnnounced() != nil
	}).GetShutdownAnnounced().GetAddress()
	// EVERY HANDOVER ANNOUNCES THE ONE SHARED ADDRESS its socket holder keeps;
	// the third daemon is proved by its own handover record below.
	if second != first {
		t.Fatalf("the second handover announced %q, want the shared address %q", second, first)
	}
	drainAwaitAddrFileChange(t, d, second)
	successorPID := int(handover.Context["successor_pid"].(float64))
	third := d.AwaitRunLogRecordFromAnyProcess("the second handover naming its successor", func(r harness.LogRecord) bool {
		return r.PID == successorPID && r.Operation == "daemon.rollout.handover" && r.Context["successor_pid"] != nil
	})
	thirdPID := int(third.Context["successor_pid"].(float64))
	spawnedOnTheFile := d.AwaitRunLogRecordFromAnyProcess("the first incumbent's spawn taking the env file's setting", func(r harness.LogRecord) bool {
		return r.PID == d.PID() && r.Operation == "daemon.rollout.successor_env" && r.Context["name"] == emacsUserEnvSetting
	})
	preflights := map[string]harness.LogRecord{}
	for who, pid := range map[string]int{"the successor": successorPID, "the third daemon": thirdPID} {
		preflights[who] = d.AwaitRunLogRecordFromAnyProcess(who+"'s spawn preflight", func(r harness.LogRecord) bool {
			return r.PID == pid && r.Operation == "daemon.rollout.spawn_preflight"
		})
	}

	// Assert
	t.Run("the third daemon serves the workspace", func(t *testing.T) {
		if _, err := drainDial(second).SelectWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: ws})); err != nil {
			t.Fatalf("SelectWorkspace on the third daemon = error %v, want the workspace served", err)
		}
	})
	t.Run("the first incumbent spawned its successor on the env file's settings", func(t *testing.T) {
		if spawnedOnTheFile.Level != "info" || spawnedOnTheFile.Context["now_set"] != true {
			t.Fatalf("record = %+v, want the env file's %s taken at INFO", spawnedOnTheFile, emacsUserEnvSetting)
		}
	})
	for _, who := range []string{"the successor", "the third daemon"} {
		t.Run(who+" proved it can spawn its own successor", func(t *testing.T) {
			if rec := preflights[who]; rec.Level != "info" || rec.Context["joining"] != true {
				t.Fatalf("record = %+v, want a joining daemon's passed preflight at INFO", rec)
			}
		})
	}
}

// emacsUserEnvSetting is the per-user setting emacsUserEnv's env file states
// and the daemon's own environment does not: a requirements-manifest `env`
// entry no daemon reads at boot in a way the harness depends on.
const emacsUserEnvSetting = "AGENT_REPL_GITHUB_OWNERS"

// emacsUserEnv is the environment Emacs adds to every daemon it starts
// (lisp/daemon.el agent-repl-daemon--environment): the env file `doom env`
// wrote, Doom's own header and all, and the settings Emacs states itself,
// which the file never overrides. The file states the harness's hotspot, so
// a refreshed successor keeps the world's, and one setting the harness does
// not, so the refresh is seen.
func emacsUserEnv(t *testing.T) []string {
	t.Helper()
	file := filepath.Join(t.TempDir(), "env")
	body := `;; -*- mode: lisp-interaction; coding: utf-8-unix; -*-
;; This file was auto-generated by ` + "`doom env'" + `. It contains a list of environment
;; variables scraped from your default shell (based on your settings for
;; ` + "`doom-env-allow' and `doom-env-deny'" + `).
;;
;; And load it with (doom-load-envvars-file "~/.doom.d/myenv").

("AGENT_REPL_PERSISTENT_WIFI_HOTSPOT=` + harness.FakeHotspot + `"
 "` + emacsUserEnvSetting + `=~/.claude-chain=chain-owner")
`
	if err := os.WriteFile(file, []byte(body), 0o644); err != nil {
		t.Fatalf("write the env file: %v", err)
	}
	return []string{
		userenv.EnvFile + "=" + file,
		userenv.EmacsStated + "=AGENT_REPL_STATE_DIR,MULTI_REPO_ROOT",
	}
}

// ---- a failed deploy: the deploy_failed fault on the footer -----------------

// ---- a failed deploy: on the topbar, and rolled back -----------------------

// buildFailedTopbarLine is the topbar line the harness's failing build stands as.
var buildFailedTopbarLine = "deploy failed: build: " + harness.FakeDeployBuildRefusal

// failSidecarRestart makes the next deploy judge the sidecar stale and its
// service manager restart fail, before AND after the rollback: the cache bin under
// the test's HOME is empty, so the deploy installs into it and then fails.
func failSidecarRestart(t *testing.T, d *harness.Daemon) {
	t.Helper()
	d.StageDeployBuild(harness.DeployCurrent)
	if err := buildreport.Write(d.LockDir, buildreport.ServiceSidecar, buildreport.Report{PID: d.PID(), Build: "an older sidecar"}); err != nil {
		t.Fatalf("state the sidecar's stale build report: %v", err)
	}
	verbs := harness.PlatformServiceVerbs
	for _, verb := range []string{verbs.Restart, verbs.Stop, verbs.Start} {
		d.Services.SetExitCodeFor(verb, 1)
	}
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

// ---- a failed deploy: every client is told --------------------------------

// awaitStanding reads an Emacs daemon stream until a faults_standing push
// satisfies the predicate.
func awaitStanding(t *testing.T, d *harness.Daemon, s *harness.Stream[*agentreplv1.WatchDaemonResponse], what string, pred func([]*agentreplv1.DaemonStandingFault) bool) []*agentreplv1.DaemonStandingFault {
	t.Helper()
	ctx, cancel := d.WaitCtx()
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, func(r *agentreplv1.WatchDaemonResponse) bool {
		return r.GetFaultsStanding() != nil && pred(r.GetFaultsStanding().GetFaults())
	}).GetFaultsStanding().GetFaults()
}

// standsLine reports whether a standing fault carries line.
func standsLine(faults []*agentreplv1.DaemonStandingFault, line string) bool {
	for _, f := range faults {
		if f.GetLine() == line {
			return true
		}
	}
	return false
}

func TestACLIStartedDeployWhoseBuildFailsReachesEmacs(t *testing.T) {
	t.Parallel()
	// Arrange: an Emacs holds the daemon stream, and the deploy is the CLI's.
	d := harness.StartDaemon(t, harness.Opts{ExpectWarnings: []string{"daemon.scriptrunner.run", "daemon.deploy.build", "daemon.deploy.run"}})
	emacs := d.WatchDaemonStream()
	d.StageDeployBuild(harness.DeployFails)
	ctx, cancel := d.WaitCtx()
	defer cancel()
	cli := exec.CommandContext(ctx, harness.DaemonBinary(t), "deploy", "-state-dir", d.StateDir)
	cli.Env = append(os.Environ(), "AGENT_REPL_STATE_DIR="+d.StateDir, "AGENT_REPL_FORBID_VENDOR_CALLS=1")

	// Act
	out, err := cli.CombinedOutput()

	// Assert: the verb failed with the daemon's words, and Emacs was told.
	var exit *exec.ExitError
	if !errors.As(err, &exit) || !strings.Contains(string(out), harness.FakeDeployBuildRefusal) {
		t.Fatalf("agent-repld deploy = (%v, %q), want a non-zero exit carrying the build's words", err, out)
	}
	faults := awaitStanding(t, d, emacs, "the failed deploy on the Emacs stream", func(f []*agentreplv1.DaemonStandingFault) bool {
		return standsLine(f, buildFailedTopbarLine)
	})
	if got := faults[0]; got.GetFaultId() == "" || got.GetFault().GetDeployFailed().GetBuild().GetStep() != "build" {
		t.Fatalf("standing fault = %v, want its id and the typed build arm", got)
	}
}
