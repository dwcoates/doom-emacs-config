//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/integration/harness"
	"agent-repld/internal/ids"
	"agent-repld/internal/rollout"

	"connectrpc.com/connect"
)

// openRaw sends OpenWorkspace exactly as given, without attaching to the
// fake's control socket — for the bring-up-death test, whose shim never
// opens one.
// startRaw opens the fixture's workspace and starts its session through the
// restart verb (an open starts no shim, owner ruling 2026-10-28), answering
// the restart's own outcome.
func (f *fixture) startRaw() (*agentreplv1.RestartWorkspaceResponse, error) {
	f.t.Helper()
	opened, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil || opened.Msg.GetSuccess() == nil {
		f.t.Fatalf("OpenWorkspace = (%v, %v), want a success", opened, err)
	}
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestOpenWorkspaceSpawnsTheFakeShimWithTheContractedArgvAndEnv(t *testing.T) {
	t.Parallel()
	// Arrange / Act
	f := newCanonical(t)
	info := f.shim.Info()

	// Assert: argv.
	if _, ok := info.Flag("--listen"); !ok {
		t.Fatalf("shim argv %v has no --listen", info.Argv)
	}
	if got, ok := info.Flag("--store-socket"); !ok || got != f.d.StoreSocket {
		t.Fatalf("shim --store-socket = %q (present=%v), want the explicitly passed socket %q (flag beats env)", got, ok, f.d.StoreSocket)
	}
	if got, ok := info.Flag("--log-fd"); !ok || got != "3" {
		t.Fatalf("shim --log-fd = %q (present=%v), want \"3\"", got, ok)
	}
	if !info.HasFlag("--fake") {
		t.Fatalf("shim argv %v has no --fake, want the harness's -fake daemon to pass it through", info.Argv)
	}

	// Assert: env.
	if got := info.Env["CLAUDE_CONFIG_DIR"]; got != f.d.DefaultConfigDir {
		t.Fatalf("shim CLAUDE_CONFIG_DIR = %q, want the default account root %q", got, f.d.DefaultConfigDir)
	}
	if got := info.Env["AGENT_REPL_OWNED"]; got != "1" {
		t.Fatalf("shim AGENT_REPL_OWNED = %q, want \"1\"", got)
	}
	if got := info.Env["AGENT_REPL_STATE_DIR"]; got != f.d.StateDir {
		t.Fatalf("shim AGENT_REPL_STATE_DIR = %q, want %q", got, f.d.StateDir)
	}
	if got := info.Env["SHIM_BUILD_SHA"]; got == "" {
		t.Fatalf("shim SHIM_BUILD_SHA is unset, want the daemon's stamp")
	}
	if got := info.Env["AGENT_REPL_FORBID_VENDOR_CALLS"]; got != "1" {
		t.Fatalf("shim AGENT_REPL_FORBID_VENDOR_CALLS = %q, want \"1\"", got)
	}
	// AGENT_REPL_SESSION_ID is the host session identity (shimclient.EnvSessionID,
	// log correlation only): it must equal the very id the host stream serves
	// for this session (agentrepl/v1's HostSessionExisting.id).
	hostView := harness.AwaitView(t, f.d.Ctx(), f.host, "the host session identity",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetHost().GetExisting().GetId().GetValue() != ""
		})
	wantSessionID := hostView.GetHost().GetExisting().GetId().GetValue()
	if got := info.Env["AGENT_REPL_SESSION_ID"]; got == "" || got != wantSessionID {
		t.Fatalf("shim AGENT_REPL_SESSION_ID = %q, want the host session id %q", got, wantSessionID)
	}

	// Assert: cwd.
	if info.Cwd != f.repo.Dir {
		t.Fatalf("shim cwd = %q, want the workspace dir %q", info.Cwd, f.repo.Dir)
	}
}

func TestFakeShimExitingDuringBringUpEndsBringUpImmediately(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonicalRegistered(t)
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.health.session", "daemon.shimclient.exit", "daemon.shimclient.spawn",
		"daemon.workspace.bring_up", "daemon.workspace.restart")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ExitOn: harness.ExitOnStartup, ExitCode: 7, Stderr: "boom: fake bring-up death"})
	footer := f.d.WatchFooter(f.ws)

	// Act
	resp, err := f.startRaw()

	// Assert: bring-up ended on the DEATH — the exit is the evidence, so no
	// timeout was needed to explain it — and the start answered relaunch_failed.
	if err != nil {
		t.Fatalf("the start onto a dying shim = transport error %v, want the relaunch_failed arm", err)
	}
	if resp.GetError().GetRelaunchFailed() == nil {
		t.Fatalf("the start onto a dying shim = %v, want RestartWorkspaceError.relaunch_failed", resp)
	}

	// Assert: the footer shows disconnected.start_failed.
	//
	// The host stream's shim_start_failed fault is NOT asserted here: the
	// workspace has no session record at all (the bring-up died before one was
	// made), so its host view is the `none` arm, which carries no faults.
	awaitFooter(t, f, footer, "footer disconnected.start_failed", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetStartFailed() != nil
	})

	// Assert: the fault IS recorded — SessionHealth is the surface that reads
	// it, since it needs only a registered workspace (never a live session),
	// unlike the host stream's faults, which live on the HostSessionLive arm
	// and so cannot exist for a bring-up that never got that far.
	health, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("SessionHealth after a bring-up death = error %v, want an unhealthy answer", err)
	}
	unhealthy := health.Msg.GetSuccess().GetUnhealthy()
	if unhealthy == nil {
		t.Fatalf("SessionHealth = %v, want SessionHealthUnhealthy", health.Msg)
	}
	var startFailed *agentreplv1.SessionFaultShimStartFailed
	for _, flt := range unhealthy.GetFaults() {
		if sf := flt.GetShimStartFailed(); sf != nil {
			startFailed = sf
		}
	}
	if startFailed == nil {
		t.Fatalf("SessionHealth faults = %v, want a shim_start_failed fault", unhealthy.GetFaults())
	}
	if startFailed.GetExitCode() != 7 {
		t.Fatalf("shim_start_failed.exit_code = %d, want 7", startFailed.GetExitCode())
	}
	if !strings.Contains(startFailed.GetStderrTail(), "boom: fake bring-up death") {
		t.Fatalf("shim_start_failed.stderr_tail = %q, want it to carry the fake's stderr", startFailed.GetStderrTail())
	}
}

func TestReopeningAWorkspaceWithAPriorSessionResumesItsVendorSession(t *testing.T) {
	t.Parallel()
	// Arrange: open once to mint a vendor session, then kill it so the
	// workspace's session record carries a vendor id with no live shim.
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.kill")
	f.shim.ExpectStartSession()
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()

	// Act: reopen.
	harness.OpenAndStart(t, f.d, f.ws)
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")

	// Assert
	req := shim.ExpectStartSession()
	resume := req.GetResume()
	if resume == nil {
		t.Fatalf("StartSession request = %v, want a resume source for a workspace with a prior vendor session", req)
	}
	if resume.GetVendorSessionId() != vendorID {
		t.Fatalf("StartSession(resume).vendor_session_id = %q, want the prior session's %q", resume.GetVendorSessionId(), vendorID)
	}
}

func TestStartSessionResumeColdStandsAGateBlockingReopenUntilAnswered(t *testing.T) {
	t.Parallel()
	// Arrange: kill to get a resumable session, then script the resume as cold.
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.remoteControlOff()
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens:   123456,
		LastRequestAtMS: 1_700_000_000_000,
		RequestedModel:  "sonnet",
		CacheTTLMS:      300_000,
	}})
	feed := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)
	host := f.d.WatchHost(f.ws)

	// Act
	harness.OpenAndStart(t, f.d, f.ws)
	req := f.gateAttempt()
	if req.GetResume().GetColdRemediation() != nil {
		t.Fatalf("the first resume attempt named a remediation %v, want the daemon's first attempt to learn the cost before paying it", req.GetResume().GetColdRemediation())
	}

	// Assert: a standing FeedColdGate row with the shim's facts.
	gateRow := awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})
	standing := gateRow.GetColdGate().GetStanding()
	if standing.GetContextTokens().GetTokens() != 123456 {
		t.Fatalf("cold gate context_tokens = %d, want 123456", standing.GetContextTokens().GetTokens())
	}

	// Assert: footer waiting.cold_gate.
	awaitFooter(t, f, footer, "footer waiting.cold_gate", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWaiting().GetColdGate() != nil
	})

	// Assert: the host view carries the gate, so Emacs hides the input while
	// the webapp docks the banner.
	harness.AwaitView(t, f.d.Ctx(), host, "host gate cold_gate", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetGate().GetColdGate() != nil
	})

	// Assert: no re-open until AnswerColdGate — the gate's shim is stood
	// down, so no shim runs to re-open through until the answer spawns one.
	f.awaitGateShimStoodDown()

	// Act: AnswerColdGate{pay}.
	if _, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
	})); err != nil {
		t.Fatalf("AnswerColdGate{pay} = error %v, want a success", err)
	}

	// Assert: the shim the answer spawned sees the retry with
	// cold_remediation{pay}.
	retry := f.answerShim().ExpectStartSession()
	if retry.GetResume().GetColdRemediation().GetPay() == nil {
		t.Fatalf("the retry's cold_remediation = %v, want {pay}", retry.GetResume().GetColdRemediation())
	}

	// Assert: the answered gate leaves the host view, so the input returns.
	harness.AwaitView(t, f.d.Ctx(), host, "host gate absent", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost() != nil && r.GetHost().Gate == nil
	})
}

// ---- critique 4: topbar account for a MULTI_REPO_ROOT workspace ----

// ---- critique 5: account-switch transcript porting ----

func TestAccountSwitchPortsTheTranscriptAcrossADaemonBoot(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace OUTSIDE the multi-repo root, opened so it has a
	// vendor transcript filed under the default root.
	f := newOpened(t, harness.Opts{DurableState: true})
	// The arrangement KILLS the session before the reboot, and that trail is
	// evidence of the kill it asked for.
	expectSessionKillRecords(f.d)
	f.shim.ExpectStartSession()
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}
	if !harness.HasTranscript(f.d.DefaultConfigDir, f.repo.Dir, vendorID) {
		t.Fatalf("no transcript filed under the default root at %s, want the fake shim to have laid one down at StartSession",
			harness.TranscriptPath(f.d.DefaultConfigDir, f.repo.Dir, vendorID))
	}
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()

	// Act: reboot on the SAME state root and account roots, but with
	// $MULTI_REPO_ROOT now covering this workspace's directory -- the account
	// switch across a boot.
	f.d.Kill()
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		ExtraArgs: []string{
			"--default-config-dir", f.d.DefaultConfigDir,
			"--multi-repo-config-dir", f.d.MultiRepoConfigDir,
		},
		ExtraEnv: []string{
			"AGENT_REPL_LOCK_DIR=" + f.d.LockDir,
			"MULTI_REPO_ROOT=" + filepath.Dir(f.repo.Dir),
		},
	})
	harness.OpenAndStart(t, successor, f.ws)

	// Assert: the transcript was MOVED into the newly routed root before the
	// resume, and gone from the old one.
	if harness.HasTranscript(f.d.DefaultConfigDir, f.repo.Dir, vendorID) {
		t.Fatalf("the transcript is still under the old root %s after the account switch, want it moved", f.d.DefaultConfigDir)
	}
	if !harness.HasTranscript(f.d.MultiRepoConfigDir, f.repo.Dir, vendorID) {
		t.Fatalf("the transcript was not found under the newly routed root %s after the account switch", f.d.MultiRepoConfigDir)
	}

	// Assert: CLAUDE_CONFIG_DIR flips to the newly routed root.
	if got := successor.Shim(f.ws).Info().Env["CLAUDE_CONFIG_DIR"]; got != f.d.MultiRepoConfigDir {
		t.Fatalf("CLAUDE_CONFIG_DIR = %q after the account switch, want the newly routed root %q", got, f.d.MultiRepoConfigDir)
	}
}

func TestKillWorkspaceForceKillsTheSessionAndReapsTheShim(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// A DELIBERATE TEARDOWN DECLARES NOTHING, and the sweep at cleanup is what
	// proves it. A kill asks the shim for exactly what then happens: it ends
	// its standing streams and exits, and every side that observes that reads
	// the stand-down latch the ask set. What this list used to hold -- the two
	// standing streams' ends, the severed link, the fault that followed it and
	// the shim's own clean exit -- was this daemon recording its own act as
	// five failures, and it is what put thirteen records in a realtest run.
	f.shim.ExpectStartSession()
	host := f.d.WatchHost(f.ws)
	roster := f.d.WatchRoster()

	// Act
	resp, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("KillWorkspace = %v, want a success", resp.Msg)
	}

	// Assert: KillSession{force:true} was sent. It is read from the shim's own
	// LOG, not its control socket: KillWorkspace reaps the process before it
	// answers, and the fake's in-memory recorder dies with it.
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if !killed.GetForce() {
		t.Fatalf("KillSession.force = false, want true (KillWorkspace is the forced tear-down)")
	}

	// Assert: the shim process is reaped.
	f.shim.AwaitGone()

	// Assert: HostWorkspace shows terminal{rehydratable:true}.
	awaitRow_ := harness.AwaitView(t, f.d.Ctx(), host, "terminal{rehydratable:true}", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		term := r.GetHost().GetExisting().GetTerminal()
		return term != nil && term.GetRehydratable()
	})
	_ = awaitRow_

	// Assert: the roster shows the killed workspace INACTIVE. A kill marks the
	// workspace closed in its fast half, before its session dies, so the tab
	// goes the moment the verb is accepted (BeginKill); a closed workspace with
	// nothing live behind it is `inactive`, which sits above every session
	// state (sidebar status.go), so the session's death never reads `dead`.
	awaitRoster(t, f.d, roster, "the roster row inactive and closed", func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetInactive() != nil && row.GetClosed().GetClosed()
	})
}

func TestRestartWorkspaceForcedInterruptsFirst(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a graceful stand-down the fake shim ends by exiting, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWorkspaceRestartWarnings()
	f.shim.ExpectStartSession()
	f.submit("long running work", "k-restart-forced", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()

	// Act
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("RestartWorkspace{force:true} = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, want a success", resp.Msg)
	}
	// A forced restart interrupts THE TURN first -- KillTurn{force:true} --
	// and the stand-down that follows is FORCED too: a forced bounce does not
	// wait for in-flight work, and whatever is still live (monitors, background
	// shells, background subagents) runs inside the shim's vendor child and
	// ends with it (endpoint_deploy.proto, DeployRequest.force).
	// THE KILL IS READ FROM THE OLD SHIM'S DURABLE LOG: the restart answers
	// once it HAPPENED (owner ruling, 2026-10-08), by when the old shim has
	// exited and its control socket is closed.
	killedTurn := &shimv1.KillTurnRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillTurn, killedTurn)
	if !killedTurn.GetForce() {
		t.Fatalf("KillTurn.force = false on a forced restart, want true: the running turn is interrupted rather than waited out")
	}
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if !killed.GetForce() {
		t.Fatalf("KillSession.force = false on a forced restart, want the forced stand-down")
	}
}

func TestHibernationParksAnIdleSessionAndRevivesOnPrompt(t *testing.T) {
	t.Parallel()
	// Arrange: a very short idle cutoff so hibernation fires promptly.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50, ExpectWarnings: []string{"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session", "daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up"}})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	// THE START IS READ FROM THE SHIM'S DURABLE LOG, never popped live. At a
	// 50ms cutoff the sweep can hibernate the shim, which then exits, before
	// this line runs, and a live pop then met a closed control socket
	// ("broken pipe") on a loaded host.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	roster := f.d.WatchRoster()

	// Assert: Hibernate then KillSession fire once the session goes idle. Both
	// are read from the shim's LOG rather than its control socket: the fake
	// exits on the accepted KillSession, so the socket is gone before a second
	// control round trip can complete.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if killed.GetForce() {
		t.Fatalf("hibernation's KillSession.force = true, want a quiet close (the session is already compacted and idle)")
	}
	// Assert: the ORDER — the shim's hibernate ack is what earns the
	// stand-down, so KillSession is never sent before it. The shim's own log
	// is the record of both, and its order is the order they arrived in.
	assertShimRequestOrder(t, f, harness.RPCHibernate, harness.RPCKillSession)
	f.shim.AwaitGone()

	// Assert: the roster keeps an IDLE arm. A hibernation is a park, not a
	// fault: the frontend must not be able to tell a parked workspace from an
	// idle one, so `severed` and `dead` are both wrong here.
	awaitRoster(t, f.d, roster, "the parked roster row staying idle", func(r *frontendv1.RosterPush) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && (row.GetReady() != nil || row.GetDone() != nil)
	})

	// Assert: the host stream keeps the workspace LIVE with the shim
	// unattached, which is the whole of what a park is allowed to show.
	host := f.d.WatchHost(f.ws)
	harness.AwaitView(t, f.d.Ctx(), host, "the parked workspace live with the shim unattached", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		live := r.GetHost().GetExisting().GetLive()
		return live != nil && !live.GetShimAttached()
	})

	// Act: a prompt revives the session.
	f.submit("wake up", "k-hibernation-revive", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")

	// Assert: revival resumes the vendor session.
	req := shim.ExpectStartSession()
	if req.GetResume() == nil {
		t.Fatalf("StartSession on revival = %v, want a resume source", req)
	}

	// Assert: the prompt is delivered once the revived session is ready.
	shim.ExpectStartTurn()
}

// TestABounceOfANeverTurnedSessionComesUpFresh is the e2e run-2 repro as an
// assertion: a session that pre-minted a vendor session id and never took a
// turn has NO transcript, so the staleness bounce's resume named a
// conversation the shim rightly refuses as `unknown_session`, no client was
// installed, and every later prompt answered `no_session` forever.
func TestABounceOfANeverTurnedSessionComesUpFresh(t *testing.T) {
	t.Parallel()
	// Arrange: the fake reports a build that is not the installed bundle's, so
	// the mount bounces the shim — and the fake withholds the transcript until the
	// first turn, exactly as the vendor does.
	f := newRegistered(t, harness.Opts{ExtraEnv: []string{"FAKESHIM_BUILD_SHA=older-build"}})
	f.d.ExpectWarnings("daemon.account.find_transcript",
		"daemon.workspace.bring_up", "daemon.workspace.kill",
		// The fake's replacement reports the SAME older build (its
		// FAKESHIM_BUILD_SHA outlives the bounce), which the staleness judge
		// names as a bounce that did not take.
		"daemon.rollout.staleness")
	// The sweep covers every test; the declared records are evidence of the bounce's stand-down, the abandoned conversation the classifier records, and the shim death the bounce drives.
	f.d.ExpectWorkspaceRestartWarnings()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{NoTranscriptUntilTurn: true})

	// Act
	if resp, err := f.startRaw(); err != nil || resp.GetSuccess() == nil {
		t.Fatalf("the start = (%v, %v), want a success", resp, err)
	}
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the completed build-staleness bounce", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.relaunch" && r.Message == "relaunched the workspace's shim"
	})

	// Assert: the bounce came up FRESH rather than failing its resume, and the
	// workspace takes prompts afterwards.
	for _, r := range f.d.RunLog() {
		if r.Message == "the resume failed; the workspace carries its own error" {
			t.Fatalf("the bounce recorded a relaunch_resume_failed fault: %+v", r)
		}
	}
	resp := f.submit("after the bounce", "k-bounce-fresh", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt after the bounce = %v, want an accepted prompt rather than no_session", resp)
	}
}

func TestBuildStalenessBounceRelaunchesAStaleShimAtFreeness(t *testing.T) {
	t.Parallel()
	// Arrange: the fake reports a build that is not the installed bundle's, so
	// the mount finds the session on an older build.
	f := newRegistered(t, harness.Opts{ExtraEnv: []string{"FAKESHIM_BUILD_SHA=older-build"}})
	// THE STAND-DOWN ITSELF IS SILENT NOW. What this list held for the two
	// standing streams, the severed link and the fault behind it was the
	// daemon recording its own act once per observer; every observer reads the
	// shim client's stand-down latch instead, so a bounce this daemon ordered
	// is an ordinary event on every side that sees it.
	//
	// WHAT REMAINS IS THE FAKE'S OWN VIOLENCE, and it is left loud on purpose:
	// the fake shim EXITS while answering KillSession, so the rpc it was
	// answering genuinely fails, the relaunch genuinely waits its window out
	// before forcing, and the client's redial genuinely races the death. Those
	// are failures, not the teardown.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.rollout.relaunch",
		"daemon.shimclient.kill_session", "daemon.shimclient.redial",
		// The replacement reports the same older build, which the staleness
		// judge names as a bounce that did not take -- and does not repeat.
		"daemon.rollout.staleness")
	if resp, err := f.startRaw(); err != nil || resp.GetSuccess() == nil {
		t.Fatalf("the start = (%v, %v), want a success", resp, err)
	}

	// Assert: the stale shim was bounced onto the deployed build. The daemon's
	// own record is the assertion: the relaunch prelaunches beside the running
	// shim and swaps, so no single control socket spans the bounce.
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the completed build-staleness bounce", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.relaunch" && r.Message == "relaunched the workspace's shim"
	})
	spawns := f.d.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn")
	if spawns < 2 {
		t.Fatalf("shim spawns = %d, want the original plus the bounce's replacement", spawns)
	}

	// Act: mount again. The relaunched shim reports the SAME older build.
	if opened, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil || opened.Msg.GetSuccess() == nil {
		t.Fatalf("the second OpenWorkspace = (%v, %v), want a success", opened, err)
	}

	// Assert: THE BOUNCE FIRES ONCE PER STAMP. A check that did not remember
	// what it had already bounced for would bounce again on every mount,
	// spawning a process per round forever.
	if got := f.d.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn"); got != spawns {
		t.Fatalf("shim spawns = %d after a second mount, want the %d already made: the stamp was already bounced for", got, spawns)
	}
}

func TestCrashBootAdoptsARunningShimWithoutASecondSpawn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	watchesBefore := f.shim.Count(harness.RPCWatchSession)
	if watchesBefore == 0 {
		t.Fatal("the fake reports no WatchSession opens before the crash, want at least one from the first daemon")
	}

	// Act: kill the daemon with SIGKILL while the fake shim holds its locks.
	f.d.Kill()

	successor := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir}})

	// Assert: no second spawn — the same fake process, still reachable at
	// the same control socket, is adopted rather than replaced.
	shim := successor.ShimAt(successor.SocketPath(f.ws) + ".ctl")
	info := shim.Info()
	if info.PID != f.shim.Info().PID {
		t.Fatalf("the successor talks to pid %d, want the SAME surviving fake pid %d (adoption, no second spawn)", info.PID, f.shim.Info().PID)
	}

	// Assert: the fake sees a NEW WatchSession from the successor. The daemon's
	// own record of re-opening it is the synchronization point — the adoption
	// runs at boot, off the rpc path, so there is nothing else to wait on.
	successor.AwaitWorkspaceLogOperationCount(f.repo.Dir, "daemon.sessionwatcher.watch_session", 1)
	// The daemon's own record and the fake's counter are two observers of the
	// same open, and neither orders the other; the fake's recorder answers
	// once it has taken the successor's open.
	shim.AwaitCount(harness.RPCWatchSession, watchesBefore+1)

	// The exact "intent manifest absent -> UNKNOWN/PRESERVED per session in
	// the host faults, never a count" vocabulary names no generated arm this
	// suite could find (see report): the observable adoption facts above are
	// asserted; the UNKNOWN/PRESERVED classification itself is not.
}

// ---- critique 3: CloseWorkspace with a held prompt ----

func TestRestartWorkspaceReapsTheOldShimBeforeResumingOnTheNew(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, the shim death the stand-down drives, the shim link it severs.
	f.d.ExpectWorkspaceRestartWarnings()
	f.shim.ExpectStartSession()
	oldPID := f.shim.Info().PID

	// Act: a restart is immediate, with nothing to wait on.
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = (%v, %v), want a success", resp, err)
	}

	// Assert: the resume reaches the NEW shim.
	resume := f.d.ShimAt(prelaunchControlSocket(f.d, f.ws, 1)).ExpectStartSession()
	if resume.GetResume() == nil {
		t.Fatalf("StartSession on the prelaunched shim = %v, want a resume source", resume)
	}

	// Assert: by the time that resume arrived, the old process was ALREADY
	// gone. internal/rollout/relaunch.go's standDown passes the reap gate
	// (<-exited) before Install and Resume are ever called, so this is a
	// near-instant confirmation of an already-settled fact, not a wait.
	harness.AwaitProcessExit(t, shortTimeout(t, f.d.Ctx(), 200*time.Millisecond), oldPID)
}

// TestRestartWorkspaceForcedContinuesTheInterruptedTurnWithoutResubmittingIt
// pins both halves of a restart's cut: the interrupted prompt is never sent
// again as a fresh turn, and the cut work is continued by the resumption
// (owner ruling, 2026-10-30), the restarted session's one turn.
func TestRestartWorkspaceForcedContinuesTheInterruptedTurnWithoutResubmittingIt(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a graceful stand-down the fake shim ends by exiting, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWorkspaceRestartWarnings()
	f.shim.ExpectStartSession()
	f.submit("long running work", "k-restart-forced-no-redrive", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()

	// Act
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("RestartWorkspace{force:true} = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, want a success", resp.Msg)
	}
	// THE KILL IS READ FROM THE OLD SHIM'S DURABLE LOG: the restart answers
	// once it HAPPENED (owner ruling, 2026-10-08), by when the old shim has
	// exited and its control socket is closed.
	killedTurn := &shimv1.KillTurnRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillTurn, killedTurn)
	if !killedTurn.GetForce() {
		t.Fatalf("KillTurn.force = false on a forced restart, want true: the running turn is interrupted rather than waited out")
	}

	second := f.d.ShimAt(prelaunchControlSocket(f.d, f.ws, 1))
	resume := second.ExpectStartSession()
	if resume.GetResume() == nil {
		t.Fatalf("StartSession on the resumed shim = %v, want a resume source", resume)
	}

	// Assert: the one turn the resumed shim is asked to start is the
	// resumption, never the interrupted prompt resubmitted.
	if st := second.ExpectStartTurn(); st.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_RESUME_AFTER_RESTART {
		t.Fatalf("the resumed shim's turn has origin %v, want the resumption, never the cut prompt again", st.GetOrigin())
	}
	expectRPCCount(t, second, harness.RPCStartTurn, 1)
}

// ---- critique 13: bounce accountability ----

// TestCrashBootWithNoManifestRecordsNoBounceUnknownForAnAdoptedWorkspace
// covers BOUNCE ACCOUNTABILITY for the case the manifest cannot describe: the
// outgoing daemon crashed or was force-killed, so it wrote NO manifest at all.
//
// A SESSION THE SUCCESSOR ADOPTED IS ACCOUNTED FOR BY ITS ADOPTION: it
// survived, a process answered the dial, and it is served. So the successor
// records it PRESERVED and resolved, and records NO bounce_unknown for it at
// all -- no WARN, no fault, nothing for the healthy attach to close. Before,
// the boot recorded bounce_unknown at WARN ("needs a human") for the workspace
// it had just adopted, and the adoption's healthy attach closed it at once. A
// session the lock says survived and the boot could NOT adopt still records
// bounce_unknown and keeps it standing; that is pinned where it can be staged
// (internal/rollout TestNoManifestLeavesAnOpenBounceUnknownOnlyForASessionNobodyAdopted,
// internal/boot TestTheReconcileIsHandedEverySessionTheBootCouldNotAdopt).
func TestCrashBootWithNoManifestRecordsNoBounceUnknownForAnAdoptedWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace whose shim SURVIVES the daemon's death, so
	// the successor adopts it.
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()

	// Act: kill the daemon and leave the shim (and its workspace lock) alone,
	// writing no manifest — which is exactly what a crash leaves behind.
	f.d.Kill()
	successor := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir}})

	// Assert: the adoption is what accounts for the session, at INFO.
	successor.AwaitLogRecord(successor.RunLogPath(), "the adopted survivors accounted for", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.reconcile" && r.Level == "info" &&
			r.Message == "sessions survived a bounce that wrote no intent manifest; this boot adopted every one, which accounts for it"
	})
	// Assert: no bounce_unknown was recorded, so none was closed either. The
	// reconcile is one synchronous boot step, and the record above is its
	// summary, so everything it wrote is on disk by now. The cleanup sweep
	// fails on any WARN, which is the no-WARN half.
	for _, r := range successor.RunLog() {
		if r.Context["kind"] == "bounce_unknown" || r.Message == "a session's bounce disposition needs a human" {
			t.Fatalf("record %+v: an adopted workspace must record no bounce_unknown at all", r)
		}
	}
}

func TestCrashBootWithADeadManifestPidTakesTheOrdinaryDeadShimPath(t *testing.T) {
	t.Parallel()
	// Arrange: an opened workspace whose shim will be gone before restart.
	f := newOpened(t, harness.Opts{DurableState: true})
	f.shim.ExpectStartSession()
	oldPID := f.shim.Info().PID
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}

	// Kill the daemon AND the shim: the workspace lock reads FREE at restart,
	// which is what "a pid that is gone" means to the lock probe the
	// disposition is computed against.
	f.d.Kill()
	if proc, err := os.FindProcess(oldPID); err == nil {
		_ = proc.Signal(syscall.SIGKILL)
	}
	f.d.AwaitProcessExit(oldPID)

	// A manifest naming this session as one the outgoing daemon meant to
	// PRESERVE, whose pid is now gone: disposition(preserve, free) = DIED.
	writeIntentManifest(t, f.d, rollout.ManifestSession{
		Workspace:       ids.WorkspaceID(f.ws.GetId()),
		Dir:             f.repo.Dir,
		ShimPID:         oldPID,
		VendorSessionID: vendorID,
		Intent:          rollout.IntentPreserve,
	})

	// Act: restart on the same state root.
	successor := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir}})

	// Assert: the death is recorded, resolved, and says it is the ordinary
	// dead-shim path -- no WARN, and nothing left standing on the strip that
	// no verb could ever close. The successor's cleanup sweep fails on any
	// undeclared warning, which is the no-WARN half.
	successor.AwaitLogRecord(successor.RunLogPath(), "the resolved DIED disposition", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.rollout.reconcile" && r.Level == "info" && r.Context["disposition"] == "DIED"
	})
}

// ---- critique 14: SessionStarted.live_work ----

// ---- critique 17 (this agent's share): AnswerColdGate{clear} ----

// ---- critique 25 (this agent's share): KillWorkspace preserves data ----

// assertShimRequestOrder fails unless the named rpcs appear in the fake shim's
// own request log in the order given. The log is the shim's record of what
// arrived, so its order IS the arrival order.
func assertShimRequestOrder(t *testing.T, f *fixture, first, second string) {
	t.Helper()
	firstAt, secondAt := -1, -1
	for i, r := range f.d.WorkspaceLog(f.repo.Dir, "shim") {
		switch r.Operation {
		case "shim.fake." + first:
			if firstAt < 0 {
				firstAt = i
			}
		case "shim.fake." + second:
			if secondAt < 0 {
				secondAt = i
			}
		}
	}
	if firstAt < 0 || secondAt < 0 {
		t.Fatalf("the shim log records %s at %d and %s at %d, want both present", first, firstAt, second, secondAt)
	}
	if firstAt > secondAt {
		t.Fatalf("the shim received %s before %s, want %s first", second, first, first)
	}
}

// ---------------------------------------------------------------------------
// audit-3 critique 4: a hibernate refusal or failure DEFERS the stand-down —
// the sweep never forces a session down over a hibernate it could not get an
// ack for. internal/drain/sweep.go's hibernate returns false (deferring)
// before ever calling KillSession on both the transport-failure and the
// typed-refusal branches.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// audit-3 critique 5: spawn-on-mount revival.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// audit-3 critique 20: cold-gate and interrupt arms.
// ---------------------------------------------------------------------------

// A VENDOR THAT FAILS TO START INSIDE A HEALTHY SHIM IS A TYPED REFUSAL. The
// shim process is up and serving — only its StartSession answer is a refusal —
// so the daemon relays the shim's own verdict rather than letting it escape as
// an untyped Connect internal. LANDING 9 landed the arm:
// OpenWorkspaceError.vendor_start_failed carries the shim's `detail`.
func TestAVendorStartFailureIsRelayedByNameAndNeverEscapesAsInternal(t *testing.T) {
	t.Parallel()
	// Arrange
	const shimDetail = "the vendor SDK threw before its first message"
	f := newCanonicalRegistered(t)
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.workspace.open",
		"daemon.health.session", "daemon.shimclient.exit", "daemon.shimclient.redial", "daemon.shimclient.spawn")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{VendorStartFailed: shimDetail})

	// Act
	resp, err := f.startRaw()

	// Assert
	if err != nil {
		t.Fatalf("a start onto a refused vendor start = transport error %v, want the relaunch_failed arm", err)
	}
	failed := resp.GetError().GetRelaunchFailed()
	if failed == nil {
		t.Fatalf("the start = %v, want error.relaunch_failed", resp)
	}
	if !strings.Contains(failed.GetDetail(), shimDetail) {
		t.Fatalf("relaunch_failed.detail = %q, want it to carry the shim's own account %q", failed.GetDetail(), shimDetail)
	}
}

// A VENDOR THAT IS SLOW TO START IS RETRIED, AND THE PROMPT WAITS FOR IT
// (vendor-start resilience, 2026-10-02). The fake answers the first two
// StartSessions with a RETRYABLE vendor_start_failed; the daemon asks the same
// shim again on its backoff, and the prompt submitted while the session was
// not up is held under the reconnect hold and delivered once it is.
func TestARetriedVendorStartDeliversThePromptHeldWhileItRetried(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonicalRegistered(t)
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{
		VendorStartFailTimes:  2,
		VendorStartFailDetail: "supportedModels did not answer in 3s",
	})

	// Act
	resp := f.submit("hello after the retries", "k-vendor-retry", origin)

	// Assert
	turn := resp.GetSuccess().GetTurn().GetTurn().GetValue()
	if turn == "" {
		t.Fatalf("SubmitPrompt = %v, want a minted turn held for the session", resp)
	}
	shim := f.d.Shim(f.ws)
	started := shim.ExpectStartTurn()
	if got := started.GetTurn().GetValue(); got != turn {
		t.Fatalf("StartTurn = turn %q, want the held prompt %q delivered once the session came up", got, turn)
	}
	if got := shim.Count(harness.RPCStartSession); got != 3 {
		t.Fatalf("StartSession calls = %d, want two refused and one that started", got)
	}
}

// A FAILED START KEEPS ITS SHIM HELD, AND THE NEXT START REUSES IT: the
// vendor rejects the first StartSession outright, and the next open starts
// the session on the SAME shim -- never a second spawn, never an adoption of
// the shim this daemon already holds.
func TestTheStartAfterARejectedVendorStartReusesItsShim(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonicalRegistered(t)
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.workspace.restart")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{VendorStartRejectTimes: 1, VendorStartFailDetail: "invalid api key"})
	if resp, err := f.startRaw(); err != nil || resp.GetError().GetRelaunchFailed() == nil {
		t.Fatalf("the start = (%v, %v), want the vendor's rejection", resp, err)
	}

	// Act: a prompt is what starts it next.
	resp := f.submit("after the rejection", "k-after-reject", origin)

	// Assert
	if resp.GetError() != nil {
		t.Fatalf("SubmitPrompt = %v, want the prompt taken", resp)
	}
	f.d.Shim(f.ws).ExpectStartTurn()
	if got := f.d.Shim(f.ws).Count(harness.RPCStartSession); got != 2 {
		t.Fatalf("StartSession calls on the one shim = %d, want the rejected one and the one that started", got)
	}
}

// TestAParkedWorkspaceKeepsAnOpenComposerOnItsHostView is the host half of the
// same park, and the assertion the sibling lease-republish fix
// (drain.Deps.PublishHost) exists for: the host view's composer arm is
// composed from the OCCUPANCY LEASE, so every push taken while the
// hibernation lease stood said `draining`, and Emacs's input.el refuses a
// submission on that gate ("composer closed: daemon draining").
//
// That closes the only revival path a hibernated workspace has, because
// reviving it IS a prompt. TestHibernationParksAnIdleSessionAndRevivesOnPrompt
// already asserts `existing.live` with shim_attached=false; the COMPOSER is
// what a client reads to decide it may submit at all, and nothing asserted it.
//
// COST: the same 50ms cutoff and the same two shim round trips as the roster
// test above, plus one host push; measured at 0.31s wall inside the parallel
// suite.
func TestAParkedWorkspaceKeepsAnOpenComposerOnItsHostView(t *testing.T) {
	t.Parallel()
	// Arrange: a very short idle cutoff so hibernation fires promptly.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50, ExpectWarnings: []string{"daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session", "daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up"}})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	// THE START IS READ FROM THE SHIM'S DURABLE LOG, never popped live. At a
	// 50ms cutoff the sweep can hibernate the shim, which then exits, before
	// this line runs, and a live pop then met a closed control socket
	// ("broken pipe") on a loaded host.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	// THE STREAM IS OPENED BEFORE THE PARK, which is what makes this a
	// regression guard rather than a re-resolution. WatchHostWorkspace
	// composes a fresh view for each new subscriber, so a client that
	// subscribes AFTER the lease is gone reads an open composer whether or not
	// anything republished; the client this defect was reported against had
	// been streaming since the mount, and its last push was the stale one.
	host := f.d.WatchHost(f.ws)
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, &shimv1.KillSessionRequest{})

	// Act
	f.shim.AwaitGone()

	// Assert: live, unattached, and OPEN — the three facts a client needs to
	// be allowed to type the prompt that revives the session.
	settled := harness.AwaitView(t, f.d.Ctx(), host, "the parked workspace's composer reopening", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		live := r.GetHost().GetExisting().GetLive()
		return live != nil && !live.GetShimAttached() && live.GetOpen() != nil
	})
	if live := settled.GetHost().GetExisting().GetLive(); live.GetDraining() != nil {
		t.Fatalf("the parked workspace's composer = draining, want open: %v", live)
	}
}

// TestAPromptRevivesAWorkspaceWhoseShimWasKilled is the headless-run defect
// end to end. A workspace ran a turn to done, its shim was SIGKILLed out from
// under the daemon, and the roster settled on `dead` — and the NEXT prompt was
// delivered to the dead client rather than reviving the session: StartTurn
// dialed a socket nothing was listening on, SubmitPrompt answered
// `unavailable`, and the workspace stayed dead forever with the prompt held as
// an outage. A dead shim is not a live client, so the submission takes the
// same revival path a hibernated workspace takes.
func TestAPromptRevivesAWorkspaceWhoseShimWasKilled(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of the shim death this test drives, the link it severs, and the bring-up the revival runs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.workspace.bring_up")
	f.shim.ExpectStartSession()
	roster := f.d.WatchRoster()
	statusIs := func(pred func(*frontendv1.RosterRow) bool) func(*frontendv1.RosterPush) bool {
		return func(r *frontendv1.RosterPush) bool {
			row := rosterRow(r, f.ws.GetId())
			return row != nil && pred(row)
		}
	}

	// Arrange: one turn runs to done, so the session has a conversation to
	// resume — the very state a headless run was in when the shim died.
	f.submit("do the thing", "k-dead-revive-1", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	// The daemon's own opening of the turn, not merely the shim's receipt of
	// it: see the note on the parked-footer test above.
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitRoster(t, f.d, roster, "done after the first turn", statusIs(func(row *frontendv1.RosterRow) bool {
		return row.GetDone() != nil
	}))

	// Arrange: the shim is killed out from under the daemon.
	pid := f.shim.Info().PID
	killShim(t, f, f.shim)
	awaitRoster(t, f.d, roster, "the roster row dead after the shim was killed", statusIs(func(row *frontendv1.RosterRow) bool {
		return row.GetDead() != nil
	}))

	// Act
	f.submit("wake up", "k-dead-revive-2", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	revived := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")

	// Assert: a NEW shim is brought up on the recorded conversation.
	req := revived.ExpectStartSession()
	if req.GetResume() == nil {
		t.Fatalf("StartSession on the revival = %v, want a resume source (the session took a turn)", req)
	}
	if revivedPID := revived.Info().PID; revivedPID == pid {
		t.Fatalf("the revival's shim pid = %d, want a process other than the killed %d", revivedPID, pid)
	}

	// Assert: the held prompt is delivered once the revived session is ready.
	revived.ExpectStartTurn()
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 2)

	// Assert: the roster row LEAVES dead and settles on the revived turn.
	revived.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))
	awaitRoster(t, f.d, roster, "the revived roster row settling out of dead", statusIs(func(row *frontendv1.RosterRow) bool {
		return row.GetDone() != nil
	}))
}
