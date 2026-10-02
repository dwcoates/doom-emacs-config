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

	"claude-repld/integration/harness"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"

	"connectrpc.com/connect"
)

// openRaw sends OpenWorkspace exactly as given, without attaching to the
// fake's control socket — for the bring-up-death test, whose shim never
// opens one.
func (f *fixture) openRaw() (*agentreplv1.OpenWorkspaceResponse, error) {
	f.t.Helper()
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestOpenWorkspaceSpawnsTheFakeShimWithTheContractedArgvAndEnv(t *testing.T) {
	t.Parallel()
	// Arrange / Act
	f := newOpened(t, harness.Opts{})
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

func TestReadinessGatesOnTheFirstHealthyDiagnostics(t *testing.T) {
	t.Parallel()
	// Arrange: a shim that withholds its opening diagnostics.
	f := newRegistered(t, harness.Opts{})
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{DelayDiagnostics: true})
	host := f.d.WatchHost(f.ws)

	// Act: open. Bring-up BLOCKS on readiness, so it runs in the background
	// while the test drives the delayed diagnostics from the other side.
	done := make(chan error, 1)
	go func() {
		_, err := f.openRaw()
		done <- err
	}()

	// Assert: while diagnostics is withheld the workspace has NO session at
	// all — bring-up is what records one, and it has not finished.
	//
	// SETTLED BEHAVIOR (read from internal/workspace/sessions.go and
	// internal/shimclient/supervisor.go, not rationalized here): Fleet.Start
	// calls f.remember — the only thing that populates HostSessionFacts, which
	// hostExisting requires before it can compose ANY `existing` arm — only
	// AFTER bringUpClient returns, and Supervisor.Spawn (what bringUpClient
	// calls to bring the shim up) itself blocks internally until the shim's
	// first healthy diagnostics arrives. So there is no session record for any
	// arm — including `existing.live.shim_attached:false` — to attach to while
	// diagnostics is withheld; `host.none` is the only answer the daemon can
	// give. SPEC.md's "HostWorkspace shows shim_attached:false until it
	// arrives, then true" describes a state this architecture cannot reach;
	// see the report for the proposed correction.
	harness.AwaitView(t, f.d.Ctx(), host, "the workspace with no session while readiness is withheld",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetHost().GetNone() != nil
		})
	select {
	case err := <-done:
		t.Fatalf("OpenWorkspace returned %v before any healthy diagnostics, want it gated on readiness", err)
	default:
	}

	f.shim = f.d.Shim(f.ws)
	f.shim.PushHealthyWhenSubscribed()

	// Assert: readiness lands, the rpc answers, and the shim reads attached.
	if err := <-done; err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success once the shim came up healthy", err)
	}
	harness.AwaitView(t, f.d.Ctx(), host, "shim_attached true after the delayed diagnostics push",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetHost().GetExisting().GetLive().GetShimAttached()
		})
}

func TestFakeShimExitingDuringBringUpEndsBringUpImmediately(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.health.session", "daemon.shimclient.exit", "daemon.shimclient.spawn",
		"daemon.workspace.bring_up", "daemon.workspace.open")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ExitOn: harness.ExitOnStartup, ExitCode: 7, Stderr: "boom: fake bring-up death"})
	footer := f.d.WatchFooter(f.ws)

	// Act
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))

	// Assert: bring-up ended on the DEATH — the exit is the evidence, so no
	// timeout was needed to explain it — and answered the landed spawn_failed
	// arm.
	if err != nil {
		t.Fatalf("OpenWorkspace onto a dying shim = transport error %v, want the spawn_failed arm", err)
	}
	if resp.Msg.GetError().GetSpawnFailed() == nil {
		t.Fatalf("OpenWorkspace onto a dying shim = %v, want OpenWorkspaceError.spawn_failed", resp.Msg)
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

func TestOpenWorkspaceWithNoPriorConversationStartsAFreshSession(t *testing.T) {
	t.Parallel()
	// Arrange / Act
	f := newOpened(t, harness.Opts{})

	// Assert
	req := f.shim.ExpectStartSession()
	if req.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want a fresh source for a workspace with no prior conversation", req)
	}
}

func TestReopeningAWorkspaceWithAPriorSessionResumesItsVendorSession(t *testing.T) {
	t.Parallel()
	// Arrange: open once to mint a vendor session, then kill it so the
	// workspace's session record carries a vendor id with no live shim.
	f := newOpened(t, harness.Opts{})
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
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (reopen) = error %v, want a success", err)
	}
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

func TestResumingAMissingVendorTranscriptComesUpFresh(t *testing.T) {
	t.Parallel()
	// Arrange: a session with a conversation to resume, killed and REAPED, and
	// then its transcript removed from under both account roots. The recorded
	// conversation cannot be resumed — but the workspace must keep a LIVE
	// session, so it comes up fresh with the abandoned id as the record.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the missing transcript the test stages, the abandoned conversation the classifier records, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.account.find_transcript", "daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.exit",
		"daemon.shimclient.kill_session", "daemon.shimclient.redial", "daemon.workspace.kill",
		"daemon.workspace.open", "daemon.workspace.bring_up")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.RemoveTranscripts(f.repo.Dir)

	// Act
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))

	// Assert: a live session, opened FRESH.
	if err != nil {
		t.Fatalf("OpenWorkspace on a missing transcript = transport error %v, want a fresh session", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenWorkspace on a missing transcript = %v, want a success", resp.Msg)
	}
	req := &shimv1.StartSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, req)
	if req.GetFresh() == nil {
		t.Fatalf("StartSession = %v, want the fresh source for an unresumable conversation", req)
	}
}

func TestAMissingTranscriptRecordsTheAbandonedConversation(t *testing.T) {
	t.Parallel()
	// Arrange: as above, and THE WORKSPACE TAKES A TURN FIRST. That turn is
	// what makes the vanished transcript an abandonment of real history rather
	// than the ordinary state of an id bounced before it ever spoke, and it is
	// the whole reason this bring-up is loud.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a session fault the test opens, the missing transcript the test stages, the abandoned conversation the classifier records, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.account.find_transcript", "daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.exit",
		"daemon.shimclient.kill_session", "daemon.shimclient.redial", "daemon.workspace.kill",
		"daemon.workspace.open", "daemon.workspace.bring_up")
	f.submit("say something", "k-engaged-before-the-transcript-vanishes", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	// The turn row is written BEFORE StartTurn is forwarded (promptqueue's
	// deliver), so the shim's own log of the call is the happens-after that
	// says the workspace is durably engaged.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartTurn, &shimv1.StartTurnRequest{})
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.RemoveTranscripts(f.repo.Dir)

	// Act
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a fresh session", err)
	}

	// Assert.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the abandoned conversation stated loudly", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.workspace.bring_up" &&
			r.Message == "the recorded conversation has no transcript on disk; the session comes up FRESH"
	})
}

func TestAMissingTranscriptForAConversationThatNeverSpokeIsOrdinary(t *testing.T) {
	t.Parallel()
	// Arrange: the SAME staging with NO turn ever taken. A vendor id minted at
	// spawn and stood down before its first turn writes no transcript, so
	// coming up fresh loses nothing — this is the ordinary state of a
	// stale-build relaunch bounce, and it must be recorded as such rather than
	// warned about.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a session fault the test opens, the shim death the test drives, the shim link the test severs. daemon.workspace.bring_up is DELIBERATELY ABSENT: this bring-up must produce no warning of its own.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.exit",
		"daemon.shimclient.kill_session", "daemon.shimclient.redial", "daemon.workspace.kill",
		"daemon.workspace.open")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.RemoveTranscripts(f.repo.Dir)

	// Act
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a fresh session", err)
	}

	// Assert: the ordinary notice lands at INFO, and the cleanup sweep — which
	// this test did NOT let off daemon.workspace.bring_up — proves no warning
	// or fault-bearing record was written beside it.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the ordinary never-engaged notice", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.workspace.bring_up" && r.Level == "info" &&
			r.Message == "the recorded conversation never took a turn and wrote no transcript; the session comes up FRESH"
	})
}

func TestStartSessionResumeColdStandsAGateBlockingReopenUntilAnswered(t *testing.T) {
	t.Parallel()
	// Arrange: kill to get a resumable session, then script the resume as cold.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
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

	// Act
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success carrying the gate on the feed", err)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	req := shim.ExpectStartSession()
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

	// Assert: no re-open until AnswerColdGate — the shim receives no second
	// StartSession within the probe window.
	if shim.Count(harness.RPCStartSession) != 1 {
		t.Fatalf("StartSession count = %d before any AnswerColdGate, want exactly 1 (no re-open)", shim.Count(harness.RPCStartSession))
	}

	// Act: AnswerColdGate{pay}.
	if _, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
	})); err != nil {
		t.Fatalf("AnswerColdGate{pay} = error %v, want a success", err)
	}

	// Assert: the shim sees the retry with cold_remediation{pay}.
	retry := shim.ExpectStartSession()
	if retry.GetResume().GetColdRemediation().GetPay() == nil {
		t.Fatalf("the retry's cold_remediation = %v, want {pay}", retry.GetResume().GetColdRemediation())
	}
}

func TestAnswerColdGateCompactEchoesExactly(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 999, LastRequestAtMS: 1, RequestedModel: "sonnet", CacheTTLMS: 1,
	}})
	feed := f.watchRootFeed()
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success", err)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	gateRow := awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})

	// The answer names a model the gate ITSELF served: the compact menu is the
	// closed set the daemon echoes against, so a model outside it is refused
	// (its own test) and could never reach the shim to be echoed.
	menu := gateRow.GetColdGate().GetStanding().GetCompact()
	if len(menu.GetModels()) == 0 {
		t.Fatalf("the cold gate's compact menu = %v, want at least one model offered", menu)
	}
	servedModel := menu.GetModels()[0].GetModel()

	// Act
	answered, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice: &agentreplv1.AnswerColdGateRequest_Compact{Compact: &agentreplv1.AnswerColdGateCompact{
			Model: &conversationv1.AgentModel{Name: servedModel.GetName()},
			Scope: conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS,
		}},
	}))
	if err != nil {
		t.Fatalf("AnswerColdGate{compact} = error %v, want a success", err)
	}
	if answered.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerColdGate{compact} = %v, want a success", answered.Msg)
	}

	// Assert: the retry echoes the compact choice exactly.
	retry := shim.ExpectStartSession()
	compact := retry.GetResume().GetColdRemediation().GetCompact()
	if compact == nil {
		t.Fatalf("the retry's cold_remediation = %v, want {compact}", retry.GetResume().GetColdRemediation())
	}
	if compact.GetModel().GetName() != servedModel.GetName() {
		t.Fatalf("compact.model = %q, want the served %q echoed exactly", compact.GetModel().GetName(), servedModel.GetName())
	}
	if compact.GetScope() != conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_PROMPTS {
		t.Fatalf("compact.scope = %v, want SESSION_COMPACT_SCOPE_PROMPTS echoed exactly", compact.GetScope())
	}
}

// A COMPACTION'S PROGRESS ON THE SESSION STREAM REACHES THE FOOTER through the
// session watcher's own route, never the unrouted WARN (standing order: zero
// warnings; the harness's warning sweep fails the test on any undeclared
// WARN): the running phase stands as the salient compaction line, and the
// conclusion is announced as the compaction_concluded transient.
func TestACompactionsProgressReachesTheFooterWithNoWarning(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)

	// Act: the compaction states a running phase on the session stream.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_CompactionProgress{
		CompactionProgress: &conversationv1.SessionCompactionProgress{
			Phase:        conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING,
			TokensBefore: 101_600,
		}}})

	// Assert: it stands as the salient compaction line.
	awaitFooter(t, f, footer, "the running compaction's salient line", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetWorking().GetActivity().GetSalient().GetCompaction().GetText() ==
			"summarizing the conversation (101.6k)…"
	})

	// Act: the compaction concludes.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_CompactionProgress{
		CompactionProgress: &conversationv1.SessionCompactionProgress{
			Phase:        conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED,
			TokensBefore: 101_600,
			TokensAfter:  12_400,
		}}})

	// Assert: the salient line is gone and the conclusion is announced.
	awaitFooter(t, f, footer, "the compaction_concluded transient", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetTransient().GetCompactionConcluded().GetText() ==
			"compacted and resumed (101.6k → 12.4k)"
	})
}

func TestAnswerColdGateRefusesAScopeTheMenuNeverServed(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 1, LastRequestAtMS: 1, RequestedModel: "sonnet", CacheTTLMS: 1,
	}})
	feed := f.watchRootFeed()
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success", err)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	gateRow := awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})

	// Act: SESSION_COMPACT_SCOPE_UNSPECIFIED is never a served menu value.
	resp, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice: &agentreplv1.AnswerColdGateRequest_Compact{Compact: &agentreplv1.AnswerColdGateCompact{
			Model: &conversationv1.AgentModel{Name: "no-such-model-in-the-served-menu"},
			Scope: conversationv1.SessionCompactScope_SESSION_COMPACT_SCOPE_ALL,
		}},
	}))

	// Assert: unserved_remediation IS a landed arm, so it answers as one.
	if err != nil {
		t.Fatalf("AnswerColdGate{compact} with an unserved model = transport error %v, want the unserved_remediation arm", err)
	}
	if resp.Msg.GetError().GetUnservedRemediation() == nil {
		t.Fatalf("AnswerColdGate = %v, want AnswerColdGateError.unserved_remediation", resp.Msg)
	}
}

func TestTheConfigDirIsDeterminedByTheMultiRepoRoot(t *testing.T) {
	t.Parallel()
	// Arrange: a repository OUTSIDE the multi-repo root.
	d := newDaemon(t, harness.Opts{})
	defaultRepo := harness.NewRepo(t)
	defaultWS := harness.Register(t, d, defaultRepo.Dir)

	// Act
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: defaultWS})); err != nil {
		t.Fatalf("OpenWorkspace (default account) = error %v, want a success", err)
	}
	defaultShim := d.Shim(defaultWS)

	// Assert
	if got := defaultShim.Info().Env["CLAUDE_CONFIG_DIR"]; got != d.DefaultConfigDir {
		t.Fatalf("CLAUDE_CONFIG_DIR = %q for a workspace outside MULTI_REPO_ROOT, want the default account root %q", got, d.DefaultConfigDir)
	}
}

func TestAWorkspaceUnderTheMultiRepoRootSpawnsWithTheMultiRepoAccount(t *testing.T) {
	t.Parallel()
	// Arrange: the repository lives directly UNDER $MULTI_REPO_ROOT, which is
	// the only input the account routing takes.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepoAt(t, filepath.Join(d.MultiRepoRoot, "under-the-multi-root"))
	ws := harness.Register(t, d, repo.Dir)

	// Act
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}

	// Assert
	if got := d.Shim(ws).Info().Env["CLAUDE_CONFIG_DIR"]; got != d.MultiRepoConfigDir {
		t.Fatalf("CLAUDE_CONFIG_DIR = %q for a workspace under MULTI_REPO_ROOT, want the multi-repo account root %q", got, d.MultiRepoConfigDir)
	}
}

func TestALoggedOutAccountRootDrawsLoggedOut(t *testing.T) {
	t.Parallel()
	// Arrange
	d := harness.StartDaemon(t, harness.Opts{DefaultAccountEmail: harness.LoggedOut})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	topbar := d.WatchTopbar(ws)

	// Act
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}
	d.Shim(ws).PushHealthy()

	// Assert
	awaitTopbar(t, &fixture{d: d, ws: ws, t: t}, topbar, "the account drawn logged_out", func(v *frontendv1.TopbarView) bool {
		return v.GetAccount().GetLoggedOut() != nil
	})
}

// ---- critique 4: topbar account for a MULTI_REPO_ROOT workspace ----

func TestATopbarInsideTheMultiRepoRootShowsTheMultiRepoAccountEmail(t *testing.T) {
	t.Parallel()
	// Arrange: a repository directly under $MULTI_REPO_ROOT, with the two
	// account roots given DISTINCT emails so a topbar reading the wrong root
	// cannot pass by accident.
	d := harness.StartDaemon(t, harness.Opts{
		DefaultAccountEmail:   "default-acct@example.invalid",
		MultiRepoAccountEmail: "multi-acct@example.invalid",
	})
	repo := harness.NewRepoAt(t, filepath.Join(d.MultiRepoRoot, "inside-the-root"))
	ws := harness.Register(t, d, repo.Dir)
	topbar := d.WatchTopbar(ws)

	// Act
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}
	d.Shim(ws).PushHealthy()

	// Assert
	view := awaitTopbar(t, &fixture{d: d, ws: ws, t: t}, topbar, "the account drawn logged_in with an email", func(v *frontendv1.TopbarView) bool {
		return v.GetAccount().GetLoggedIn() != nil
	})
	if got := view.GetAccount().GetLoggedIn().GetEmail(); got != "multi-acct@example.invalid" {
		t.Fatalf("topbar account email = %q for a workspace under MULTI_REPO_ROOT, want the multi-repo account's %q", got, "multi-acct@example.invalid")
	}
}

func TestATopbarOutsideTheMultiRepoRootShowsTheDefaultAccountEmail(t *testing.T) {
	t.Parallel()
	// Arrange: a sibling repository OUTSIDE $MULTI_REPO_ROOT, with the two
	// account roots given distinct emails.
	d := harness.StartDaemon(t, harness.Opts{
		DefaultAccountEmail:   "default-acct@example.invalid",
		MultiRepoAccountEmail: "multi-acct@example.invalid",
	})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	topbar := d.WatchTopbar(ws)

	// Act
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}
	d.Shim(ws).PushHealthy()

	// Assert
	view := awaitTopbar(t, &fixture{d: d, ws: ws, t: t}, topbar, "the account drawn logged_in with an email", func(v *frontendv1.TopbarView) bool {
		return v.GetAccount().GetLoggedIn() != nil
	})
	if got := view.GetAccount().GetLoggedIn().GetEmail(); got != "default-acct@example.invalid" {
		t.Fatalf("topbar account email = %q for a workspace outside MULTI_REPO_ROOT, want the default account's %q", got, "default-acct@example.invalid")
	}
}

func TestHostVendorClaudeConfigDirIsTheRoutedRootForAMultiRepoWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a repository directly under $MULTI_REPO_ROOT.
	d := harness.StartDaemon(t, harness.Opts{})
	repo := harness.NewRepoAt(t, filepath.Join(d.MultiRepoRoot, "inside-the-root-vendor-claude"))
	ws := harness.Register(t, d, repo.Dir)
	host := d.WatchHost(ws)

	// Act
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}
	d.Shim(ws).PushHealthy()

	// Assert: the vendor arm's config_dir names the ROUTED root, not merely
	// the env var the shim's own process saw.
	view := harness.AwaitView(t, d.Ctx(), host, "the live session carrying a vendor claude arm",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetHost().GetExisting().GetLive().GetClaude() != nil
		})
	if got := view.GetHost().GetExisting().GetLive().GetClaude().GetConfigDir(); got != d.MultiRepoConfigDir {
		t.Fatalf("HostVendorClaude.config_dir = %q for a workspace under MULTI_REPO_ROOT, want the multi-repo account root %q", got, d.MultiRepoConfigDir)
	}
}

// ---- critique 5: account-switch transcript porting ----

func TestAForkedChildOutsideTheMultiRepoRootDoesNotInheritTheParentsAccount(t *testing.T) {
	t.Parallel()
	// Arrange: a parent workspace INSIDE the multi-repo root, opened so it has
	// a vendor conversation to fork.
	d := harness.StartDaemon(t, harness.Opts{})
	parentRepo := harness.NewRepoAt(t, filepath.Join(d.MultiRepoRoot, "fork-parent"))
	parentWS := harness.Register(t, d, parentRepo.Dir)
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: parentWS})); err != nil {
		t.Fatalf("OpenWorkspace(parent) = error %v, want a success", err)
	}
	parentShim := d.Shim(parentWS)
	parentShim.ExpectStartSession()
	if parentShim.Info().VendorSessionID == "" {
		t.Fatal("the fake shim reports no vendor session id for the parent after StartSession(fresh)")
	}

	// A child repository OUTSIDE the multi-repo root -- a different repository
	// entirely, since a fork's child worktree is always adjacent to ITS OWN
	// repository directory and never to the parent's.
	childRepo := harness.NewRepo(t)
	childRepoRef := mergeRepositoryRef(t, d, childRepo)

	// Act: create the child as a FORK of the parent.
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: childRepoRef,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			Name: strPtr("forked-child-outside-root"),
		}},
		Parent: &agentreplv1.CreateWorkspaceParent{
			Workspace: parentWS,
			Fork:      &agentreplv1.CreateWorkspaceFork{},
		},
	}))
	if err != nil {
		t.Fatalf("CreateWorkspace(fork) = error %v, want a success", err)
	}
	childWS := resp.Msg.GetSuccess().GetWorkspace()
	if childWS.GetId() == "" {
		t.Fatalf("CreateWorkspace(fork) = %v, want a success carrying a workspace ref", resp.Msg)
	}

	// Assert: the child spawns with the DEFAULT root -- its own directory
	// decides its account, never the parent's.
	childShim := d.Shim(childWS)
	if got := childShim.Info().Env["CLAUDE_CONFIG_DIR"]; got != d.DefaultConfigDir {
		t.Fatalf("CLAUDE_CONFIG_DIR = %q for a forked child outside MULTI_REPO_ROOT, want the default account root %q (no inheritance from a parent inside the root)", got, d.DefaultConfigDir)
	}
}

func TestAccountSwitchPortsTheTranscriptAcrossADaemonBoot(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace OUTSIDE the multi-repo root, opened so it has a
	// vendor transcript filed under the default root.
	f := newOpened(t, harness.Opts{})
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
	if _, err := successor.Client().OpenWorkspace(successor.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace after the routing switch = error %v, want a success", err)
	}

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
	f := newOpened(t, harness.Opts{})
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
	awaitRoster(t, f.d, roster, "the roster row inactive and closed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetInactive() != nil && row.GetClosed().GetClosed()
	})
}

func TestCloseWorkspaceWithNothingLiveSucceedsAndLeavesTheShimRunning(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace with nothing live = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseWorkspace = %v, want a success", resp.Msg)
	}
	if f.shim.Count(harness.RPCKillSession) != 0 {
		t.Fatalf("KillSession was sent %d times after a plain CloseWorkspace, want 0: the session is untouched", f.shim.Count(harness.RPCKillSession))
	}
}

func TestCloseWorkspaceWithATurnInFlightAnswersBlocked(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	footer := f.d.WatchFooter(f.ws)
	f.submit("do the thing", "k-close-blocked", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert: CloseWorkspaceError.blocked IS a landed arm, so the refusal is a
	// response arm rather than a transport error.
	if err != nil {
		t.Fatalf("CloseWorkspace with a turn in flight = transport error %v, want the blocked arm", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("CloseWorkspace with a turn in flight = %v, want CloseWorkspaceError.blocked", resp.Msg)
	}
	awaitFooter(t, f, footer, "footer closing.blocked", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetClosing().GetBlocked() != nil
	})
	// internal/workspace/close.go logs WARN under this operation itself
	// whenever closeBlocker names a reason, independent of the arm-landing
	// machinery in internal/workspace/refusal.go.
	f.d.ExpectWarnings("daemon.workspace.close")
}

// TestCloseWorkspaceBlockedCarriesTheQuietChecksEvidence is landing 7: the
// blocked arm states WHY, so a caller with no footer can say it — and
// `summary` is the same composed sentence the footer's activity line draws.
func TestCloseWorkspaceBlockedCarriesTheQuietChecksEvidence(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.submit("do the thing", "k-close-evidence", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace with a turn in flight = transport error %v, want the blocked arm", err)
	}
	blocked := resp.Msg.GetError().GetBlocked()
	if blocked == nil || !blocked.GetTurnInFlight() || blocked.GetSummary() == "" {
		t.Fatalf("CloseWorkspaceBlocked = %v, want turn_in_flight true with a composed summary", blocked)
	}
	f.d.ExpectWarnings("daemon.workspace.close")
}

func TestCloseWorkspaceWithAQueuedMergeRefuses(t *testing.T) {
	t.Parallel()
	// Arrange: a workspace the daemon CREATED, so it carries the merge layout
	// facts an enqueue needs, with a second one ahead of it in its repo's queue
	// so its own merge stays queued rather than running to a terminal.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	first := mergeCreateChild(t, d, repoRef, "ahead", "do the first thing",
		&agentreplv1.CreateWorkspaceMergeActions{BeforeWsMerge: said("hold the queue open")})
	second := mergeCreateChild(t, d, repoRef, "behind", "do the second thing", nil)

	// The first merge stops in a before-merge prompt nobody answers, holding
	// its repository's slot, so the second one waits in the queue. (A PARKED
	// merge would not hold it: it yields the slot, owner ruling 2026-09-28.)
	harness.CommitWork(t, first.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: first.ws, Source: harness.OwnBranch(false)})); err != nil {
		t.Fatalf("MergeWorkspace(first) = error %v, want the merge enqueued", err)
	}
	first.shim.ExpectStartTurn()
	harness.CommitWork(t, second.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: second.ws, Source: harness.OwnBranch(false)})); err != nil {
		t.Fatalf("MergeWorkspace(second) = error %v, want the merge enqueued", err)
	}
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the second workspace's queued merge", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, second.ws.GetId()).GetMergeQueued() != nil
	})

	// Act
	resp, err := d.Client().CloseWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: second.ws}))

	// Assert: the landed blocked arm, not a transport error.
	if err != nil {
		t.Fatalf("CloseWorkspace with a queued merge = transport error %v, want the blocked arm", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("CloseWorkspace with a queued merge = %v, want CloseWorkspaceError.blocked", resp.Msg)
	}
	// The Act's own refusal names a LANDED arm, so it warns about nothing.
}

func TestCloseWorkspaceWithAStandingColdGateSucceeds(t *testing.T) {
	t.Parallel()
	// Arrange: stand a cold gate with no turn and no live async work.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 1, LastRequestAtMS: 1, RequestedModel: "sonnet", CacheTTLMS: 1,
	}})
	feed := f.watchRootFeed()
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success", err)
	}
	f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl").ExpectStartSession()
	awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("CloseWorkspace with a standing cold gate = error %v, want a success: a standing gate is not live work", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseWorkspace = %v, want a success", resp.Msg)
	}
}

func TestNukeWorkspaceRemovesTheWorktreeAndBranchAndTheRow(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "nuke-me")
	ws := harness.Register(t, d, dir)
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the registered row", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, ws.GetId()) != nil
	})

	// Act
	resp, err := d.Client().NukeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: ws}))

	// Assert
	if err != nil {
		t.Fatalf("NukeWorkspace = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("NukeWorkspace = %v, want a success", resp.Msg)
	}
	if repo.HasWorktree(dir) {
		t.Fatalf("worktree %s still registered in the fake git world after NukeWorkspace, want it removed", dir)
	}
	if repo.HasBranch("nuke-me") {
		t.Fatalf("branch %q still exists after NukeWorkspace, want it removed", "nuke-me")
	}
	awaitRoster(t, d, roster, "the row gone", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRow(r, ws.GetId()) == nil
	})
}

func TestRestartWorkspaceForcedInterruptsFirst(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a graceful stand-down the fake shim ends by exiting, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.watch_session",
		"daemon.rollout.relaunch", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.shimclient.exit", "daemon.shimclient.kill_session")
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
	killedTurn := f.shim.ExpectKillTurn()
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
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up")
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
	awaitRoster(t, f.d, roster, "the parked roster row staying idle", func(r *frontendv1.WorkspaceRoster) bool {
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
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{NoTranscriptUntilTurn: true})
	// The sweep covers every test; the declared records are evidence of the bounce's stand-down, the abandoned conversation the classifier records, and the shim death the bounce drives.
	f.d.ExpectWarnings("daemon.account.find_transcript", "daemon.health.open_fault",
		"daemon.rollout.relaunch", "daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.reopen",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.shimclient.redial",
		"daemon.workspace.bring_up", "daemon.workspace.kill",
		// The fake's replacement reports the SAME older build (its
		// FAKESHIM_BUILD_SHA outlives the bounce), which the staleness judge
		// names as a bounce that did not take.
		"daemon.rollout.staleness")

	// Act
	if _, err := f.openRaw(); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
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
	if _, err := f.openRaw(); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
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
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("the second OpenWorkspace = error %v, want a success", err)
	}

	// Assert: THE BOUNCE FIRES ONCE PER STAMP. A check that did not remember
	// what it had already bounced for would bounce again on every mount,
	// spawning a process per round forever.
	if got := f.d.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn"); got != spawns {
		t.Fatalf("shim spawns = %d after a second mount, want the %d already made: the stamp was already bounced for", got, spawns)
	}
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
}

func TestCrashBootAdoptsARunningShimWithoutASecondSpawn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
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
	// same open, and neither orders the other; the counter is polled to the
	// suite's deadline rather than sampled once.
	deadline := time.Now().Add(10 * time.Second)
	for shim.Count(harness.RPCWatchSession) <= watchesBefore {
		if time.Now().After(deadline) {
			t.Fatalf("WatchSession count = %d after adoption, want more than the pre-crash count %d (the successor re-subscribes)",
				shim.Count(harness.RPCWatchSession), watchesBefore)
		}
	}

	// The exact "intent manifest absent -> UNKNOWN/PRESERVED per session in
	// the host faults, never a count" vocabulary names no generated arm this
	// suite could find (see report): the observable adoption facts above are
	// asserted; the UNKNOWN/PRESERVED classification itself is not.
}

// ---- critique 3: CloseWorkspace with a held prompt ----

func TestCloseWorkspaceWithAHeldPromptRefuses(t *testing.T) {
	t.Parallel()
	// Arrange: hibernate an idle session, then submit a revival prompt while
	// the revival's new shim withholds its diagnostics. Nothing else is live
	// (no turn, no detached work) at that point, which is what lets
	// closeBlocker (internal/workspace/open.go) reach its held_prompts branch
	// instead of returning turn_in_flight first.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	// THE START IS READ FROM THE SHIM'S DURABLE LOG, never popped live. At a
	// 50ms cutoff the sweep can hibernate the shim, which then exits, before
	// this line runs, and a live pop then met a closed control socket
	// ("broken pipe") on a loaded host.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})

	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, &shimv1.KillSessionRequest{})
	f.shim.AwaitGone()

	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{DelayDiagnostics: true})
	footer := f.d.WatchFooter(f.ws)
	held := f.submit("wake up", "k-close-held-prompt", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if held.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt during revival = %v, want a minted TurnId even though delivery is held", held)
	}

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert: the landed blocked arm.
	if err != nil {
		t.Fatalf("CloseWorkspace with a held prompt = transport error %v, want the blocked arm", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("CloseWorkspace with a held prompt = %v, want CloseWorkspaceError.blocked", resp.Msg)
	}

	// The footer's status precedence puts `disconnected` above `closing`
	// (internal/resolve/footer/status.go), and the revival's shim is still
	// withholding its diagnostics, so the link does not serve yet and nothing
	// the footer could say about the close is drawn. Letting that shim answer
	// is what brings the link up; the close refusal is LATCHED (SetClosing is
	// cleared only by a successful close or a re-open), so it is drawn as soon
	// as the link serves.
	// ShimAt, not Shim: Shim caches its control connection per socket path and
	// would answer with the hibernated shim's dead one.
	f.shim = f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	f.shim.PushHealthyWhenSubscribed()

	// Assert: the footer names the held-prompt cause specifically (not
	// turn_in_flight or live_work).
	view := awaitFooter(t, f, footer, "footer closing.blocked naming the held prompt", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetClosing().GetBlocked() != nil
	})
	text := view.GetStrip().GetStatus().GetClosing().GetActivity().GetSalient().GetCloseBlocked().GetText()
	if !strings.Contains(text, "held prompt") {
		t.Fatalf("closing.blocked activity text = %q, want it to name the held-prompt cause", text)
	}
	// The hibernation's own stand-down is loud by design, and the fake makes it
	// louder: the fake shim EXITS on accepting KillSession, so the call it was
	// answering fails, the client records the death, and each of the shim's two
	// standing streams ends without the session ending. Every one of these is
	// that one stand-down, honestly recorded once per observer -- the same set
	// TestABuildStampBounceFiresOnceAndStandsTheOldShimDown declares.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.workspace.close",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session",
		"daemon.shimclient.redial", "daemon.workspace.bring_up",
		"daemon.sessionwatcher.watch_session", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.link_fault", "daemon.health.open_fault")
}

func TestRestartWorkspaceReapsTheOldShimBeforeResumingOnTheNew(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, the shim death the stand-down drives, the shim link it severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.rollout.relaunch",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.exit", "daemon.shimclient.kill_session")
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
	harness.AwaitProcessGone(t, shortTimeout(t, f.d.Ctx(), 200*time.Millisecond), oldPID)
}

func TestRestartWorkspaceForcedDoesNotRedriveTheInterruptedTurn(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a graceful stand-down the fake shim ends by exiting, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.rollout.relaunch",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.exit",
		"daemon.shimclient.kill_session", "daemon.shimclient.redial")
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
	killedTurn := f.shim.ExpectKillTurn()
	if !killedTurn.GetForce() {
		t.Fatalf("KillTurn.force = false on a forced restart, want true: the running turn is interrupted rather than waited out")
	}

	second := f.d.ShimAt(prelaunchControlSocket(f.d, f.ws, 1))
	resume := second.ExpectStartSession()
	if resume.GetResume() == nil {
		t.Fatalf("StartSession on the resumed shim = %v, want a resume source", resume)
	}

	// Assert: the interrupted turn is NOT re-driven -- the daemon never
	// resubmits it as a fresh StartTurn once the resumed shim is up.
	expectNoRPC(t, second, harness.RPCStartTurn, harness.ProbeWindow)
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
	f := newOpened(t, harness.Opts{})
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
	f := newOpened(t, harness.Opts{})
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
	harness.AwaitProcessGone(t, f.d.Ctx(), oldPID)

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

func TestSessionStartedRestoredLiveWorkRoutesToTheRootFeed(t *testing.T) {
	t.Parallel()
	// Arrange: a registered-but-unopened workspace whose profile states one
	// already-live detached shell, so OpenWorkspace's SessionStarted carries
	// it as restored live work.
	f := newRegistered(t, harness.Opts{})
	work := detachedShell("restored-shell-1", "sleep 100")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{
		LiveWork: harness.EncodeLiveWork(t, work),
	})
	feed := f.watchRootFeed() // subscribed BEFORE open, so nothing races the restore

	// Act: the session comes up restoring the item, and the main agent's book
	// serves the call that launched it — the card the head is drawn in place
	// of, on the feed of the agent that owns the work.
	f.open()
	f.shim.PushAgentFrame(mainAgent, shellCallFrame("restored-shell-1", "sleep 100"))

	// Assert: the restored item's row lands on the ROOT feed. A shell's row
	// there is its HEAD (FeedRow.shell_head) — `detached_shell` is the spool
	// BODY, drawn only on the bubble's own sub-feed.
	awaitShellHead(t, f, feed, "the restored live-work row on the root feed")
}

func TestSessionStartedDetachedOriginLiveWorkWithNoKindIsAnErrorAndSkipped(t *testing.T) {
	t.Parallel()
	// Arrange: a restored live item whose origin is `detached` and which
	// states NO kind. The kind is the announcement's own
	// (AgentDetachedWork.kind); one without it is malformed, and
	// internal/sessionwatcher/route.go's resolveDetachedLocked refuses it at
	// ERROR before any view takes it.
	f := newRegistered(t, harness.Opts{})
	unknown := &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: "restored-detached-1"},
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: "some-earlier-activity"},
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{
		LiveWork: harness.EncodeLiveWork(t, unknown),
	})

	// Act
	f.open()

	// Assert: the daemon logs the error and the item is skipped -- the
	// restored live-work set settles at zero rather than crashing bring-up.
	f.d.AwaitWorkspaceLogOperation(f.repo.Dir, "daemon.sessionwatcher.detached_kind_unknown")
	awaitLiveWork(t, f, 0)
	f.d.ExpectWarnings("daemon.sessionwatcher.detached_kind_unknown")
}

// ---- critique 17 (this agent's share): AnswerColdGate{clear} ----

func TestAnswerColdGateClearEchoesExactly(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 1, LastRequestAtMS: 1, RequestedModel: "sonnet", CacheTTLMS: 1,
	}})
	feed := f.watchRootFeed()
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success", err)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	gateRow := awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})

	// Act
	answered, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice:    &agentreplv1.AnswerColdGateRequest_Clear{Clear: &agentreplv1.AnswerColdGateClear{}},
	}))
	if err != nil {
		t.Fatalf("AnswerColdGate{clear} = error %v, want a success", err)
	}
	if answered.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerColdGate{clear} = %v, want a success", answered.Msg)
	}

	// Assert: the retry echoes the clear choice exactly.
	retry := shim.ExpectStartSession()
	if retry.GetResume().GetColdRemediation().GetClear() == nil {
		t.Fatalf("the retry's cold_remediation = %v, want {clear}", retry.GetResume().GetColdRemediation())
	}
}

// ---- critique 25 (this agent's share): KillWorkspace preserves data ----

func TestKillWorkspaceLeavesTheWorktreeAndBranchIntact(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a session fault the test opens.
	d.ExpectWarnings("daemon.health.open_fault")
	// KillWorkspace ends a live session on purpose; that trail is the ACT here.
	expectSessionKillRecords(d)
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "kill-keep-data")
	ws := harness.Register(t, d, dir)
	if _, err := d.Client().OpenWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}
	shim := d.Shim(ws)
	shim.ExpectStartSession()

	// Act
	resp, err := d.Client().KillWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: ws}))
	if err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("KillWorkspace = %v, want a success", resp.Msg)
	}
	shim.AwaitGone()

	// Assert: the worktree and branch both still exist -- Kill ends the
	// session, never the workspace's data.
	if !repo.HasWorktree(dir) {
		t.Fatalf("worktree %s is gone after KillWorkspace, want the workspace's data left intact", dir)
	}
	if !repo.HasBranch("kill-keep-data") {
		t.Fatalf("branch %q is gone after KillWorkspace, want the workspace's data left intact", "kill-keep-data")
	}
}

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

// TestHibernateTransportFailureDefersTheStandDown covers the transport-level
// failure: c.deps.Stand.Hibernate itself errors (shimclient's unary() passes
// a raw connect error through), which internal/drain/sweep.go's hibernate()
// logs at ERROR under daemon.drain.sweep ("the hibernate directive failed;
// deferring the hibernation") — NOT WARN. The sweep's typed-refusal branch
// (the other test below) is the one that logs WARN; a transport failure never
// reaches that branch at all, so this test asserts the ERROR record the
// source actually produces.
func TestHibernateTransportFailureDefersTheStandDown(t *testing.T) {
	t.Parallel()
	// Arrange: a very short idle cutoff so the sweep fires promptly, and a
	// Hibernate transport failure the fake is BORN with, so no sweep pass can
	// ever get through to a real (successful) hibernate and force a
	// KillSession for real. It used to queue twenty scripted failures after
	// the workspace was already open, which raced the sweep at both ends: an
	// early sweep hibernated for real before the first was filed, and a
	// loaded run exhausted the twenty.
	f := newOpenedWithProfile(t, harness.Opts{IdleCutoffMS: 50}, harness.ShimProfile{HibernateFailure: "transport blew up"})
	f.shim.ExpectStartSession()
	// The shim client records each refused call at ERROR of its own — that
	// record IS the transport failure this test scripts — and the sweep then
	// records the deferral.
	f.d.ExpectWarnings("daemon.drain.sweep", "daemon.shimclient.hibernate")

	// Act: wait for the sweep to log the failed directive.
	rec := f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"),
		"an ERROR record for the failed hibernate directive",
		func(r harness.LogRecord) bool {
			return r.Operation == "daemon.drain.sweep" && strings.EqualFold(r.Level, "error")
		})

	// Assert: the record is the hibernate-directive failure, not some other
	// daemon.drain.sweep error.
	if !strings.Contains(rec.Message, "hibernate directive failed") {
		t.Fatalf("daemon.drain.sweep ERROR record = %q, want it to name the failed hibernate directive", rec.Message)
	}

	// Assert: the daemon never forces the session down over the refused
	// hibernate.
	expectNoRPC(t, f.shim, harness.RPCKillSession, harness.ProbeWindow)
}

// TestHibernateTurnInFlightRefusalDefersTheStandDown covers the shim's own
// typed refusal: HibernateError.turn_in_flight (shim.v1/endpoint_hibernate.proto).
// The sweep logs this at DEBUG only ("a turn is in flight; deferring the
// hibernation") — quieter than the OTHER typed refusals (compaction_failed,
// no_session), which log WARN — so this test asserts no WARN fires at all.
func TestHibernateTurnInFlightRefusalDefersTheStandDown(t *testing.T) {
	t.Parallel()
	// Arrange: the refusal is in force from the fake's BIRTH. Scripted after
	// the open, it raced the 50ms sweep — an early pass hibernated for real
	// and killed the fake — and a queue of twenty ran out under load.
	f := newOpenedWithProfile(t, harness.Opts{IdleCutoffMS: 50}, harness.ShimProfile{HibernateTurnInFlight: true})
	f.shim.ExpectStartSession()

	// Act: wait for at least one refused Hibernate attempt.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})

	// Assert: the daemon never forces the session down over the deferred
	// hibernate.
	expectNoRPC(t, f.shim, harness.RPCKillSession, harness.ProbeWindow)
}

// ---------------------------------------------------------------------------
// audit-3 critique 5: spawn-on-mount revival.
// ---------------------------------------------------------------------------

// TestOpenWorkspaceOnAHibernatedRowSendsStartSessionResume covers OpenWorkspace
// re-mounting a hibernated session DIRECTLY (rather than through a revived
// prompt, which TestHibernationParksAnIdleSessionAndRevivesOnPrompt already
// covers): the row's session.Terminal.Kind is drain.TerminalHibernated
// ("hibernated"), decideSource (internal/workspace/sessions.go) treats
// anything but "deleted" as resumable, so the mount resumes the vendor
// session exactly as a revival does.
//
// THE 50ms CUTOFF BELONGS TO THE ARRANGEMENT AND NOTHING ELSE, and that split
// is why this test runs two daemons. The park needs a compressed cutoff; the
// ACT needs the daemon NOT to have one, because a re-mounted session is born
// idle and a still-armed 50ms sweep parks it again — taking its control socket
// with it — while OpenWorkspace is still bringing it up. Nothing in the
// assertion could then say whether it observed the mount or the race, and the
// suite lost a run to it. The hibernated terminal is DURABLE (wsm's session
// row), so a successor on the same state root reads exactly the row this test
// is about, with no sweep armed to disturb the mount. It is the same
// stop-and-restart arrangement TestOpenWorkspaceOnATerminallyDeletedSession-
// AnswersSessionDeleted uses to reach its own terminal row.
func TestOpenWorkspaceOnAHibernatedRowSendsStartSessionResume(t *testing.T) {
	t.Parallel()
	// Arrange: hibernate the session via the idle sweep, then retire the
	// daemon that had the cutoff armed.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up")
	// THE START IS READ FROM THE SHIM'S DURABLE LOG, never popped live. At a
	// 50ms cutoff the sweep can hibernate the shim, which then exits, before
	// this line runs, and a live pop then met a closed control socket
	// ("broken pipe") on a loaded host.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	f.shim.AwaitGone()
	// THE PARK IS NOT DONE UNTIL THE DAEMON SAYS SO. The shim's exit is the
	// sweep's means, not its completion: until the daemon has recorded the
	// hibernation the workspace still reads as OPEN to it, and an OpenWorkspace
	// arriving in that window is answered as the idempotent no-op it looks like
	// — success, no second mount, no StartSession at all.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the sweep's own hibernation record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.drain.sweep" && strings.Contains(r.Message, "hibernated an idle session")
	})
	f.d.Stop()

	// Act: a successor with NO compressed cutoff re-mounts the parked session.
	// THE ACCOUNT ROOTS COME WITH IT. StartDaemon mints fresh ones per daemon,
	// and the resume's transcript was filed under the incumbent's — a
	// successor given new roots finds no transcript for the vendor session it
	// is resuming, which is a different subject than this test's.
	successor := harness.StartDaemon(t, harness.Opts{
		StateDir: f.d.StateDir,
		ExtraArgs: []string{
			"--default-config-dir", f.d.DefaultConfigDir,
			"--multi-repo-config-dir", f.d.MultiRepoConfigDir,
		},
		ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir},
	})
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills.
	successor.ExpectWarnings("daemon.workspace.bring_up")
	if _, err := successor.Client().OpenWorkspace(successor.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace on a hibernated workspace = error %v, want a success", err)
	}
	// The request is read from the fake's DURABLE log, which spans both shims
	// and both daemons; the wait names the RESUME because the Arrange's own
	// fresh StartSession is already in that log.
	req := &shimv1.StartSessionRequest{}
	successor.AwaitShimLoggedRequestMatching(f.repo.Dir, harness.RPCStartSession,
		"a StartSession resuming the hibernated session", req,
		func() bool { return req.GetResume() != nil })

	// Assert
	if req.GetResume() == nil {
		t.Fatalf("StartSession request = %v, want a resume source reviving the hibernated session", req)
	}
}

// TestOpenWorkspaceOnATerminallyDeletedSessionAnswersSessionDeleted covers
// OpenWorkspaceError.session_deleted (endpoint_open_workspace.proto). NOTHING
// in production ever writes the "deleted" session terminal today (grepped
// every internal/**/*.go call to wsm.SetSessionTerminal: teardown.go writes
// "killed", drain/sweep.go writes drain.TerminalHibernated) — so this test
// produces the row the way harness.WithDB's own doc prescribes: corrupt one
// column of a real (killed) session row, stop, and restart, watching the
// successor refuse it. See internal/workspace/sessions.go's decideSource,
// which refuses BEFORE any spawn.
func TestOpenWorkspaceOnATerminallyDeletedSessionAnswersSessionDeleted(t *testing.T) {
	t.Parallel()
	// Arrange: kill to get a real, whole session-terminal row, then corrupt
	// just its kind to "deleted" (wsm's terminalDeleted spelling).
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills.
	f.d.ExpectWarnings("daemon.workspace.open")
	// The arrangement KILLS the session to get a whole session-terminal row.
	expectSessionKillRecords(f.d)
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.Stop()
	f.d.CorruptRow("sessions", "terminal_kind", "workspace_id", f.ws.GetId(), "deleted")

	// Act: restart on the same state root and reopen.
	successor := harness.StartDaemon(t, harness.Opts{StateDir: f.d.StateDir, ExtraEnv: []string{"AGENT_REPL_LOCK_DIR=" + f.d.LockDir}})
	// The sweep covers every test; the declared records are evidence of a bring-up the test blocks or kills.
	successor.ExpectWarnings("daemon.workspace.open")
	resp, err := successor.Client().OpenWorkspace(successor.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))

	// Assert: the landed session_deleted arm, and nothing spawned to be
	// refused BY (the guard reads the durable record before any process
	// spawns, mirroring TestResumingAMissingVendorTranscriptIsRefusedBeforeSpawn).
	if err != nil {
		t.Fatalf("OpenWorkspace on a terminally deleted session = transport error %v, want the session_deleted arm", err)
	}
	if resp.Msg.GetError().GetSessionDeleted() == nil {
		t.Fatalf("OpenWorkspace on a terminally deleted session = %v, want OpenWorkspaceError.session_deleted", resp.Msg)
	}
	if got := successor.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn"); got != 0 {
		t.Fatalf("shim spawn records = %d after the refusal, want 0: the guard refuses BEFORE the spawn", got)
	}
}

// ---------------------------------------------------------------------------
// audit-3 critique 20: cold-gate and interrupt arms.
// ---------------------------------------------------------------------------

// TestAnswerColdGateOnAnAlreadyResolvedGateAnswersNoColdGate covers
// AnswerColdGateError.no_cold_gate (endpoint_answer_cold_gate.proto):
// internal/workspace/sessions.go's Fleet.Stop and the successful-resolve path
// both delete the served gate from Fleet.coldGates, so a SECOND answer
// against the same (now resolved) gate id finds none standing.
func TestAnswerColdGateOnAnAlreadyResolvedGateAnswersNoColdGate(t *testing.T) {
	t.Parallel()
	// Arrange: stand a cold gate and resolve it once.
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 1, LastRequestAtMS: 1, RequestedModel: "sonnet", CacheTTLMS: 1,
	}})
	feed := f.watchRootFeed()
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success", err)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	gateRow := awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})
	resolved, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
	}))
	if err != nil || resolved.Msg.GetSuccess() == nil {
		t.Fatalf("the first AnswerColdGate{pay} = (%v, %v), want a success setting up the resolved gate", resolved.Msg, err)
	}
	shim.ExpectStartSession()

	// Act: answer the SAME gate id again.
	resp, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("AnswerColdGate on an already-resolved gate = transport error %v, want the no_cold_gate arm", err)
	}
	if resp.Msg.GetError().GetNoColdGate() == nil {
		t.Fatalf("AnswerColdGate on an already-resolved gate = %v, want error.no_cold_gate", resp.Msg)
	}
}

// TestAnswerColdGateWithNoLiveShimStandsTheGateAgain pins what an answer to a
// gate whose shim has died comes to. The answer is TAKEN (AnswerColdGate
// answers success: the re-open runs after the answer, and
// AnswerColdGateError.no_session is not produced since 2026-09-29), and the
// re-open's failure is the footer's `cold_gate_reopen_failed` fault, with the
// gate stood again so the choice is the user's once more.
//
// Reaching it needs a standing gate AND a dead session AT ONCE, which no
// daemon verb produces: KillWorkspace's teardown (Fleet.Stop) clears the gate
// together with the session. So the shim's PROCESS is killed out from under a
// standing gate, and Fleet.ResumeCold reads the client's REAPED state before
// touching the link.
func TestAnswerColdGateWithNoLiveShimStandsTheGateAgain(t *testing.T) {
	t.Parallel()
	// Arrange: stand a cold gate, then kill the shim PROCESS directly (never
	// through KillWorkspace, which would also clear the coldGates record).
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up",
		"daemon.workspace.kill")
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{ColdOnResume: &harness.ShimColdFacts{
		ContextTokens: 1, LastRequestAtMS: 1, RequestedModel: "sonnet", CacheTTLMS: 1,
	}})
	feed := f.watchRootFeed()
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenWorkspace (cold resume) = error %v, want a success", err)
	}
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	gateRow := awaitRow(t, f, feed, "the cold gate row", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})
	footer := f.d.WatchFooter(f.ws)
	killShim(t, f, shim)

	// Act
	resp, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      gateRow.GetId(),
		Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
	}))

	// Assert: the answer is taken.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("AnswerColdGate against a dead shim under a standing gate = (%v, %v), want success: the re-open runs after the answer", resp, err)
	}
	// Assert: the re-open's failure is the footer's fault line.
	awaitFooter(t, f, footer, "the cold_gate_reopen_failed fault", func(v *frontendv1.FooterView) bool {
		return strings.Contains(v.GetStrip().GetStatus().String(), "cold_gate_reopen_failed")
	})
	// Assert: and the gate stands again.
	awaitRow(t, f, feed, "the cold gate stood again", func(r *frontendv1.FeedRow) bool {
		return r.GetColdGate().GetStanding() != nil
	})
}

// TestAnswerColdGateOnAWorkspaceWithNoSessionAtAllAnswersNoColdGate documents
// the OTHER "no session" shape, which is NOT the no_session arm: a workspace
// that has never had any session, or whose cold gate has already been
// cleared, hits the ColdGate check FIRST (internal/workspace/answers.go), so
// it always answers no_cold_gate, never no_session — no_cold_gate is checked
// before liveness. This locks that ordering down so a future reordering of
// the two checks is caught here rather than only in the no_session test above.
func TestAnswerColdGateOnAWorkspaceWithNoSessionAtAllAnswersNoColdGate(t *testing.T) {
	t.Parallel()
	// Arrange: a registered workspace that was never opened.
	f := newRegistered(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().AnswerColdGate(f.d.Ctx(), connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
		Workspace: f.ws,
		Gate:      &frontendv1.FeedId{Value: "not-a-real-gate-id"},
		Choice:    &agentreplv1.AnswerColdGateRequest_Pay{Pay: &agentreplv1.AnswerColdGatePay{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("AnswerColdGate on a workspace with no session at all = transport error %v, want the no_cold_gate arm", err)
	}
	if resp.Msg.GetError().GetNoColdGate() == nil {
		t.Fatalf("AnswerColdGate on a workspace with no session at all = %v, want error.no_cold_gate (checked before liveness)", resp.Msg)
	}
}

// TestInterruptTurnAgainstAShimReportingNoSessionAnswersNoSession targets
// InterruptError.no_session (endpoint_interrupt.proto). A workspace with NO
// live session at all answers `nothing_running` instead — a SUCCESS, not this
// arm (TestInterruptWithNothingRunningAnswersNothingRunning; see
// internal/workspace/interrupt.go's `if !live || !hasShim` branch, which
// returns before ever reaching the shim). The only reachable route to the
// TYPED no_session error is the shim's OWN KillTurnFailure.no_session cause,
// propagated by name (internal/workspace/shimarms.go's killTurnArm ->
// ArmShimNoSession = "no_session", which server/refuse.go's reflection-based
// setArm matches directly against InterruptError's no_session field) while
// Freeness still reports a turn open.
func TestInterruptTurnAgainstAShimReportingNoSessionAnswersNoSession(t *testing.T) {
	t.Parallel()
	// Arrange: a turn appears open, but the shim itself reports no session.
	f := newOpened(t, harness.Opts{})
	f.submit("start the long task", "k-running", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.shim.Answer(harness.RPCKillTurn, &shimv1.KillTurnResponse{
		Result: &shimv1.KillTurnResponse_Failure{Failure: &shimv1.KillTurnFailure{
			Cause:  &shimv1.KillTurnFailure_NoSession{NoSession: &shimv1.KillTurnNoSession{}},
			Detail: "no session is open on this shim",
		}},
	})

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt{turn} against a shim reporting no_session = transport error %v, want the no_session arm", err)
	}
	if resp.Msg.GetError().GetNoSession() == nil {
		t.Fatalf("Interrupt{turn} against a shim reporting no_session = %v, want error.no_session", resp.Msg)
	}
}

// TestInterruptTurnWithATransportFailureAnswersShimRefused targets
// InterruptError.shim_refused{detail} (endpoint_interrupt.proto's arm 8:
// "A typed shim refusal relayed"). Per its doc comment this is meant for a
// shim refusal the daemon does not otherwise have a specific arm for.
//
// EXPECTED RED, for two independent reasons found by grepping the whole
// production tree:
//  1. AnswerFailure is a TRANSPORT-level failure (fakeshim/server.go's
//     `scripted` returns connect.NewError(CodeInternal, ...)), and
//     internal/workspace/sender.go's KillTurn passes a transport error
//     straight through UNWRAPPED — it is never a *workspace.ShimRefusal. So
//     interrupt.go's `AsShimRefusal(err)` fails, the confirm/no_session
//     branches are never reached, and the verb returns a plain wrapped error,
//     which the server answers as a raw Connect error, not a typed arm at
//     all.
//  2. Even where a real *ShimRefusal DOES reach server/refuse.go, the arm
//     name it carries is switched onto the CONCRETE named field the shim
//     itself reported (e.g. "no_session", "live", "not_the_open_turn" —
//     server.setArm matches the refusal's Arm string directly against
//     InterruptError's oneof field names by reflection). No production code
//     anywhere ever sets Arm to the literal string "shim_refused": grepping
//     `"shim_refused"` and `ShimRefused` across internal/**/*.go (excluding
//     _test.go) returns zero producers. The arm is UNREACHABLE as things
//     stand.
func TestInterruptTurnWithATransportFailureAnswersShimRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a turn kill the test fails.
	f.d.ExpectWarnings("daemon.shimclient.kill_turn", "daemon.workspace.interrupt")
	f.submit("start the long task", "k-running", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	f.shim.AnswerFailure(harness.RPCKillTurn, "the vendor refused the kill")

	// Act
	resp, err := f.d.Client().Interrupt(f.d.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: f.ws,
		Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("Interrupt{turn} on a shim transport failure = transport error %v, want the shim_refused arm carrying its detail", err)
	}
	refused := resp.Msg.GetError().GetShimRefused()
	if refused == nil {
		t.Fatalf("Interrupt{turn} on a shim transport failure = %v, want error.shim_refused", resp.Msg)
	}
	if refused.GetDetail() != "the vendor refused the kill" {
		t.Fatalf("shim_refused.detail = %q, want the shim's own detail %q", refused.GetDetail(), "the vendor refused the kill")
	}
}

// A VENDOR THAT FAILS TO START INSIDE A HEALTHY SHIM IS A TYPED REFUSAL. The
// shim process is up and serving — only its StartSession answer is a refusal —
// so the daemon relays the shim's own verdict rather than letting it escape as
// an untyped Connect internal. LANDING 9 landed the arm:
// OpenWorkspaceError.vendor_start_failed carries the shim's `detail`.
func TestAVendorStartFailureIsRelayedByNameAndNeverEscapesAsInternal(t *testing.T) {
	t.Parallel()
	// Arrange
	const shimDetail = "the vendor SDK threw before its first message"
	f := newRegistered(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.workspace.open",
		"daemon.health.session", "daemon.shimclient.exit", "daemon.shimclient.redial", "daemon.shimclient.spawn")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{VendorStartFailed: shimDetail})

	// Act
	resp, err := f.openRaw()

	// Assert
	if err != nil {
		t.Fatalf("OpenWorkspace onto a refused vendor start = transport error %v, want the typed vendor_start_failed arm", err)
	}
	failed := resp.GetError().GetVendorStartFailed()
	if failed == nil {
		t.Fatalf("OpenWorkspace = %v, want error.vendor_start_failed", resp)
	}
	if failed.GetDetail() != shimDetail {
		t.Fatalf("vendor_start_failed.detail = %q, want the shim's own account %q", failed.GetDetail(), shimDetail)
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
	f := newRegistered(t, harness.Opts{})
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

// A RESTART BRINGS UP A WORKSPACE WHOSE VENDOR NEVER STARTED (the 2026-10-02
// incident): it is accepted at once -- never registered behind a freeness a
// session that never started cannot reach -- and the relaunched shim resumes
// the session once the vendor can start.
func TestARestartBringsUpAWorkspaceWhoseVendorWasRejected(t *testing.T) {
	t.Parallel()
	// Arrange: the vendor refuses the start, so the workspace has no session.
	f := newRegistered(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.workspace.bring_up", "daemon.workspace.open",
		"daemon.health.session", "daemon.shimclient.exit", "daemon.shimclient.redial", "daemon.shimclient.spawn")
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{VendorStartFailed: "invalid api key"})
	if resp, err := f.openRaw(); err != nil || resp.GetError().GetVendorStartFailed() == nil {
		t.Fatalf("OpenWorkspace = (%v, %v), want the vendor's rejection", resp, err)
	}
	// The user fixes what the vendor refused.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{})

	// Act
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws}))

	// Assert
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = (%v, %v), want a success", resp, err)
	}
	started := f.d.ShimAt(prelaunchControlSocket(f.d, f.ws, 1)).ExpectStartSession()
	if started.GetSource() == nil {
		t.Fatalf("StartSession on the relaunched shim = %v, want the session brought up", started)
	}
}

// TestAParkedRowIsIdleOnEveryRosterResolvedAfterTheHibernationRecord is the
// roster half of "A PARKED SESSION IS IDLE, NOT BROKEN"
// (internal/resolve/sidebar/status.go), asserted where the real sequence
// actually breaks it.
//
// The roster resolver publishes on the events it is handed, and the LAST event
// a stand-down produces is the shim link going dead — handed during the
// hibernation's KillSession, BEFORE the sweep writes the session's hibernated
// terminal. The row resolved on that event therefore reads `dead`, and in the
// headless run of the real editor nothing republished afterwards: the daemon
// log's last daemon.sidebar.row for the workspace said `dead`, and Emacs
// painted the tab blue for a session the daemon had parked on purpose.
//
// The roster stream is opened AFTER the sweep's own hibernation record so the
// first view it is served is one resolved from the parked record. A stream
// opened earlier would match on a push taken BEFORE the park, which is how
// TestHibernationParksAnIdleSessionAndRevivesOnPrompt's own roster assertion
// stayed green through the defect.
//
// COST: the 50ms cutoff is the whole of the arrangement's wait; the test's
// own edges are the two shim round trips and one roster push, measured at
// 0.31s wall inside the parallel suite.
func TestAParkedRowIsIdleOnEveryRosterResolvedAfterTheHibernationRecord(t *testing.T) {
	t.Parallel()
	// Arrange: a very short idle cutoff so hibernation fires promptly.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up")
	// THE START IS READ FROM THE SHIM'S DURABLE LOG, never popped live. At a
	// 50ms cutoff the sweep can hibernate the shim, which then exits, before
	// this line runs, and a live pop then met a closed control socket
	// ("broken pipe") on a loaded host.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, &shimv1.KillSessionRequest{})
	f.shim.AwaitGone()

	// Act: wait for the durable stand-down record, then ask for the roster.
	// THE RECORD IS THE PARK'S COMPLETION: the shim's exit is the sweep's
	// means, and the session terminal the roster reads is written after it.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the sweep's own hibernation record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.drain.sweep" && strings.Contains(r.Message, "hibernated an idle session")
	})
	roster := f.d.WatchRoster()

	// Assert: the row settles on an IDLE arm. `dead` and `severed` both report
	// a fault, and there is none: the daemon put the route down itself.
	settled := awaitRoster(t, f.d, roster, "the parked row settling on an idle arm", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && (row.GetReady() != nil || row.GetDone() != nil)
	})
	row := rosterRow(settled, f.ws.GetId())
	if row.GetDead() != nil || row.GetSevered() != nil {
		t.Fatalf("the parked roster row = %v, want an idle arm and never a fault arm", row)
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
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up")
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

// TestAParkedWorkspacesFooterIsIdleAndTheIndicatorReportsNoFault is the
// SESSION-SCOPED half of the same park, and the one the webapp cannot work
// around: the footer's `disconnected` step read the link the stand-down killed
// as `disconnected · dead`, the topbar's indicator hollowed to the dead glyph,
// and the webapp's composer gate IS the footer's word (webapp/src/main.ts — a
// `disconnected` status closes the composer), so the page could not submit the
// prompt that revives the session. Observed in a headless run of the real
// editor, in the 05-tab-arms-lifecycle scenario's 15-arm-hibernated case.
//
// THE STREAMS ARE OPENED AFTER THE SWEEP'S OWN HIBERNATION RECORD, exactly as
// the roster test above does and for the same measured reason. Both topics
// replay their LATEST value to a new subscriber, so what a late subscriber is
// served is the last view the daemon resolved — which during the defect was
// the link-death one. Opening either stream before the park instead matches
// on a push taken while the session was still live: with the wiring disabled,
// a footer stream opened early settled on `idle` and asserted nothing at all.
//
// The REVIVAL half is the guard on the park's release: the park must not
// outlive the shim the reviving prompt spawns, or a later real death would be
// masked as a stand-down nobody ordered.
//
// COST: the same 50ms cutoff and the same two shim round trips as the roster
// and composer tests above, plus the revival's own StartSession, StartTurn and
// turn conclusion; measured at 0.42s wall inside the parallel suite. The whole
// `make integration` wall time was 17.9s before this test and 17.9s and 20.5s
// on the two runs after it, at load averages of 10.8 and 18.6 -- this test's
// half-second is inside the suite's own load-driven spread, not on top of it.
func TestAParkedWorkspacesFooterIsIdleAndTheIndicatorReportsNoFault(t *testing.T) {
	t.Parallel()
	// Arrange: a very short idle cutoff so hibernation fires promptly.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	// The sweep covers every test; the declared records are evidence of a KillSession the fake shim answers by exiting, a bring-up the test blocks or kills, a session fault the test opens, the shim death the test drives, the shim link the test severs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault", "daemon.sessionwatcher.link_fault",
		"daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.shimclient.kill_session", "daemon.workspace.bring_up")
	// THE START IS READ FROM THE SHIM'S DURABLE LOG, never popped live. At a
	// 50ms cutoff the sweep can hibernate the shim, which then exits, before
	// this line runs, and a live pop then met a closed control socket
	// ("broken pipe") on a loaded host.
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCStartSession, &shimv1.StartSessionRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCHibernate, &shimv1.HibernateRequest{})
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, &shimv1.KillSessionRequest{})
	f.shim.AwaitGone()
	// THE RECORD IS THE PARK'S COMPLETION: the shim's exit is the sweep's
	// means, and the terminal the park is derived from is written after it.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the sweep's own hibernation record", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.drain.sweep" && strings.Contains(r.Message, "hibernated an idle session")
	})
	footer := f.d.WatchFooter(f.ws)
	topbar := f.d.WatchTopbar(f.ws)

	// Assert: the strip settles on the IDLE family. `disconnected` is the word
	// the webapp closes its composer on, and there is no fault to report.
	settled := awaitFooter(t, f, footer, "the parked footer settling on an idle status", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle() != nil
	})
	if settled.GetStrip().GetStatus().GetAgentReplFault() != nil {
		t.Fatalf("the parked footer status = %v, want an idle status and never disconnected", settled.GetStrip().GetStatus())
	}

	// Assert: the indicator reports an ABSENT session rather than a broken
	// one. `dead` ("the session's process is gone") is a fault; the daemon put
	// this route down itself.
	awaitTopbar(t, f, topbar, "the parked connectivity indicator reporting an absent session", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetTitle() == "no session is running"
	})

	// Act: the prompt revives the session, and its turn runs to a conclusion.
	f.submit("wake up", "k-parked-footer-revive", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	shim := f.d.ShimAt(f.d.SocketPath(f.ws) + ".ctl")
	shim.ExpectStartSession()
	shim.ExpectStartTurn()
	// AND THEN WAIT FOR THE DAEMON TO HAVE OPENED IT. The fake shim records
	// StartTurn when the REQUEST arrives, and the daemon names the session's
	// main agent from that call's ANSWER (promptqueue/deliver.go's
	// SetMainAgent, one line before OnTurnOpened) — so a terminal pushed on
	// the request is racing the answer, and a terminal that wins is WITHHELD
	// (sessionwatcher/route.go's `turn_end_withheld`) and never attributed to
	// any turn. The footer then never leaves running and this test waits out
	// its whole budget for an edge the daemon deliberately did not draw.
	f.d.AwaitWorkspaceLogOperationCount(f.repo.Dir, harness.OpTurnOpened, 1)
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: ordinary again — idle with the link attached, which is what
	// tells that the park was LIFTED rather than still standing over a route
	// that happens to serve.
	awaitFooter(t, f, footer, "the revived footer back on idle.done", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetIdle().GetDone() != nil
	})
	awaitTopbar(t, f, topbar, "the revived connectivity indicator back on a serving route", func(v *frontendv1.TopbarView) bool {
		return v.GetConnectivity().GetTitle() == "connected to the session"
	})
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
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the shim death this test drives, the link it severs, and the bring-up the revival runs.
	//
	// daemon.shimclient.gather_title_digest is the same class and is declared
	// for the same reason: the title gather is an ordinary unary call to the
	// session's shim, so whether it is in flight when the SIGKILL lands is a
	// matter of scheduling. When it is, it comes back `unavailable: unexpected
	// EOF` and is recorded at ERROR -- correctly, since this daemon did NOT
	// order the teardown and so shimclient's stand-down latch is not set. The
	// record is evidence of the very kill this test performs.
	f.d.ExpectWarnings("daemon.shimclient.redial", "daemon.sessionwatcher.reopen", "daemon.health.open_fault",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_agent", "daemon.sessionwatcher.watch_session",
		"daemon.shimclient.exit", "daemon.workspace.bring_up", "daemon.shimclient.gather_title_digest")
	f.shim.ExpectStartSession()
	roster := f.d.WatchRoster()
	statusIs := func(pred func(*frontendv1.RosterRow) bool) func(*frontendv1.WorkspaceRoster) bool {
		return func(r *frontendv1.WorkspaceRoster) bool {
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
