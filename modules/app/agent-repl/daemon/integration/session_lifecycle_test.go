//go:build integration

package integration

import (
	"path/filepath"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/integration/harness"

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

	// Assert: cwd.
	if info.Cwd != f.repo.Dir {
		t.Fatalf("shim cwd = %q, want the workspace dir %q", info.Cwd, f.repo.Dir)
	}
}

func TestReadinessGatesOnTheFirstHealthyDiagnostics(t *testing.T) {
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
	// (The proto's `existing` arm with shim_attached false is NOT asserted
	// here: it describes a session this daemon knows of but is not attached to,
	// and a bring-up that never completed records no session to describe.)
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
	// Arrange
	f := newRegistered(t, harness.Opts{})
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
	// made), so its host view is the `none` arm, which carries no faults. The
	// fault IS recorded — the health surface is where it is readable.
	awaitFooter(t, f, footer, "footer disconnected.start_failed", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetDisconnected().GetStartFailed() != nil
	})
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestOpenWorkspaceWithNoPriorConversationStartsAFreshSession(t *testing.T) {
	// Arrange / Act
	f := newOpened(t, harness.Opts{})

	// Assert
	req := f.shim.ExpectStartSession()
	if req.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want a fresh source for a workspace with no prior conversation", req)
	}
}

func TestReopeningAWorkspaceWithAPriorSessionResumesItsVendorSession(t *testing.T) {
	// Arrange: open once to mint a vendor session, then kill it so the
	// workspace's session record carries a vendor id with no live shim.
	f := newOpened(t, harness.Opts{})
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

func TestResumingAMissingVendorTranscriptIsRefusedBeforeSpawn(t *testing.T) {
	// Arrange: a session with a conversation to resume, killed and REAPED, and
	// then its transcript removed from under both account roots. A vanished
	// transcript yields no death evidence, so the guard refuses the resume
	// before any process spawns rather than letting the redial ladder loop
	// forever on an unchangeable fact.
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	if _, err := f.d.Client().KillWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.KillWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("KillWorkspace = error %v, want a success", err)
	}
	f.shim.AwaitGone()
	f.d.RemoveTranscripts(f.repo.Dir)
	spawnsBefore := f.d.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn")

	// Act
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))

	// Assert: the landed transcript_missing arm, and nothing spawned to be
	// refused BY.
	if err != nil {
		t.Fatalf("OpenWorkspace on a missing transcript = transport error %v, want the transcript_missing arm", err)
	}
	if resp.Msg.GetError().GetTranscriptMissing() == nil {
		t.Fatalf("OpenWorkspace on a missing transcript = %v, want OpenWorkspaceError.transcript_missing", resp.Msg)
	}
	if got := f.d.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn"); got != spawnsBefore {
		t.Fatalf("shim spawn records = %d after the refusal, want the %d before it: the guard refuses BEFORE the spawn", got, spawnsBefore)
	}
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestStartSessionResumeColdStandsAGateBlockingReopenUntilAnswered(t *testing.T) {
	// Arrange: kill to get a resumable session, then script the resume as cold.
	f := newOpened(t, harness.Opts{})
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
	// Arrange
	f := newOpened(t, harness.Opts{})
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

func TestAnswerColdGateRefusesAScopeTheMenuNeverServed(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
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
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestTheConfigDirIsDeterminedByTheMultiRepoRoot(t *testing.T) {
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

func TestKillWorkspaceForceKillsTheSessionAndReapsTheShim(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
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

	// Assert: the roster shows dead.
	awaitRoster(t, f.d, roster, "the roster row dead", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetDead() != nil
	})
}

func TestCloseWorkspaceWithNothingLiveSucceedsAndLeavesTheShimRunning(t *testing.T) {
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
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestCloseWorkspaceWithAQueuedMergeRefuses(t *testing.T) {
	// Arrange: a workspace the daemon CREATED, so it carries the merge layout
	// facts an enqueue needs, with a second one ahead of it in its repo's queue
	// so its own merge stays queued rather than running to a terminal.
	repo := harness.NewRepo(t)
	d := harness.StartDaemon(t, harness.Opts{SelfRepo: repo.Dir})
	repoRef := mergeRepositoryRef(t, d, repo)
	first := mergeCreateChild(t, d, repoRef, "ahead", "do the first thing", nil)
	repo.ScriptConflict(repo.Dir, mergeBranchOf(t, first.ws), "conflict.txt")
	second := mergeCreateChild(t, d, repoRef, "behind", "do the second thing", nil)

	// The first merge parks on its scripted conflict and holds the repo lock,
	// so the second one waits in the queue.
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: first.ws})); err != nil {
		t.Fatalf("MergeWorkspace(first) = error %v, want the merge enqueued", err)
	}
	first.shim.ExpectStartTurn()
	d.AwaitWorkspaceLogOperationCount(first.ws.GetDir(), harness.OpTurnOpened, 2)
	first.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID("conflict-brief-done")))
	host := d.WatchHost(first.ws)
	awaitView(t, first, host, "the first merge parked", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetExisting().GetLive().GetMergeParked() != nil
	})
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: second.ws})); err != nil {
		t.Fatalf("MergeWorkspace(second) = error %v, want the merge enqueued", err)
	}
	roster := d.WatchRoster()
	awaitRoster(t, d, roster, "the second workspace's queued merge", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, second.ws.GetId())
		return row.GetMergeQueued() != nil || row.GetMergeEnqueuing() != nil
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
	d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestCloseWorkspaceWithAStandingColdGateSucceeds(t *testing.T) {
	// Arrange: stand a cold gate with no turn and no live async work.
	f := newOpened(t, harness.Opts{})
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
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.submit("long running work", "k-restart-forced", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()

	// Act
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws, Force: true}))

	// Assert
	if err != nil {
		t.Fatalf("RestartWorkspace{force:true} = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, want a success", resp.Msg)
	}
	// A forced restart interrupts THE TURN first -- KillTurn{force:true} --
	// which is what lets the relaunch engine's freeness wait complete without
	// the turn's own terminal frame. The stand-down that follows is still the
	// engine's GRACEFUL KillSession: the force is the caller's verdict on the
	// running turn, never on the session's own shutdown.
	killedTurn := f.shim.ExpectKillTurn()
	if !killedTurn.GetForce() {
		t.Fatalf("KillTurn.force = false on a forced restart, want true: the running turn is interrupted rather than waited out")
	}
	killed := &shimv1.KillSessionRequest{}
	f.d.AwaitShimLoggedRequest(f.repo.Dir, harness.RPCKillSession, killed)
	if killed.GetForce() {
		t.Fatalf("KillSession.force = true, want the engine's graceful stand-down")
	}
}

func TestRestartWorkspaceGracefulHoldsPromptsWithBuildRefreshAndDrainsAfterReadiness(t *testing.T) {
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.shim.ExpectStartSession()
	f.submit("long running work", "k-restart-graceful-turn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.ExpectStartTurn()
	holds := f.d.WatchHolds(f.ws)

	// Act: a graceful restart is accepted behind the in-flight work.
	resp, err := f.d.Client().RestartWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.RestartWorkspaceRequest{Workspace: f.ws, Force: false}))
	if err != nil {
		t.Fatalf("RestartWorkspace{force:false} = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("RestartWorkspace = %v, want a success", resp.Msg)
	}

	// The verb ACCEPTS and the relaunch engine runs behind it, so the
	// restart-pending hold stands a moment later. The host composer's
	// `restarting` arm is that moment, and it is what a prompt submitted
	// "meanwhile" has to arrive after to be held for the RESTART rather than
	// for the turn still running.
	hostStream := f.d.WatchHost(f.ws)
	harness.AwaitView(t, f.d.Ctx(), hostStream, "the host composer restarting",
		func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
			return r.GetHost().GetExisting().GetLive().GetRestarting() != nil
		})

	// A prompt submitted meanwhile is held rather than forwarded.
	held := f.submit("meanwhile", "k-restart-graceful-meanwhile", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	if held.GetSuccess().GetTurn().GetTurn().GetValue() == "" {
		t.Fatalf("SubmitPrompt while restart-held = %v, want a minted TurnId even though delivery is held", held)
	}

	// Assert: the tray shows it held under the build_refresh/restart hold.
	awaitTray := harness.AwaitView(t, f.d.Ctx(), holds, "the meanwhile prompt held for the restart", func(tray *frontendv1.DaemonHoldTray) bool {
		for _, item := range tray.GetItems() {
			if item.GetPrompt().GetBuildRefresh() != nil {
				return true
			}
		}
		return false
	})
	_ = awaitTray

	// Act: end the in-flight turn, letting the graceful restart proceed.
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the held prompt drains once the restarted session is ready
	// again — the tray empties of the build_refresh entry.
	harness.AwaitView(t, f.d.Ctx(), holds, "the tray drained of the build_refresh hold after readiness", func(tray *frontendv1.DaemonHoldTray) bool {
		for _, item := range tray.GetItems() {
			if item.GetPrompt().GetBuildRefresh() != nil {
				return false
			}
		}
		return true
	})
}

func TestHibernationParksAnIdleSessionAndRevivesOnPrompt(t *testing.T) {
	// Arrange: a very short idle cutoff so hibernation fires promptly.
	f := newOpened(t, harness.Opts{IdleCutoffMS: 50})
	f.shim.ExpectStartSession()
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
	f.shim.AwaitGone()

	// Assert: the roster shows the parked state per the sidebar arms — a
	// session whose backing process is gone but the workspace is
	// recoverable, distinct from a crash (RosterRowStatusSevered is the only
	// arm matching that description; the sidebar proto has no dedicated
	// "hibernated" arm — see report).
	awaitRoster(t, f.d, roster, "the parked roster row", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, f.ws.GetId())
		return row != nil && row.GetSevered() != nil
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

func TestBuildStalenessBounceRelaunchesAStaleShimAtFreeness(t *testing.T) {
	// Arrange: the deployed stamp disagrees with what the fake reports, so the
	// mount finds the session on an older build.
	f := newRegistered(t, harness.Opts{ExtraEnv: []string{"AGENT_REPL_DEPLOY_STAMP=deployed-sha"}})
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

	// Act: mount again. The relaunched shim reports the SAME older stamp.
	if _, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("the second OpenWorkspace = error %v, want a success", err)
	}

	// Assert: THE BOUNCE FIRES ONCE PER STAMP. A check that did not remember
	// what it had already bounced for would bounce again on every mount,
	// spawning a process per round forever.
	if got := f.d.WorkspaceLogOperationCount(f.repo.Dir, "daemon.shimclient.spawn"); got != spawns {
		t.Fatalf("shim spawns = %d after a second mount, want the %d already made: the stamp was already bounced for", got, spawns)
	}
	f.d.ExpectWarnings(harness.AllowAllWarnings)
}

func TestCrashBootAdoptsARunningShimWithoutASecondSpawn(t *testing.T) {
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
