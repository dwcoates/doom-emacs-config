//go:build integration

package integration

import (
	"os"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"
)

// ---------------------------------------------------------------------------
// DaemonHealth
// ---------------------------------------------------------------------------

func TestDaemonHealthIdentityChangesAfterAProcessRestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	first := newDaemon(t, harness.Opts{DurableState: true})
	firstResponse, err := first.Client().DaemonHealth(first.Ctx(), healthRequest())
	if err != nil {
		t.Fatalf("first DaemonHealth = error %v, want a success", err)
	}
	firstIdentity := firstResponse.Msg.GetSuccess().GetIdentity()
	stateDir := first.StateDir
	first.Stop()

	// Act.
	second := harness.StartDaemon(t, harness.Opts{StateDir: stateDir})
	secondResponse, err := second.Client().DaemonHealth(second.Ctx(), healthRequest())

	// Assert.
	if err != nil {
		t.Fatalf("second DaemonHealth = error %v, want a success", err)
	}
	secondIdentity := secondResponse.Msg.GetSuccess().GetIdentity()
	if firstIdentity.GetInstanceId() == "" || secondIdentity.GetInstanceId() == "" {
		t.Fatalf("daemon identities = first %v, second %v; want both instance ids", firstIdentity, secondIdentity)
	}
	if firstIdentity.GetInstanceId() == secondIdentity.GetInstanceId() {
		t.Fatalf("daemon instance id after restart = %q, want a new identity", secondIdentity.GetInstanceId())
	}
	if firstIdentity.GetPid() == secondIdentity.GetPid() {
		t.Fatalf("daemon pid after restart = %d, want a new process", secondIdentity.GetPid())
	}
	if secondIdentity.GetPid() != int64(second.PID()) {
		t.Fatalf("DaemonHealth pid = %d, want serving pid %d", secondIdentity.GetPid(), second.PID())
	}
	if firstIdentity.GetBuildSha() != secondIdentity.GetBuildSha() || secondIdentity.GetBuildSha() == "" {
		t.Fatalf("daemon build identity = first %q, second %q; want one non-empty deployed build", firstIdentity.GetBuildSha(), secondIdentity.GetBuildSha())
	}
}

// TestADaemonRestartDoesNotReopenAClosedPromptsDirFault checks the OTHER half
// of closure being real: once a fault is closed, a fresh runtime reading the
// same faults table must not resurrect it merely because it once stood.
func TestADaemonRestartDoesNotReopenAClosedPromptsDirFault(t *testing.T) {
	t.Parallel()
	// Arrange: open then close the prompts-dir fault, on a state root a
	// restart will reuse.
	d := newDaemon(t, harness.Opts{DurableState: true})
	// The support workspaces created below have in-flight turns and no session,
	// and closing them is a warning by design.
	d.ExpectWarnings("daemon.workspace.request_command_support",
		"daemon.health.open_fault", "daemon.health.daemon")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	if err := os.RemoveAll(d.PromptsDir); err != nil {
		t.Fatalf("removing the prompts dir: %v", err)
	}
	if _, err := d.Client().RequestCommandSupport(d.Ctx(), connect.NewRequest(&agentreplv1.RequestCommandSupportRequest{
		Workspace: ws,
		Command:   "/status",
	})); err == nil {
		t.Log("RequestCommandSupport succeeded before the dir was restored; the fault is asserted below regardless")
	}
	harness.LinkPrompts(t, d.PromptsDir)
	if _, err := d.Client().RequestCommandSupport(d.Ctx(), connect.NewRequest(&agentreplv1.RequestCommandSupportRequest{
		Workspace: ws,
		Command:   "/status2",
	})); err != nil {
		t.Fatalf("RequestCommandSupport after repair = error %v, want a success", err)
	}
	stateDir := d.StateDir

	// Act: stop this daemon and start a fresh one on the SAME state root, so
	// the faults table (wsm.db) persists across the restart.
	d.Stop()
	// The sweep covers every test; the declared records are evidence of the in-flight turn the restart orphans.
	nd := harness.StartDaemon(t, harness.Opts{ExpectWarnings: []string{"daemon.promptqueue.restore_holds"}, StateDir: stateDir})

	// Assert
	resp, err := nd.Client().DaemonHealth(nd.Ctx(), healthRequest())
	if err != nil {
		t.Fatalf("DaemonHealth after restart = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("DaemonHealth after a restart following a closed fault = %v, want success{healthy}: a closed fault must not reopen across a restart", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// SessionHealth
// ---------------------------------------------------------------------------

func TestSessionHealthRelaysAnUnhealthyDiagnosticsPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// A shim-reported fault IS a fault: the health reporter opens it and
	// answers unhealthy, and each is recorded loudly by design. The routing to
	// the topbar is the only DEBUG half of this.
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.health.session")

	// Act: the shim reports itself unhealthy.
	f.shim.PushUnhealthy(&conversationv1.SessionFault{
		Component: "store client",
		Detail:    "the store socket went away",
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	})
	// The fault's own record is the daemon's evidence that it absorbed the
	// push: the health reporter records it before any view is drawn, so the
	// health probe below is not racing the push.
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the session fault recorded", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.health.open_fault"
	})

	resp, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("SessionHealth = error %v, want a success carrying the fault", err)
	}
	unhealthy := resp.Msg.GetSuccess().GetUnhealthy()
	if unhealthy == nil || len(unhealthy.GetFaults()) == 0 {
		t.Fatalf("SessionHealth after an unhealthy push = %v, want success{unhealthy{faults}}", resp.Msg)
	}
	if got := unhealthy.GetFaults()[0].GetShimReported(); got == nil {
		t.Fatalf("the relayed fault = %v, want the shim_reported arm", unhealthy.GetFaults()[0])
	}
}

// TestSessionHealthAfterTheShimExitsReportsShimDied is the SessionHealth
// counterpart of TestFooterShimExitFlipsToDeadAndStopsRedials: a shim that
// exits mid-session, rather than merely losing its link, is the shim_died
// arm and carries the exit code.
func TestSessionHealthAfterTheShimExitsReportsShimDied(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// A lost link is EVIDENCE, recorded loudly: the watcher says so, the
	// health reporter opens the fault, and SessionHealth answers unhealthy.
	// daemon.shimclient.redial is the adopted-death witness doing its job: the
	// monitor can see the stream break before the exit is decoded, so it says
	// "shim link broke; redialing" and then, the moment the death is evidence,
	// "redial stopped". Before the witness was wired the redials looped
	// forever instead; both records are failure-path evidence and stay loud.
	f.d.ExpectWarnings("daemon.sessionwatcher.reopen", "daemon.shimclient.exit", "daemon.shimclient.redial",
		"daemon.sessionwatcher.watch_session", "daemon.sessionwatcher.watch_agent",
		"daemon.sessionwatcher.link_fault", "daemon.health.open_fault", "daemon.health.session")
	footer := f.d.WatchFooter(f.ws)

	// The daemon brings a shim that died on its own straight back, and the
	// revival's HEALTHY ATTACH closes shim_died (workspace/sessions.go: the
	// attach is the shim's first healthy diagnostics, before any
	// StartSession). The revived shim therefore WITHHOLDS those diagnostics,
	// so the dead state this test is about stands for as long as the test
	// looks at it. Holding StartSession instead left it standing only until
	// the attach: under a full run the revival attached between the footer's
	// "dead" and the SessionHealth call, which then answered link_severed.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{DelayDiagnostics: true})

	// Act: the fake shim process exits outright, mid-session.
	f.shim.Exit(1, "simulated crash")
	awaitFooter(t, f, footer, "disconnected.dead once the shim exits", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetDead() != nil
	})

	// Assert
	resp, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("SessionHealth = error %v, want a success carrying the fault", err)
	}
	unhealthy := resp.Msg.GetSuccess().GetUnhealthy()
	if unhealthy == nil || len(unhealthy.GetFaults()) == 0 {
		t.Fatalf("SessionHealth after the shim exits = %v, want success{unhealthy{faults}}", resp.Msg)
	}
	died := unhealthy.GetFaults()[0].GetShimDied()
	if died == nil {
		t.Fatalf("the reported fault = %v, want the shim_died arm", unhealthy.GetFaults()[0])
	}
	if got := died.GetExitCode(); got != 1 {
		t.Fatalf("shim_died.exit_code = %d, want 1", got)
	}
}

// TestSessionHealthAfterTheLinkIsSeveredReportsLinkSevered is the
// SessionHealth counterpart of TestFooterLinkDeathFlipsToSeveredAndTheDaemonRedials:
// a live shim whose stream was severed (never exited) is the link_severed
// arm, distinct from shim_died.
// TestALinkSeveredUnderALiveShimIsReportedUntilItServesAgain covers a
// severing's whole life: the footer draws the link severed, the session's
// link_severed fault opens, and once the watches re-open and serve under the
// same live shim the fault closes on its link_restored edge, leaving the
// session healthy (2026-11-16: rows stood severed while their workspaces
// worked).
func TestALinkSeveredUnderALiveShimIsReportedUntilItServesAgain(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
	// The sweep covers every test; the declared records are evidence of a session fault the test opens, the shim link the test severs.
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.health.session",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.reopen",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.redial")
	footer := f.d.WatchFooter(f.ws)

	// Act: sever the session stream without killing the shim process.
	f.shim.DropStream(harness.StreamSession)

	// Assert: drawn severed, then the severing closed on its own edge.
	awaitFooter(t, f, footer, "disconnected.severed once the link dies", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetSevered() != nil
	})
	f.d.AwaitOwnRunLogRecord("the severing closed once the link served again", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.health.close_on_edge" && r.Context["kind"] == "link_severed" && r.Context["edge"] == "link_restored"
	})
	resp, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil || resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("SessionHealth = (%v, %v), want healthy once the link serves again", resp, err)
	}
}

// ---------------------------------------------------------------------------
// ClientLog
// ---------------------------------------------------------------------------

func TestClientLogWritesARecordIntoTheWebappSink(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonicalRegistered(t)
	sentTimestamp := "2026-09-10T16:34:56.789Z"
	instant, err := time.Parse(time.RFC3339, sentTimestamp)
	if err != nil {
		t.Fatalf("parse fixture timestamp: %v", err)
	}
	wantTimestamp := instant.In(time.Local).Format("2006-01-02T15:04:05.000000-07:00")
	context, err := structpb.NewStruct(map[string]any{"pane": "composer"})
	if err != nil {
		t.Fatalf("building the log context: %v", err)
	}

	// Act
	resp, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Source: &agentreplv1.ClientLogRequest_Workspace{Workspace: f.ws},
		Record: &agentreplv1.ClientLogRecord{
			Site: "0123456789ab", Template: "a test record",
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "command-dispatch.deferred",
			Message:   "the webview deferred a command",
			Context:   context,
			Timestamp: sentTimestamp,
			Verbose:   false,
		},
	}))

	// Assert
	if err != nil {
		t.Fatalf("ClientLog = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("ClientLog = %v, want a success", resp.Msg)
	}
	rec := f.d.AwaitLogRecord(harness.ClientLogPath(f.ws), "the client's record", func(r harness.LogRecord) bool {
		return r.Operation == "command-dispatch.deferred"
	})
	if !strings.Contains(rec.Message, "deferred a command") {
		t.Fatalf("the persisted record = %q, want the client's own sentence", rec.Raw)
	}
	if rec.Level != "info" {
		t.Fatalf("the persisted record's level = %q, want %q for an info record", rec.Level, "info")
	}
	if rec.Timestamp != wantTimestamp {
		t.Fatalf("the persisted record's timestamp = %q, want the client's %q", rec.Timestamp, wantTimestamp)
	}
	if rec.Verbosity != "normal" {
		t.Fatalf("the persisted record's verbosity = %q, want normal", rec.Verbosity)
	}
	if got := rec.Context["pane"]; got != "composer" {
		t.Fatalf("the persisted record's context.pane = %v, want %q verbatim", got, "composer")
	}
}

// ---------------------------------------------------------------------------
// The login pty
// ---------------------------------------------------------------------------

func TestOpenLoginSpawnsThePtyAndReplaysItsScrollback(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegisteredInGroup(t, groupLoginTerminal)

	// Act
	resp, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("OpenLogin = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenLogin = %v, want a success naming the config dir", resp.Msg)
	}
	stream := f.d.WatchLogin(f.ws)

	// Assert: the scrollback the pty already produced is replayed to a late
	// subscriber, which is the never-miss invariant for this stream.
	loginAwaitMarker(t, f, stream, harness.FakeClaudeLoginMarker)
}

// ---------------------------------------------------------------------------
// OpenInEditor and OpenExternal
// ---------------------------------------------------------------------------

func TestOpenExternalInvokesTheConfiguredLauncher(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonicalRegistered(t)
	// The launcher is the daemon's one stub, so the case's own link is the
	// one only it opens.
	url := "https://example.invalid/report/" + f.ws.GetId()

	// Act
	if _, err := f.d.Client().OpenExternal(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: f.ws,
		Url:       url,
	})); err != nil {
		t.Fatalf("OpenExternal = error %v, want a success", err)
	}

	// Assert
	var invocations []harness.Invocation
	for _, inv := range f.d.Browser.Invocations() {
		if loginArgvHas(inv.Argv, url) {
			invocations = append(invocations, inv)
		}
	}
	if len(invocations) != 1 {
		t.Fatalf("the browser launcher ran %d times, want exactly once: %+v", len(invocations), invocations)
	}
}

// TestOpenExternalWithNoBrowserConfiguredAnswersNoBrowserConfigured exercises
// OpenExternalError's no_browser_configured arm. The daemon is started with
// `--no-browser`, which is the operator's explicit statement that this host
// has no external browser: cmd/agent-repld/graph.go then leaves the Browser
// dependency nil and internal/workspace/links.go refuses.
func TestOpenExternalWithNoBrowserConfiguredAnswersNoBrowserConfigured(t *testing.T) {
	t.Parallel()
	// Arrange
	// The graph says so out loud at boot: a daemon that cannot open a link is
	// not silently degraded.
	f := newRegistered(t, harness.Opts{ExtraArgs: []string{"--no-browser"}, ExpectWarnings: []string{"daemon.cmd.graph"}})

	// Act
	resp, err := f.d.Client().OpenExternal(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: f.ws,
		Url:       "https://example.invalid/report",
	}))

	// Assert
	if err != nil {
		t.Fatalf("OpenExternal(no browser) = error %v, want a success carrying error{no_browser_configured}", err)
	}
	if got := resp.Msg.GetError().GetNoBrowserConfigured(); got == nil {
		t.Fatalf("OpenExternal(no browser) = %v, want error{no_browser_configured}", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// Suite-local helpers
// ---------------------------------------------------------------------------

// loginAwaitMarker reads terminal bytes until the accumulated output carries
// the text, so a marker split across writes still satisfies the wait.
func loginAwaitMarker(t *testing.T, f *fixture, s *harness.Stream[*agentreplv1.LoginTerminalOutput], want string) {
	t.Helper()
	var seen strings.Builder
	harness.AwaitView(t, f.d.Ctx(), s, "the terminal text "+want, func(o *agentreplv1.LoginTerminalOutput) bool {
		seen.Write(o.GetBytes().GetData())
		return strings.Contains(seen.String(), want)
	})
}

// loginArgvHas reports whether an argument vector carries an exact argument.
func loginArgvHas(argv []string, want string) bool {
	for _, a := range argv {
		if a == want {
			return true
		}
	}
	return false
}
