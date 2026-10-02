//go:build integration

package integration

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/types/known/structpb"
)

// ---------------------------------------------------------------------------
// DaemonHealth
// ---------------------------------------------------------------------------

func TestDaemonHealthOnAFreshDaemonIsHealthy(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	resp, err := d.Client().DaemonHealth(d.Ctx(), healthRequest())

	// Assert
	if err != nil {
		t.Fatalf("DaemonHealth = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("DaemonHealth = %v, want success{healthy}", resp.Msg)
	}
}

func TestDaemonHealthIdentityChangesAfterAProcessRestart(t *testing.T) {
	t.Parallel()
	// Arrange.
	first := newDaemon(t, harness.Opts{})
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

func TestDaemonHealthWithAnOpenFaultIsUnhealthy(t *testing.T) {
	t.Parallel()
	// Arrange: the prompts directory the daemon booted with is taken away, the
	// one fault a test can open without breaking the daemon's own boot.
	d := newDaemon(t, harness.Opts{})
	// The test's own subject IS the daemon's loud failure: the missing brief
	// is read (and the fault opened) inside RequestCommandSupport, which logs
	// ERROR under daemon.workspace.request_command_support
	// (internal/workspace/commandsupport.go).
	// Opening the fault and the daemon reading unhealthy are the SUBJECT of
	// this test, and each is recorded loudly by design.
	d.ExpectWarnings("daemon.workspace.request_command_support",
		"daemon.health.open_fault", "daemon.health.daemon")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	if err := os.RemoveAll(d.PromptsDir); err != nil {
		t.Fatalf("removing the prompts dir: %v", err)
	}
	// A verb that reads a brief is what discovers the missing directory.
	if _, err := d.Client().RequestCommandSupport(d.Ctx(), connect.NewRequest(&agentreplv1.RequestCommandSupportRequest{
		Workspace: ws,
		Command:   "/status",
	})); err == nil {
		t.Log("RequestCommandSupport succeeded; the fault is asserted through DaemonHealth below")
	}

	// Act
	resp, err := d.Client().DaemonHealth(d.Ctx(), healthRequest())

	// Assert
	if err != nil {
		t.Fatalf("DaemonHealth = error %v, want a success carrying the fault", err)
	}
	unhealthy := resp.Msg.GetSuccess().GetUnhealthy()
	if unhealthy == nil || len(unhealthy.GetFaults()) == 0 {
		t.Fatalf("DaemonHealth with a missing prompts dir = %v, want success{unhealthy{faults}}", resp.Msg)
	}
	// The KIND is the point: a missing prompts dir is CLOSURE-TYPED, and its
	// own field carries the exact path that is not there.
	missing := unhealthy.GetFaults()[0].GetPromptsDirMissing()
	if missing == nil {
		t.Fatalf("the reported fault = %v, want the prompts_dir_missing arm", unhealthy.GetFaults()[0])
	}
	if got := missing.GetPath(); got != d.PromptsDir {
		t.Fatalf("prompts_dir_missing.path = %q, want the daemon's own PromptsDir %q", got, d.PromptsDir)
	}
}

// TestRestoringThePromptsDirClosesTheFault expects the fault this daemon opened
// above to CLOSE once the directory comes back and a brief reads again — the
// symmetric half of "open" a fault record must have to be a record of a
// CONDITION rather than a one-way trip.
func TestRestoringThePromptsDirClosesTheFault(t *testing.T) {
	t.Parallel()
	// Arrange: open the fault exactly as the sibling test above does.
	d := newDaemon(t, harness.Opts{})
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
	preRepair, err := d.Client().DaemonHealth(d.Ctx(), healthRequest())
	if err != nil {
		t.Fatalf("DaemonHealth before repair = error %v, want a success carrying the fault", err)
	}
	if preRepair.Msg.GetSuccess().GetUnhealthy() == nil {
		t.Fatalf("DaemonHealth before repair = %v, want success{unhealthy}", preRepair.Msg)
	}

	// Act: the directory comes back, and a brief read through it succeeds.
	harness.CopyPrompts(t, d.PromptsDir)
	if _, err := d.Client().RequestCommandSupport(d.Ctx(), connect.NewRequest(&agentreplv1.RequestCommandSupportRequest{
		Workspace: ws,
		Command:   "/status2",
	})); err != nil {
		t.Fatalf("RequestCommandSupport after repair = error %v, want a success now that the brief reads", err)
	}

	// Assert
	resp, err := d.Client().DaemonHealth(d.Ctx(), healthRequest())
	if err != nil {
		t.Fatalf("DaemonHealth after repair = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("DaemonHealth after the prompts dir was restored = %v, want success{healthy}", resp.Msg)
	}
}

// TestADaemonRestartDoesNotReopenAClosedPromptsDirFault checks the OTHER half
// of closure being real: once a fault is closed, a fresh runtime reading the
// same faults table must not resurrect it merely because it once stood.
func TestADaemonRestartDoesNotReopenAClosedPromptsDirFault(t *testing.T) {
	t.Parallel()
	// Arrange: open then close the prompts-dir fault, on a state root a
	// restart will reuse.
	d := newDaemon(t, harness.Opts{})
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
	harness.CopyPrompts(t, d.PromptsDir)
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
	nd := harness.StartDaemon(t, harness.Opts{StateDir: stateDir})
	// The sweep covers every test; the declared records are evidence of the in-flight turn the restart orphans.
	nd.ExpectWarnings("daemon.promptqueue.restore_holds")

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

func TestSessionHealthForALiveSessionIsHealthy(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))

	// Assert
	if err != nil {
		t.Fatalf("SessionHealth = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("SessionHealth of a live session = %v, want success{healthy}", resp.Msg)
	}
}

func TestSessionHealthRelaysAnUnhealthyDiagnosticsPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// A shim-reported fault IS a fault: the health reporter opens it and
	// answers unhealthy, and each is recorded loudly by design. The routing to
	// the topbar is the only DEBUG half of this.
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.health.session")
	topbar := f.d.WatchTopbar(f.ws)

	// Act: the shim reports itself unhealthy.
	f.shim.PushUnhealthy(&conversationv1.SessionFault{
		Component: "store client",
		Detail:    "the store socket went away",
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	})
	// The topbar's warning strip is the daemon's own evidence that it absorbed
	// the fault, so the health probe below is not racing the push.
	awaitTopbar(t, f, topbar, "a topbar warning for the session fault", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) > 0
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

// TestSessionHealthReturnsToHealthyAfterAHealthyDiagnosticsPush is the
// retraction half of TestSessionHealthRelaysAnUnhealthyDiagnosticsPush: the
// shim's health verdict is the WHOLE verdict on every push
// (cmd/claude-repld/lifecycle.go OnSessionDiagnostics), so a healthy push
// closes the standing shim-reported fault and SessionHealth reads healthy
// again without a restart.
func TestSessionHealthReturnsToHealthyAfterAHealthyDiagnosticsPush(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.health.session")
	topbar := f.d.WatchTopbar(f.ws)
	f.shim.PushUnhealthy(&conversationv1.SessionFault{
		Component: "store client",
		Detail:    "the store socket went away",
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	})
	awaitTopbar(t, f, topbar, "a topbar warning for the session fault", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) > 0
	})
	pre, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil || pre.Msg.GetSuccess().GetUnhealthy() == nil {
		t.Fatalf("SessionHealth before the retraction = %v (err %v), want success{unhealthy}", pre.Msg, err)
	}

	// Act: the shim retracts by pushing a healthy diagnostics verdict.
	f.shim.PushHealthy()
	awaitTopbar(t, f, topbar, "the topbar warning retracted", func(v *frontendv1.TopbarView) bool {
		return len(v.GetWarnings().GetWarnings()) == 0
	})

	// Assert
	resp, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("SessionHealth after the retraction = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess().GetHealthy() == nil {
		t.Fatalf("SessionHealth after a healthy diagnostics push = %v, want success{healthy}", resp.Msg)
	}
}

// TestSessionHealthAfterTheShimExitsReportsShimDied is the SessionHealth
// counterpart of TestFooterShimExitFlipsToDeadAndStopsRedials: a shim that
// exits mid-session, rather than merely losing its link, is the shim_died
// arm and carries the exit code.
func TestSessionHealthAfterTheShimExitsReportsShimDied(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
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

	// The daemon brings a shim that died on its own straight back; the
	// revived shim HOLDS its StartSession, so the dead state this test is
	// about stands for as long as the test looks at it.
	f.d.WriteShimProfile(f.repo.Dir, harness.ShimProfile{HangStartSession: true})

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
func TestSessionHealthAfterTheLinkIsSeveredReportsLinkSevered(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of a session fault the test opens, the shim link the test severs.
	f.d.ExpectWarnings("daemon.health.open_fault", "daemon.health.session",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.reopen",
		"daemon.sessionwatcher.watch_session", "daemon.shimclient.redial")
	footer := f.d.WatchFooter(f.ws)

	// Act: sever the session stream without killing the shim process.
	f.shim.DropStream(harness.StreamSession)
	awaitFooter(t, f, footer, "disconnected.severed once the link dies", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetAgentReplFault().GetSevered() != nil
	})

	// Assert
	resp, err := f.d.Client().SessionHealth(f.d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("SessionHealth = error %v, want a success carrying the fault", err)
	}
	unhealthy := resp.Msg.GetSuccess().GetUnhealthy()
	if unhealthy == nil || len(unhealthy.GetFaults()) == 0 {
		t.Fatalf("SessionHealth with a severed link = %v, want success{unhealthy{faults}}", resp.Msg)
	}
	if got := unhealthy.GetFaults()[0].GetLinkSevered(); got == nil {
		t.Fatalf("the reported fault = %v, want the link_severed arm", unhealthy.GetFaults()[0])
	}
}

func TestSessionHealthOfAnUnknownWorkspaceIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// unknown_workspace is a LANDED SessionHealthError arm
	// (endpoint_session_health.proto): server.resolveRefLogging fills its Arm
	// and answers it in band, so it is logged at DEBUG, never WARN
	// (internal/server/refuse.go refuse()).

	// Act
	resp, err := d.Client().SessionHealth(d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()},
	}))

	// Assert: the arm is LANDED, so the answer is always the in-band typed
	// arm, never a transport error (settled by reading resolveRefLogging in
	// internal/server/refuse.go, per critique 22).
	if err != nil {
		t.Fatalf("SessionHealth(unknown) = error %v, want a success carrying error{unknown_workspace}", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("SessionHealth(unknown) = %v, want error{unknown_workspace}", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// ClientLog
// ---------------------------------------------------------------------------

func TestClientLogWritesARecordIntoTheWebappSink(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
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
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
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

func TestClientLogWritesASidecarRecordIntoTheSidecarSink(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newRegistered(t, harness.Opts{})
	sentTimestamp := "2026-09-10T16:34:56.789Z"
	instant, err := time.Parse(time.RFC3339, sentTimestamp)
	if err != nil {
		t.Fatalf("parse fixture timestamp: %v", err)
	}
	wantTimestamp := instant.In(time.Local).Format("2006-01-02T15:04:05.000000-07:00")

	// Act.
	resp, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "sidecar.transcript.read",
			Message:   "the sidecar read a transcript",
			Timestamp: sentTimestamp,
			Verbose:   true,
			Runtime: &agentreplv1.ClientLogRecord_Sidecar{
				Sidecar: &agentreplv1.ClientLogRuntimeSidecar{},
			},
		},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ClientLog = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("ClientLog = %v, want a success", resp.Msg)
	}
	rec := f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.ws.GetDir(), "sidecar"), "the sidecar's record", func(r harness.LogRecord) bool {
		return r.Operation == "sidecar.transcript.read"
	})
	if rec.Runtime != "sidecar" {
		t.Fatalf("the persisted record's runtime = %q, want sidecar", rec.Runtime)
	}
	if rec.Timestamp != wantTimestamp {
		t.Fatalf("the persisted record's timestamp = %q, want the client's %q", rec.Timestamp, wantTimestamp)
	}
	if rec.Verbosity != "verbose" {
		t.Fatalf("the persisted record's verbosity = %q, want verbose", rec.Verbosity)
	}
}

func TestClientLogPersistsTheClientsVerboseClass(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newRegistered(t, harness.Opts{})

	// Act.
	resp, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.trace",
			Message:   "the webview traced a branch",
			Verbose:   true,
		},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("ClientLog = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("ClientLog = %v, want a success", resp.Msg)
	}
	rec := f.d.AwaitLogRecord(harness.ClientLogPath(f.ws), "the client's verbose record", func(r harness.LogRecord) bool {
		return r.Operation == "webapp.trace"
	})
	if rec.Verbosity != "verbose" {
		t.Fatalf("the persisted record's verbosity = %q, want verbose", rec.Verbosity)
	}
}

// TestClientLogAtWarnLandsAtWarnInTheWebappSink is the warn half of the level
// discipline: the record's level arm, not any fixed level, is what the sink
// persists.
func TestClientLogAtWarnLandsAtWarnInTheWebappSink(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Warn{Warn: &agentreplv1.ClientLogLevelWarn{}},
			Operation: "webapp.render.slow",
			Message:   "a render took too long",
		},
	}))

	// Assert
	if err != nil {
		t.Fatalf("ClientLog = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("ClientLog = %v, want a success", resp.Msg)
	}
	rec := f.d.AwaitLogRecord(harness.ClientLogPath(f.ws), "the client's warn record", func(r harness.LogRecord) bool {
		return r.Operation == "webapp.render.slow"
	})
	if rec.Level != "warn" {
		t.Fatalf("the persisted record's level = %q, want %q", rec.Level, "warn")
	}
}

// TestClientLogAtErrorLandsAtErrorInTheWebappSink is the error half.
func TestClientLogAtErrorLandsAtErrorInTheWebappSink(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Error{Error: &agentreplv1.ClientLogLevelError{}},
			Operation: "webapp.crash",
			Message:   "the webview threw uncaught",
		},
	}))

	// Assert
	if err != nil {
		t.Fatalf("ClientLog = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("ClientLog = %v, want a success", resp.Msg)
	}
	rec := f.d.AwaitLogRecord(harness.ClientLogPath(f.ws), "the client's error record", func(r harness.LogRecord) bool {
		return r.Operation == "webapp.crash"
	})
	if rec.Level != "error" {
		t.Fatalf("the persisted record's level = %q, want %q", rec.Level, "error")
	}
}

// TestClientLogWithAnUnsetLevelAnswersInvalidArgumentNamingLevel is the
// validation half: the level oneof is not optional, and an unset one is a
// Connect InvalidArgument naming the field, never an arm.
func TestClientLogWithAnUnsetLevelAnswersInvalidArgumentNamingLevel(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	_, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
			Operation: "webapp.something",
			Message:   "a message with no level",
		},
	}))

	// Assert
	if err == nil {
		t.Fatalf("ClientLog with no level = success, want InvalidArgument naming level")
	}
	if connectCode(err) != connect.CodeInvalidArgument {
		t.Fatalf("ClientLog with no level = error %v, want CodeInvalidArgument", err)
	}
	if !containsField(err, "level") {
		t.Fatalf("ClientLog with no level = error %v, want it to name the field", err)
	}
}

// TestClientLogOnAnUnknownWorkspaceIsRefused: `ClientLogError.unknown_workspace`
// landed 2026-09-12 (realtest 8, finding E), so the refusal is now IN BAND — a
// typed arm, no Connect error, and no unlanded-arm warning. It is also ordinary
// traffic: a forwarder learns its workspace is gone only by being told, so the
// daemon records the refusal at INFO and this test expects no warnings at all.
func TestClientLogOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})

	// Act
	resp, err := d.Client().ClientLog(d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()},
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Info{Info: &agentreplv1.ClientLogLevelInfo{}},
			Operation: "webapp.something",
			Message:   "a message",
		},
	}))

	// Assert
	if err != nil {
		t.Fatalf("ClientLog(unknown) = %v, want the typed refusal", err)
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("ClientLog(unknown) = %v, want unknown_workspace", resp.Msg.GetResult())
	}
}

// TestClientLogNeverAppearsInTheDaemonRunLog confines a client's own record to
// the workspace's webapp sink: the daemon's run log is the DAEMON's own
// account of itself, and a client's diagnostic sentence is not that.
func TestClientLogNeverAppearsInTheDaemonRunLog(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

	// Act
	if _, err := f.d.Client().ClientLog(f.d.Ctx(), connect.NewRequest(&agentreplv1.ClientLogRequest{
		Workspace: f.ws,
		Record: &agentreplv1.ClientLogRecord{
			Level:     &agentreplv1.ClientLogRecord_Error{Error: &agentreplv1.ClientLogLevelError{}},
			Operation: "webapp.crash",
			Message:   "a fatal client error",
		},
	})); err != nil {
		t.Fatalf("ClientLog = error %v, want a success", err)
	}
	f.d.AwaitLogRecord(harness.ClientLogPath(f.ws), "the client's record", func(r harness.LogRecord) bool {
		return r.Operation == "webapp.crash"
	})

	// Assert
	for _, r := range f.d.RunLog() {
		if r.Operation == "webapp.crash" {
			t.Fatalf("the run log carries the client's own record %+v, want it confined to the workspace's webapp sink", r)
		}
	}
}

// ---------------------------------------------------------------------------
// The login pty
// ---------------------------------------------------------------------------

func TestOpenLoginSpawnsThePtyAndReplaysItsScrollback(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})

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

func TestSendLoginInputIsEchoedBackOnTheStream(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	if _, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenLogin = error %v, want a success", err)
	}
	stream := f.d.WatchLogin(f.ws)
	loginAwaitMarker(t, f, stream, harness.FakeClaudeLoginMarker)

	// Act
	if _, err := f.d.Client().SendLoginInput(f.d.Ctx(), connect.NewRequest(&agentreplv1.SendLoginInputRequest{
		Workspace: f.ws,
		Input:     &agentreplv1.SendLoginInputRequest_Keystrokes{Keystrokes: &agentreplv1.LoginTerminalKeystrokes{Data: []byte("hello\n")}},
	})); err != nil {
		t.Fatalf("SendLoginInput = error %v, want a success", err)
	}

	// Assert
	loginAwaitMarker(t, f, stream, "echo:hello")
}

func TestSendLoginInputResizeIsAccepted(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	if _, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenLogin = error %v, want a success", err)
	}

	// Act
	resp, err := f.d.Client().SendLoginInput(f.d.Ctx(), connect.NewRequest(&agentreplv1.SendLoginInputRequest{
		Workspace: f.ws,
		Input:     &agentreplv1.SendLoginInputRequest_Resize{Resize: &agentreplv1.LoginTerminalResize{Rows: 40, Cols: 120}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("SendLoginInput{resize} = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("SendLoginInput{resize} = %v, want a success", resp.Msg)
	}
}

func TestSendLoginInputWithNoLoginOpenIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	// no_login_open is answered through s.refuse (internal/server/login.go),
	// which logs a landed arm at DEBUG, never WARN.

	// Act
	resp, err := f.d.Client().SendLoginInput(f.d.Ctx(), connect.NewRequest(&agentreplv1.SendLoginInputRequest{
		Workspace: f.ws,
		Input:     &agentreplv1.SendLoginInputRequest_Keystrokes{Keystrokes: &agentreplv1.LoginTerminalKeystrokes{Data: []byte("x")}},
	}))

	// Assert: no_login_open is a LANDED SendLoginInputError arm — the
	// in-band typed answer, never a transport error (settled by reading
	// internal/server/login.go's SendLoginInput, per critique 22: the arm
	// string "no_login_open" matches SendLoginInputError's own oneof field,
	// so s.refuse's setArm succeeds and answers in band).
	if err != nil {
		t.Fatalf("SendLoginInput with no login open = error %v, want a success carrying error{no_login_open}", err)
	}
	if resp.Msg.GetError().GetNoLoginOpen() == nil {
		t.Fatalf("SendLoginInput with no login open = %v, want error{no_login_open}", resp.Msg)
	}
}

func TestCloseLoginEndsTheStreamWithClosed(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	if _, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("OpenLogin = error %v, want a success", err)
	}
	stream := f.d.WatchLogin(f.ws)
	loginAwaitMarker(t, f, stream, harness.FakeClaudeLoginMarker)

	// Act
	if _, err := f.d.Client().CloseLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseLoginRequest{Workspace: f.ws})); err != nil {
		t.Fatalf("CloseLogin = error %v, want a success", err)
	}

	// Assert
	harness.AwaitView(t, f.d.Ctx(), stream, "the closed terminus", func(o *agentreplv1.LoginTerminalOutput) bool {
		return o.GetClosed() != nil
	})
}

func TestASecondOpenLoginJoinsTheSamePty(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	first, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("the first OpenLogin = error %v, want a success", err)
	}
	stream := f.d.WatchLogin(f.ws)
	loginAwaitMarker(t, f, stream, harness.FakeClaudeLoginMarker)

	// Act
	second, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws}))
	if err != nil {
		t.Fatalf("the second OpenLogin = error %v, want it to join the running pty", err)
	}

	// Assert: the same pty, so the same config dir and NO second banner.
	if second.Msg.GetSuccess().GetConfigDir() != first.Msg.GetSuccess().GetConfigDir() {
		t.Fatalf("the second OpenLogin = config dir %q, want the first's %q",
			second.Msg.GetSuccess().GetConfigDir(), first.Msg.GetSuccess().GetConfigDir())
	}
	harness.ExpectNoPush(t, stream, harness.ProbeWindow, "a second login banner from a second pty")
}

// awaitLoginMarkerOn reads terminal bytes on a bare *harness.Daemon (no
// *fixture available) until the accumulated output carries
// harness.FakeClaudeLoginMarker, and returns everything accumulated so a
// caller can count occurrences rather than merely detect one.
func awaitLoginMarkerOn(t *testing.T, d *harness.Daemon, s *harness.Stream[*agentreplv1.LoginTerminalOutput]) string {
	t.Helper()
	var seen strings.Builder
	harness.AwaitView(t, d.Ctx(), s, "the terminal text "+harness.FakeClaudeLoginMarker, func(o *agentreplv1.LoginTerminalOutput) bool {
		seen.Write(o.GetBytes().GetData())
		return strings.Contains(seen.String(), harness.FakeClaudeLoginMarker)
	})
	return seen.String()
}

func TestOpenLoginFromTwoWorkspacesOutsideTheMultiRepoRootSharesOnePty(t *testing.T) {
	t.Parallel()
	// Arrange: two DIFFERENT workspaces whose repositories are both OUTSIDE
	// $MULTI_REPO_ROOT, so both route to the same default account root
	// (internal/login/manager.go keys its session map by CONFIG DIR, not by
	// workspace -- login idempotence is PER ACCOUNT).
	d := newDaemon(t, harness.Opts{})
	repoA := harness.NewRepo(t)
	wsA := harness.Register(t, d, repoA.Dir)
	repoB := harness.NewRepo(t)
	wsB := harness.Register(t, d, repoB.Dir)

	// Act
	first, err := d.Client().OpenLogin(d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: wsA}))
	if err != nil {
		t.Fatalf("the first OpenLogin = error %v, want a success", err)
	}
	second, err := d.Client().OpenLogin(d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: wsB}))
	if err != nil {
		t.Fatalf("the second OpenLogin (a different workspace, the same account root) = error %v, want a success", err)
	}

	// Assert: the same account root, so the SAME pty -- one banner, not two.
	if second.Msg.GetSuccess().GetConfigDir() != first.Msg.GetSuccess().GetConfigDir() {
		t.Fatalf("OpenLogin config dir = %q for the second workspace, want the first's %q (both route to the default account)",
			second.Msg.GetSuccess().GetConfigDir(), first.Msg.GetSuccess().GetConfigDir())
	}
	stream := d.WatchLogin(wsB)
	seen := awaitLoginMarkerOn(t, d, stream)
	if got := strings.Count(seen, harness.FakeClaudeLoginMarker); got != 1 {
		t.Fatalf("login marker count = %d in the shared pty's scrollback (seen by a late subscriber on the SECOND workspace), want exactly 1: the second OpenLogin joined rather than spawning a second pty", got)
	}
}

func TestOpenLoginUnderTwoDifferentAccountRootsSpawnsTwoDistinctPtys(t *testing.T) {
	t.Parallel()
	// Arrange: one workspace's repository lives directly UNDER
	// $MULTI_REPO_ROOT, the other outside it, so the two route to DIFFERENT
	// account roots (the same routing the existing account tests exercise in
	// session_lifecycle_test.go's TestAWorkspaceUnderTheMultiRepoRootSpawnsWithTheMultiRepoAccount).
	d := newDaemon(t, harness.Opts{})
	multiRepo := harness.NewRepoAt(t, filepath.Join(d.MultiRepoRoot, "under-the-multi-root"))
	multiWS := harness.Register(t, d, multiRepo.Dir)
	defaultRepo := harness.NewRepo(t)
	defaultWS := harness.Register(t, d, defaultRepo.Dir)

	// Act
	multiResp, err := d.Client().OpenLogin(d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: multiWS}))
	if err != nil {
		t.Fatalf("OpenLogin under the multi-repo root = error %v, want a success", err)
	}
	defaultResp, err := d.Client().OpenLogin(d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: defaultWS}))
	if err != nil {
		t.Fatalf("OpenLogin outside the multi-repo root = error %v, want a success", err)
	}

	// Assert: two distinct account roots, matching the two config dirs the
	// harness minted.
	if got, want := multiResp.Msg.GetSuccess().GetConfigDir(), d.MultiRepoConfigDir; got != want {
		t.Fatalf("OpenLogin under the multi-repo root = config dir %q, want the multi-repo account root %q", got, want)
	}
	if got, want := defaultResp.Msg.GetSuccess().GetConfigDir(), d.DefaultConfigDir; got != want {
		t.Fatalf("OpenLogin outside the multi-repo root = config dir %q, want the default account root %q", got, want)
	}

	// Assert: TWO ptys -- each workspace's own terminal shows exactly one
	// banner, and neither pty is the other's.
	multiSeen := awaitLoginMarkerOn(t, d, d.WatchLogin(multiWS))
	if got := strings.Count(multiSeen, harness.FakeClaudeLoginMarker); got != 1 {
		t.Fatalf("login marker count = %d on the multi-repo account's pty, want exactly 1", got)
	}
	defaultSeen := awaitLoginMarkerOn(t, d, d.WatchLogin(defaultWS))
	if got := strings.Count(defaultSeen, harness.FakeClaudeLoginMarker); got != 1 {
		t.Fatalf("login marker count = %d on the default account's pty, want exactly 1", got)
	}
}

func TestCloseLoginWithNothingOpenAnswersSuccess(t *testing.T) {
	t.Parallel()
	// Arrange: no OpenLogin has ever run on this workspace.
	f := newRegistered(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().CloseLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseLoginRequest{Workspace: f.ws}))

	// Assert: closing an ABSENT login is SUCCESS -- the desired state already
	// holds (endpoint_close_login.proto, internal/login/manager.go's Close:
	// `!ok` returns nil, never login.ErrNoSession).
	if err != nil {
		t.Fatalf("CloseLogin with nothing open = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("CloseLogin with nothing open = %v, want success{}", resp.Msg)
	}
}

func TestOpenLoginWhoseVendorBinaryFailsToSpawnAnswersSpawnFailed(t *testing.T) {
	t.Parallel()
	// Arrange: AGENT_REPL_CLAUDE_BIN names a path that does not exist, so
	// pty.Start's exec genuinely fails -- distinct from the vendor guard's
	// refusal, which fires only for the DEFAULT binary "claude"
	// (internal/login/manager.go's spawn: "an explicit path ... is by
	// construction something else ... so refusing it would forbid the very
	// thing the knob exists to allow").
	f := newRegistered(t, harness.Opts{ExtraEnv: []string{"AGENT_REPL_CLAUDE_BIN=/nonexistent/bogus-claude-bin"}})
	f.d.ExpectWarnings("daemon.login.open")

	// Act
	resp, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws}))

	// Assert: OpenLoginError.spawn_failed is the landed arm
	// (endpoint_open_login.proto); server.OpenLogin's fallback
	// (internal/server/login.go) maps any error asRefusal does not recognize
	// onto it.
	if err != nil {
		t.Fatalf("OpenLogin with an unspawnable vendor binary = error %v, want a success carrying error{spawn_failed}", err)
	}
	spawnFailed := resp.Msg.GetError().GetSpawnFailed()
	if spawnFailed == nil {
		t.Fatalf("OpenLogin with an unspawnable vendor binary = %v, want error{spawn_failed}", resp.Msg)
	}
	if !strings.Contains(spawnFailed.GetDetail(), "/nonexistent/bogus-claude-bin") {
		t.Fatalf("spawn_failed.detail = %q, want it to name the vendor binary that failed to spawn", spawnFailed.GetDetail())
	}
}

func TestOpenLoginUnderNoFakeIsRefusedNamingTheLoginSite(t *testing.T) {
	t.Parallel()
	// Arrange: NoFake withholds AGENT_REPL_CLAUDE_BIN, so the login manager
	// falls back to the default vendor binary "claude"
	// (internal/login/manager.go's DefaultVendorBin) and its guard check
	// actually runs: envc.VendorGuard refuses it, naming the site "login"
	// (internal/login/manager.go's guardSite), because
	// AGENT_REPL_FORBID_VENDOR_CALLS=1 is every test process's contract
	// (AGENTS.md).
	f := newRegistered(t, harness.Opts{NoFake: true})
	f.d.ExpectWarnings("daemon.login.open")

	// Act
	resp, err := f.d.Client().OpenLogin(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: f.ws}))

	// Assert: internal/server/refuse.go's asRefusal does not recognize
	// *envc.ForbiddenError, so OpenLogin's fallback (internal/server/login.go)
	// answers the landed spawn_failed arm rather than a bare connect error --
	// the refusal SHAPE the source actually produces.
	if err != nil {
		t.Fatalf("OpenLogin under NoFake = error %v, want a success carrying error{spawn_failed}", err)
	}
	spawnFailed := resp.Msg.GetError().GetSpawnFailed()
	if spawnFailed == nil {
		t.Fatalf("OpenLogin under NoFake = %v, want error{spawn_failed}", resp.Msg)
	}
	if !strings.Contains(spawnFailed.GetDetail(), "login") {
		t.Fatalf("spawn_failed.detail = %q, want it to name the refused site \"login\"", spawnFailed.GetDetail())
	}
}

func TestASubmitPromptRequiringClassificationUnderNoFakeIsHeldForTurnEnd(t *testing.T) {
	t.Parallel()
	// Arrange: NoFake means the classifier reaches its real vendor-backed
	// implementation (internal/classifier/vendor.go), guarded by
	// envc.VendorGuard, which refuses the "classifier" site while
	// AGENT_REPL_FORBID_VENDOR_CALLS=1 (every test process's contract,
	// AGENTS.md). A turn must be running so an incoming prompt is actually
	// classified rather than started fresh.
	f := newOpened(t, harness.Opts{NoFake: true})
	f.d.ExpectWarnings("daemon.promptqueue.classify")
	f.submit("start the work", "k-running", origin)
	f.shim.ExpectStartTurn()
	holds := f.d.WatchHolds(f.ws)
	awaitView(t, f, holds, "the initial empty tray", func(tray *frontendv1.DaemonHoldTray) bool {
		return len(tray.GetItems()) == 0
	})

	// Act: an ordinary follow-up, no explicit-interrupt keyword, so the
	// classifier is actually consulted rather than answered by the fast path
	// (classifier.ExplicitInterrupts).
	resp := f.submit("please also check the other file", "k-held", origin)
	turn := resp.GetSuccess().GetTurn().GetTurn()
	if turn.GetValue() == "" {
		t.Fatalf("SubmitPrompt while a turn runs = %v, want a minted TurnId (it is HELD, not refused)", resp)
	}

	// Assert: the classifying push comes first, synchronously
	// (internal/promptqueue/classify.go's hold).
	classifying := harness.AwaitNext(t, f.d.Ctx(), holds, "the classifying push")
	if p := promptHeldEntry(classifying, turn); p == nil || p.GetClassifying() == nil {
		t.Fatalf("the first tray push for the held prompt = %v, want the transient classifying arm", p)
	}

	// Assert: the guard's refusal is NOT a verdict (classifier.Judge's doc
	// comment), but the prompt's true state is: it waits for the running
	// turn to end. The failure is the daemon's, logged at ERROR under
	// daemon.promptqueue.classify; the tray never shows "unclassified".
	verdict := harness.AwaitNext(t, f.d.Ctx(), holds, "the hold_for_turn_end verdict")
	p := promptHeldEntry(verdict, turn)
	if p == nil || p.GetHoldForTurnEnd() == nil {
		t.Fatalf("the verdict for the held prompt under NoFake = %v, want hold_for_turn_end", p)
	}
}

// ---------------------------------------------------------------------------
// OpenInEditor and OpenExternal
// ---------------------------------------------------------------------------

func TestOpenExternalInvokesTheConfiguredLauncher(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	const url = "https://example.invalid/report"
	// THE FIXTURE HAS NO CHROME, and that is the condition the account->profile
	// routing warns on by design: externalbrowser.go's ProfileForAccount is
	// documented "FALLBACK IS LOUD, NEVER SILENT" -- a non-empty account email
	// whose Chrome Local State cannot be read routes to the pinned default at
	// WARN naming the email, because a link that quietly landed in the wrong
	// profile is what that record exists to prevent. The daemon's HOME here is a
	// t.TempDir() with no Chrome installation, so the record is evidence of the
	// fixture rather than of anything wrong, and the level stays as ruled.
	f.d.ExpectWarnings("daemon.externalbrowser.profile_for_account")

	// Act
	if _, err := f.d.Client().OpenExternal(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: f.ws,
		Url:       url,
	})); err != nil {
		t.Fatalf("OpenExternal = error %v, want a success", err)
	}

	// Assert
	invocations := f.d.Browser.Invocations()
	if len(invocations) != 1 {
		t.Fatalf("the browser launcher ran %d times, want exactly once: %+v", len(invocations), invocations)
	}
	if !loginArgvHas(invocations[0].Argv, url) {
		t.Fatalf("the launcher argv = %v, want it to carry %q", invocations[0].Argv, url)
	}
}

// TestOpenExternalWithAnUnparseableUrlAnswersInvalidUrl exercises
// OpenExternalError's invalid_url arm (endpoint_open_external.proto: "the url
// does not parse").
func TestOpenExternalWithAnUnparseableUrlAnswersInvalidUrl(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	// internal/workspace/links.go's OpenExternal refuses an unparseable url
	// under ArmUnservedAnswer ("unserved_answer"), which is not a field
	// OpenExternalError carries: the arm the contract landed
	// (endpoint_open_external.proto's invalid_url) is not the one the verb
	// raises, so today's answer is server.UnlandedArm's transport error at
	// WARN under daemon.refusal.unlanded_arm, not the typed arm below.
	f.d.ExpectWarnings("daemon.refusal.unlanded_arm")

	// Act
	resp, err := f.d.Client().OpenExternal(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: f.ws,
		Url:       "::not a url",
	}))

	// Assert
	if err != nil {
		t.Fatalf("OpenExternal(unparseable url) = error %v, want a success carrying error{invalid_url}", err)
	}
	if got := resp.Msg.GetError().GetInvalidUrl(); got == nil {
		t.Fatalf("OpenExternal(unparseable url) = %v, want error{invalid_url}", resp.Msg)
	}
}

// TestOpenExternalWithAFailingLauncherAnswersLaunchFailed exercises
// OpenExternalError's launch_failed{detail} arm.
func TestOpenExternalWithAFailingLauncherAnswersLaunchFailed(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.d.Browser.SetExitCode(1)
	// A launcher that will not run is recorded at WARN under the verb's own
	// operation and then ANSWERED as launch_failed{detail}.
	f.d.ExpectWarnings("daemon.workspace.open_external", "daemon.externalbrowser.open")
	// THE FIXTURE HAS NO CHROME, and that is the condition the account->profile
	// routing warns on by design: externalbrowser.go's ProfileForAccount is
	// documented "FALLBACK IS LOUD, NEVER SILENT" -- a non-empty account email
	// whose Chrome Local State cannot be read routes to the pinned default at
	// WARN naming the email, because a link that quietly landed in the wrong
	// profile is what that record exists to prevent. The daemon's HOME here is a
	// t.TempDir() with no Chrome installation, so the record is evidence of the
	// fixture rather than of anything wrong, and the level stays as ruled.
	f.d.ExpectWarnings("daemon.externalbrowser.profile_for_account")

	// Act
	resp, err := f.d.Client().OpenExternal(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenExternalRequest{
		Workspace: f.ws,
		Url:       "https://example.invalid/report",
	}))

	// Assert
	if err != nil {
		t.Fatalf("OpenExternal(failing launcher) = error %v, want a success carrying error{launch_failed}", err)
	}
	got := resp.Msg.GetError().GetLaunchFailed()
	if got == nil {
		t.Fatalf("OpenExternal(failing launcher) = %v, want error{launch_failed}", resp.Msg)
	}
	if got.GetDetail() == "" {
		t.Fatalf("launch_failed.detail = %q, want the launcher's own account of the failure", got.GetDetail())
	}
}

// TestOpenExternalWithNoBrowserConfiguredAnswersNoBrowserConfigured exercises
// OpenExternalError's no_browser_configured arm. The daemon is started with
// `--no-browser`, which is the operator's explicit statement that this host
// has no external browser: cmd/claude-repld/graph.go then leaves the Browser
// dependency nil and internal/workspace/links.go refuses.
func TestOpenExternalWithNoBrowserConfiguredAnswersNoBrowserConfigured(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newRegistered(t, harness.Opts{ExtraArgs: []string{"--no-browser"}})
	// The graph says so out loud at boot: a daemon that cannot open a link is
	// not silently degraded.
	f.d.ExpectWarnings("daemon.cmd.graph")

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
