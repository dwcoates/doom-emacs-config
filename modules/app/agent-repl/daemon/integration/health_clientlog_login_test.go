//go:build integration

package integration

import (
	"os"
	"strings"
	"testing"

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

func TestDaemonHealthWithAnOpenFaultIsUnhealthy(t *testing.T) {
	// Arrange: the prompts directory the daemon booted with is taken away, the
	// one fault a test can open without breaking the daemon's own boot.
	d := newDaemon(t, harness.Opts{})
	d.ExpectWarnings(harness.AllowAllWarnings)
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
}

// ---------------------------------------------------------------------------
// SessionHealth
// ---------------------------------------------------------------------------

func TestSessionHealthForALiveSessionIsHealthy(t *testing.T) {
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
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings(harness.AllowAllWarnings)
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

func TestSessionHealthOfAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	d.ExpectWarnings(harness.AllowAllWarnings)

	// Act
	resp, err := d.Client().SessionHealth(d.Ctx(), connect.NewRequest(&agentreplv1.SessionHealthRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()},
	}))

	// Assert
	if err != nil {
		if connectCode(err) != connect.CodeNotFound {
			t.Fatalf("SessionHealth(unknown) = error %v, want NotFound or the typed arm", err)
		}
		return
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("SessionHealth(unknown) = %v, want error{unknown_workspace}", resp.Msg)
	}
}

// ---------------------------------------------------------------------------
// ClientLog
// ---------------------------------------------------------------------------

func TestClientLogWritesARecordIntoTheWebappSink(t *testing.T) {
	// Arrange
	f := newRegistered(t, harness.Opts{})
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
}

// ---------------------------------------------------------------------------
// The login pty
// ---------------------------------------------------------------------------

func TestOpenLoginSpawnsThePtyAndReplaysItsScrollback(t *testing.T) {
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
	// Arrange
	f := newRegistered(t, harness.Opts{})
	f.d.ExpectWarnings(harness.AllowAllWarnings)

	// Act
	resp, err := f.d.Client().SendLoginInput(f.d.Ctx(), connect.NewRequest(&agentreplv1.SendLoginInputRequest{
		Workspace: f.ws,
		Input:     &agentreplv1.SendLoginInputRequest_Keystrokes{Keystrokes: &agentreplv1.LoginTerminalKeystrokes{Data: []byte("x")}},
	}))

	// Assert
	if err != nil {
		if connectCode(err) != connect.CodeFailedPrecondition {
			t.Fatalf("SendLoginInput with no login open = error %v, want the no_login_open refusal", err)
		}
		return
	}
	if resp.Msg.GetError().GetNoLoginOpen() == nil {
		t.Fatalf("SendLoginInput with no login open = %v, want error{no_login_open}", resp.Msg)
	}
}

func TestCloseLoginEndsTheStreamWithClosed(t *testing.T) {
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

// ---------------------------------------------------------------------------
// OpenInEditor and OpenExternal
// ---------------------------------------------------------------------------

func TestOpenInEditorRelaysThePushOntoTheHostStream(t *testing.T) {
	// Arrange
	f := newRegistered(t, harness.Opts{})
	host := f.d.WatchHost(f.ws)

	// Act
	line := uint32(42)
	if _, err := f.d.Client().OpenInEditor(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: f.ws,
		Path:      "README.md",
		Line:      &line,
	})); err != nil {
		t.Fatalf("OpenInEditor = error %v, want a success", err)
	}

	// Assert
	push := harness.AwaitView(t, f.d.Ctx(), host, "the open_in_editor push", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetOpenInEditor() != nil
	})
	if got := push.GetOpenInEditor(); got.GetPath() != "README.md" || got.GetLine() != line {
		t.Fatalf("the open_in_editor push = %v, want README.md:42", got)
	}
}

func TestOpenInEditorOnAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange
	d := newDaemon(t, harness.Opts{})
	d.ExpectWarnings(harness.AllowAllWarnings)

	// Act
	resp, err := d.Client().OpenInEditor(d.Ctx(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{
		Workspace: &workspacev1.WorkspaceRef{Id: "no-such-workspace", Dir: t.TempDir()},
		Path:      "README.md",
	}))

	// Assert
	if err != nil {
		if connectCode(err) != connect.CodeNotFound {
			t.Fatalf("OpenInEditor(unknown) = error %v, want NotFound or the typed arm", err)
		}
		return
	}
	if resp.Msg.GetError().GetUnknownWorkspace() == nil {
		t.Fatalf("OpenInEditor(unknown) = %v, want error{unknown_workspace}", resp.Msg)
	}
}

func TestOpenExternalInvokesTheConfiguredLauncher(t *testing.T) {
	// Arrange
	f := newRegistered(t, harness.Opts{})
	const url = "https://example.invalid/report"

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
