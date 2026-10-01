package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/login"
	"claude-repld/internal/merge"
)

// TestOpenLoginAnswersTheConfigDir pins that the caller learns which account
// root the flow runs under.
func TestOpenLoginAnswersTheConfigDir(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.configDir = "/accounts/one"

	// Act.
	resp, err := h.Client.OpenLogin(context.Background(),
		connect.NewRequest(&agentreplv1.OpenLoginRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenLogin: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetConfigDir(); got != "/accounts/one" {
		t.Fatalf("config_dir = %q, want /accounts/one", got)
	}
}

// TestSendLoginInputRefusesWithNoLoginOpen pins the unary arm the contract
// carries for an absent pty.
func TestSendLoginInputRefusesWithNoLoginOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.sendErr = login.ErrNoSession

	// Act.
	resp, err := h.Client.SendLoginInput(context.Background(),
		connect.NewRequest(&agentreplv1.SendLoginInputRequest{
			Workspace: ref(),
			Input: &agentreplv1.SendLoginInputRequest_Keystrokes{
				Keystrokes: &agentreplv1.LoginTerminalKeystrokes{Data: []byte("y")},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SendLoginInput: %v", err)
	}
	if resp.Msg.GetError().GetNoLoginOpen() == nil {
		t.Fatalf("result = %v, want no_login_open", resp.Msg.GetResult())
	}
}

// TestCloseLoginOnAnAbsentSessionIsSuccess pins the contract: closing a login
// that is not open is a success, not an error.
func TestCloseLoginOnAnAbsentSessionIsSuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.closeErr = login.ErrNoSession

	// Act.
	resp, err := h.Client.CloseLogin(context.Background(),
		connect.NewRequest(&agentreplv1.CloseLoginRequest{Workspace: ref()}))

	// Assert.
	if err != nil {
		t.Fatalf("CloseLogin: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestWatchLoginTerminalStreamsBytes pins that the pty's raw output reaches the
// client verbatim.
func TestWatchLoginTerminalStreamsBytes(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.watchFrames = make(chan login.Output, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchLoginTerminal(ctx,
		connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("WatchLoginTerminal: %v", err)
	}
	h.Login.watchFrames <- login.Output{Bytes: []byte("Enter code:")}
	if !stream.Receive() {
		t.Fatalf("receive the bytes: %v", stream.Err())
	}

	// Assert.
	if got := string(stream.Msg().GetBytes().GetData()); got != "Enter code:" {
		t.Fatalf("bytes = %q, want the pty's output", got)
	}
}

// TestWatchLoginTerminalEndsOnTheClosedFrame pins that `closed` is the stream's
// LAST frame.
func TestWatchLoginTerminalEndsOnTheClosedFrame(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.watchFrames = make(chan login.Output, 1)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchLoginTerminal(ctx,
		connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("WatchLoginTerminal: %v", err)
	}

	// Act.
	h.Login.watchFrames <- login.Output{Closed: true}
	if !stream.Receive() {
		t.Fatalf("receive the closed frame: %v", stream.Err())
	}

	// Assert.
	if stream.Msg().GetClosed() == nil {
		t.Fatalf("frame = %v, want closed", stream.Msg().GetOutput())
	}
	if stream.Receive() {
		t.Fatal("the stream kept going after the closed frame")
	}
}

// TestWatchLoginTerminalRefusesWithNoLoginOpen pins the STREAM's unlanded arm:
// WatchLoginTerminal has no error message, so a refused open is the sentinel.
func TestWatchLoginTerminalRefusesWithNoLoginOpen(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Login.watchErr = login.ErrNoSession
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act.
	stream, err := h.Client.WatchLoginTerminal(ctx,
		connect.NewRequest(&agentreplv1.WatchLoginTerminalRequest{Workspace: ref()}))
	if err == nil {
		stream.Receive()
		err = stream.Err()
	}

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "WatchLoginTerminal closed the stream: no_login_open") {
		t.Fatalf("error = %v, want the transport-closed no_login_open cause", err)
	}
}

// TestOpenInEditorRelaysOntoTheHostStream pins the ruling: the daemon validates
// the workspace and RELAYS the click; it opens nothing itself.
func TestOpenInEditorRelaysOntoTheHostStream(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchHostWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("open the host stream: %v", err)
	}

	// Act: the verbs relay through the server's own relay face.
	line := uint32(42)
	h.Server.Relay().OpenInEditor(testWorkspaceID, "lisp/core.el", &line)
	push := receiveHostEvent(t, stream)

	// Assert.
	got := push.GetOpenInEditor()
	if got.GetPath() != "lisp/core.el" || got.GetLine() != 42 {
		t.Fatalf("open_in_editor = %v, want lisp/core.el:42", got)
	}
}

func TestOpenInEditorOpensAWorkspaceFileThroughTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_WorkspaceFile{WorkspaceFile: &agentreplv1.OpenInEditorWorkspaceFile{Path: "lisp/core.el"}}}))

	// Assert.
	if err != nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "file:lisp/core.el" {
		t.Fatalf("OpenInEditor = %v, opens %v, want the workspace file relayed", err, h.Verbs.editorOpens)
	}
}

func TestOpenInEditorOpensAMergeTestLogByItsToken(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.logPaths = map[string]string{"lease-1/2": "/state/merge-logs/lease-1-tests-2.log"}

	// Act.
	_, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "lease-1/2"}}}))

	// Assert.
	if err != nil || len(h.Verbs.editorOpens) != 1 || h.Verbs.editorOpens[0] != "daemon:/state/merge-logs/lease-1-tests-2.log" {
		t.Fatalf("OpenInEditor = %v, opens %v, want the log relayed", err, h.Verbs.editorOpens)
	}
}

func TestOpenInEditorAnswersAnUnknownMergeTestLog(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Merge.logErr = &merge.RefusalError{Arm: merge.ArmUnknownMergeTestLog, Reason: "no such log"}

	// Act.
	resp, err := h.Client.OpenInEditor(context.Background(), connect.NewRequest(&agentreplv1.OpenInEditorRequest{Workspace: ref(),
		Target: &agentreplv1.OpenInEditorRequest_MergeTestLog{MergeTestLog: &frontendv1.FeedMergeTestLogToken{Value: "x"}}}))

	// Assert.
	if err != nil {
		t.Fatalf("OpenInEditor: %v", err)
	}
	if resp.Msg.GetError().GetUnknownMergeTestLog() == nil || len(h.Verbs.editorOpens) != 0 {
		t.Fatalf("result = %v, opens %v, want unknown_merge_test_log and nothing relayed", resp.Msg.GetResult(), h.Verbs.editorOpens)
	}
}
