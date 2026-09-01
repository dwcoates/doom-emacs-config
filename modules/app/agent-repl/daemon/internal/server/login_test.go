package server

import (
	"context"
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/login"
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
	if err == nil || !strings.Contains(err.Error(), "WatchLoginTerminalError.no_login_open") {
		t.Fatalf("error = %v, want the no_login_open sentinel", err)
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
	if !stream.Receive() {
		t.Fatalf("receive the relay: %v", stream.Err())
	}

	// Assert.
	got := stream.Msg().GetOpenInEditor()
	if got.GetPath() != "lisp/core.el" || got.GetLine() != 42 {
		t.Fatalf("open_in_editor = %v, want lisp/core.el:42", got)
	}
}
