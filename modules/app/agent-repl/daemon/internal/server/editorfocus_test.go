package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// unfocusedEditor is the focus an Emacs stream in these tests connects with.
func unfocusedEditor() *agentreplv1.EditorFocus {
	return &agentreplv1.EditorFocus{Focus: &agentreplv1.EditorFocus_Unfocused{Unfocused: &agentreplv1.EditorFocusUnfocused{}}}
}

// testEditorInstance is the Emacs process identity every test Emacs stream
// carries.
func testEditorInstance() *agentreplv1.EditorInstance {
	return &agentreplv1.EditorInstance{Value: "emacs-test"}
}

// focusedEditor is the focus a focused Emacs reports.
func focusedEditor() *agentreplv1.EditorFocus {
	return &agentreplv1.EditorFocus{Focus: &agentreplv1.EditorFocus_Focused{Focused: &agentreplv1.EditorFocusFocused{}}}
}

// emacsDaemonStream opens an Emacs WatchDaemon stream connecting with focus.
// The call returns once the stream's headers arrive, which the daemon flushes
// on acceptance, after the focus is attached.
func emacsDaemonStream(t *testing.T, h *harness, ctx context.Context, focus *agentreplv1.EditorFocus) *connect.ServerStreamForClient[agentreplv1.WatchDaemonResponse] {
	t.Helper()
	stream, err := h.Client.WatchDaemon(ctx, connect.NewRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "elisp-test", Focus: focus, Instance: testEditorInstance()}},
	}))
	if err != nil {
		t.Fatalf("open the daemon stream: %v", err)
	}
	return stream
}

func reportFocus(t *testing.T, h *harness, focus *agentreplv1.EditorFocus) (*agentreplv1.ReportEditorFocusResponse, error) {
	t.Helper()
	resp, err := h.Client.ReportEditorFocus(context.Background(),
		connect.NewRequest(&agentreplv1.ReportEditorFocusRequest{Focus: focus}))
	if err != nil {
		return nil, err
	}
	return resp.Msg, nil
}

func TestAnEmacsStreamAttachesTheFocusItConnectsWith(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// Act
	emacsDaemonStream(t, h, ctx, focusedEditor())

	// Assert
	if !h.Focus.Focused() {
		t.Fatal("the stream's connect-time focus did not stand")
	}
}

func TestReportEditorFocusMovesTheFocus(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	emacsDaemonStream(t, h, ctx, unfocusedEditor())

	// Act
	resp, err := reportFocus(t, h, focusedEditor())

	// Assert
	if err != nil || resp.GetSuccess() == nil {
		t.Fatalf("ReportEditorFocus = (%v, %v), want success", resp, err)
	}
	if !h.Focus.Focused() {
		t.Fatal("the reported focus did not stand")
	}
}

func TestReportEditorFocusWithNoEmacsStreamIsRefused(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	resp, err := reportFocus(t, h, focusedEditor())

	// Assert
	if err != nil {
		t.Fatalf("ReportEditorFocus: %v", err)
	}
	if resp.GetError().GetNoEmacsStream() == nil {
		t.Fatalf("response = %v, want the no_emacs_stream arm", resp)
	}
	if h.Focus.Focused() {
		t.Fatal("a refused report changed the focus")
	}
}

func TestReportEditorFocusWithNoFocusIsInvalid(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := reportFocus(t, h, &agentreplv1.EditorFocus{})

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want invalid_argument", connect.CodeOf(err))
	}
}

func TestAnEmacsStreamWithNoFocusIsInvalid(t *testing.T) {
	// Act
	err := validateWatchDaemonRequest(&agentreplv1.WatchDaemonRequest{
		Client: &agentreplv1.WatchDaemonRequest_Emacs{Emacs: &agentreplv1.WatchDaemonEmacs{ElispBuild: "elisp-test", Instance: testEditorInstance()}},
	})

	// Assert
	if err == nil || err.Code() != connect.CodeInvalidArgument {
		t.Fatalf("validation = %v, want invalid_argument for a missing focus", err)
	}
}

func TestNotificationClickedReachesTheHostStream(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	stream, err := h.Client.WatchHostWorkspace(ctx,
		connect.NewRequest(&agentreplv1.WatchHostWorkspaceRequest{Workspace: ref()}))
	if err != nil {
		t.Fatalf("open the host stream: %v", err)
	}

	// Act
	h.Server.NotificationClicked(testWorkspaceID)
	push := receiveHostEvent(t, stream)

	// Assert
	if push.GetNotificationClicked() == nil {
		t.Fatalf("push = %v, want notification_clicked", push)
	}
}
