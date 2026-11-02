package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
)

// notifyWorkspace calls NotifyWorkspace for the harness's workspace.
func notifyWorkspace(h *harness, workspace *workspacev1.WorkspaceRef, message string) (*connect.Response[agentreplv1.NotifyWorkspaceResponse], error) {
	return h.Client.NotifyWorkspace(context.Background(), connect.NewRequest(&agentreplv1.NotifyWorkspaceRequest{
		Workspace: workspace, Message: message,
	}))
}

func TestNotifyWorkspaceAnswersTheNotifiersDecision(t *testing.T) {
	tests := []struct {
		name     string
		decision desktopnotify.Decision
		check    func(*agentreplv1.NotifyWorkspaceResponse) bool
	}{
		{name: "the banner is being posted", decision: desktopnotify.Dispatched, check: func(r *agentreplv1.NotifyWorkspaceResponse) bool {
			return r.GetSuccess().GetPosting() != nil
		}},
		{name: "Emacs is focused", decision: desktopnotify.EditorFocused, check: func(r *agentreplv1.NotifyWorkspaceResponse) bool {
			return r.GetSuccess().GetEditorFocused() != nil
		}},
		{name: "no banner program", decision: desktopnotify.NoProgram, check: func(r *agentreplv1.NotifyWorkspaceResponse) bool {
			return r.GetError().GetNoBannerProgram().GetCause() == "terminal-notifier is not installed"
		}},
		{name: "the daemon is standing down", decision: desktopnotify.StandingDown, check: func(r *agentreplv1.NotifyWorkspaceResponse) bool {
			return r.GetError().GetStandingDown() != nil
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Banners.decision = tt.decision
			h.Banners.noProgramCause = "terminal-notifier is not installed"

			// Act
			resp, err := notifyWorkspace(h, ref(), "The build is green")

			// Assert
			if err != nil || !tt.check(resp.Msg) {
				t.Fatalf("NotifyWorkspace = %v, %v", resp, err)
			}
		})
	}
}

func TestNotifyWorkspaceHandsTheNotifierTheTargetAndMessage(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	if _, err := notifyWorkspace(h, ref(), "The build is green"); err != nil {
		t.Fatalf("NotifyWorkspace: %v", err)
	}

	// Assert
	got := h.Banners.notifies()
	if len(got) != 1 || got[0] != (fakeNotify{ws: testWorkspaceID, message: "The build is green"}) {
		t.Fatalf("notifies = %+v, want the target and the message", got)
	}
}

func TestNotifyWorkspaceRecordsTheDecision(t *testing.T) {
	// Arrange
	h := newHarness(t)
	log := dlog.NewTestLogger()
	h.Surfaces.workspace = log
	h.Banners.decision = desktopnotify.EditorFocused

	// Act
	if _, err := notifyWorkspace(h, ref(), "The build is green"); err != nil {
		t.Fatalf("NotifyWorkspace: %v", err)
	}

	// Assert
	r, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "info", Operation: opNotifyWorkspace, Message: "an agent's desktop banner was asked for"})
	if !ok || r.Context["decision"] != "editor_focused" {
		t.Fatalf("record = %+v (found %v), want the decision", r, ok)
	}
}

func TestNotifyWorkspaceIsRefused(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(h *harness)
		workspace *workspacev1.WorkspaceRef
		check     func(*agentreplv1.NotifyWorkspaceError) bool
	}{
		{name: "an unknown workspace", arrange: func(*harness) {}, workspace: &workspacev1.WorkspaceRef{Id: "nope", Dir: "/nope"},
			check: func(e *agentreplv1.NotifyWorkspaceError) bool { return e.GetUnknownWorkspace().GetWorkspace().GetId() == "nope" }},
		{name: "a ref whose dir disagrees", arrange: func(*harness) {}, workspace: &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: "/elsewhere"},
			check: func(e *agentreplv1.NotifyWorkspaceError) bool {
				return e.GetWorkspaceRefMismatch().GetRegistryDir() == testWorkspaceDir
			}},
		{name: "a closed workspace", arrange: func(h *harness) {
			ws := h.DB.workspaces[testWorkspaceID]
			ws.Closed = true
			h.DB.workspaces[testWorkspaceID] = ws
		}, workspace: ref(), check: func(e *agentreplv1.NotifyWorkspaceError) bool { return e.GetWorkspaceClosed() != nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			resp, err := notifyWorkspace(h, tt.workspace, "The build is green")

			// Assert
			if err != nil || !tt.check(resp.Msg.GetError()) {
				t.Fatalf("NotifyWorkspace = %v, %v", resp, err)
			}
			if got := h.Banners.notifies(); len(got) != 0 {
				t.Fatalf("a refused request posted %+v", got)
			}
		})
	}
}

func TestNotifyWorkspaceValidation(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.NotifyWorkspaceRequest
	}{
		{name: "no workspace", req: &agentreplv1.NotifyWorkspaceRequest{Message: "x"}},
		{name: "a blank message", req: &agentreplv1.NotifyWorkspaceRequest{Workspace: ref(), Message: " "}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			_, err := h.Client.NotifyWorkspace(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if connect.CodeOf(err) != connect.CodeInvalidArgument || len(h.Banners.notifies()) != 0 {
				t.Fatalf("NotifyWorkspace = %v, want invalid_argument and nothing posted", err)
			}
		})
	}
}
