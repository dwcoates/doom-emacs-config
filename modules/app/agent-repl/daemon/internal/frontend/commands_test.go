package frontend

import (
	"context"
	"errors"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// mockHandler records the last method invoked and can inject an error.
type mockHandler struct {
	called string
	err    error

	lastWorkspace string
	lastRequestID string
	lastResyncSeq uint64
}

func (m *mockHandler) SubmitPrompt(_ context.Context, ws, rid string, _ *frontendv1.SubmitPromptCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "submit_prompt", ws, rid
	return m.err
}
func (m *mockHandler) Interrupt(_ context.Context, ws, rid string, _ *frontendv1.InterruptCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "interrupt", ws, rid
	return m.err
}
func (m *mockHandler) AnswerPermission(_ context.Context, ws, rid string, _ *frontendv1.PermissionAnswerCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "permission_answer", ws, rid
	return m.err
}
func (m *mockHandler) MergeWorkspace(_ context.Context, ws, rid string, _ *frontendv1.MergeWorkspaceCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "merge_workspace", ws, rid
	return m.err
}
func (m *mockHandler) CloseWorkspace(_ context.Context, ws, rid string, _ *frontendv1.CloseWorkspaceCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "close_workspace", ws, rid
	return m.err
}
func (m *mockHandler) OpenWorkspace(_ context.Context, ws, rid string, _ *frontendv1.OpenWorkspaceCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "open_workspace", ws, rid
	return m.err
}
func (m *mockHandler) Resync(_ context.Context, ws, rid string, cmd *frontendv1.ResyncCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID, m.lastResyncSeq = "resync", ws, rid, cmd.GetFromSeq()
	return m.err
}
func (m *mockHandler) CreateSession(_ context.Context, ws, rid string, _ *frontendv1.CreateSessionCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "create_session", ws, rid
	return m.err
}
func (m *mockHandler) DeleteSession(_ context.Context, ws, rid string, _ *frontendv1.DeleteSessionCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "delete_session", ws, rid
	return m.err
}
func (m *mockHandler) Shutdown(_ context.Context, ws, rid string, _ *frontendv1.ShutdownCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "shutdown", ws, rid
	return m.err
}
func (m *mockHandler) ClientLog(_ context.Context, ws, rid string, _ *frontendv1.ClientLogCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "client_log", ws, rid
	return m.err
}

func TestDispatchRoutesEachCommand(t *testing.T) {
	tests := []struct {
		name    string
		cmd     *frontendv1.FrontendCommand
		wantHit string
	}{
		{
			name:    "submit prompt",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r1", Workspace: "ws1", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"}}},
			wantHit: "submit_prompt",
		},
		{
			name:    "interrupt",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r2", Workspace: "ws2", Command: &frontendv1.FrontendCommand_Interrupt{Interrupt: &frontendv1.InterruptCmd{Hard: true}}},
			wantHit: "interrupt",
		},
		{
			name:    "permission answer",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r3", Command: &frontendv1.FrontendCommand_PermissionAnswer{PermissionAnswer: &frontendv1.PermissionAnswerCmd{Allow: true}}},
			wantHit: "permission_answer",
		},
		{
			name:    "merge workspace",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r4", Command: &frontendv1.FrontendCommand_MergeWorkspace{MergeWorkspace: &frontendv1.MergeWorkspaceCmd{Handler: "cherry-pick"}}},
			wantHit: "merge_workspace",
		},
		{
			name:    "close workspace",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r5", Command: &frontendv1.FrontendCommand_CloseWorkspace{CloseWorkspace: &frontendv1.CloseWorkspaceCmd{}}},
			wantHit: "close_workspace",
		},
		{
			name:    "open workspace",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r6", Command: &frontendv1.FrontendCommand_OpenWorkspace{OpenWorkspace: &frontendv1.OpenWorkspaceCmd{}}},
			wantHit: "open_workspace",
		},
		{
			name:    "resync",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r7", Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{FromSeq: 42}}},
			wantHit: "resync",
		},
		{
			name:    "create session",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r8", Workspace: "ws8", Command: &frontendv1.FrontendCommand_CreateSession{CreateSession: &frontendv1.CreateSessionCmd{Cwd: "ws8"}}},
			wantHit: "create_session",
		},
		{
			name:    "delete session",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r9", Workspace: "ws9", Command: &frontendv1.FrontendCommand_DeleteSession{DeleteSession: &frontendv1.DeleteSessionCmd{SessionId: "s_9"}}},
			wantHit: "delete_session",
		},
		{
			name:    "shutdown",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r10", Command: &frontendv1.FrontendCommand_Shutdown{Shutdown: &frontendv1.ShutdownCmd{}}},
			wantHit: "shutdown",
		},
		{
			name:    "client log",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r11", Workspace: "ws11", Command: &frontendv1.FrontendCommand_ClientLog{ClientLog: &frontendv1.ClientLogCmd{Message: "seq gap"}}},
			wantHit: "client_log",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := &mockHandler{}

			// Act.
			ack := Dispatch(context.Background(), h, tc.cmd)

			// Assert.
			if h.called != tc.wantHit {
				t.Errorf("handler called %q, want %q", h.called, tc.wantHit)
			}
			if !ack.GetOk() {
				t.Errorf("ack not ok: %s", ack.GetError())
			}
			if ack.GetRequestId() != tc.cmd.GetRequestId() {
				t.Errorf("ack request_id = %q, want %q", ack.GetRequestId(), tc.cmd.GetRequestId())
			}
		})
	}
}

func TestDispatchResyncCarriesSeq(t *testing.T) {
	// Arrange.
	h := &mockHandler{}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{FromSeq: 99}}}

	// Act.
	Dispatch(context.Background(), h, cmd)

	// Assert.
	if h.lastResyncSeq != 99 {
		t.Errorf("resync seq = %d, want 99", h.lastResyncSeq)
	}
}

func TestDispatchHandlerErrorBecomesFailingAck(t *testing.T) {
	// Arrange.
	h := &mockHandler{err: errors.New("submit exploded")}
	cmd := &frontendv1.FrontendCommand{RequestId: "r8", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), h, cmd)

	// Assert.
	if ack.GetOk() {
		t.Fatal("expected failing ack")
	}
	if ack.GetError() != "submit exploded" {
		t.Errorf("ack error = %q, want %q", ack.GetError(), "submit exploded")
	}
	if ack.GetRequestId() != "r8" {
		t.Errorf("ack request_id = %q, want r8", ack.GetRequestId())
	}
}

func TestDispatchUnknownCommandFailsLoudly(t *testing.T) {
	// Arrange: a command with an empty oneof (no command arm set).
	h := &mockHandler{}
	cmd := &frontendv1.FrontendCommand{RequestId: "r9", Workspace: "wsX"}

	// Act.
	ack := Dispatch(context.Background(), h, cmd)

	// Assert: loud failing ack, no handler method invoked.
	if h.called != "" {
		t.Errorf("no handler should be called for unknown command, got %q", h.called)
	}
	if ack.GetOk() {
		t.Fatal("expected failing ack for unknown command")
	}
	if ack.GetError() == "" {
		t.Error("expected non-empty error on unknown-command ack")
	}
	if ack.GetRequestId() != "r9" {
		t.Errorf("ack request_id = %q, want r9", ack.GetRequestId())
	}
}

func TestDispatchNilCommand(t *testing.T) {
	// Act.
	ack := Dispatch(context.Background(), &mockHandler{}, nil)

	// Assert.
	if ack.GetOk() || ack.GetError() == "" {
		t.Errorf("nil command should produce a loud failing ack, got %v", ack)
	}
}
