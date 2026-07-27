package frontend

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
)

// mockHandler records the last method invoked and can inject an error.
type mockHandler struct {
	called string
	err    error

	lastWorkspace string
	lastRequestID string
	lastResyncSeq uint64
}

func (m *mockHandler) CreateWorkspace(_ context.Context, ws, rid string, _ *frontendv1.CreateWorkspaceCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "create_workspace", ws, rid
	return m.err
}
func (m *mockHandler) WorkspaceMaterialized(_ context.Context, ws, rid string, _ *frontendv1.WorkspaceMaterializedCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "workspace_materialized", ws, rid
	return m.err
}
func (m *mockHandler) HostActionCompleted(_ context.Context, ws, rid string, _ *frontendv1.HostActionCompletedCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "host_action_completed", ws, rid
	return m.err
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
func (m *mockHandler) ForceQueueEntry(_ context.Context, ws, rid string, _ *frontendv1.QueueForceCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "queue_force", ws, rid
	return m.err
}
func (m *mockHandler) AcceptQueueEntry(_ context.Context, ws, rid string, _ *frontendv1.QueueAcceptCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "queue_accept", ws, rid
	return m.err
}
func (m *mockHandler) CancelQueueEntry(_ context.Context, ws, rid string, _ *frontendv1.QueueCancelCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "queue_cancel", ws, rid
	return m.err
}
func (m *mockHandler) PaintAck(_ context.Context, ws, rid string, _ *frontendv1.PaintAckCmd) error {
	m.called, m.lastWorkspace, m.lastRequestID = "paint_ack", ws, rid
	return m.err
}

func TestDispatchRoutesEachCommand(t *testing.T) {
	tests := []struct {
		name    string
		cmd     *frontendv1.FrontendCommand
		wantHit string
	}{
		{
			name:    "create workspace",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r0", Command: &frontendv1.FrontendCommand_CreateWorkspace{CreateWorkspace: &frontendv1.CreateWorkspaceCmd{RequestedName: "new", GitRoot: "/repo"}}},
			wantHit: "create_workspace",
		},
		{
			name:    "workspace materialized",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r0b", Command: &frontendv1.FrontendCommand_WorkspaceMaterialized{WorkspaceMaterialized: &frontendv1.WorkspaceMaterializedCmd{JobId: "job-1"}}},
			wantHit: "workspace_materialized",
		},
		{
			name:    "host action completed",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r0c", Command: &frontendv1.FrontendCommand_HostActionCompleted{HostActionCompleted: &frontendv1.HostActionCompletedCmd{ActionId: "action-1", Ok: true}}},
			wantHit: "host_action_completed",
		},
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
		{
			name:    "queue force",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r12", Workspace: "ws12", Command: &frontendv1.FrontendCommand_QueueForce{QueueForce: &frontendv1.QueueForceCmd{EntryId: "q_1"}}},
			wantHit: "queue_force",
		},
		{
			// A frontend's attestation that it PAINTED the history — the half
			// of green's promise the daemon cannot observe for itself.
			name:    "paint ack",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r17", Workspace: "ws17", Command: &frontendv1.FrontendCommand_PaintAck{PaintAck: &frontendv1.PaintAckCmd{ThroughSeq: 42}}},
			wantHit: "paint_ack",
		},
		{
			// Seq 0 is a REAL attestation of an empty history, not an absent
			// one: it is what lets a never-prompted session read as ready.
			name:    "paint ack of an empty history",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r18", Workspace: "ws18", Command: &frontendv1.FrontendCommand_PaintAck{PaintAck: &frontendv1.PaintAckCmd{ThroughSeq: 0}}},
			wantHit: "paint_ack",
		},
		{
			name:    "queue accept",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r13", Workspace: "ws13", Command: &frontendv1.FrontendCommand_QueueAccept{QueueAccept: &frontendv1.QueueAcceptCmd{EntryId: "q_1"}}},
			wantHit: "queue_accept",
		},
		{
			name:    "queue cancel",
			cmd:     &frontendv1.FrontendCommand{RequestId: "r14", Workspace: "ws14", Command: &frontendv1.FrontendCommand_QueueCancel{QueueCancel: &frontendv1.QueueCancelCmd{EntryId: "q_1"}}},
			wantHit: "queue_cancel",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := &mockHandler{}

			// Act.
			ack := Dispatch(context.Background(), nil, h, tc.cmd)

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
	Dispatch(context.Background(), nil, h, cmd)

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
	ack := Dispatch(context.Background(), nil, h, cmd)

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
	ack := Dispatch(context.Background(), nil, h, cmd)

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
	ack := Dispatch(context.Background(), nil, &mockHandler{}, nil)

	// Assert.
	if ack.GetOk() || ack.GetError() == "" {
		t.Errorf("nil command should produce a loud failing ack, got %v", ack)
	}
}

// --- CommandAck classification (F4) -----------------------------------------
//
// Before this, a refused command reached Emacs as raw Go text and reached the
// webapp as nothing at all. These assert the ack now carries the CLASSIFIED
// account beside the legacy string, one edge case per test.

func TestDispatchFailingAckCarriesAClassifiedFailure(t *testing.T) {
	// Arrange.
	h := &mockHandler{err: errors.New("submit exploded")}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), nil, h, cmd)

	// Assert.
	if ack.GetFailure() == nil {
		t.Fatal("a failing ack carried no classified failure; the webapp renders nothing from the string")
	}
}

func TestDispatchOkAckCarriesNoFailure(t *testing.T) {
	// Arrange.
	h := &mockHandler{}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), nil, h, cmd)

	// Assert.
	if ack.GetFailure() != nil {
		t.Fatalf("a successful ack carried a failure: %v", ack.GetFailure())
	}
}

func TestDispatchClassifiesEachSentinel(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want errclass.Type
	}{
		{"shim not connected", errclass.ErrShimNotConnected, errclass.TypeShimNotConnected},
		{"shim nack", errclass.ErrShimNack, errclass.TypeShimRejected},
		{"shim ack timeout", errclass.ErrShimAckTimeout, errclass.TypeShimAckTimeout},
		{"shim version mismatch", errclass.ErrShimVersionMismatch, errclass.TypeShimVersionMismatch},
		{"shim seq regression", errclass.ErrShimSeqRegression, errclass.TypeShimSeqRegression},
		{"not live session", errclass.ErrNotLiveSession, errclass.TypeSessionNotLive},
		{"repull in flight", errclass.ErrRepullInFlight, errclass.TypeHistoryRepullInFlight},
		{"repull truncated", errclass.ErrRepullTruncated, errclass.TypeHistoryReplayTruncated},
		{"interrupt undelivered", errclass.ErrInterruptUndelivered, errclass.TypeInterruptUndelivered},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			h := &mockHandler{err: tc.err}
			cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

			// Act.
			ack := Dispatch(context.Background(), nil, h, cmd)

			// Assert.
			if got := ack.GetFailure().GetErrorType(); got != string(tc.want) {
				t.Fatalf("error_type = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestDispatchClassifiesAWrappedNackWithItsReason(t *testing.T) {
	// Arrange: the shape control.go actually produces. The shim's raw reason
	// is machinery and must land in source_detail rather than in the headline.
	h := &mockHandler{err: fmt.Errorf("%w: request_id=r reason=%q", errclass.ErrShimNack, "store rejected batch")}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), nil, h, cmd)

	// Assert.
	if !strings.Contains(ack.GetFailure().GetSourceDetail(), "store rejected batch") {
		t.Fatalf("source_detail = %q, want the shim's raw reason", ack.GetFailure().GetSourceDetail())
	}
}

func TestDispatchFallsThroughLoudlyForAnUnclassifiedError(t *testing.T) {
	// Arrange: an error matching no sentinel.
	var logged []string
	logf := func(format string, args ...any) { logged = append(logged, fmt.Sprintf(format, args...)) }
	h := &mockHandler{err: errors.New("submit exploded")}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), logf, h, cmd)

	// Assert.
	if ack.GetFailure().GetErrorType() != string(errclass.TypeInternalUnclassified) {
		t.Fatalf("error_type = %q, want %q", ack.GetFailure().GetErrorType(), errclass.TypeInternalUnclassified)
	}
	if len(logged) == 0 {
		t.Fatal("an unclassified command error passed SILENTLY; the fallthrough must be loud")
	}
}

func TestDispatchKeepsTheLegacyErrorStringBesideTheFailure(t *testing.T) {
	// Arrange: Emacs's echo is the only surface rendering a refusal today, so
	// the string must survive until both frontends read the classified field.
	h := &mockHandler{err: errors.New("submit exploded")}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), nil, h, cmd)

	// Assert.
	if ack.GetError() != "submit exploded" {
		t.Fatalf("ack error = %q, want the legacy text preserved", ack.GetError())
	}
}

func TestDispatchClassifiesAnUnknownCommand(t *testing.T) {
	// Arrange: an empty oneof.
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Workspace: "wsX"}

	// Act.
	ack := Dispatch(context.Background(), nil, &mockHandler{}, cmd)

	// Assert.
	if ack.GetFailure() == nil {
		t.Fatal("an unknown command produced no classified failure")
	}
}

func TestDispatchClassifiesANilCommand(t *testing.T) {
	// Arrange.
	// Act.
	ack := Dispatch(context.Background(), nil, &mockHandler{}, nil)

	// Assert.
	if ack.GetFailure() == nil {
		t.Fatal("a nil command produced no classified failure")
	}
}

func TestDispatchNeverEmitsAClientPrefixedType(t *testing.T) {
	// Arrange: the namespace partition — `client.` belongs to the frontends.
	h := &mockHandler{err: errclass.ErrShimNack}
	cmd := &frontendv1.FrontendCommand{RequestId: "r", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{}}}

	// Act.
	ack := Dispatch(context.Background(), nil, h, cmd)

	// Assert.
	if !errclass.IsDaemonType(ack.GetFailure().GetErrorType()) {
		t.Fatalf("daemon emitted %q, which is frontend-reserved", ack.GetFailure().GetErrorType())
	}
}
