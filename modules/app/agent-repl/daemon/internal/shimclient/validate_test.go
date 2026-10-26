package shimclient

import (
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"
)

// validStartTurnRequest is the smallest legal StartTurn.
func validStartTurnRequest() *shimv1.StartTurnRequest {
	return &shimv1.StartTurnRequest{
		Turn:    &conversationv1.TurnId{Value: "turn-1"},
		Said:    validUserSaid(),
		Origin:  conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Opening: &shimv1.StartTurnRequest_TailOnly{TailOnly: &shimv1.StartTurnTailOnly{}},
	}
}

// tailOnlyWatch is the smallest legal WatchAgent: the main agent, no history.
func tailOnlyWatch() *shimv1.WatchAgentRequest {
	return &shimv1.WatchAgentRequest{
		Opening: &shimv1.WatchAgentRequest_TailOnly{TailOnly: &shimv1.WatchAgentTailOnly{}},
	}
}

// validUserSaid is the smallest legal prompt body.
func validUserSaid() *conversationv1.UserSaid {
	return &conversationv1.UserSaid{
		Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Text{
					Text: &conversationv1.TextBlock{Text: "hello"},
				},
			}},
		},
	}
}

// validModel is the smallest legal model echo.
func validModel() *conversationv1.AgentModel {
	return &conversationv1.AgentModel{Name: "claude-opus-5"}
}

// validMode is the smallest legal permission mode.
func validMode() *conversationv1.AgentPermissionMode {
	return &conversationv1.AgentPermissionMode{
		Mode: &conversationv1.AgentPermissionMode_Default{
			Default: &conversationv1.AgentPermissionModeDefault{},
		},
	}
}

// TestValidateStartSessionRequest covers each way a start can be illegal.
func TestValidateStartSessionRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.StartSessionRequest
		field string
	}{
		{
			name:  "no source arm",
			req:   &shimv1.StartSessionRequest{WorkspaceName: "parser-fix"},
			field: "StartSessionRequest.source",
		},
		{
			name: "no workspace name",
			req: &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{
				Fresh: &shimv1.StartSessionFresh{Model: validModel(), PermissionMode: validMode()},
			}},
			field: "StartSessionRequest.workspace_name",
		},
		{
			name: "fresh with a model whose name is empty",
			req: &shimv1.StartSessionRequest{WorkspaceName: "parser-fix", Source: &shimv1.StartSessionRequest_Fresh{
				Fresh: &shimv1.StartSessionFresh{Model: &conversationv1.AgentModel{}, PermissionMode: validMode()},
			}},
			field: "StartSessionRequest.fresh.model.name",
		},
		{
			name: "fresh without a permission mode",
			req: &shimv1.StartSessionRequest{WorkspaceName: "parser-fix", Source: &shimv1.StartSessionRequest_Fresh{
				Fresh: &shimv1.StartSessionFresh{Model: validModel()},
			}},
			field: "StartSessionRequest.fresh.permission_mode",
		},
		{
			name: "resume without a vendor id",
			req: &shimv1.StartSessionRequest{WorkspaceName: "parser-fix", Source: &shimv1.StartSessionRequest_Resume{
				Resume: &shimv1.StartSessionResume{},
			}},
			field: "StartSessionRequest.resume.vendor_session_id",
		},
		{
			name: "cold remediation with no arm",
			req: &shimv1.StartSessionRequest{WorkspaceName: "parser-fix", Source: &shimv1.StartSessionRequest_Resume{
				Resume: &shimv1.StartSessionResume{
					VendorSessionId: "vendor-1",
					ColdRemediation: &conversationv1.SessionColdRemediation{},
				},
			}},
			field: "StartSessionRequest.resume.cold_remediation.remediation",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateStartSessionRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateStartSessionRequestAccepts asserts a legal fresh start passes.
func TestValidateStartSessionRequestAccepts(t *testing.T) {
	// Arrange.
	req := &shimv1.StartSessionRequest{WorkspaceName: "parser-fix", Source: &shimv1.StartSessionRequest_Fresh{
		Fresh: &shimv1.StartSessionFresh{Model: validModel(), PermissionMode: validMode()},
	}}

	// Act.
	err := validateStartSessionRequest(req)

	// Assert.
	if err != nil {
		t.Fatalf("validateStartSessionRequest() = %v, want nil", err)
	}
}

// TestValidateStartTurnRequest covers each way a turn can be illegal.
func TestValidateStartTurnRequest(t *testing.T) {
	tests := []struct {
		name  string
		mut   func(*shimv1.StartTurnRequest)
		field string
	}{
		{name: "no turn", mut: func(r *shimv1.StartTurnRequest) { r.Turn = nil }, field: "StartTurnRequest.turn"},
		{name: "empty turn value", mut: func(r *shimv1.StartTurnRequest) { r.Turn = &conversationv1.TurnId{} }, field: "StartTurnRequest.turn.value"},
		{name: "no prompt", mut: func(r *shimv1.StartTurnRequest) { r.Said = nil }, field: "StartTurnRequest.said"},
		{name: "no blocks", mut: func(r *shimv1.StartTurnRequest) {
			r.Said = &conversationv1.UserSaid{Content: &conversationv1.UserContent{}}
		}, field: "StartTurnRequest.said.content.blocks"},
		{name: "unspecified origin", mut: func(r *shimv1.StartTurnRequest) {
			r.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED
		}, field: "StartTurnRequest.origin"},
		{name: "unset opening", mut: func(r *shimv1.StartTurnRequest) { r.Opening = nil }, field: "StartTurnRequest.opening"},
		{name: "nil tail-only arm", mut: func(r *shimv1.StartTurnRequest) {
			r.Opening = &shimv1.StartTurnRequest_TailOnly{}
		}, field: "StartTurnRequest.tail_only"},
		{name: "empty known-through", mut: func(r *shimv1.StartTurnRequest) {
			r.Opening = &shimv1.StartTurnRequest_KnownThrough{KnownThrough: &conversationv1.HistoryPointer{}}
		}, field: "StartTurnRequest.known_through.value"},
		{name: "a sender on a prompt no agent sent", mut: func(r *shimv1.StartTurnRequest) {
			r.SentBy = validSentBy()
		}, field: "StartTurnRequest.sent_by"},
		{name: "an agent's prompt naming no sender", mut: func(r *shimv1.StartTurnRequest) {
			r.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
		}, field: "StartTurnRequest.sent_by"},
		{name: "a sender with no workspace id", mut: func(r *shimv1.StartTurnRequest) {
			r.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
			r.SentBy = validSentBy()
			r.SentBy.WorkspaceId = ""
		}, field: "StartTurnRequest.sent_by.workspace_id"},
		{name: "a sender's name set but empty", mut: func(r *shimv1.StartTurnRequest) {
			r.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
			r.SentBy = validSentBy()
			r.SentBy.WorkspaceName = new(string)
		}, field: "StartTurnRequest.sent_by.workspace_name"},
		{name: "a sender's mode naming no mode", mut: func(r *shimv1.StartTurnRequest) {
			r.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
			r.SentBy = validSentBy()
			r.SentBy.PermissionMode = &conversationv1.AgentPermissionMode{}
		}, field: "StartTurnRequest.sent_by.permission_mode.mode"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			req := validStartTurnRequest()
			tc.mut(req)

			// Act.
			err := validateStartTurnRequest(req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// validSentBy is the fullest legal sender: named, with its mode.
func validSentBy() *shimv1.StartTurnSentBy {
	name := "sender"
	return &shimv1.StartTurnSentBy{
		WorkspaceId:   "ws-sender",
		WorkspaceName: &name,
		PermissionMode: &conversationv1.AgentPermissionMode{
			Mode: &conversationv1.AgentPermissionMode_Auto{Auto: &conversationv1.AgentPermissionModeAuto{}},
		},
	}
}

// TestValidateStartTurnRequestAcceptsASender: an agent's prompt naming its
// sender is legal, named or not, with its mode or without one.
func TestValidateStartTurnRequestAcceptsASender(t *testing.T) {
	tests := []struct {
		name string
		mut  func(*shimv1.StartTurnSentBy)
	}{
		{name: "named, with its mode", mut: func(*shimv1.StartTurnSentBy) {}},
		{name: "a sender the daemon cannot name", mut: func(s *shimv1.StartTurnSentBy) { s.WorkspaceName = nil }},
		{name: "a sender whose mode the daemon does not hold", mut: func(s *shimv1.StartTurnSentBy) { s.PermissionMode = nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			req := validStartTurnRequest()
			req.Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT
			req.SentBy = validSentBy()
			tc.mut(req.SentBy)

			// Act.
			err := validateStartTurnRequest(req)

			// Assert.
			if err != nil {
				t.Fatalf("validateStartTurnRequest = %v, want nil", err)
			}
		})
	}
}

// TestValidateWatchAgentRequest covers the watch's own illegal shapes.
func TestValidateWatchAgentRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.WatchAgentRequest
		field string
	}{
		{name: "unset opening", req: &shimv1.WatchAgentRequest{}, field: "WatchAgentRequest.opening"},
		{
			name:  "nil tail-only arm",
			req:   &shimv1.WatchAgentRequest{Opening: &shimv1.WatchAgentRequest_TailOnly{}},
			field: "WatchAgentRequest.tail_only",
		},
		{
			name: "empty target",
			req: &shimv1.WatchAgentRequest{
				Target:  &conversationv1.AgentId{},
				Opening: &shimv1.WatchAgentRequest_TailOnly{TailOnly: &shimv1.WatchAgentTailOnly{}},
			},
			field: "WatchAgentRequest.target.value",
		},
		{
			name: "empty known-through",
			req: &shimv1.WatchAgentRequest{
				Opening: &shimv1.WatchAgentRequest_KnownThrough{KnownThrough: &conversationv1.HistoryPointer{}},
			},
			field: "WatchAgentRequest.known_through.value",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateWatchAgentRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateUpdateAgentRequest covers the write path's illegal shapes.
func TestValidateUpdateAgentRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.UpdateAgentRequest
		field string
	}{
		{name: "no input", req: &shimv1.UpdateAgentRequest{}, field: "UpdateAgentRequest.input"},
		{
			name:  "input with no arm",
			req:   &shimv1.UpdateAgentRequest{Input: &conversationv1.AgentInput{}},
			field: "UpdateAgentRequest.input.input",
		},
		{
			name: "answer with no arm",
			req: &shimv1.UpdateAgentRequest{Input: &conversationv1.AgentInput{
				Input: &conversationv1.AgentInput_Answer{Answer: &conversationv1.AgentAnswer{}},
			}},
			field: "UpdateAgentRequest.input.answer.answer",
		},
		{
			name: "prompt with no blocks",
			req: &shimv1.UpdateAgentRequest{Input: &conversationv1.AgentInput{
				Input: &conversationv1.AgentInput_Prompt{Prompt: &conversationv1.UserSaid{
					Content: &conversationv1.UserContent{},
				}},
			}},
			field: "UpdateAgentRequest.input.prompt.content.blocks",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateUpdateAgentRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateReadHistoryRequest covers the paged read's illegal shapes.
func TestValidateReadHistoryRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.ReadHistoryRequest
		field string
	}{
		{
			name:  "no position arm",
			req:   &shimv1.ReadHistoryRequest{},
			field: "ReadHistoryRequest.position",
		},
		{
			name: "empty after pointer",
			req: &shimv1.ReadHistoryRequest{
				Position: &shimv1.ReadHistoryRequest_After{After: &conversationv1.HistoryPointer{}},
			},
			field: "ReadHistoryRequest.after.value",
		},
		{
			name: "a bound that names no instant",
			req: &shimv1.ReadHistoryRequest{
				Position: &shimv1.ReadHistoryRequest_Through{Through: &conversationv1.ConversationThrough{}},
			},
			field: "ReadHistoryRequest.through.at_ms",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateReadHistoryRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateResolvePeerDeliveryRequest covers the delivery lookup's illegal
// shapes.
func TestValidateResolvePeerDeliveryRequest(t *testing.T) {
	tests := []struct {
		name string
		req  *shimv1.ResolvePeerDeliveryRequest
	}{
		{name: "no delivery id", req: &shimv1.ResolvePeerDeliveryRequest{}},
		{name: "an empty delivery id", req: &shimv1.ResolvePeerDeliveryRequest{DeliveryId: &conversationv1.PeerDeliveryId{}}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateResolvePeerDeliveryRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, "ResolvePeerDeliveryRequest.delivery_id")
		})
	}
}

// TestValidateResolveVendorTaskRequest covers the task lookup's illegal shapes.
func TestValidateResolveVendorTaskRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.ResolveVendorTaskRequest
		field string
	}{
		{name: "no session", req: &shimv1.ResolveVendorTaskRequest{VendorTaskId: "a47cd"}, field: "ResolveVendorTaskRequest.session"},
		{name: "no task id", req: &shimv1.ResolveVendorTaskRequest{Session: &conversationv1.AgentId{Value: "agent-main"}}, field: "ResolveVendorTaskRequest.vendor_task_id"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateResolveVendorTaskRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateResolvePeerDeliveryRequestAcceptsADeliveryID pins the legal shape.
func TestValidateResolvePeerDeliveryRequestAcceptsADeliveryID(t *testing.T) {
	// Arrange.
	req := &shimv1.ResolvePeerDeliveryRequest{DeliveryId: &conversationv1.PeerDeliveryId{Value: "m-1"}}

	// Act.
	err := validateResolvePeerDeliveryRequest(req)

	// Assert.
	if err != nil {
		t.Fatalf("validateResolvePeerDeliveryRequest = %v, want a delivery id accepted", err)
	}
}

// TestValidateReadHistoryRequestAcceptsABoundedRead pins the read a fork's
// inherited past is paged by.
func TestValidateReadHistoryRequestAcceptsABoundedRead(t *testing.T) {
	// Arrange.
	req := &shimv1.ReadHistoryRequest{
		Target:   &conversationv1.AgentId{Value: "book-parent"},
		Position: &shimv1.ReadHistoryRequest_Through{Through: &conversationv1.ConversationThrough{AtMs: 42}},
	}

	// Act.
	err := validateReadHistoryRequest(req)

	// Assert.
	if err != nil {
		t.Fatalf("validateReadHistoryRequest = %v, want a bounded read accepted", err)
	}
}

// TestValidateGatherTitleDigestRequest asserts a nil request is the only
// illegal shape (the message carries no fields).
func TestValidateGatherTitleDigestRequest(t *testing.T) {
	// Arrange, Act — a nil request is refused.
	err := validateGatherTitleDigestRequest(nil)

	// Assert.
	assertInvalidField(t, err, "GatherTitleDigestRequest")

	// Arrange, Act — an empty (non-nil) request is legal.
	if err := validateGatherTitleDigestRequest(&shimv1.GatherTitleDigestRequest{}); err != nil {
		t.Fatalf("validateGatherTitleDigestRequest(empty) = %v, want nil", err)
	}
}

// TestValidateKillTurnRequest asserts the turn a kill names is required.
func TestValidateKillTurnRequest(t *testing.T) {
	// Arrange, Act.
	err := validateKillTurnRequest(&shimv1.KillTurnRequest{})

	// Assert.
	assertInvalidField(t, err, "KillTurnRequest.turn")
}

// TestValidateStopBashRequest asserts the handle a stop names is required.
func TestValidateStopBashRequest(t *testing.T) {
	// Arrange, Act.
	err := validateStopBashRequest(&shimv1.StopBashRequest{})

	// Assert.
	assertInvalidField(t, err, "StopBashRequest.work")
}

// TestValidateDetachForegroundRequest asserts the unit a detach names is
// required.
func TestValidateDetachForegroundRequest(t *testing.T) {
	// Arrange, Act.
	err := validateDetachForegroundRequest(&shimv1.DetachForegroundRequest{})

	// Assert.
	assertInvalidField(t, err, "DetachForegroundRequest.unit")
}

// TestValidateSetSessionModelRequest asserts the model a switch names is
// required.
func TestValidateSetSessionModelRequest(t *testing.T) {
	// Arrange, Act.
	err := validateSetSessionModelRequest(&shimv1.SetSessionModelRequest{})

	// Assert.
	assertInvalidField(t, err, "SetSessionModelRequest.model")
}

// TestValidateSetSessionEffortRequest asserts the level an effort change
// names is never UNSPECIFIED.
func TestValidateSetSessionEffortRequest(t *testing.T) {
	// Arrange, Act.
	err := validateSetSessionEffortRequest(&shimv1.SetSessionEffortRequest{})

	// Assert.
	assertInvalidField(t, err, "SetSessionEffortRequest.effort")
}

// TestValidateSetSessionEffortRequestAcceptsANamedLevel asserts a named level
// passes.
func TestValidateSetSessionEffortRequestAcceptsANamedLevel(t *testing.T) {
	// Arrange, Act.
	err := validateSetSessionEffortRequest(&shimv1.SetSessionEffortRequest{
		Effort: conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH,
	})

	// Assert.
	if err != nil {
		t.Errorf("err = %v, want nil", err)
	}
}

// TestValidateSetSessionPermissionModeRequest asserts the mode a switch names
// is required.
func TestValidateSetSessionPermissionModeRequest(t *testing.T) {
	// Arrange, Act.
	err := validateSetSessionPermissionModeRequest(&shimv1.SetSessionPermissionModeRequest{})

	// Assert.
	assertInvalidField(t, err, "SetSessionPermissionModeRequest.permission_mode")
}

// TestValidateSendKeepaliveRequest asserts the empty keep-alive request is
// sent, and only its absence refused.
func TestValidateSendKeepaliveRequest(t *testing.T) {
	tests := []struct {
		name    string
		req     *shimv1.SendKeepaliveRequest
		invalid string
	}{
		{"nil", nil, "SendKeepaliveRequest"},
		{"empty", &shimv1.SendKeepaliveRequest{}, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := validateSendKeepaliveRequest(tt.req)

			// Assert.
			if tt.invalid == "" {
				if err != nil {
					t.Fatalf("validateSendKeepaliveRequest = %v, want nil", err)
				}
				return
			}
			assertInvalidField(t, err, tt.invalid)
		})
	}
}

// TestValidateRetryVendorErrorNowRequest asserts the empty retry request is
// sent, and only its absence refused.
func TestValidateRetryVendorErrorNowRequest(t *testing.T) {
	tests := []struct {
		name    string
		req     *shimv1.RetryVendorErrorNowRequest
		invalid string
	}{
		{"nil", nil, "RetryVendorErrorNowRequest"},
		{"empty", &shimv1.RetryVendorErrorNowRequest{}, ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := validateRetryVendorErrorNowRequest(tt.req)

			// Assert.
			if tt.invalid == "" {
				if err != nil {
					t.Fatalf("validateRetryVendorErrorNowRequest = %v, want nil", err)
				}
				return
			}
			assertInvalidField(t, err, tt.invalid)
		})
	}
}

// TestValidateNilRequestIsRefused asserts a nil request body is refused rather
// than sent.
func TestValidateNilRequestIsRefused(t *testing.T) {
	// Arrange, Act.
	err := validateHibernateRequest(nil)

	// Assert.
	assertInvalidField(t, err, "HibernateRequest")
}

// assertInvalidField asserts err is an InvalidRequestError naming field.
func assertInvalidField(t *testing.T, err error, field string) {
	t.Helper()

	var invalidErr *InvalidRequestError
	if !errors.As(err, &invalidErr) {
		t.Fatalf("error = %v, want *InvalidRequestError", err)
	}
	if invalidErr.Field != field {
		t.Fatalf("field = %q, want %q", invalidErr.Field, field)
	}
}

// TestValidateStartSessionFreshAcceptsAnUnsetModel is the landing-7 contract:
// StartSessionFresh.model is optional, and UNSET means the SDK's own default
// rather than a missing required field.
func TestValidateStartSessionFreshAcceptsAnUnsetModel(t *testing.T) {
	// Arrange.
	req := &shimv1.StartSessionRequest{WorkspaceName: "parser-fix", Source: &shimv1.StartSessionRequest_Fresh{
		Fresh: &shimv1.StartSessionFresh{PermissionMode: validMode()},
	}}

	// Act.
	err := validateStartSessionRequest(req)

	// Assert.
	if err != nil {
		t.Fatalf("validateStartSessionRequest with no model = %v, want it accepted", err)
	}
}

// TestValidateWatchAgentRequestAcceptsEachOpening asserts both openings the
// daemon asks for — no history, or catch-up past a held pointer — are legal.
func TestValidateWatchAgentRequestAcceptsEachOpening(t *testing.T) {
	tests := []struct {
		name string
		req  *shimv1.WatchAgentRequest
	}{
		{name: "tail only", req: tailOnlyWatch()},
		{name: "known through", req: &shimv1.WatchAgentRequest{
			Opening: &shimv1.WatchAgentRequest_KnownThrough{KnownThrough: &conversationv1.HistoryPointer{Value: "ptr-1"}},
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateWatchAgentRequest(tc.req)

			// Assert.
			if err != nil {
				t.Fatalf("validateWatchAgentRequest() = %v, want nil", err)
			}
		})
	}
}

// TestValidateStartTurnRequestAcceptsEachOpening asserts both openings the
// daemon asks a turn's page for are legal.
func TestValidateStartTurnRequestAcceptsEachOpening(t *testing.T) {
	tests := []struct {
		name    string
		opening func(*shimv1.StartTurnRequest)
	}{
		{name: "tail only", opening: func(*shimv1.StartTurnRequest) {}},
		{name: "known through", opening: func(r *shimv1.StartTurnRequest) {
			r.Opening = &shimv1.StartTurnRequest_KnownThrough{KnownThrough: &conversationv1.HistoryPointer{Value: "ptr-1"}}
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			req := validStartTurnRequest()
			tc.opening(req)

			// Act.
			err := validateStartTurnRequest(req)

			// Assert.
			if err != nil {
				t.Fatalf("validateStartTurnRequest() = %v, want nil", err)
			}
		})
	}
}

// TestValidateForkSessionRequest covers the fork's illegal shapes.
func TestValidateForkSessionRequest(t *testing.T) {
	whole := &shimv1.ForkPoint{Extent: &shimv1.ForkPoint_Whole{Whole: &shimv1.ForkPointWhole{}}}
	tests := []struct {
		name  string
		req   *shimv1.ForkSessionRequest
		field string
	}{
		{name: "nil", req: nil, field: "ForkSessionRequest"},
		{name: "no parent session", req: &shimv1.ForkSessionRequest{ParentWorkspaceDir: "/p", Point: whole}, field: "ForkSessionRequest.parent_vendor_session_id"},
		{name: "no parent dir", req: &shimv1.ForkSessionRequest{ParentVendorSessionId: "v", Point: whole}, field: "ForkSessionRequest.parent_workspace_dir"},
		{name: "no point", req: &shimv1.ForkSessionRequest{ParentVendorSessionId: "v", ParentWorkspaceDir: "/p"}, field: "ForkSessionRequest.point"},
		{name: "no extent", req: &shimv1.ForkSessionRequest{ParentVendorSessionId: "v", ParentWorkspaceDir: "/p", Point: &shimv1.ForkPoint{}}, field: "ForkSessionRequest.point.extent"},
		{
			name: "a cut naming no record",
			req: &shimv1.ForkSessionRequest{ParentVendorSessionId: "v", ParentWorkspaceDir: "/p", Point: &shimv1.ForkPoint{
				Extent: &shimv1.ForkPoint_ThroughRecord{ThroughRecord: &shimv1.ForkPointThroughRecord{}},
			}},
			field: "ForkSessionRequest.point.through_record.record",
		},
		{
			name: "a cut before no record",
			req: &shimv1.ForkSessionRequest{ParentVendorSessionId: "v", ParentWorkspaceDir: "/p", Point: &shimv1.ForkPoint{
				Extent: &shimv1.ForkPoint_BeforeRecord{BeforeRecord: &shimv1.ForkPointBeforeRecord{Record: &shimv1.ForkRecordToken{}}},
			}},
			field: "ForkSessionRequest.point.before_record.record",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := validateForkSessionRequest(tc.req)

			// Assert.
			assertInvalidField(t, err, tc.field)
		})
	}
}

// TestValidateForkRequestsAcceptLegalShapes pins the shapes a fork sends.
func TestValidateForkRequestsAcceptLegalShapes(t *testing.T) {
	// Arrange.
	fork := &shimv1.ForkSessionRequest{ParentVendorSessionId: "v", ParentWorkspaceDir: "/p", Point: &shimv1.ForkPoint{
		Extent: &shimv1.ForkPoint_ThroughRecord{ThroughRecord: &shimv1.ForkPointThroughRecord{Record: &shimv1.ForkRecordToken{Value: "r"}}},
	}}

	// Act, Assert.
	if err := validateForkSessionRequest(fork); err != nil {
		t.Fatalf("validateForkSessionRequest = %v, want accepted", err)
	}
	if err := validateGetForkPointRequest(&shimv1.GetForkPointRequest{}); err != nil {
		t.Fatalf("validateGetForkPointRequest = %v, want accepted", err)
	}
	assertInvalidField(t, validateGetForkPointRequest(nil), "GetForkPointRequest")
}
