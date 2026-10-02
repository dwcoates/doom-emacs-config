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
		Turn:     &conversationv1.TurnId{Value: "turn-1"},
		Said:     validUserSaid(),
		Origin:   conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		PageSize: DefaultPageSize,
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
			req:   &shimv1.StartSessionRequest{},
			field: "StartSessionRequest.source",
		},
		{
			name: "fresh with a model whose name is empty",
			req: &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{
				Fresh: &shimv1.StartSessionFresh{Model: &conversationv1.AgentModel{}, PermissionMode: validMode()},
			}},
			field: "StartSessionRequest.fresh.model.name",
		},
		{
			name: "fresh without a permission mode",
			req: &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{
				Fresh: &shimv1.StartSessionFresh{Model: validModel()},
			}},
			field: "StartSessionRequest.fresh.permission_mode",
		},
		{
			name: "resume without a vendor id",
			req: &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Resume{
				Resume: &shimv1.StartSessionResume{},
			}},
			field: "StartSessionRequest.resume.vendor_session_id",
		},
		{
			name: "cold remediation with no arm",
			req: &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Resume{
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
	req := &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{
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
		{name: "zero page size", mut: func(r *shimv1.StartTurnRequest) { r.PageSize = 0 }, field: "StartTurnRequest.page_size"},
		{name: "empty known-through", mut: func(r *shimv1.StartTurnRequest) {
			r.KnownThrough = &conversationv1.HistoryPointer{}
		}, field: "StartTurnRequest.known_through.value"},
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

// TestValidateWatchAgentRequest covers the watch's own illegal shapes.
func TestValidateWatchAgentRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *shimv1.WatchAgentRequest
		field string
	}{
		{name: "zero page size", req: &shimv1.WatchAgentRequest{}, field: "WatchAgentRequest.page_size"},
		{
			name:  "empty target",
			req:   &shimv1.WatchAgentRequest{PageSize: DefaultPageSize, Target: &conversationv1.AgentId{}},
			field: "WatchAgentRequest.target.value",
		},
		{
			name:  "empty known-through",
			req:   &shimv1.WatchAgentRequest{PageSize: DefaultPageSize, KnownThrough: &conversationv1.HistoryPointer{}},
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
		{name: "zero page size", req: &shimv1.ReadHistoryRequest{}, field: "ReadHistoryRequest.page_size"},
		{
			name:  "no position arm",
			req:   &shimv1.ReadHistoryRequest{PageSize: DefaultPageSize},
			field: "ReadHistoryRequest.position",
		},
		{
			name: "empty after pointer",
			req: &shimv1.ReadHistoryRequest{
				PageSize: DefaultPageSize,
				Position: &shimv1.ReadHistoryRequest_After{After: &conversationv1.HistoryPointer{}},
			},
			field: "ReadHistoryRequest.after.value",
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
	req := &shimv1.StartSessionRequest{Source: &shimv1.StartSessionRequest_Fresh{
		Fresh: &shimv1.StartSessionFresh{PermissionMode: validMode()},
	}}

	// Act.
	err := validateStartSessionRequest(req)

	// Assert.
	if err != nil {
		t.Fatalf("validateStartSessionRequest with no model = %v, want it accepted", err)
	}
}
