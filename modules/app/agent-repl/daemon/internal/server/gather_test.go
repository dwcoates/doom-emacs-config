package server

import (
	"context"
	"errors"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/gather"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// fakeGathers is the gather service: Create submits once to the harness
// workspace through the request's own Submit, as the real service does per
// target, and answers resp or err.
type fakeGathers struct {
	req     gather.Request
	target  wsm.Workspace
	turn    ids.TurnID
	refused *agentreplv1.SubmitPromptError
	subErr  error
	resp    *agentreplv1.GatherWorkspacesResponse
	status  *agentreplv1.GetGatherStatusResponse
	asked   wsm.GatherID
	err     error
}

func (f *fakeGathers) Create(ctx context.Context, req gather.Request) (*agentreplv1.GatherWorkspacesResponse, error) {
	f.req = req
	if f.target.ID != "" {
		f.turn, f.refused, f.subErr = req.Submit(ctx, f.target, req.Key+"/"+string(f.target.ID))
	}
	if f.err != nil {
		return nil, f.err
	}
	if f.resp == nil {
		return &agentreplv1.GatherWorkspacesResponse{Result: &agentreplv1.GatherWorkspacesResponse_Success{Success: &agentreplv1.GatherWorkspacesSuccess{Gather: &agentreplv1.GatherRef{Id: "g-1"}}}}, nil
	}
	return f.resp, nil
}

func (f *fakeGathers) Status(_ context.Context, id wsm.GatherID) (*agentreplv1.GetGatherStatusResponse, error) {
	f.asked = id
	return f.status, f.err
}

func gatherRequest(sentBy *workspacev1.WorkspaceRef) *agentreplv1.GatherWorkspacesRequest {
	return &agentreplv1.GatherWorkspacesRequest{Caller: sentBy, TargetNames: []string{"target"}, Said: workspace.SaidText("status?"), IdempotencyKey: "k-1", AsUser: true}
}

func TestValidateGatherWorkspacesRequest(t *testing.T) {
	caller := &workspacev1.WorkspaceRef{Id: "c", Dir: "/c"}
	tests := []struct {
		name  string
		req   *agentreplv1.GatherWorkspacesRequest
		valid bool
	}{
		{"a whole request", gatherRequest(caller), true},
		{"no caller", &agentreplv1.GatherWorkspacesRequest{TargetNames: []string{"a"}, Said: workspace.SaidText("x"), IdempotencyKey: "k"}, false},
		{"no target", &agentreplv1.GatherWorkspacesRequest{Caller: caller, Said: workspace.SaidText("x"), IdempotencyKey: "k"}, false},
		{"a blank target name", &agentreplv1.GatherWorkspacesRequest{Caller: caller, TargetNames: []string{"a", " "}, Said: workspace.SaidText("x"), IdempotencyKey: "k"}, false},
		{"no prompt", &agentreplv1.GatherWorkspacesRequest{Caller: caller, TargetNames: []string{"a"}, IdempotencyKey: "k"}, false},
		{"no key", &agentreplv1.GatherWorkspacesRequest{Caller: caller, TargetNames: []string{"a"}, Said: workspace.SaidText("x")}, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := validateGatherWorkspacesRequest(tc.req)

			// Assert
			if (err == nil) != tc.valid {
				t.Fatalf("validate = %v, want valid=%v", err, tc.valid)
			}
		})
	}
}

func TestValidateGetGatherStatusRequest(t *testing.T) {
	tests := []struct {
		name  string
		req   *agentreplv1.GetGatherStatusRequest
		valid bool
	}{
		{"a ref", &agentreplv1.GetGatherStatusRequest{Gather: &agentreplv1.GatherRef{Id: "g"}}, true},
		{"no ref", &agentreplv1.GetGatherStatusRequest{}, false},
		{"a blank ref", &agentreplv1.GetGatherStatusRequest{Gather: &agentreplv1.GatherRef{}}, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := validateGetGatherStatusRequest(tc.req)

			// Assert
			if (err == nil) != tc.valid {
				t.Fatalf("validate = %v, want valid=%v", err, tc.valid)
			}
		})
	}
}

func TestGatherWorkspacesPromptsEachTargetAsAnAgentSentPromptFromTheCaller(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentBy := registerSender(h)
	h.Gathers.target = h.DB.workspaces[testWorkspaceID]
	h.Prompts.outcome = prompthandler.Outcome{Turn: "minted"}

	// Act
	resp, err := h.Client.GatherWorkspaces(context.Background(), connect.NewRequest(gatherRequest(sentBy)))

	// Assert
	if err != nil || resp.Msg.GetSuccess().GetGather().GetId() != "g-1" {
		t.Fatalf("GatherWorkspaces = %v, %v", resp, err)
	}
	if h.Gathers.turn != "minted" || h.Gathers.refused != nil || h.Gathers.subErr != nil {
		t.Fatalf("submit = %q, %v, %v; want the minted turn", h.Gathers.turn, h.Gathers.refused, h.Gathers.subErr)
	}
	if h.Prompts.lastOrigin != conversationv1.PromptOrigin_PROMPT_ORIGIN_AGENT_SENT || h.Prompts.lastSentBy == nil || *h.Prompts.lastSentBy != senderWorkspaceID {
		t.Fatalf("submitted as %v from %v, want an agent-sent prompt from the caller", h.Prompts.lastOrigin, h.Prompts.lastSentBy)
	}
	if h.Gathers.req.Caller != senderWorkspaceID || h.Gathers.req.PromptText != "status?" || h.Gathers.req.Key != "k-1" || len(h.Gathers.req.Names) != 1 {
		t.Fatalf("request = %+v, want the caller, prompt, key and names handed on", h.Gathers.req)
	}
}

func TestGatherWorkspacesHandsATargetsRefusalBackTyped(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentBy := registerSender(h)
	h.Gathers.target = h.DB.workspaces[testWorkspaceID]
	h.Prompts.err = promptqueue.ErrNoSession

	// Act
	_, err := h.Client.GatherWorkspaces(context.Background(), connect.NewRequest(gatherRequest(sentBy)))

	// Assert
	if err != nil || h.Gathers.refused.GetNoSession() == nil || h.Gathers.turn != "" {
		t.Fatalf("submit = %q, %v, %v; want the no_session refusal handed back", h.Gathers.turn, h.Gathers.refused, err)
	}
}

func TestGatherWorkspacesFailsAnAcceptanceThatMintedNoTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	sentBy := registerSender(h)
	h.Gathers.target = h.DB.workspaces[testWorkspaceID]
	h.Prompts.outcome = prompthandler.Outcome{Recognition: prompthandler.RecognizedAskAnswer}

	// Act
	if _, err := h.Client.GatherWorkspaces(context.Background(), connect.NewRequest(gatherRequest(sentBy))); err != nil {
		t.Fatal(err)
	}

	// Assert
	if h.Gathers.subErr == nil {
		t.Fatal("an agent-sent prompt accepted as no turn was handed back as a turn")
	}
}

func TestGatherFailureNamesHowTheServiceFailed(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want connect.Code
	}{
		{"the caller went away", context.Canceled, connect.CodeCanceled},
		{"the service stopped", gather.ErrStopped, connect.CodeUnavailable},
		{"anything else", errors.New("io"), connect.CodeInternal},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert
			if got := gatherFailure(tc.err).Code(); got != tc.want {
				t.Fatalf("code = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestGetGatherStatusAnswersTheServicesRead(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Gathers.status = &agentreplv1.GetGatherStatusResponse{Result: &agentreplv1.GetGatherStatusResponse_Error{Error: &agentreplv1.GetGatherStatusError{
		Cause: &agentreplv1.GetGatherStatusError_UnknownGather{UnknownGather: &agentreplv1.GetGatherStatusUnknownGather{}}}}}

	// Act
	resp, err := h.Client.GetGatherStatus(context.Background(), connect.NewRequest(&agentreplv1.GetGatherStatusRequest{Gather: &agentreplv1.GatherRef{Id: "g-9"}}))

	// Assert
	if err != nil || resp.Msg.GetError().GetUnknownGather() == nil || h.Gathers.asked != "g-9" {
		t.Fatalf("GetGatherStatus = %v, %v, asked %q", resp, err, h.Gathers.asked)
	}
}

func TestGetGatherStatusSurfacesTheServicesFailure(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.Gathers.err = errors.New("io")

	// Act
	_, err := h.Client.GetGatherStatus(context.Background(), connect.NewRequest(&agentreplv1.GetGatherStatusRequest{Gather: &agentreplv1.GatherRef{Id: "g-9"}}))

	// Assert
	if code := connectCode(t, err); code != connect.CodeInternal {
		t.Fatalf("code = %v, want Internal", code)
	}
}
