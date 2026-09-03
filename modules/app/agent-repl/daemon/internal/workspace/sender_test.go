package workspace

import (
	"context"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
)

// fakeSenderClient answers each shim verb with whatever a test arranged.
type fakeSenderClient struct {
	startTurn      *shimv1.StartTurnResponse
	updateAgent    *shimv1.UpdateAgentResponse
	killTurn       *shimv1.KillTurnResponse
	setModel       *shimv1.SetSessionModelResponse
	setMode        *shimv1.SetSessionPermissionModeResponse
	startTurnReq   *shimv1.StartTurnRequest
	updateAgentReq *shimv1.UpdateAgentRequest
	setModelReq    *shimv1.SetSessionModelRequest
}

func (c *fakeSenderClient) StartTurn(_ context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	c.startTurnReq = req
	return c.startTurn, nil
}

func (c *fakeSenderClient) UpdateAgent(_ context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error) {
	c.updateAgentReq = req
	return c.updateAgent, nil
}

func (c *fakeSenderClient) KillTurn(context.Context, *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	return c.killTurn, nil
}

func (c *fakeSenderClient) SetSessionModel(_ context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error) {
	c.setModelReq = req
	return c.setModel, nil
}

func (c *fakeSenderClient) SetSessionPermissionMode(context.Context, *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error) {
	return c.setMode, nil
}

// TestSenderStartTurnCarriesTheDaemonsMintedTurn covers the identity the shim
// adopts rather than mints: every durable record the turn produces is stamped
// with it.
func TestSenderStartTurnCarriesTheDaemonsMintedTurn(t *testing.T) {
	// Arrange
	client := &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
		Result: &shimv1.StartTurnResponse_Success{Success: &shimv1.StartTurnSuccess{}},
	}}
	s := &sender{client: client}

	// Act
	if _, err := s.StartTurn(context.Background(), ids.TurnID("turn-1"), nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("StartTurn: %v", err)
	}

	// Assert
	if got := client.startTurnReq.GetTurn().GetValue(); got != "turn-1" {
		t.Fatalf("StartTurn turn = %q, want the daemon's minted turn-1", got)
	}
}

// TestSenderStartTurnReturnsTheSuccessWhole covers what the queue cannot
// recover from an error: the main agent and the opening page.
func TestSenderStartTurnReturnsTheSuccessWhole(t *testing.T) {
	// Arrange
	want := &shimv1.StartTurnSuccess{
		Prompt: &conversationv1.AgentPrompt{Agent: &conversationv1.AgentId{Value: "main-1"}},
	}
	s := &sender{client: &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
		Result: &shimv1.StartTurnResponse_Success{Success: want},
	}}}

	// Act
	got, err := s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if err != nil {
		t.Fatalf("StartTurn: %v", err)
	}
	if got.GetPrompt().GetAgent().GetValue() != "main-1" {
		t.Fatalf("StartTurn success = %v, want the prompt naming the main agent", got)
	}
}

// TestSenderStartTurnCarriesTheRefusalArm covers the typed refusal: the caller
// learns WHICH refusal it was rather than a sentence.
func TestSenderStartTurnCarriesTheRefusalArm(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
		Result: &shimv1.StartTurnResponse_Failure{Failure: &shimv1.StartTurnFailure{
			Detail: "the query is gone",
			Kind:   &shimv1.StartTurnFailure_QueryDead{QueryDead: &shimv1.StartTurnQueryDead{}},
		}},
	}}}

	// Act
	_, err := s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok {
		t.Fatalf("StartTurn = %v, want a typed shim refusal", err)
	}
	if refusal.Arm != "query_dead" {
		t.Fatalf("refusal arm = %q, want query_dead", refusal.Arm)
	}
}

// TestSenderStartTurnRefusesAnEmptyResponse covers the wire value that is
// neither arm, which is illegal and is surfaced rather than read as success.
func TestSenderStartTurnRefusesAnEmptyResponse(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{}}}

	// Act
	_, err := s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	if err == nil {
		t.Fatal("StartTurn accepted a response carrying neither arm")
	}
}

// TestSenderPromptAgentAddressesTheAgent covers the bubble composer's route:
// the prompt is delivered to the agent the row names.
func TestSenderPromptAgentAddressesTheAgent(t *testing.T) {
	// Arrange
	client := &fakeSenderClient{updateAgent: &shimv1.UpdateAgentResponse{
		Result: &shimv1.UpdateAgentResponse_Success{Success: &shimv1.UpdateAgentSuccess{}},
	}}
	s := &sender{client: client}

	// Act
	if err := s.PromptAgent(context.Background(), &conversationv1.AgentId{Value: "sub-1"}, nil); err != nil {
		t.Fatalf("PromptAgent: %v", err)
	}

	// Assert
	if got := client.updateAgentReq.GetTarget().GetValue(); got != "sub-1" {
		t.Fatalf("UpdateAgent target = %q, want sub-1", got)
	}
}

// TestSenderSetModelCarriesTheCatalogRefusal covers the arm a mode switch
// against an unserved option produces.
func TestSenderSetModelCarriesTheCatalogRefusal(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{setModel: &shimv1.SetSessionModelResponse{
		Result: &shimv1.SetSessionModelResponse_Failure{Failure: &shimv1.SetSessionModelFailure{
			Detail: "not served",
			Cause:  &shimv1.SetSessionModelFailure_ModelNotInCatalog{ModelNotInCatalog: &shimv1.SetSessionModelNotInCatalog{}},
		}},
	}}}

	// Act
	err := s.SetModel(context.Background(), "invented")

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok || refusal.Arm != "model_not_in_catalog" {
		t.Fatalf("SetModel = %v, want the model_not_in_catalog refusal", err)
	}
}

// TestSenderSetPermissionModeCarriesTheNoSessionRefusal covers a mode switch
// against a session that is not up.
func TestSenderSetPermissionModeCarriesTheNoSessionRefusal(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{setMode: &shimv1.SetSessionPermissionModeResponse{
		Result: &shimv1.SetSessionPermissionModeResponse_Failure{Failure: &shimv1.SetSessionPermissionModeFailure{
			Detail: "no session",
			Kind:   &shimv1.SetSessionPermissionModeFailure_NoSession{NoSession: &shimv1.SetSessionPermissionModeNoSession{}},
		}},
	}}}

	// Act
	err := s.SetPermissionMode(context.Background(), "plan")

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok || refusal.Arm != ArmShimNoSession {
		t.Fatalf("SetPermissionMode = %v, want the no_session refusal", err)
	}
}

// TestSenderKillTurnCarriesTheNotTheOpenTurnRefusal covers an interjection
// naming a turn that is not the open one.
func TestSenderKillTurnCarriesTheNotTheOpenTurnRefusal(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{killTurn: &shimv1.KillTurnResponse{
		Result: &shimv1.KillTurnResponse_Failure{Failure: &shimv1.KillTurnFailure{
			Detail: "another turn is open",
			Cause:  &shimv1.KillTurnFailure_NotTheOpenTurn{NotTheOpenTurn: &shimv1.KillTurnNotTheOpenTurn{}},
		}},
	}}}

	// Act
	err := s.KillTurn(context.Background(), "turn-1", false)

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok || refusal.Arm != ArmShimNotTheOpenTurn {
		t.Fatalf("KillTurn = %v, want the not_the_open_turn refusal", err)
	}
}

// TestSenderSetModelStatesTheColdThresholdPolicy covers the field whose unset
// ZERO refused every switch: the shim refuses `cold` STRICTLY ABOVE the stated
// threshold, so a zero threshold made a served model come back refused, and
// the refusal had no arm to land on.
func TestSenderSetModelStatesTheColdThresholdPolicy(t *testing.T) {
	// Arrange
	client := &fakeSenderClient{setModel: &shimv1.SetSessionModelResponse{
		Result: &shimv1.SetSessionModelResponse_Success{Success: &shimv1.SetSessionModelSuccess{}},
	}}
	s := &sender{client: client}

	// Act
	if err := s.SetModel(context.Background(), "sonnet"); err != nil {
		t.Fatalf("SetModel: %v", err)
	}

	// Assert
	if got := client.setModelReq.GetColdThresholdTokens(); got != coldThresholdPolicy {
		t.Fatalf("cold_threshold_tokens = %d, want the daemon's stated policy %d", got, uint64(coldThresholdPolicy))
	}
}

// TestSenderSetModelCarriesTheColdRefusal covers the arm the shim raises when
// it judges the switch cold anyway. It has no `SetModelError` arm yet
// (ERROR-ARMS.md holds the row), so it must at least reach the caller NAMED
// rather than collapsed into a bare sentence.
func TestSenderSetModelCarriesTheColdRefusal(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{setModel: &shimv1.SetSessionModelResponse{
		Result: &shimv1.SetSessionModelResponse_Failure{Failure: &shimv1.SetSessionModelFailure{
			Detail: "switching discards a warm cache",
			Cause: &shimv1.SetSessionModelFailure_Cold{
				Cold: &conversationv1.SessionCold{ContextTokens: 42},
			},
		}},
	}}}

	// Act
	err := s.SetModel(context.Background(), "sonnet")

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok || refusal.Arm != ArmShimCold {
		t.Fatalf("SetModel = %v, want the cold refusal", err)
	}
}
