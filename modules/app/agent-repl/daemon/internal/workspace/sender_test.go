package workspace

import (
	"context"
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/sessionwatcher"
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
	killTurnReq    *shimv1.KillTurnRequest
	killTurnErr    error
}

func (c *fakeSenderClient) StartTurn(_ context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	c.startTurnReq = req
	return c.startTurn, nil
}

func (c *fakeSenderClient) UpdateAgent(_ context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error) {
	c.updateAgentReq = req
	return c.updateAgent, nil
}

func (c *fakeSenderClient) KillTurn(_ context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	c.killTurnReq = req
	if c.killTurnErr != nil {
		return nil, c.killTurnErr
	}
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

// TestSenderStartTurnNeverAsksForHistory covers the owner's rule on the turn
// path: an accepted turn's opening page replays nothing. It catches up from
// the newest main pointer the daemon holds, and opens tail_only without one.
func TestSenderStartTurnNeverAsksForHistory(t *testing.T) {
	tests := []struct {
		name      string
		known     func() *conversationv1.HistoryPointer
		wantKnown string
		wantTail  bool
	}{
		{
			name:      "the main watch's pointer bounds the page",
			known:     func() *conversationv1.HistoryPointer { return &conversationv1.HistoryPointer{Value: "ptr-41"} },
			wantKnown: "ptr-41",
		},
		{
			name:     "a main watch served nothing opens tail only",
			known:    func() *conversationv1.HistoryPointer { return nil },
			wantTail: true,
		},
		{
			name:     "a sender with no watcher opens tail only",
			wantTail: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			client := &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
				Result: &shimv1.StartTurnResponse_Success{Success: &shimv1.StartTurnSuccess{}},
			}}
			s := &sender{client: client, known: tt.known}

			// Act.
			if _, err := s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
				t.Fatalf("StartTurn: %v", err)
			}

			// Assert.
			if got := client.startTurnReq.GetTailOnly() != nil; got != tt.wantTail {
				t.Fatalf("tail_only = %v, want %v", got, tt.wantTail)
			}
			if got := client.startTurnReq.GetKnownThrough().GetValue(); got != tt.wantKnown {
				t.Fatalf("known_through = %q, want %q", got, tt.wantKnown)
			}
		})
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

// TestSenderStartTurnLeavesADaemonDoubleSubmitTerminal covers a
// turn_already_open, which is always the daemon's own bug: it is carried up as
// its typed arm rather than retried.
func TestSenderStartTurnLeavesADaemonDoubleSubmitTerminal(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
		Result: &shimv1.StartTurnResponse_Failure{Failure: &shimv1.StartTurnFailure{
			Detail: "turn t-9 is already in flight",
			Kind: &shimv1.StartTurnFailure_TurnAlreadyOpen{
				TurnAlreadyOpen: &shimv1.StartTurnTurnAlreadyOpen{},
			},
		}},
	}}}

	// Act
	_, err := s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok {
		t.Fatalf("StartTurn = %v, want a typed shim refusal", err)
	}
	if refusal.Arm != "turn_already_open" {
		t.Fatalf("refusal arm = %q, want turn_already_open", refusal.Arm)
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
	err := s.KillTurn(context.Background(), "turn-1", false, nil)

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

// senderPointer is the main-watch pointer the live watcher states.
var senderPointer = &conversationv1.HistoryPointer{Value: "ptr-main-12"}

// TestFleetSenderStatesTheMainWatchsNewestPointer covers the pointer a turn is
// bounded by: it is read from the LIVE watcher at the call, not when the
// sender was handed out.
func TestFleetSenderStatesTheMainWatchsNewestPointer(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	got, ok := f.fleet.Sender(ws.ID)
	if !ok {
		t.Fatal("Sender: no sender for a live workspace")
	}
	f.watcher.pointers = sessionwatcher.Pointers{Main: senderPointer}

	// Act.
	known := got.(*sender).knownThrough()

	// Assert.
	if known.GetValue() != senderPointer.GetValue() {
		t.Fatalf("known_through = %q, want the main watch's newest %q", known.GetValue(), senderPointer.GetValue())
	}
}

// killTurnKilled is a shim's plain success answer to a KillTurn.
func killTurnKilled() *shimv1.KillTurnResponse {
	return &shimv1.KillTurnResponse{Result: &shimv1.KillTurnResponse_Success{Success: &shimv1.KillTurnSuccess{}}}
}

// TestKillTurnBuildsTheRequestFromItsArguments covers the one request builder:
// the turn, the force and the commanded_by travel exactly as the caller stated
// them, an unstated command included.
func TestKillTurnBuildsTheRequestFromItsArguments(t *testing.T) {
	interjection := &conversationv1.AgentInterruptedByUser{
		Command: &conversationv1.AgentInterruptedByUser_Interjection{
			Interjection: &conversationv1.AgentInterruptedByUserInterjection{},
		},
	}
	tests := []struct {
		name        string
		force       bool
		commandedBy *conversationv1.AgentInterruptedByUser
	}{
		{name: "a forced kill with no command stated", force: true},
		{name: "a direct stop", commandedBy: directCommand()},
		{name: "an interjection", commandedBy: interjection},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			client := &fakeSenderClient{killTurn: killTurnKilled()}
			want := &shimv1.KillTurnRequest{
				Turn:        &conversationv1.TurnId{Value: "turn-1"},
				Force:       tt.force,
				CommandedBy: tt.commandedBy,
			}

			// Act
			err := killTurn(context.Background(), client, "turn-1", tt.force, tt.commandedBy)

			// Assert
			if err != nil {
				t.Fatalf("killTurn = %v, want nil", err)
			}
			if !proto.Equal(client.killTurnReq, want) {
				t.Fatalf("request = %v, want %v", client.killTurnReq, want)
			}
		})
	}
}

// TestKillTurnRelaysTheRefusalArm covers the one reading of the answer: a
// refusal comes back as the typed arm, not as a success.
func TestKillTurnRelaysTheRefusalArm(t *testing.T) {
	// Arrange
	client := &fakeSenderClient{killTurn: &shimv1.KillTurnResponse{
		Result: &shimv1.KillTurnResponse_Failure{Failure: &shimv1.KillTurnFailure{
			Cause: &shimv1.KillTurnFailure_NoTurnOpen{NoTurnOpen: &shimv1.KillTurnNoTurnOpen{}},
		}},
	}}

	// Act
	err := killTurn(context.Background(), client, "turn-1", false, nil)

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok || refusal.Arm != ArmShimNoTurnOpen {
		t.Fatalf("killTurn = %v, want the no_turn_open refusal", err)
	}
}

// TestKillTurnReturnsTheTransportError covers a call that never reached an
// answer: the transport's error is returned as it is, never as a refusal.
func TestKillTurnReturnsTheTransportError(t *testing.T) {
	// Arrange
	transport := errors.New("the socket closed")
	client := &fakeSenderClient{killTurnErr: transport}

	// Act
	err := killTurn(context.Background(), client, "turn-1", false, nil)

	// Assert
	if !errors.Is(err, transport) {
		t.Fatalf("killTurn = %v, want %v", err, transport)
	}
}

// TestBothKillTurnCallersSendTheSharedRequest pins that the queue's sender and
// the verbs' shim adapter go through the one builder: the same arguments put
// the same request on the wire from either.
func TestBothKillTurnCallersSendTheSharedRequest(t *testing.T) {
	tests := []struct {
		name string
		kill func(client *fakeClient) error
	}{
		{
			name: "the queue's sender",
			kill: func(client *fakeClient) error {
				return (&sender{client: client}).KillTurn(context.Background(), "turn-1", true, directCommand())
			},
		},
		{
			name: "the verbs' shim adapter",
			kill: func(client *fakeClient) error {
				return (&shimAdapter{client: client}).KillTurn(context.Background(), "turn-1", true, directCommand())
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			client := &fakeClient{}
			want := &shimv1.KillTurnRequest{Turn: &conversationv1.TurnId{Value: "turn-1"}, Force: true, CommandedBy: directCommand()}

			// Act
			err := tt.kill(client)

			// Assert
			if err != nil {
				t.Fatalf("KillTurn = %v, want nil", err)
			}
			if len(client.killTurns) != 1 || !proto.Equal(client.killTurns[0], want) {
				t.Fatalf("requests = %v, want exactly %v", client.killTurns, want)
			}
		})
	}
}

// TestSenderJoinsOrStartsTheTurnAsItsVerbSays covers the one flag that tells
// the two verbs apart on the wire: JoinRunningTurn asks the shim to join the
// running turn, and StartTurn never does.
func TestSenderJoinsOrStartsTheTurnAsItsVerbSays(t *testing.T) {
	tests := []struct {
		name string
		call func(*sender) (*shimv1.StartTurnSuccess, error)
		want bool
	}{
		{name: "StartTurn waits for no running turn", call: func(s *sender) (*shimv1.StartTurnSuccess, error) {
			return s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
		}, want: false},
		{name: "JoinRunningTurn joins it", call: func(s *sender) (*shimv1.StartTurnSuccess, error) {
			return s.JoinRunningTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
		}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			client := &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
				Result: &shimv1.StartTurnResponse_Success{Success: &shimv1.StartTurnSuccess{}},
			}}
			s := &sender{client: client}

			// Act
			if _, err := tt.call(s); err != nil {
				t.Fatalf("call: %v", err)
			}

			// Assert
			if got := client.startTurnReq.GetJoinRunningTurn(); got != tt.want {
				t.Fatalf("join_running_turn = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestSenderJoinRunningTurnCarriesTheRefusalArm covers a join the shim
// refused: the caller learns which refusal, as it does for StartTurn.
func TestSenderJoinRunningTurnCarriesTheRefusalArm(t *testing.T) {
	// Arrange
	s := &sender{client: &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
		Result: &shimv1.StartTurnResponse_Failure{Failure: &shimv1.StartTurnFailure{
			Detail: "turn t-2 already waits to join turn t-1",
			Kind: &shimv1.StartTurnFailure_TurnAlreadyOpen{
				TurnAlreadyOpen: &shimv1.StartTurnTurnAlreadyOpen{},
			},
		}},
	}}}

	// Act
	_, err := s.JoinRunningTurn(context.Background(), "turn-3", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)

	// Assert
	refusal, ok := AsShimRefusal(err)
	if !ok || refusal.Arm != "turn_already_open" {
		t.Fatalf("JoinRunningTurn = %v, want a typed turn_already_open refusal", err)
	}
}

// TestSenderStartsAnInterjectionWithItsVendorNote covers the note a prompt
// that interrupted the running turn carries to the shim, and that no other
// start carries one.
func TestSenderStartsAnInterjectionWithItsVendorNote(t *testing.T) {
	tests := []struct {
		name     string
		call     func(*sender) (*shimv1.StartTurnSuccess, error)
		wantNote *string
	}{
		{name: "StartTurn carries no note", call: func(s *sender) (*shimv1.StartTurnSuccess, error) {
			return s.StartTurn(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT)
		}},
		{name: "StartInterjection carries its note", call: func(s *sender) (*shimv1.StartTurnSuccess, error) {
			return s.StartInterjection(context.Background(), "turn-1", nil, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, "why the work was cut")
		}, wantNote: proto.String("why the work was cut")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			client := &fakeSenderClient{startTurn: &shimv1.StartTurnResponse{
				Result: &shimv1.StartTurnResponse_Success{Success: &shimv1.StartTurnSuccess{}},
			}}
			s := &sender{client: client}

			// Act
			if _, err := tt.call(s); err != nil {
				t.Fatalf("call: %v", err)
			}

			// Assert
			got := client.startTurnReq.VendorNote
			if (got == nil) != (tt.wantNote == nil) || (got != nil && *got != *tt.wantNote) {
				t.Fatalf("vendor_note = %v, want %v", got, tt.wantNote)
			}
		})
	}
}
