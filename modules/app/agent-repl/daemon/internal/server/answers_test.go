package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/workspace"
)

// TestInterruptAnswersNothingRunningAsASuccess pins that "nothing was running"
// is an ANSWER, not a failure.
func TestInterruptAnswersNothingRunningAsASuccess(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{NothingRunning: true}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if resp.Msg.GetSuccess().GetNothingRunning() == nil {
		t.Fatalf("result = %v, want nothing_running", resp.Msg.GetResult())
	}
}

// TestInterruptAnswersTheDetachedCount pins that a fan-wide interrupt reports
// how many detached items it stopped.
func TestInterruptAnswersTheDetachedCount(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{DetachedCount: 3}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_AllAgents{AllAgents: &agentreplv1.InterruptAllAgents{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if got := resp.Msg.GetSuccess().GetInterruptedDetached().GetCount(); got != 3 {
		t.Fatalf("count = %d, want 3", got)
	}
}

// TestInterruptRaisesTheConfirmChallenge pins the unconfirmed turn interrupt
// with live detached agents: the user is asked once, with the count.
func TestInterruptRaisesTheConfirmChallenge(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptErr = &workspace.ConfirmRequired{LiveAgentCount: 2}

	// Act.
	resp, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target:    &agentreplv1.InterruptRequest_Turn{Turn: &agentreplv1.InterruptTurn{}},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("Interrupt: %v", err)
	}
	if got := resp.Msg.GetError().GetConfirmRequired().GetLiveAgentCount(); got != 2 {
		t.Fatalf("live_agent_count = %d, want 2", got)
	}
}

// TestInterruptDecodesTheDetachedTarget pins that the clicked bubble's FeedId
// reaches the verbs as the decoded ref.
func TestInterruptDecodesTheDetachedTarget(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.interruptOutcome = workspace.InterruptOutcome{DetachedCount: 1}

	// Act.
	if _, err := h.Client.Interrupt(context.Background(),
		connect.NewRequest(&agentreplv1.InterruptRequest{
			Workspace: ref(),
			Target: &agentreplv1.InterruptRequest_Detached{
				Detached: feedIDFor(testWorkspaceID),
			},
		})); err != nil {
		t.Fatalf("Interrupt: %v", err)
	}

	// Assert.
	if h.Verbs.interruptTarget.Detached == nil {
		t.Fatal("the detached target did not reach the verbs")
	}
}

// TestAnswerPermissionMapsTheNoStandingOfferArm pins that granting a standing
// the ask never offered is refused by its own arm.
func TestAnswerPermissionMapsTheNoStandingOfferArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerPermissionErr = &workspace.Refusal{
		Arm: workspace.ArmNoStandingOffer, Reason: "the ask offered none",
	}

	// Act.
	resp, err := h.Client.AnswerPermission(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace:  ref(),
			Permission: permissionIDFor(testWorkspaceID, "ask-1"),
			Answer: &agentreplv1.AnswerPermissionRequest_AllowStanding{
				AllowStanding: &agentreplv1.AnswerPermissionAllowStanding{},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}
	if resp.Msg.GetError().GetNoStandingOffer() == nil {
		t.Fatalf("result = %v, want no_standing_offer", resp.Msg.GetResult())
	}
}

// TestAnswerPermissionRenamesUnservedToAskNotStanding pins the per-rpc rename:
// the verbs' ArmUnservedAnswer is AnswerPermissionError.ask_not_standing.
func TestAnswerPermissionRenamesUnservedToAskNotStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerPermissionErr = &workspace.Refusal{
		Arm: workspace.ArmUnservedAnswer, Reason: "no ask standing", NotFound: true,
	}

	// Act.
	resp, err := h.Client.AnswerPermission(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerPermissionRequest{
			Workspace:  ref(),
			Permission: permissionIDFor(testWorkspaceID, "ask-1"),
			Answer: &agentreplv1.AnswerPermissionRequest_AllowOnce{
				AllowOnce: &agentreplv1.AnswerPermissionAllowOnce{},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerPermission: %v", err)
	}
	if resp.Msg.GetError().GetAskNotStanding() == nil {
		t.Fatalf("result = %v, want ask_not_standing", resp.Msg.GetResult())
	}
}

// TestAnswerQuestionRenamesUnservedToUnservedValue pins the OTHER half of the
// rename: an unserved VALUE (not an unknown ask) is unserved_value.
func TestAnswerQuestionRenamesUnservedToUnservedValue(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.answerQuestionErr = &workspace.Refusal{
		Arm: workspace.ArmUnservedAnswer, Reason: "that label was never offered",
	}

	// Act.
	resp, err := h.Client.AnswerQuestion(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerQuestionRequest{
			Workspace: ref(),
			Question:  questionIDFor(testWorkspaceID, "ask-2"),
			Answers: []*agentreplv1.AnswerQuestionAnswer{
				{QuestionText: "which?", Chosen: []string{"invented"}},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerQuestion: %v", err)
	}
	if got := resp.Msg.GetError().GetUnservedValue(); got == nil {
		t.Fatalf("result = %v, want unserved_value", resp.Msg.GetResult())
	}
}

// TestAnswerColdGateRequiresACompactScope pins that a compaction scope is never
// defaulted: the shim's remediation needs it and the trace carries only a model.
func TestAnswerColdGateRequiresACompactScope(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.AnswerColdGate(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerColdGateRequest{
			Workspace: ref(),
			Gate:      coldGateIDFor(testWorkspaceID),
			Choice: &agentreplv1.AnswerColdGateRequest_Compact{
				Compact: &agentreplv1.AnswerColdGateCompact{
					Model: &conversationv1.AgentModel{Name: "claude-opus-5"},
				},
			},
		}))

	// Assert.
	if code := connectCode(t, err); code != connect.CodeInvalidArgument {
		t.Fatalf("code = %v, want InvalidArgument", code)
	}
}
