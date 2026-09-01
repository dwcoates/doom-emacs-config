package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/promptqueue"
	"claude-repld/internal/workspace"
)

// releaseRequest is the release action on one held prompt.
func releaseRequest() *agentreplv1.UpdateHeldPromptRequest {
	return &agentreplv1.UpdateHeldPromptRequest{
		Workspace: ref(),
		Turn:      &conversationv1.TurnId{Value: "turn-1"},
		Action: &agentreplv1.UpdateHeldPromptRequest_Release{
			Release: &agentreplv1.UpdateHeldPromptRelease{},
		},
	}
}

// acceptRequest is the accept action on one held prompt.
func acceptRequest() *agentreplv1.UpdateHeldPromptRequest {
	return &agentreplv1.UpdateHeldPromptRequest{
		Workspace: ref(),
		Turn:      &conversationv1.TurnId{Value: "turn-1"},
		Action: &agentreplv1.UpdateHeldPromptRequest_Accept{
			Accept: &agentreplv1.UpdateHeldPromptAccept{},
		},
	}
}

// TestUpdateHeldPromptReleaseReachesTheQueue pins that release delivers through
// the queue's own verb rather than through a second delivery path.
func TestUpdateHeldPromptReleaseReachesTheQueue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.UpdateHeldPrompt(context.Background(), connect.NewRequest(releaseRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt: %v", err)
	}
	if resp.Msg.GetSuccess() == nil || len(h.Queue.released) != 1 {
		t.Fatalf("released = %v, result = %v", h.Queue.released, resp.Msg.GetResult())
	}
}

// TestUpdateHeldPromptMapsNoSuchHold pins the queue's unknown-hold refusal.
func TestUpdateHeldPromptMapsNoSuchHold(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Queue.releaseErr = promptqueue.ErrNoSuchHold

	// Act.
	resp, err := h.Client.UpdateHeldPrompt(context.Background(), connect.NewRequest(releaseRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt: %v", err)
	}
	if resp.Msg.GetError().GetNoSuchHold() == nil {
		t.Fatalf("result = %v, want no_such_hold", resp.Msg.GetResult())
	}
}

// TestUpdateHeldPromptMapsAcceptNotApplicable pins that `accept` is legal ONLY
// on a hold_for_turn_end verdict.
func TestUpdateHeldPromptMapsAcceptNotApplicable(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Queue.acceptErr = promptqueue.ErrAcceptNotApplicable

	// Act.
	resp, err := h.Client.UpdateHeldPrompt(context.Background(), connect.NewRequest(acceptRequest()))

	// Assert.
	if err != nil {
		t.Fatalf("UpdateHeldPrompt: %v", err)
	}
	if resp.Msg.GetError().GetAcceptNotApplicable() == nil {
		t.Fatalf("result = %v, want accept_not_applicable", resp.Msg.GetResult())
	}
}

// TestAnswerHeldOfferAnswersTheDequeue pins that the tray's one offer reaches
// the merge orchestrator.
func TestAnswerHeldOfferAnswersTheDequeue(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.AnswerHeldOffer(context.Background(),
		connect.NewRequest(&agentreplv1.AnswerHeldOfferRequest{
			Workspace: ref(),
			Answer: &agentreplv1.AnswerHeldOfferRequest_MergeDequeue{
				MergeDequeue: &agentreplv1.AnswerHeldOfferMergeDequeue{
					Decision: &agentreplv1.AnswerHeldOfferMergeDequeue_Keep{
						Keep: &agentreplv1.AnswerHeldOfferKeep{},
					},
				},
			},
		}))

	// Assert.
	if err != nil {
		t.Fatalf("AnswerHeldOffer: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
}

// TestSetPermissionModeMapsTheModeNotServedArm pins that the daemon accepts
// only the modes its own picker served.
func TestSetPermissionModeMapsTheModeNotServedArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.setPermissionModeErr = &workspace.Refusal{
		Arm: workspace.ArmModeNotServed, Reason: "the picker never served it",
	}

	// Act.
	resp, err := h.Client.SetPermissionMode(context.Background(),
		connect.NewRequest(&agentreplv1.SetPermissionModeRequest{
			Workspace: ref(), Mode: "bypass",
		}))

	// Assert.
	if err != nil {
		t.Fatalf("SetPermissionMode: %v", err)
	}
	if resp.Msg.GetError().GetModeNotServed() == nil {
		t.Fatalf("result = %v, want mode_not_served", resp.Msg.GetResult())
	}
}
