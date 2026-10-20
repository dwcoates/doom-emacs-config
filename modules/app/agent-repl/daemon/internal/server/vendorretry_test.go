package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/workspace"
)

// retryVendorErrorRequest names the fixture workspace.
func retryVendorErrorRequest() *agentreplv1.RetryVendorErrorRequest {
	return &agentreplv1.RetryVendorErrorRequest{Workspace: ref()}
}

func TestRetryVendorErrorAnswersEachShimOutcomeByItsArm(t *testing.T) {
	tests := []struct {
		name     string
		answered *shimv1.RetryVendorErrorNowResponse
		want     func(*agentreplv1.RetryVendorErrorSuccess) bool
	}{
		{
			name:     "retried",
			answered: &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Retried{Retried: &shimv1.RetryVendorErrorNowRetried{}}},
			want:     func(s *agentreplv1.RetryVendorErrorSuccess) bool { return s.GetRetried() != nil },
		},
		{
			name:     "nothing scheduled",
			answered: &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_NothingScheduled{NothingScheduled: &shimv1.RetryVendorErrorNowNothingScheduled{}}},
			want:     func(s *agentreplv1.RetryVendorErrorSuccess) bool { return s.GetNothingScheduled() != nil },
		},
		{
			name:     "busy carries its detail",
			answered: &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Busy{Busy: &shimv1.RetryVendorErrorNowBusy{Detail: "a turn is in flight"}}},
			want:     func(s *agentreplv1.RetryVendorErrorSuccess) bool { return s.GetBusy().GetDetail() == "a turn is in flight" },
		},
		{
			name:     "ended carries its detail",
			answered: &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Ended{Ended: &shimv1.RetryVendorErrorNowEnded{Detail: "the session is standing down"}}},
			want:     func(s *agentreplv1.RetryVendorErrorSuccess) bool { return s.GetEnded().GetDetail() == "the session is standing down" },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.retryResponse = tt.answered

			// Act.
			resp, err := h.Client.RetryVendorError(context.Background(), connect.NewRequest(retryVendorErrorRequest()))

			// Assert.
			if err != nil {
				t.Fatalf("RetryVendorError: %v", err)
			}
			if !tt.want(resp.Msg.GetSuccess()) {
				t.Fatalf("result = %v, want the arm", resp.Msg.GetResult())
			}
		})
	}
}

func TestRetryVendorErrorAsksTheVerbOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.retryResponse = &shimv1.RetryVendorErrorNowResponse{Outcome: &shimv1.RetryVendorErrorNowResponse_Retried{Retried: &shimv1.RetryVendorErrorNowRetried{}}}

	// Act.
	if _, err := h.Client.RetryVendorError(context.Background(), connect.NewRequest(retryVendorErrorRequest())); err != nil {
		t.Fatalf("RetryVendorError: %v", err)
	}

	// Assert.
	if h.Verbs.retries != 1 {
		t.Fatalf("verb asks = %d, want 1", h.Verbs.retries)
	}
}

func TestRetryVendorErrorRefusesAMissingWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.Client.RetryVendorError(context.Background(), connect.NewRequest(&agentreplv1.RetryVendorErrorRequest{}))

	// Assert.
	if connect.CodeOf(err) != connect.CodeInvalidArgument || h.Verbs.retries != 0 {
		t.Fatalf("err = %v, asks = %d; want InvalidArgument and no ask", err, h.Verbs.retries)
	}
}

func TestRetryVendorErrorAnswersNoSessionByItsArm(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.retryErr = &workspace.ShimRefusal{Verb: "RetryVendorErrorNow", Arm: workspace.ArmShimNoSession, Detail: "no"}

	// Act.
	resp, err := h.Client.RetryVendorError(context.Background(), connect.NewRequest(retryVendorErrorRequest()))

	// Assert.
	if err != nil || resp.Msg.GetError().GetNoSession() == nil {
		t.Fatalf("RetryVendorError = (%v, %v), want the no_session arm", resp, err)
	}
}

func TestRetryVendorErrorFailsOnAShimAnswerNamingNoOutcome(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.Verbs.retryResponse = &shimv1.RetryVendorErrorNowResponse{}

	// Act.
	_, err := h.Client.RetryVendorError(context.Background(), connect.NewRequest(retryVendorErrorRequest()))

	// Assert.
	if err == nil {
		t.Fatal("RetryVendorError answered an outcome-less shim answer as a success")
	}
}
