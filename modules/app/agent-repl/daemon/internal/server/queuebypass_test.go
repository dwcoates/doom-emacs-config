package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/workspace"
)

// bypassOn is an on request naming CONTEXT (nil: none).
func bypassOn(named *frontendv1.TopbarQueueBypassContext) *agentreplv1.UpdateQueueBypassModeRequest {
	return &agentreplv1.UpdateQueueBypassModeRequest{Workspace: ref(), Action: &agentreplv1.UpdateQueueBypassModeRequest_On{
		On: &agentreplv1.UpdateQueueBypassModeOn{Context: named}}}
}

func TestUpdateQueueBypassModeHandsTheVerbTheNamedContext(t *testing.T) {
	tests := []struct {
		name string
		req  *agentreplv1.UpdateQueueBypassModeRequest
		want workspace.QueueBypassContext
	}{
		{name: "an on naming no context keeps the default", req: bypassOn(nil), want: workspace.QueueBypassContextDefault},
		{name: "full main context", req: bypassOn(&frontendv1.TopbarQueueBypassContext{
			Context: &frontendv1.TopbarQueueBypassContext_FullMain{FullMain: &frontendv1.TopbarQueueBypassContextFullMain{}}}), want: workspace.QueueBypassContextFullMain},
		{name: "compacted main context", req: bypassOn(&frontendv1.TopbarQueueBypassContext{
			Context: &frontendv1.TopbarQueueBypassContext_CompactedMain{CompactedMain: &frontendv1.TopbarQueueBypassContextCompactedMain{}}}), want: workspace.QueueBypassContextCompactedMain},
		{name: "existing context", req: bypassOn(&frontendv1.TopbarQueueBypassContext{
			Context: &frontendv1.TopbarQueueBypassContext_Existing{Existing: &frontendv1.TopbarQueueBypassContextExisting{}}}), want: workspace.QueueBypassContextExisting},
		{name: "existing plus compacted main context", req: bypassOn(&frontendv1.TopbarQueueBypassContext{
			Context: &frontendv1.TopbarQueueBypassContext_ExistingPlusCompactedMain{ExistingPlusCompactedMain: &frontendv1.TopbarQueueBypassContextExistingPlusCompactedMain{}}}), want: workspace.QueueBypassContextExistingPlusCompactedMain},
		{name: "a toggle keeps the default", req: &agentreplv1.UpdateQueueBypassModeRequest{Workspace: ref(),
			Action: &agentreplv1.UpdateQueueBypassModeRequest_Toggle{Toggle: &agentreplv1.UpdateQueueBypassModeToggle{}}}, want: workspace.QueueBypassContextDefault},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.bypassOn = true

			// Act
			resp, err := h.Client.UpdateQueueBypassMode(context.Background(), connect.NewRequest(tt.req))

			// Assert
			if err != nil || !resp.Msg.GetSuccess().GetOn() {
				t.Fatalf("UpdateQueueBypassMode = %v, %v; want success on", resp, err)
			}
			if len(h.Verbs.bypassContexts) != 1 || h.Verbs.bypassContexts[0] != tt.want {
				t.Fatalf("contexts = %v, want exactly %s", h.Verbs.bypassContexts, tt.want)
			}
		})
	}
}

func TestUpdateQueueBypassModeRefusesAContextNamingNoArm(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.Client.UpdateQueueBypassMode(context.Background(), connect.NewRequest(bypassOn(&frontendv1.TopbarQueueBypassContext{})))

	// Assert
	if connect.CodeOf(err) != connect.CodeInvalidArgument {
		t.Fatalf("err = %v, want InvalidArgument", err)
	}
	if len(h.Verbs.bypassChanges) != 0 {
		t.Fatalf("changes = %v, want the verb never asked", h.Verbs.bypassChanges)
	}
}

func TestUpdateQueueBypassModeAnswersTheContextRefusals(t *testing.T) {
	tests := []struct {
		name   string
		err    error
		assert func(t *testing.T, e *agentreplv1.UpdateQueueBypassModeError)
	}{
		{
			name: "no bypass conversation",
			err:  &workspace.Refusal{Arm: workspace.ArmNoBypassConversation, Reason: "nothing parked"},
			assert: func(t *testing.T, e *agentreplv1.UpdateQueueBypassModeError) {
				if e.GetNoBypassConversation() == nil {
					t.Fatalf("error = %v, want no_bypass_conversation", e)
				}
			},
		},
		{
			name: "compaction failed",
			err:  &workspace.Refusal{Arm: workspace.ArmCompactionFailed, Reason: "failed", Fields: map[string]any{"detail": "the summarizer failed"}},
			assert: func(t *testing.T, e *agentreplv1.UpdateQueueBypassModeError) {
				if got := e.GetCompactionFailed().GetDetail(); got != "the summarizer failed" {
					t.Fatalf("compaction_failed detail = %q, want the shim's account", got)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.Verbs.bypassErr = tt.err

			// Act
			resp, err := h.Client.UpdateQueueBypassMode(context.Background(), connect.NewRequest(bypassOn(nil)))

			// Assert
			if err != nil {
				t.Fatalf("UpdateQueueBypassMode: %v", err)
			}
			tt.assert(t, resp.Msg.GetError())
		})
	}
}
