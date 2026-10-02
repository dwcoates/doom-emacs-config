package server

import (
	"context"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/workspace"
)

// setEffortRequest picks LEVEL for the fixture workspace.
func setEffortRequest(level conversationv1.AgentEffortLevel) *agentreplv1.SetEffortRequest {
	return &agentreplv1.SetEffortRequest{Workspace: ref(), Effort: level}
}

func TestSetEffortRelaysTheEchoedLevelToTheVerb(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	resp, err := h.Client.SetEffort(context.Background(),
		connect.NewRequest(setEffortRequest(conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH)))

	// Assert.
	if err != nil {
		t.Fatalf("SetEffort: %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("result = %v, want success", resp.Msg.GetResult())
	}
	if h.Verbs.setEffort != conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH {
		t.Fatalf("verb saw %v, want the echoed high", h.Verbs.setEffort)
	}
}

func TestSetEffortRefusesAnInvalidLevel(t *testing.T) {
	tests := []struct {
		name  string
		level conversationv1.AgentEffortLevel
	}{
		{"unspecified", conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED},
		{"a number the contract does not define", conversationv1.AgentEffortLevel(99)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			_, err := h.Client.SetEffort(context.Background(), connect.NewRequest(setEffortRequest(tt.level)))

			// Assert.
			if connect.CodeOf(err) != connect.CodeInvalidArgument {
				t.Fatalf("err = %v, want InvalidArgument", err)
			}
		})
	}
}

func TestSetEffortAnswersEachRefusalByItsArm(t *testing.T) {
	tests := []struct {
		name string
		err  error
		want func(*agentreplv1.SetEffortError) bool
	}{
		{
			name: "a level the selector never served",
			err:  &workspace.Refusal{Arm: workspace.ArmEffortNotSupported, Reason: "not served"},
			want: func(e *agentreplv1.SetEffortError) bool { return e.GetNotSupported() != nil },
		},
		{
			name: "the shim's own not_supported",
			err:  &workspace.ShimRefusal{Verb: "SetSessionEffort", Arm: workspace.ArmEffortNotSupported, Detail: "no"},
			want: func(e *agentreplv1.SetEffortError) bool { return e.GetNotSupported() != nil },
		},
		{
			name: "no session",
			err:  &workspace.ShimRefusal{Verb: "SetSessionEffort", Arm: workspace.ArmShimNoSession, Detail: "no"},
			want: func(e *agentreplv1.SetEffortError) bool { return e.GetNoSession() != nil },
		},
		{
			name: "the vendor's refusal carries its detail",
			err:  &workspace.ShimRefusal{Verb: "SetSessionEffort", Arm: "vendor_refused", Detail: "the vendor said no"},
			want: func(e *agentreplv1.SetEffortError) bool {
				return e.GetVendorRefused().GetDetail() == "the vendor said no"
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			h.Verbs.setEffortErr = tt.err

			// Act.
			resp, err := h.Client.SetEffort(context.Background(),
				connect.NewRequest(setEffortRequest(conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH)))

			// Assert.
			if err != nil {
				t.Fatalf("SetEffort answered a transport error: %v", err)
			}
			if !tt.want(resp.Msg.GetError()) {
				t.Fatalf("result = %v, want the arm", resp.Msg.GetResult())
			}
		})
	}
}
