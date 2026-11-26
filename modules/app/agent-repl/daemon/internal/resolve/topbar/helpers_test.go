package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// responseSettled is a settled response unit carrying U.
func responseSettled(unit string, u *conversationv1.TokenUsage) *conversationv1.AgentActivity {
	return &conversationv1.AgentActivity{
		ActivityId: &conversationv1.AgentActivityId{Value: unit},
		Usage:      u,
		Item: &conversationv1.AgentActivity_Response{
			Response: &conversationv1.AgentResponse{
				Result: &conversationv1.AgentResponse_Success{
					Success: &conversationv1.AgentResponseSuccess{},
				},
			},
		},
	}
}

// usage builds one canonical token record.
func usage(read, written, unwritten, output, thinking uint64) *conversationv1.TokenUsage {
	return &conversationv1.TokenUsage{
		InputHits:            &conversationv1.TokenCacheHits{Read: read},
		InputMisses:          &conversationv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
		OutputTokens:         output,
		OutputThinkingTokens: thinking,
	}
}

// otherWS is a second workspace, for the conditions that stand on every strip.
const otherWS = ids.WorkspaceID("ws-2")

// readyOther binds and readies otherWS as the harness readies testWS.
func readyOther(t *testing.T, h *harness) {
	t.Helper()
	if err := h.r.SetWorkspaceDir(otherWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.SetNaming(otherWS, Naming{Name: "other", ConfigDir: "/Users/dev/.claude"})
	h.r.SetAccount(otherWS, testAccount("dev@example.com"))
}
