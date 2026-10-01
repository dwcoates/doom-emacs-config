package promptqueue

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/wsm"
)

// TestExemptFromLeaseIsTheMergesOwnOrigins covers the one rule both the
// submission path and the restamp apply.
func TestExemptFromLeaseIsTheMergesOwnOrigins(t *testing.T) {
	tests := []struct {
		name   string
		holder wsm.LeaseHolder
		origin conversationv1.PromptOrigin
		want   bool
	}{
		{"a merge brief under the merge", wsm.HolderMerge, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, true},
		{"a displaced turn's resume under the merge", wsm.HolderMerge, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_DISPLACED_TURN_RESUME, true},
		{"the user under the merge", wsm.HolderMerge, conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT, false},
		{"a merge brief under a restart", wsm.HolderRestart, conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := exemptFromLease(wsm.Lease{Holder: tt.holder}, tt.origin)

			// Assert
			if got != tt.want {
				t.Fatalf("exemptFromLease = %v, want %v", got, tt.want)
			}
		})
	}
}
