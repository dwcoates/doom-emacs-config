package tokenutilization

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func TestSyntheticModelReconciliationExcluded(t *testing.T) {
	tests := []struct {
		name   string
		model  string
		totals *frontendv1.TokenUsageTotals
		want   bool
	}{
		{
			name:   "zero usage synthetic entry is excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{},
			want:   true,
		},
		{
			name:   "synthetic entry with no totals at all is excluded",
			model:  SyntheticModelIdentity,
			totals: nil,
			want:   true,
		},
		{
			name:   "synthetic entry claiming input tokens is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{InputTokens: 1},
			want:   false,
		},
		{
			name:   "synthetic entry claiming output tokens is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{OutputTokens: 1},
			want:   false,
		},
		{
			name:   "synthetic entry claiming cache reads is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{CacheReadInputTokens: 1},
			want:   false,
		},
		{
			name:   "synthetic entry claiming cache creation is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{CacheCreationInputTokens: 1},
			want:   false,
		},
		{
			name:   "synthetic entry claiming ephemeral 5m cache creation is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{CacheCreation: &frontendv1.TokenCacheCreation{Ephemeral_5MInputTokens: 1}},
			want:   false,
		},
		{
			name:   "synthetic entry claiming ephemeral 1h cache creation is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{CacheCreation: &frontendv1.TokenCacheCreation{Ephemeral_1HInputTokens: 1}},
			want:   false,
		},
		{
			name:   "synthetic entry claiming web search requests is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{ServerToolUse: &frontendv1.TokenServerToolUse{WebSearchRequests: 1}},
			want:   false,
		},
		{
			name:   "synthetic entry claiming web fetch requests is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{ServerToolUse: &frontendv1.TokenServerToolUse{WebFetchRequests: 1}},
			want:   false,
		},
		{
			name:   "synthetic entry claiming thinking tokens is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{OutputDetails: &frontendv1.TokenOutputDetails{ThinkingTokens: 1}},
			want:   false,
		},
		{
			name:   "synthetic entry claiming negative usage is not excluded",
			model:  SyntheticModelIdentity,
			totals: &frontendv1.TokenUsageTotals{OutputTokens: -1},
			want:   false,
		},
		{
			name:   "a zero usage real model is not excluded",
			model:  "claude-opus-4",
			totals: &frontendv1.TokenUsageTotals{},
			want:   false,
		},
		{
			name:   "a blank model identity is not excluded",
			model:  "",
			totals: &frontendv1.TokenUsageTotals{},
			want:   false,
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange is the table row.

			// Act.
			got := SyntheticModelReconciliationExcluded(test.model, test.totals)

			// Assert.
			if got != test.want {
				t.Fatalf("SyntheticModelReconciliationExcluded(%q, %+v) = %t, want %t", test.model, test.totals, got, test.want)
			}
		})
	}
}
