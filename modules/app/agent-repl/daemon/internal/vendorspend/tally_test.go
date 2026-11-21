package vendorspend

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestTally(t *testing.T) {
	cold := &conversationv1.VendorTokens{CacheWrite_5M: 2000, UncachedInput: 3, Output: 5}
	warm := &conversationv1.VendorTokens{CacheRead: 2000, UncachedInput: 3, Output: 5}
	tests := []struct {
		name  string
		calls []Call
		want  []string
	}{
		{name: "a run's first cold call is its first call", calls: []Call{{Model: "m", Tokens: cold}}, want: []string{"cold_rewrite|first_call|1"}},
		{name: "a later cold call states no reason", calls: []Call{{Model: "m", Tokens: cold}, {Model: "m", Tokens: cold}}, want: []string{"cold_rewrite|first_call|1", "cold_rewrite|unstated|1"}},
		{name: "warm calls of one kind are one entry", calls: []Call{{Model: "m", Tokens: warm}, {Model: "m", Tokens: warm}}, want: []string{"warm||2"}},
		{name: "calls of two models are two entries", calls: []Call{{Model: "m", Tokens: warm}, {Model: "n", Tokens: warm}}, want: []string{"warm||1", "warm||1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := Tally(tt.calls, conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_HEADLESS, conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_PROMPT_SUMMARY)
			// Assert
			if len(got) != len(tt.want) {
				t.Fatalf("Tally = %d entries, want %d", len(got), len(tt.want))
			}
			for i, e := range got {
				if err := Validate(e); err != nil {
					t.Fatalf("entry %d does not validate: %v", i, err)
				}
				if s := OutcomeLabel(e.GetCacheOutcome()) + "|" + ReasonLabel(e.GetColdReason()) + "|" + string(rune('0'+e.GetCalls())); s != tt.want[i] {
					t.Errorf("entry %d = %s, want %s", i, s, tt.want[i])
				}
			}
		})
	}
}

func TestTallySumsTokensAndFilesTheBucket(t *testing.T) {
	// Arrange
	calls := []Call{
		{Model: "m", Tokens: &conversationv1.VendorTokens{CacheRead: 30_000, CacheWrite_1H: 10, Output: 5}},
		{Model: "m", Tokens: &conversationv1.VendorTokens{CacheRead: 30_000, CacheWrite_1H: 20, Output: 7}},
	}
	// Act
	got := Tally(calls, conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_HEADLESS, conversationv1.VendorCallCause_VENDOR_CALL_CAUSE_HEADLESS_QUESTION)
	// Assert
	if len(got) != 1 || got[0].GetTokens().GetCacheRead() != 60_000 || got[0].GetTokens().GetCacheWrite_1H() != 30 || got[0].GetTokens().GetOutput() != 12 || got[0].GetContextLeTokens() != 50_000 {
		t.Fatalf("Tally = %v, want one entry summing both calls in the 50k bucket", got)
	}
}
