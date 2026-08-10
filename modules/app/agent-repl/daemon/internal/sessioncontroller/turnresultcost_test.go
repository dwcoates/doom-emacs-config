package sessioncontroller

import (
	"testing"
)

// THE RESULT MEASURES WHAT THE TURN PAID, AND ONLY THAT. Both cache misses sum
// into the expensive figure and the cache read is excluded, because the read is
// the standing prefix presented again — the cost the keep-alive exists to keep
// paying instead of a re-ingest.
//
// THE CACHE READ IS NOT ADDED BACK ANYWHERE ON THIS SHAPE. A result's usage sums
// every model call the turn made, so a sum including the read would count the
// standing prefix once per round trip rather than measuring the conversation.
// The conversation's size comes from one response instead (contextsize.go).
func TestTurnResultCostMeasuresWhatTheTurnPaidAtUncachedRates(t *testing.T) {
	tests := []struct {
		name          string
		input         int64
		cacheCreation int64
		cacheRead     int64
		wantUncached  int64
	}{
		{
			name:         "a warm compaction pays almost nothing for a huge read",
			input:        4,
			cacheRead:    1_500_000,
			wantUncached: 4,
		},
		{
			name:          "a cold compaction pays for the whole context it read",
			input:         2,
			cacheCreation: 1_500_000,
			wantUncached:  1_500_002,
		},
		{
			name:          "both uncached buckets sum, and the read is excluded",
			input:         6_000,
			cacheCreation: 6_000,
			cacheRead:     30_000,
			wantUncached:  12_000,
		},
		{
			name:         "a result that reported nothing reduces to zeroes",
			wantUncached: 0,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := mustTurnResultCost(t, "turn-1", usageOf(tc.input, tc.cacheCreation, tc.cacheRead))

			// Assert.
			if got.expensiveInputTokens() != tc.wantUncached {
				t.Fatalf("expensive_input_tokens = %d, want %d", got.expensiveInputTokens(), tc.wantUncached)
			}
		})
	}
}

// A TERMINAL RESULT NO LONGER MOVES THE CONVERSATION'S SIZE. It measured the
// turn's work rather than the context's occupancy, and routing it into the floor
// is the defect contextsize.go replaced.
func TestNoteTurnResultCostLeavesTheConversationSizeAlone(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteMainAgentContextSize(d, mainAgentUtilization("msg_1", 0, 0, 120_000))

	// Act.
	m.noteTurnResultCost(d, mustTurnResultCost(t, "turn-1", usageOf(0, 0, 9_000_000)))

	// Assert.
	if got := contextSizeOf(m, d); got != 120_000 {
		t.Fatalf("conversation size after a terminal result = %d, want the response-measured %d", got, 120_000)
	}
}
