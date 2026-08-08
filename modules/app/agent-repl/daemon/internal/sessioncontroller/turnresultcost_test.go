package sessioncontroller

import (
	"testing"

	"claude-repld/internal/keepalive"
)

// THE TWO DERIVED FIGURES DIFFER BY THE CACHE READ, and the difference is the
// whole reason both exist. What was PAID for at the uncached rate excludes the
// read; how BIG the standing conversation is includes it, because a cached
// token is cheap now and is still a token a cold revival re-ingests at full
// price later.
func TestTurnResultCostSeparatesWhatWasPaidFromHowBigTheContextIs(t *testing.T) {
	tests := []struct {
		name          string
		input         int64
		cacheCreation int64
		cacheRead     int64
		wantUncached  int64
		wantContext   int64
	}{
		{
			name:         "a warm compaction pays almost nothing for a huge context",
			input:        4,
			cacheRead:    1_500_000,
			wantUncached: 4,
			wantContext:  1_500_004,
		},
		{
			name:          "a cold compaction pays for the whole context it read",
			input:         2,
			cacheCreation: 1_500_000,
			wantUncached:  1_500_002,
			wantContext:   1_500_002,
		},
		{
			name:          "both uncached buckets sum, and the read is added only to the context",
			input:         6_000,
			cacheCreation: 6_000,
			cacheRead:     30_000,
			wantUncached:  12_000,
			wantContext:   42_000,
		},
		{
			name:         "a result that reported nothing reduces to zeroes",
			wantUncached: 0,
			wantContext:  0,
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
			if got.contextInputTokens() != tc.wantContext {
				t.Fatalf("context_input_tokens = %d, want %d", got.contextInputTokens(), tc.wantContext)
			}
		})
	}
}

// THE SIZE IS REMEMBERED FROM THE RESULT, which is what the warm-compaction
// floor is judged against. Nothing else in this daemon holds a figure for how
// big a live conversation is.
func TestNoteContextSizeRemembersTheObservedConversationSize(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)

	// Act.
	m.noteContextSize(d, mustTurnResultCost(t, "turn-1", usageOf(10, 0, 120_000)))

	// Assert.
	m.mu.Lock()
	got := d.lastContextInputTokens
	m.mu.Unlock()
	if got != 120_010 {
		t.Fatalf("remembered conversation size = %d, want %d", got, 120_010)
	}
}

// A RESULT THAT REPORTED NO INPUT AT ALL DOES NOT ERASE THE LAST REAL
// MEASUREMENT. An absent reading is not a reading of zero, and writing it would
// turn a known-large session into an unknown one — which the eligibility check
// reads as "do not compact", silently switching the feature off.
func TestNoteContextSizeKeepsTheLastMeasurementWhenAResultReportsNoInput(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.noteContextSize(d, mustTurnResultCost(t, "turn-1", usageOf(0, 0, keepalive.WarmCompactMinContextTokens*2)))

	// Act.
	m.noteContextSize(d, mustTurnResultCost(t, "turn-2", usageOf(0, 0, 0)))

	// Assert.
	m.mu.Lock()
	got := d.lastContextInputTokens
	m.mu.Unlock()
	if got != keepalive.WarmCompactMinContextTokens*2 {
		t.Fatalf("remembered conversation size after an empty result = %d, want the previous %d",
			got, keepalive.WarmCompactMinContextTokens*2)
	}
}
