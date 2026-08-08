package keepalive

import "testing"

// THE SECOND TERM IS THE WHOLE POINT. A cold prompt surfaces as cache CREATION
// with raw input_tokens near zero, so a measure that read input_tokens alone
// would report almost nothing for exactly the case it exists to catch.
func TestUncachedInputTokensCountsCacheCreation(t *testing.T) {
	tests := []struct {
		name          string
		input         int64
		cacheCreation int64
		want          int64
	}{
		{name: "the observed cold ping", input: 2, cacheCreation: 22862, want: 22864},
		{name: "a warm ping pays neither", input: 0, cacheCreation: 0, want: 0},
		{name: "raw input alone still counts", input: 500, cacheCreation: 0, want: 500},
		{name: "cache creation alone still counts", input: 0, cacheCreation: 500, want: 500},
		{name: "both buckets nonzero add rather than shadow each other", input: 12000, cacheCreation: 12000, want: 24000},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := UncachedInputTokens(tc.input, tc.cacheCreation)

			// Assert.
			if got != tc.want {
				t.Fatalf("UncachedInputTokens(%d, %d) = %d, want %d", tc.input, tc.cacheCreation, got, tc.want)
			}
		})
	}
}

// ONLY THE SUM CROSSES. Two buckets each comfortably under the threshold still
// name a ping that paid for the whole conversation, and either one read alone
// would call that cold ping warm — which is the exact silent-alarm shape the
// sum exists to close (AGENTS.md, "Uncached input tokens are a SUM, and the
// field names lie about it").
func TestOnlyTheUncachedSumCrossesTheColdThreshold(t *testing.T) {
	// Arrange — neither bucket reaches the 20k default on its own.
	cfg := DefaultConfig()
	const input, cacheCreation int64 = 12_000, 12_000

	// Act.
	uncached := UncachedInputTokens(input, cacheCreation)

	// Assert.
	if cfg.CameBackCold(input) || cfg.CameBackCold(cacheCreation) {
		t.Fatalf("a single bucket (%d / %d) already crosses the %d threshold, so this case cannot prove the sum is what crosses it",
			input, cacheCreation, cfg.UncachedCostAlertTokens)
	}
	if !cfg.CameBackCold(uncached) {
		t.Fatalf("CameBackCold(UncachedInputTokens(%d, %d) = %d) = false against a %d threshold; reading either bucket alone hides a ping that re-ingested the whole conversation",
			input, cacheCreation, uncached, cfg.UncachedCostAlertTokens)
	}
}

// THE THRESHOLD IS EXCLUSIVE, matching the expensive-turn alert that reads the
// same figure: a turn exactly at the threshold has not crossed it.
func TestCameBackColdComparesAgainstTheConfiguredThreshold(t *testing.T) {
	tests := []struct {
		name     string
		uncached int64
		want     bool
	}{
		{name: "over the threshold is proof the cache was gone", uncached: 20001, want: true},
		{name: "exactly at the threshold has not crossed it", uncached: 20000, want: false},
		{name: "under the threshold is an ordinary warm ping", uncached: 12, want: false},
		{name: "a ping that paid nothing", uncached: 0, want: false},
	}
	cfg := DefaultConfig()
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := cfg.CameBackCold(tc.uncached)

			// Assert.
			if got != tc.want {
				t.Fatalf("CameBackCold(%d) against a %d threshold = %v, want %v",
					tc.uncached, cfg.UncachedCostAlertTokens, got, tc.want)
			}
		})
	}
}
