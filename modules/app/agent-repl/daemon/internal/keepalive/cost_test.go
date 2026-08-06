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
