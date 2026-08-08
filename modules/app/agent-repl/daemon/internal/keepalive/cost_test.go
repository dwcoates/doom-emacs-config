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

// THE THREE INPUT BUCKETS ARE DISJOINT AND THE TRIPWIRE READS THE SUM OF TWO.
// The SDK's Usage contract is that total input = input_tokens +
// cache_creation_input_tokens + cache_read_input_tokens with no overlap, so
// each of the two counted buckets can be individually below the threshold while
// what was actually paid for at the uncached rate is above it. Reading either
// bucket alone would miss exactly that case.
func TestColdCompactionReadsTheSumOfBothUncachedBuckets(t *testing.T) {
	tests := []struct {
		name          string
		input         int64
		cacheCreation int64
		want          bool
	}{
		{
			name:          "neither bucket crosses alone but their sum does",
			input:         6_000,
			cacheCreation: 6_000,
			want:          true,
		},
		{
			name:          "the same two buckets summing below the threshold do not trip",
			input:         6_000,
			cacheCreation: 3_000,
			want:          false,
		},
		{
			name:          "cache creation alone crosses",
			input:         0,
			cacheCreation: 1_500_000,
			want:          true,
		},
		{
			name:          "raw input alone crosses",
			input:         10_001,
			cacheCreation: 0,
			want:          true,
		},
		{
			name:          "exactly at the threshold has not crossed it",
			input:         4_000,
			cacheCreation: 6_000,
			want:          false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := ColdCompaction(UncachedInputTokens(tc.input, tc.cacheCreation))

			// Assert.
			if got != tc.want {
				t.Fatalf("ColdCompaction(UncachedInputTokens(%d, %d)) = %v, want %v (threshold %d)",
					tc.input, tc.cacheCreation, got, tc.want, ColdCompactionUncachedTokens)
			}
		})
	}
}

// A CACHE READ NEVER TRIPS THE ALARM, however large. A warm compaction reads
// the entire conversation from the cache — that is precisely what it is
// scheduled before the cache's expiry to do — so counting the read would fire
// the alarm on every single success it exists to confirm.
func TestColdCompactionIgnoresCacheReadsHoweverLarge(t *testing.T) {
	// Arrange: a compaction that read 1.5 million tokens, every one of them
	// served from cache, and paid a few hundred uncached.
	const cacheRead int64 = 1_500_000
	const input, cacheCreation int64 = 200, 0

	// Act: the measure takes only the two uncached buckets, so the read is not
	// an argument to it at all — which is the guarantee under test.
	got := ColdCompaction(UncachedInputTokens(input, cacheCreation))

	// Assert.
	if got {
		t.Fatalf("a compaction that read %d tokens from cache and paid %d uncached tripped the alarm; the cached read is the success case",
			cacheRead, UncachedInputTokens(input, cacheCreation))
	}
}
