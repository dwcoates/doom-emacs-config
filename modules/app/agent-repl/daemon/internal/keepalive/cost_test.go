package keepalive

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// usage builds one canonical measurement from the two vendor buckets a cost
// verdict is taken from. The cache hit is named explicitly so a test can prove
// it is excluded rather than merely omitted.
func usage(freshInput, cacheWrite, cacheRead uint64) *frontendv1.TokenUsage {
	return &frontendv1.TokenUsage{
		InputHits:   &frontendv1.TokenCacheHits{Read: cacheRead},
		InputMisses: &frontendv1.TokenCacheMisses{Written: cacheWrite, Unwritten: freshInput},
	}
}

// ONLY THE SUM CROSSES. Two misses each comfortably under the threshold still
// name a ping that paid for the whole conversation, and either one read alone
// would call that cold ping warm — which is the exact silent-alarm shape the
// canonical input_misses grouping exists to close.
func TestOnlyTheExpensiveSumCrossesTheColdThreshold(t *testing.T) {
	// Arrange — neither miss reaches the 20k default on its own.
	cfg := DefaultConfig()
	const fresh, written uint64 = 12_000, 12_000

	// Act.
	both := usage(fresh, written, 0)

	// Assert.
	if cfg.CameBackCold(usage(fresh, 0, 0)) || cfg.CameBackCold(usage(0, written, 0)) {
		t.Fatalf("a single miss bucket (%d / %d) already crosses the %d threshold, so this case cannot prove the sum is what crosses it",
			fresh, written, cfg.UncachedCostAlertTokens)
	}
	if !cfg.CameBackCold(both) {
		t.Fatalf("CameBackCold(fresh=%d written=%d) = false against a %d threshold; reading either miss alone hides a ping that re-ingested the whole conversation",
			fresh, written, cfg.UncachedCostAlertTokens)
	}
}

// THE THRESHOLD IS EXCLUSIVE, matching the expensive-turn alert that reads the
// same figure: a turn exactly at the threshold has not crossed it.
func TestCameBackColdComparesAgainstTheConfiguredThreshold(t *testing.T) {
	tests := []struct {
		name  string
		usage *frontendv1.TokenUsage
		want  bool
	}{
		{name: "over the threshold is proof the cache was gone", usage: usage(20001, 0, 0), want: true},
		{name: "exactly at the threshold has not crossed it", usage: usage(20000, 0, 0), want: false},
		{name: "under the threshold is an ordinary warm ping", usage: usage(12, 0, 0), want: false},
		{name: "a ping that paid nothing", usage: usage(0, 0, 0), want: false},
	}
	cfg := DefaultConfig()
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := cfg.CameBackCold(tc.usage)

			// Assert.
			if got != tc.want {
				t.Fatalf("CameBackCold(%v) against a %d threshold = %v, want %v",
					tc.usage, cfg.UncachedCostAlertTokens, got, tc.want)
			}
		})
	}
}

// A CACHE HIT NEVER TRIPS THE COLD-PING ALARM, however large. The standing
// prefix presented again is exactly what the keep-alive is paying to preserve.
func TestCameBackColdIgnoresCacheHitsHoweverLarge(t *testing.T) {
	// Arrange: a ping that read 1.5 million tokens, every one of them served
	// from cache, and paid a dozen uncached.
	cfg := DefaultConfig()

	// Act.
	got := cfg.CameBackCold(usage(12, 0, 1_500_000))

	// Assert.
	if got {
		t.Fatal("a ping that read 1500000 tokens from cache and paid 12 uncached was called cold; the cached read is the warm case")
	}
}

// THE TWO MISS BUCKETS ARE DISJOINT AND THE TRIPWIRE READS THEIR SUM. Each can
// be individually below the threshold while what was actually paid for at
// uncached rates is above it. Reading either alone would miss exactly that case.
func TestColdCompactionReadsTheSumOfBothMisses(t *testing.T) {
	tests := []struct {
		name       string
		freshInput uint64
		cacheWrite uint64
		want       bool
	}{
		{
			name:       "neither miss crosses alone but their sum does",
			freshInput: 6_000,
			cacheWrite: 6_000,
			want:       true,
		},
		{
			name:       "the same two misses summing below the threshold do not trip",
			freshInput: 6_000,
			cacheWrite: 3_000,
			want:       false,
		},
		{
			name:       "a cold full-context read surfaces as cache writes and crosses",
			freshInput: 0,
			cacheWrite: 1_500_000,
			want:       true,
		},
		{
			name:       "fresh input alone crosses",
			freshInput: 10_001,
			cacheWrite: 0,
			want:       true,
		},
		{
			name:       "exactly at the threshold has not crossed it",
			freshInput: 4_000,
			cacheWrite: 6_000,
			want:       false,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := ColdCompaction(usage(tc.freshInput, tc.cacheWrite, 0))

			// Assert.
			if got != tc.want {
				t.Fatalf("ColdCompaction(fresh=%d written=%d) = %v, want %v (threshold %d)",
					tc.freshInput, tc.cacheWrite, got, tc.want, ColdCompactionUncachedTokens)
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
	const cacheRead uint64 = 1_500_000
	const freshInput, cacheWrite uint64 = 200, 0

	// Act: the cache hit rides the same canonical message the verdict is taken
	// from, so this proves the read is EXCLUDED rather than merely unsupplied.
	got := ColdCompaction(usage(freshInput, cacheWrite, cacheRead))

	// Assert.
	if got {
		t.Fatalf("a compaction that read %d tokens from cache and paid %d uncached tripped the alarm; the cached read is the success case",
			cacheRead, freshInput+cacheWrite)
	}
}
