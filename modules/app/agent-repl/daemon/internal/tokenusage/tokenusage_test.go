package tokenusage

import (
	"math"
	"testing"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// The vendor-to-canonical mapping is the whole contract of the boundary: each
// vendor counter must land in the bucket whose ECONOMICS it describes, not the
// one whose name it resembles.
func TestFromResultUsageMapsEachVendorCounterToItsEconomicBucket(t *testing.T) {
	usage := &datav1.Usage{InputTokens: 7, CacheCreationInputTokens: 11, CacheReadInputTokens: 13, OutputTokens: 17}

	got, err := FromResultUsage(usage)

	if err != nil {
		t.Fatalf("FromResultUsage returned %v, want no error", err)
	}
	if got.GetInputHits().GetRead() != 13 {
		t.Errorf("input_hits.read = %d, want the cache_read_input_tokens counter 13", got.GetInputHits().GetRead())
	}
	if got.GetInputMisses().GetWritten() != 11 {
		t.Errorf("input_misses.written = %d, want the cache_creation_input_tokens counter 11", got.GetInputMisses().GetWritten())
	}
	if got.GetInputMisses().GetUnwritten() != 7 {
		t.Errorf("input_misses.unwritten = %d, want the input_tokens counter 7", got.GetInputMisses().GetUnwritten())
	}
	if got.GetOutputTokens() != 17 {
		t.Errorf("output_tokens = %d, want 17", got.GetOutputTokens())
	}
}

func TestFromAPIUsageMapsEachVendorCounterToItsEconomicBucket(t *testing.T) {
	usage := &datav1.ApiUsage{InputTokens: 7, CacheCreationInputTokens: 11, CacheReadInputTokens: 13, OutputTokens: 17}

	got, err := FromAPIUsage(usage)

	if err != nil {
		t.Fatalf("FromAPIUsage returned %v, want no error", err)
	}
	if got.GetInputHits().GetRead() != 13 || got.GetInputMisses().GetWritten() != 11 || got.GetInputMisses().GetUnwritten() != 7 || got.GetOutputTokens() != 17 {
		t.Fatalf("canonical usage = %v, want read=13 written=11 unwritten=7 output=17", got)
	}
}

// The durable record is the READ boundary: the state store keeps the vendor
// shape and the economics are produced from it here.
func TestFromVendorUsageConvertsTheDurableRecord(t *testing.T) {
	durable := &frontendv1.VendorTokenUsage{InputTokens: 2, CacheCreationInputTokens: 3, CacheReadInputTokens: 5, OutputTokens: 8}

	got, err := FromVendorUsage(durable)

	if err != nil {
		t.Fatalf("FromVendorUsage returned %v, want no error", err)
	}
	if got.GetInputHits().GetRead() != 5 || got.GetInputMisses().GetWritten() != 3 || got.GetInputMisses().GetUnwritten() != 2 || got.GetOutputTokens() != 8 {
		t.Fatalf("canonical usage = %v, want read=5 written=3 unwritten=2 output=8", got)
	}
}

func TestFromTotalsConvertsACumulativeVendorTotal(t *testing.T) {
	totals := &frontendv1.TokenUsageTotals{InputTokens: 2, CacheCreationInputTokens: 3, CacheReadInputTokens: 5, OutputTokens: 8}

	got, err := FromTotals(totals)

	if err != nil {
		t.Fatalf("FromTotals returned %v, want no error", err)
	}
	if got.GetInputHits().GetRead() != 5 || got.GetInputMisses().GetWritten() != 3 || got.GetInputMisses().GetUnwritten() != 2 || got.GetOutputTokens() != 8 {
		t.Fatalf("canonical usage = %v, want read=5 written=3 unwritten=2 output=8", got)
	}
}

// A negative counter must be surfaced, never converted: the unsigned canonical
// field would report a number near 2^64 to the tripwire and the footer alike.
func TestFromResultUsageRejectsEachNegativeCounter(t *testing.T) {
	for _, tc := range []struct {
		name  string
		usage *datav1.Usage
	}{
		{name: "input_tokens", usage: &datav1.Usage{InputTokens: -1}},
		{name: "cache_creation_input_tokens", usage: &datav1.Usage{CacheCreationInputTokens: -1}},
		{name: "cache_read_input_tokens", usage: &datav1.Usage{CacheReadInputTokens: -1}},
		{name: "output_tokens", usage: &datav1.Usage{OutputTokens: -1}},
	} {
		t.Run(tc.name, func(t *testing.T) {
			got, err := FromResultUsage(tc.usage)

			if err == nil {
				t.Fatalf("FromResultUsage(%s=-1) = %v, want an error", tc.name, got)
			}
			if got != nil {
				t.Fatalf("FromResultUsage(%s=-1) returned usage %v alongside its error", tc.name, got)
			}
		})
	}
}

// An absent usage block is a request that reported nothing, which every caller
// already distinguishes from "no request", so it reduces to zeroes.
func TestFromResultUsageReducesAnAbsentUsageToZeroes(t *testing.T) {
	got, err := FromResultUsage(nil)

	if err != nil {
		t.Fatalf("FromResultUsage(nil) returned %v, want no error", err)
	}
	if ExpensiveInput(got) != 0 || ContextInput(got) != 0 || got.GetOutputTokens() != 0 {
		t.Fatalf("canonical usage = %v, want every figure zero", got)
	}
}

// The expensive figure is BOTH misses. A cold re-ingest surfaces as `written`
// with `unwritten` near zero, so either bucket alone under-reports the case the
// measure exists to catch.
func TestExpensiveInputSumsBothMissesAndExcludesTheHit(t *testing.T) {
	for _, tc := range []struct {
		name  string
		usage *frontendv1.TokenUsage
		want  int64
	}{
		{
			name:  "cold re-ingest surfaces as cache writes",
			usage: canonical(0, 1_500_000, 12),
			want:  1_500_000,
		},
		{
			// The live cold ping that motivated the measure: two fresh tokens
			// beside twenty-two thousand cache writes.
			name:  "the observed cold ping",
			usage: canonical(2, 22_862, 0),
			want:  22_864,
		},
		{
			name:  "fresh input alone still counts",
			usage: canonical(500, 0, 0),
			want:  500,
		},
		{
			name:  "a cache write alone still counts",
			usage: canonical(0, 500, 0),
			want:  500,
		},
		{
			name:  "two equal misses add rather than shadow each other",
			usage: canonical(12_000, 12_000, 0),
			want:  24_000,
		},
		{
			name:  "uncacheable prefix surfaces as fresh input",
			usage: canonical(1_500_000, 0, 12),
			want:  1_500_000,
		},
		{
			name:  "a warm turn pays for neither miss",
			usage: canonical(0, 0, 500_000),
			want:  0,
		},
		{
			name:  "both misses are counted together",
			usage: canonical(700, 300, 900_000),
			want:  1_000,
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := ExpensiveInput(tc.usage); got != tc.want {
				t.Fatalf("ExpensiveInput = %d, want %d", got, tc.want)
			}
		})
	}
}

// The context figure INCLUDES the cache read: a cached token is cheap now but is
// still a token a cold revival would re-ingest at full price later.
func TestContextInputIncludesTheCacheRead(t *testing.T) {
	if got := ContextInput(canonical(700, 300, 900_000)); got != 901_000 {
		t.Fatalf("ContextInput = %d, want 901000", got)
	}
}

func TestBreakdownNamesEveryBucketAndBothDerivedFigures(t *testing.T) {
	got := Breakdown(canonical(700, 300, 900_000))

	want := "input_tokens=700 cache_creation_input_tokens=300 cache_read_input_tokens=900000 uncached_input_tokens=1000 context_input_tokens=901000"
	if got != want {
		t.Fatalf("Breakdown = %q, want %q", got, want)
	}
}

// The three rates partition the prompt input and sum to 1, one per disjoint
// bucket, so the fresh rate is a SHARE and never the expensive share.
func TestDeriveRatesPartitionsTheBucketsAndLeavesTheExpensiveShareASum(t *testing.T) {
	got, ok := DeriveRates(canonical(10, 30, 60))

	if !ok {
		t.Fatal("DeriveRates reported no prompt input for a hundred-token prompt")
	}
	if got.TotalPromptInputTokens != 100 {
		t.Errorf("total = %d, want 100", got.TotalPromptInputTokens)
	}
	if got.CacheHitRate != 0.6 || got.CacheWriteRate != 0.3 || got.FreshInputRate != 0.1 {
		t.Fatalf("rates = %+v, want hit=0.6 write=0.3 fresh=0.1", got)
	}
	if sum := got.CacheHitRate + got.CacheWriteRate + got.FreshInputRate; math.Abs(sum-1) > 1e-12 {
		t.Errorf("the three rates sum to %v, want a partition of 1", sum)
	}
	if want := float64(ExpensiveInput(canonical(10, 30, 60))) / 100; got.FreshInputRate+got.CacheWriteRate != want {
		t.Errorf("expensive share = %v, want %v — the sum of the two miss rates", got.FreshInputRate+got.CacheWriteRate, want)
	}
}

// A prompt with no input has no rates rather than three zeroes or three NaNs,
// and the caller is told which answer it got.
func TestDeriveRatesReportsNoPartitionForAnEmptyPrompt(t *testing.T) {
	got, ok := DeriveRates(canonical(0, 0, 0))

	if ok {
		t.Fatalf("DeriveRates reported rates %+v for an empty prompt", got)
	}
	if got != (Rates{}) {
		t.Fatalf("DeriveRates returned %+v alongside its false, want the zero value", got)
	}
}

func canonical(unwritten, written, read uint64) *frontendv1.TokenUsage {
	return &frontendv1.TokenUsage{
		InputHits:   &frontendv1.TokenCacheHits{Read: read},
		InputMisses: &frontendv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
	}
}
