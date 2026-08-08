// Package tokenusage is the daemon's ONE boundary between the vendor's token
// counters and the canonical, economics-explicit shape every decision in this
// system is taken from.
//
// WHY A PACKAGE AND NOT AN ADDITION AT EACH CALL SITE. The vendor reports three
// disjoint input counters whose names describe where the tokens went, not what
// they were charged, so "what did this request pay for" is a sum that a reader
// has to KNOW to perform — and a system that performs it in five places is a
// system where five subsystems can come to disagree about whether one turn was
// cold. The keep-alive's cold-ping hibernation, the daemon compaction's
// cold-read tripwire, the progress footer's live ticker, and its expensive-turn
// alert are four views of the SAME figure. They read it from here.
//
// THE SHIM DOES NOT DO THIS. It translates vendor usage onto the wire faithfully
// and derives nothing; the webapp renders what this package resolved. Token
// judgment lives in the daemon, and this file is where it starts.
package tokenusage

import (
	"fmt"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// FromResultUsage converts one terminal result's vendor usage block.
func FromResultUsage(u *datav1.Usage) (*frontendv1.TokenUsage, error) {
	return fromCounters("result usage", u.GetInputTokens(), u.GetCacheCreationInputTokens(), u.GetCacheReadInputTokens(), u.GetOutputTokens())
}

// FromAPIUsage converts one assistant response's vendor usage block.
func FromAPIUsage(u *datav1.ApiUsage) (*frontendv1.TokenUsage, error) {
	return fromCounters("api usage", u.GetInputTokens(), u.GetCacheCreationInputTokens(), u.GetCacheReadInputTokens(), u.GetOutputTokens())
}

// FromVendorUsage converts the durable vendor-faithful record the state store
// holds. This is the READ boundary named in the canonical TokenUsage contract:
// persistence keeps the vendor shape, and the economics are produced here from
// it rather than stored beside it.
func FromVendorUsage(u *frontendv1.VendorTokenUsage) (*frontendv1.TokenUsage, error) {
	return fromCounters("vendor token usage", u.GetInputTokens(), u.GetCacheCreationInputTokens(), u.GetCacheReadInputTokens(), u.GetOutputTokens())
}

// FromTotals converts one cumulative vendor total.
func FromTotals(t *frontendv1.TokenUsageTotals) (*frontendv1.TokenUsage, error) {
	return fromCounters("token usage totals", t.GetInputTokens(), t.GetCacheCreationInputTokens(), t.GetCacheReadInputTokens(), t.GetOutputTokens())
}

// fromCounters is the ONE mapping from the vendor's vocabulary to the canonical
// one, and the only place the correspondence is written down:
//
//   - cache_read_input_tokens      -> input_hits.read       (the cheap bucket)
//   - cache_creation_input_tokens  -> input_misses.written  (fresh, 1.25x)
//   - input_tokens                 -> input_misses.unwritten (fresh, 1x)
//
// A NEGATIVE COUNTER IS REJECTED RATHER THAN CONVERTED. The canonical shape is
// unsigned because a count of tokens cannot be negative, and converting a
// negative int64 into it would silently produce a number near 2^64 — an
// astronomically expensive turn reported to the tripwire, the hibernation
// policy, and the footer alike. The vendor never sends one; if it ever does,
// the caller is told rather than handed a fabricated figure.
func fromCounters(source string, input, cacheCreation, cacheRead, output int64) (*frontendv1.TokenUsage, error) {
	for _, counter := range []struct {
		name  string
		value int64
	}{
		{name: "input_tokens", value: input},
		{name: "cache_creation_input_tokens", value: cacheCreation},
		{name: "cache_read_input_tokens", value: cacheRead},
		{name: "output_tokens", value: output},
	} {
		if counter.value < 0 {
			return nil, fmt.Errorf("tokenusage: %s reports negative %s=%d", source, counter.name, counter.value)
		}
	}
	return &frontendv1.TokenUsage{
		InputHits:    &frontendv1.TokenCacheHits{Read: uint64(cacheRead)},
		InputMisses:  &frontendv1.TokenCacheMisses{Written: uint64(cacheCreation), Unwritten: uint64(input)},
		OutputTokens: uint64(output),
	}, nil
}

// ExpensiveInput is what a request fed the model NEW: everything the prompt
// cache did not serve.
//
// IT IS BOTH MISSES, AND READING EITHER ALONE MISREPORTS THE CASE EVERY CALLER
// EXISTS TO CATCH. The CLI marks nearly all input cacheable, so a COLD prompt —
// a full context re-ingest, the most expensive thing that can happen — surfaces
// almost entirely as `written` while `unwritten` stays near zero; a turn whose
// prefix is deliberately uncacheable surfaces the other way round. Only the sum
// is the figure that was actually paid at uncached rates.
//
// The cache read is EXCLUDED on purpose: it is the standing prefix presented
// again, which is precisely the cost the keep-alive exists to keep paying
// instead of a re-ingest.
func ExpensiveInput(u *frontendv1.TokenUsage) int64 {
	return int64(u.GetInputMisses().GetWritten() + u.GetInputMisses().GetUnwritten())
}

// ContextInput is the TOTAL input the request presented: everything the cache
// served plus everything it did not.
//
// It is the size of the standing conversation as the model saw it, which is
// what the warm-compaction floor judges "is this conversation even big enough
// to be worth compacting" against. Cache reads are INCLUDED here and excluded
// from ExpensiveInput, and the difference is deliberate: a cached token is cheap
// NOW but is still a token a cold revival would re-ingest at full price later.
func ContextInput(u *frontendv1.TokenUsage) int64 {
	return int64(u.GetInputHits().GetRead()) + ExpensiveInput(u)
}

// Breakdown renders every bucket and both derived figures for a log line or a
// failure card's source detail.
//
// IT NAMES EVERY BUCKET, including the ones a verdict did not use. A reader
// asked to believe that a compaction read the conversation cold needs to see
// that the cache read was near zero while the written miss was enormous; a bare
// "uncached_input_tokens=1500000" is the conclusion without the evidence.
func Breakdown(u *frontendv1.TokenUsage) string {
	return fmt.Sprintf("input_tokens=%d cache_creation_input_tokens=%d cache_read_input_tokens=%d uncached_input_tokens=%d context_input_tokens=%d",
		u.GetInputMisses().GetUnwritten(), u.GetInputMisses().GetWritten(), u.GetInputHits().GetRead(),
		ExpensiveInput(u), ContextInput(u))
}

// Rates are the prompt-cache quotients, DERIVED here rather than stored beside
// the counters they come from.
//
// THE THREE PARTITION THE PROMPT INPUT and sum to 1: each names one disjoint
// bucket. FreshInputRate is the `unwritten` bucket's SHARE ALONE and is NOT the
// expensive share, which is FreshInputRate + CacheWriteRate — the same
// relationship ExpensiveInput states in tokens. Nothing may fold the write rate
// into the fresh rate: that would double-count against CacheWriteRate and break
// the partition every reader takes these to be.
type Rates struct {
	TotalPromptInputTokens int64
	CacheHitRate           float64
	CacheWriteRate         float64
	FreshInputRate         float64
}

// DeriveRates returns the partition, and whether there was any prompt input to
// partition at all. A zero total has no rates rather than three zeroes or three
// NaNs, and the caller is told which it got.
func DeriveRates(u *frontendv1.TokenUsage) (Rates, bool) {
	total := ContextInput(u)
	if total == 0 {
		return Rates{}, false
	}
	return Rates{
		TotalPromptInputTokens: total,
		CacheHitRate:           float64(u.GetInputHits().GetRead()) / float64(total),
		CacheWriteRate:         float64(u.GetInputMisses().GetWritten()) / float64(total),
		FreshInputRate:         float64(u.GetInputMisses().GetUnwritten()) / float64(total),
	}, true
}
