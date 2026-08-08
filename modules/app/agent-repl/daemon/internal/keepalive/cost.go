package keepalive

// cost.go — THE ONE ARITHMETIC that says what a turn re-ingested, and the one
// comparison that says whether a ping proves the cache was gone.
//
// Both live here because two different subsystems decide different things from
// the same figure: the progress footer raises its expensive-turn alert from it,
// and the session controller hibernates a session from it. A measure written
// out twice would let those two disagree about whether one turn was cold, which
// is the disagreement the alert and the sleep exist to be two views of.

// UncachedInputTokens is what a turn fed the model NEW: raw input plus the
// prefix this request wrote to cache.
//
// THE THREE INPUT BUCKETS ARE DISJOINT AND THIS IS THE SUM OF TWO OF THEM. The
// Anthropic SDK's Usage documentation states the contract outright — "Total
// input tokens in a request is the summation of input_tokens,
// cache_creation_input_tokens, and cache_read_input_tokens" (the SDK's
// resources/messages Usage type, mirrored by the agent SDK's apiUsage shape) —
// so the three never overlap and nothing is double counted here:
//
//   - cache_read_input_tokens is the ONLY cheap bucket: the standing prefix
//     presented again, which is precisely the cost the keep-alive exists to
//     keep paying instead of a re-ingest. It is EXCLUDED.
//   - cache_creation_input_tokens was processed fresh at full price, with the
//     cache-write premium on top. It is economically uncached at processing
//     time and is COUNTED.
//   - input_tokens was processed fresh and never written to cache at all. It
//     carries no cache label because it never entered the cache, not because it
//     was served from one. It is COUNTED.
//
// READING EITHER COUNTED BUCKET ALONE MISREPORTS THE CASE THIS EXISTS TO CATCH.
// The CLI marks nearly all input cacheable, so a COLD prompt — a full context
// re-ingest, the most expensive thing that can happen — surfaces as cache
// CREATION while raw input_tokens stays near zero; and a turn whose prefix is
// deliberately uncacheable surfaces the other way round. Only the sum is the
// figure that was actually paid for at the uncached rate.
func UncachedInputTokens(inputTokens, cacheCreationInputTokens int64) int64 {
	return inputTokens + cacheCreationInputTokens
}

// ColdCompactionUncachedTokens is the uncached input above which a
// DAEMON-INITIATED compaction is reported as a cost defect.
//
// A COMPACTION IS SUPPOSED TO BE THE CHEAPEST TURN OF THE SESSION. It reads the
// whole conversation, which is the standing cached prefix and nothing else, so
// a warm one pays cache-read rates for essentially all of it and the figure
// this threshold judges stays near zero. Ten thousand is therefore an
// order-of-magnitude alarm rather than a tuned budget: nothing a warm
// compaction legitimately does approaches it, and the live incident that
// motivated the alarm read 1.5 MILLION.
//
// It is deliberately far below the ContextCostAlert's own threshold. That one
// judges USER turns, where re-ingesting context is expensive but ordinary; this
// one judges turns the DAEMON submitted for the express purpose of not paying
// that cost, where any material uncached read means the feature bought nothing.
const ColdCompactionUncachedTokens int64 = 10_000

// ColdCompaction reports whether a daemon-initiated compaction's uncached input
// proves it read the conversation at full price rather than from the cache.
//
// uncachedInputTokens is UncachedInputTokens' answer and must be: judging
// input_tokens alone would read ~0 for the exact cold compaction this exists to
// catch, since a cold full-context read surfaces as cache creation.
func ColdCompaction(uncachedInputTokens int64) bool {
	return uncachedInputTokens > ColdCompactionUncachedTokens
}

// CameBackCold reports whether a keep-alive ping's uncached input proves the
// cache it was sent to refresh had already expired.
//
// THE PING IS A DOZEN TOKENS OF PROMPT. There is no honest reading under which
// it re-ingests a conversation's worth of context except that the prefix it was
// sent to refresh was no longer in the cache. That makes this the only DIRECT
// evidence the feature ever produces about its own premise — every other input
// to the policy is a time-since comparison, which is a prediction — so a true
// answer here overrules the prediction rather than merely annotating it.
func (c Config) CameBackCold(uncachedInputTokens int64) bool {
	return uncachedInputTokens > c.UncachedCostAlertTokens
}
