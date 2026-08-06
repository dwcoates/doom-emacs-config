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
// THE SECOND TERM IS THE WHOLE POINT. The CLI marks nearly all input cacheable,
// so a COLD prompt — a full context re-ingest, the most expensive thing that can
// happen — surfaces as cache CREATION while raw input_tokens stays near zero.
// Reading input_tokens alone would report almost nothing for exactly the case
// this figure exists to catch.
//
// CACHE READS ARE EXCLUDED, and their exclusion is the other half of the point:
// a cache read is the same standing prefix presented again, which is precisely
// the cost the keep-alive is trying to keep paying instead of a re-ingest.
func UncachedInputTokens(inputTokens, cacheCreationInputTokens int64) int64 {
	return inputTokens + cacheCreationInputTokens
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
