package keepalive

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenusage"
)

// cost.go — THE TWO COMPARISONS taken from one turn's canonical usage: whether a
// daemon compaction read the conversation at full price, and whether a ping
// proves the cache it was sent to refresh was gone.
//
// BOTH TAKE THE CANONICAL SHAPE, NOT A NUMBER. The figure they judge is
// tokenusage.ExpensiveInput — the canonical TokenUsage.input_misses total — and
// accepting the message rather than an int64 is what makes it impossible for a
// caller to hand either of them a single vendor bucket. That mistake is the one
// these thresholds exist to catch: a cold re-ingest surfaces almost entirely as
// the WRITTEN miss, so judging the unwritten one alone reads near zero for the
// most expensive turn a session can have.
//
// The two live together because two subsystems decide different things from the
// same figure — the progress footer raises its expensive-turn alert, the session
// controller hibernates the session — and a measure written out twice would let
// them disagree about whether one turn was cold, which is the disagreement the
// alert and the sleep exist to be two views of.

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

// ColdCompaction reports whether a daemon-initiated compaction's expensive
// input proves it read the conversation at full price rather than from the
// cache.
//
// It judges tokenusage.ExpensiveInput — BOTH cache misses — because a cold
// full-context read surfaces as the written miss, so judging the unwritten one
// alone would read ~0 for the exact cold compaction this exists to catch.
func ColdCompaction(usage *frontendv1.TokenUsage) bool {
	return tokenusage.ExpensiveInput(usage) > ColdCompactionUncachedTokens
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
func (c Config) CameBackCold(usage *frontendv1.TokenUsage) bool {
	return tokenusage.ExpensiveInput(usage) > c.UncachedCostAlertTokens
}
