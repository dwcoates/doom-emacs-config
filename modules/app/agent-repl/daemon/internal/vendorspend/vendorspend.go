// Package vendorspend is the daemon's half of the vendor spend vocabulary
// (conversation.v1 vendor_spend.proto, proto/vocab/vendor-spend.json): the
// context buckets, the one cache-outcome rule, the label every tag is
// published under, and the check every spend entry passes before it is
// folded, whoever produced it (a shim's measurements frame, Emacs's report,
// the daemon's own headless runs).
package vendorspend

import (
	"errors"
	"fmt"
	"slices"
	"strconv"
	"strings"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// ContextBuckets are the context buckets' inclusive upper bounds, in tokens,
// ascending. proto/vocab/vendor-spend.json holds the same list, and the
// vocabulary test compares the two.
var ContextBuckets = []uint64{25_000, 50_000, 100_000, 200_000, 300_000, 400_000, 600_000, 800_000, 1_000_000}

// AboveEveryBound is the context label of a call above every bucket.
const AboveEveryBound = "+Inf"

// BucketOf answers the bucket a context of TOKENS falls in, and whether it
// fell in one at all.
func BucketOf(tokens uint64) (uint64, bool) {
	for _, bound := range ContextBuckets {
		if tokens <= bound {
			return bound, true
		}
	}
	return 0, false
}

// ContextLabel is the `context` label of an entry's bucket: its bound, or
// AboveEveryBound for an entry above every bound.
func ContextLabel(e *conversationv1.VendorSpend) string {
	if e.ContextLeTokens == nil {
		return AboveEveryBound
	}
	return strconv.FormatUint(e.GetContextLeTokens(), 10)
}

// THE DAEMON'S ONE HOME OF THE VENDOR'S CACHE PRICES, in tenths of the base
// input price: a read at a tenth of it, a write at the one-hour tier at twice
// it. The outcome rule compares them, and the keep-alive run cap derives its
// break-even from them (cachelapse.KeepaliveRunCap). The shim's coverage rule
// names the same two (shim.engine.cold.CACHE_READ_PRICE, CACHE_WRITE_PRICE),
// and the skill's weights state them (skills/agent-repl-metrics
// spend-weights.json); the vocabulary test pins the weights file to these.
const (
	CacheReadPriceTenths  = 1
	CacheWritePriceTenths = 20
)

const (
	readPrice  = float64(CacheReadPriceTenths) / 10
	writePrice = float64(CacheWritePriceTenths) / 10
)

// OutcomeOf is THE ONE CACHE-OUTCOME RULE of conversation.v1.VendorCacheOutcome
// for a call that read READ from the cache, wrote WRITE to it (both tiers) and
// sent UNCACHED fresh, cased in proto/vocab/vendor-spend.json.
func OutcomeOf(read, write, uncached uint64) conversationv1.VendorCacheOutcome {
	context := float64(read + write + uncached)
	fresh := write + uncached
	switch {
	case float64(fresh)*writePrice <= context*readPrice:
		return conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM
	case write == 0:
		return conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_UNCACHED_NO_WRITE
	case fresh > read:
		return conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_COLD_REWRITE
	default:
		return conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_PARTIAL
	}
}

// The token kinds, each a `kind` label value.
const (
	KindUncachedInput = "uncached_input"
	KindCacheRead     = "cache_read"
	KindCacheWrite5m  = "cache_write_5m"
	KindCacheWrite1h  = "cache_write_1h"
	KindOutput        = "output"
)

// Kinds are every token kind, in the order the series are published.
var Kinds = []string{KindUncachedInput, KindCacheRead, KindCacheWrite5m, KindCacheWrite1h, KindOutput}

// TokensOf answers T's count of KIND. An unknown kind is a caller bug.
func TokensOf(t *conversationv1.VendorTokens, kind string) uint64 {
	switch kind {
	case KindUncachedInput:
		return t.GetUncachedInput()
	case KindCacheRead:
		return t.GetCacheRead()
	case KindCacheWrite5m:
		return t.GetCacheWrite_5M()
	case KindCacheWrite1h:
		return t.GetCacheWrite_1H()
	case KindOutput:
		return t.GetOutput()
	}
	panic(fmt.Sprintf("vendorspend: %q is not a token kind", kind))
}

// InputOf is what T's calls sent: their context, summed.
func InputOf(t *conversationv1.VendorTokens) uint64 {
	return t.GetUncachedInput() + t.GetCacheRead() + t.GetCacheWrite_5M() + t.GetCacheWrite_1H()
}

// label spells an enum value as a label: its name without the enum's prefix,
// lower-cased (VENDOR_CALL_CAUSE_USER_PROMPT is `user_prompt`).
func label(name, prefix string) string {
	return strings.ToLower(strings.TrimPrefix(name, prefix))
}

// CauseLabel is a cause's `cause` label.
func CauseLabel(c conversationv1.VendorCallCause) string {
	return label(c.String(), "VENDOR_CALL_CAUSE_")
}

// AgentLabel is an agent's `agent` label.
func AgentLabel(a conversationv1.VendorCallAgent) string {
	return label(a.String(), "VENDOR_CALL_AGENT_")
}

// OutcomeLabel is a cache outcome's `cache_outcome` label.
func OutcomeLabel(o conversationv1.VendorCacheOutcome) string {
	return label(o.String(), "VENDOR_CACHE_OUTCOME_")
}

// ReasonLabel is a cold reason's `cold_reason` label: empty for a WARM call's.
func ReasonLabel(r conversationv1.VendorColdReason) string {
	if r == conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSPECIFIED {
		return ""
	}
	return label(r.String(), "VENDOR_COLD_REASON_")
}

// ErrInvalid is a spend entry its producer should never have sent.
var ErrInvalid = errors.New("invalid vendor spend")

// Validate checks one spend entry against the contract: a model, a declared
// agent, cause and outcome, a cold reason exactly when the outcome is not
// WARM, a subagent type exactly for a subagent, a bucket the vocabulary
// names, and at least one call. Every error wraps ErrInvalid.
func Validate(e *conversationv1.VendorSpend) error {
	switch {
	case e.GetModel() == "":
		return fmt.Errorf("%w: an entry names no model", ErrInvalid)
	case !declared(int32(e.GetAgent()), conversationv1.VendorCallAgent_name):
		return fmt.Errorf("%w: agent %v is not a declared agent", ErrInvalid, e.GetAgent())
	case !declared(int32(e.GetCause()), conversationv1.VendorCallCause_name):
		return fmt.Errorf("%w: cause %v is not a declared cause", ErrInvalid, e.GetCause())
	case !declared(int32(e.GetCacheOutcome()), conversationv1.VendorCacheOutcome_name):
		return fmt.Errorf("%w: cache outcome %v is not a declared outcome", ErrInvalid, e.GetCacheOutcome())
	case int32(e.GetColdReason()) != 0 && !declared(int32(e.GetColdReason()), conversationv1.VendorColdReason_name):
		return fmt.Errorf("%w: cold reason %v is not a declared reason", ErrInvalid, e.GetColdReason())
	}
	warm := e.GetCacheOutcome() == conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM
	if warm != (e.GetColdReason() == conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSPECIFIED) {
		return fmt.Errorf("%w: a %s entry has cold reason %v", ErrInvalid, OutcomeLabel(e.GetCacheOutcome()), e.GetColdReason())
	}
	subagent := e.GetAgent() == conversationv1.VendorCallAgent_VENDOR_CALL_AGENT_SUBAGENT
	if subagent != (e.GetSubagentType() != "") {
		return fmt.Errorf("%w: a %s entry has subagent type %q", ErrInvalid, AgentLabel(e.GetAgent()), e.GetSubagentType())
	}
	if e.ContextLeTokens != nil && !slices.Contains(ContextBuckets, e.GetContextLeTokens()) {
		return fmt.Errorf("%w: context bound %d is not a bucket of the vocabulary", ErrInvalid, e.GetContextLeTokens())
	}
	if e.GetCalls() == 0 {
		return fmt.Errorf("%w: an entry counts no call", ErrInvalid)
	}
	return nil
}

// declared reports whether V is a declared, non-UNSPECIFIED value of an enum
// whose names are NAMES.
func declared(v int32, names map[int32]string) bool {
	_, ok := names[v]
	return ok && v != 0
}

// Account labels: which account root a call billed to.
const (
	// AccountDefault is the default account root (the personal account here).
	AccountDefault = "default"
	// AccountMultiRepo is the multi-repo root's account (the work account).
	AccountMultiRepo = "multi_repo"
	// AccountUnstated is a producer that named no account root.
	AccountUnstated = "unstated"
)
