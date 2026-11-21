package vendorspend

import (
	"fmt"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// Call is one vendor call of a one-shot run: its model and its usage.
type Call struct {
	Model  string
	Tokens *conversationv1.VendorTokens
}

// Tally files one run's CALLS, in the order the run made them, as spend
// entries of AGENT under CAUSE, judged by the one cache-outcome rule
// (OutcomeOf) and bucketed (BucketOf): the run's first call is its agent's
// first, and a later one the cache did not serve is filed with no reason a
// one-shot run can name. It is THE ONE tally of a one-shot run, the daemon's
// own headless runs and Emacs's reported ones alike.
func Tally(calls []Call, agent conversationv1.VendorCallAgent, cause conversationv1.VendorCallCause) []*conversationv1.VendorSpend {
	entries := map[string]*conversationv1.VendorSpend{}
	var order []string
	for i, c := range calls {
		t := c.Tokens
		outcome := OutcomeOf(t.GetCacheRead(), t.GetCacheWrite_5M()+t.GetCacheWrite_1H(), t.GetUncachedInput())
		reason := conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSPECIFIED
		if outcome != conversationv1.VendorCacheOutcome_VENDOR_CACHE_OUTCOME_WARM {
			reason = conversationv1.VendorColdReason_VENDOR_COLD_REASON_UNSTATED
			if i == 0 {
				reason = conversationv1.VendorColdReason_VENDOR_COLD_REASON_FIRST_CALL
			}
		}
		bound, inBound := BucketOf(InputOf(t))
		key := fmt.Sprintf("%s|%d|%d|%d|%v", c.Model, outcome, reason, bound, inBound)
		e := entries[key]
		if e == nil {
			e = &conversationv1.VendorSpend{
				Model: c.Model, Agent: agent, Cause: cause,
				CacheOutcome: outcome, ColdReason: reason, Tokens: &conversationv1.VendorTokens{},
			}
			if inBound {
				e.ContextLeTokens = &bound
			}
			entries[key] = e
			order = append(order, key)
		}
		e.Calls++
		e.Tokens.UncachedInput += t.GetUncachedInput()
		e.Tokens.CacheRead += t.GetCacheRead()
		e.Tokens.CacheWrite_5M += t.GetCacheWrite_5M()
		e.Tokens.CacheWrite_1H += t.GetCacheWrite_1H()
		e.Tokens.Output += t.GetOutput()
	}
	out := make([]*conversationv1.VendorSpend, len(order))
	for i, key := range order {
		out[i] = entries[key]
	}
	return out
}
