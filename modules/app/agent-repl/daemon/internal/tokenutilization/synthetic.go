package tokenutilization

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// SyntheticModelReconciliationExcluded reports whether a stream-plane per-model
// aggregate entry must be left out of the turn's token-ledger reconciliation
// compare against the result plane's model_usage map.
//
// Exactly one entry qualifies: the vendor's `<synthetic>` pseudo-model carrying
// no usage whatsoever. Such an entry is authored by CLI-side evidence that
// never reached the API at all — a compaction record is the canonical case:
// zero usage, "No response requested." The result plane is DEFINITIONALLY
// silent about a non-API pseudo-model, so comparing the two planes over it
// would report `model_usage.<synthetic>` as a ledger mismatch on every turn
// that contained a compaction. There is no usage to reconcile, and the absence
// on the result side is the contract rather than a discrepancy.
//
// A `<synthetic>` entry claiming ANY usage is NOT excluded. A pseudo-model that
// never reached the API cannot legitimately have spent tokens, so that entry is
// genuinely anomalous and must keep surfacing loudly through the existing
// mismatch path. No other model identity is affected in any way.
func SyntheticModelReconciliationExcluded(model string, totals *frontendv1.TokenUsageTotals) bool {
	if model != SyntheticModelIdentity {
		return false
	}
	return !totalsCarryUsage(totals)
}

// totalsCarryUsage reports whether totals claims any token or server-tool usage.
// Any nonzero counter counts, including a negative one: a negative counter is
// itself corrupt evidence and must not be mistaken for "no usage".
func totalsCarryUsage(totals *frontendv1.TokenUsageTotals) bool {
	return totals.GetInputTokens() != 0 ||
		totals.GetOutputTokens() != 0 ||
		totals.GetCacheReadInputTokens() != 0 ||
		totals.GetCacheCreationInputTokens() != 0 ||
		totals.GetCacheCreation().GetEphemeral_5MInputTokens() != 0 ||
		totals.GetCacheCreation().GetEphemeral_1HInputTokens() != 0 ||
		totals.GetServerToolUse().GetWebSearchRequests() != 0 ||
		totals.GetServerToolUse().GetWebFetchRequests() != 0 ||
		totals.GetOutputDetails().GetThinkingTokens() != 0
}
