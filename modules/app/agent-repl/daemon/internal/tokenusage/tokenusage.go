// Package tokenusage is the ONE home of token-usage reconciliation: it decides
// what a unit's newly reported usage does to the usage already held for that
// unit, and which contradictions the report makes. Every resolver that keeps
// per-unit usage (the topbar's session totals, the footer's turn accounting)
// asks it rather than comparing usages itself.
//
// A UNIT'S USAGE ARRIVES FROM TWO PRODUCERS BY DESIGN: the shim's stream plane
// writes the vendor's in-flight snapshot while a response streams
// (`TokenUsage.provisional` set), and the sidecar's file plane writes the
// settled usage from the transcript record. The settled report supersedes the
// provisional one, which is a designed supersession and never a
// contradiction; only two settled usages that differ are.
package tokenusage

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// The contradiction texts, sentence fragments whose subject is the reporting
// unit. Callers log or draw them verbatim.
const (
	// ThinkingExceedsOutput is a usage whose reasoning share exceeds the
	// output it is part of.
	ThinkingExceedsOutput = "it reports more thinking tokens than output tokens"
	// TwoDifferentUsages is two settled usages for one unit that differ.
	TwoDifferentUsages = "it reported two different usages"
)

// Decision is what a newly reported usage does to the one held for its unit.
type Decision struct {
	// Take reports that the caller replaces the held usage with the new one
	// (or records it, when none was held). When false the held usage stands
	// and the caller changes nothing it derives from it.
	Take bool
	// Contradictions are the contradictions the new report makes, each one of
	// the texts above, for the caller to record.
	Contradictions []string
}

// Same reports whether two usages are the same report, field for field,
// provisional included. Two nil usages are the same; a nil and a non-nil one
// are not.
func Same(a, b *conversationv1.TokenUsage) bool {
	if a == nil || b == nil {
		return a == b
	}
	return a.GetInputHits().GetRead() == b.GetInputHits().GetRead() &&
		a.GetInputMisses().GetWritten() == b.GetInputMisses().GetWritten() &&
		a.GetInputMisses().GetUnwritten() == b.GetInputMisses().GetUnwritten() &&
		a.GetOutputTokens() == b.GetOutputTokens() &&
		a.GetOutputThinkingTokens() == b.GetOutputThinkingTokens() &&
		a.GetProvisional() == b.GetProvisional()
}

// Reconcile decides what next, a unit's newly reported usage, does to
// previous, the usage held for that unit (nil when none is).
//
// THE RULES, in order:
//   - no next report changes nothing and contradicts nothing;
//   - a report whose thinking exceeds its output is a contradiction on its
//     own, and the rules below still apply to it;
//   - the first report for a unit is taken;
//   - the same report again changes nothing;
//   - anything replaces a provisional usage, without contradiction: a settled
//     report is the designed supersession, a provisional one a refresh;
//   - a provisional report never overrides a settled usage (the planes carry
//     no ordering, so a late in-flight snapshot can arrive after the settled
//     figure), and is no contradiction;
//   - two settled usages that differ are a producer contradiction; the later
//     one is taken.
func Reconcile(previous, next *conversationv1.TokenUsage) Decision {
	if next == nil {
		return Decision{}
	}
	var d Decision
	if next.GetOutputThinkingTokens() > next.GetOutputTokens() {
		d.Contradictions = append(d.Contradictions, ThinkingExceedsOutput)
	}
	switch {
	case previous == nil:
		d.Take = true
	case Same(previous, next):
	case previous.GetProvisional():
		d.Take = true
	case next.GetProvisional():
	default:
		d.Take = true
		d.Contradictions = append(d.Contradictions, TwoDifferentUsages)
	}
	return d
}
