package progress

// accounting.go resolves the footer's TURN-ACCOUNTING CELL from the daemon's
// own terminal-turn reconciliation record.
//
// A turn's accounting is a RECONCILIATION: the reducer in
// internal/sessioncontroller compares the usage each response reported against
// the totals the terminal result claimed, and files the outcome as a
// frontendv1.TurnAccounting. Everything the footer shows about that
// reconciliation — the verdict, the prose beside it, and the figures inside
// the prose — is resolved HERE, once, and rendered verbatim.
//
// IT USED TO BE THE CLIENT'S. The webapp held the composition (webapp/src/
// turn-accounting.ts): it decided when a record counted as incomplete, named
// the problems, and did the arithmetic for the quota delta and the throughput.
// A frontend that composes a verdict is a second author of it, and the topbar
// and the footer each drew their own copy of the same sentence. The cell is
// now the single answer, and its verdict travels as ARMS so a renderer cannot
// display "invalid" with nothing to say about why.

import (
	"fmt"
	"strconv"
	"strings"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// AccountingCell resolves one settled turn's accounting into the footer's
// fully-resolved cell.
//
// A nil record is the absence of a settled turn, and the cell is absent with
// it: an empty cell would claim a reconciliation that has not happened. Every
// non-nil record produces a cell with EXACTLY ONE verdict arm set and a
// composed summary, so no caller can publish half of one.
//
// The two degraded arms are never empty. A record whose invalid arm carries no
// problems is a defect upstream rather than a renderable state, so it is
// reported as an unstated problem instead of being silently promoted to
// complete.
func AccountingCell(a *frontendv1.TurnAccounting) *frontendv1.FooterAccountingCell {
	if a == nil {
		return nil
	}
	if a.GetInvalid() != nil {
		problems := accountingProblems(a.GetInvalid().GetProblems())
		return &frontendv1.FooterAccountingCell{
			Summary: "INVALID ACCOUNTING: " + strings.Join(problems, ", ") +
				evidenceSuffix(missingInvalidEvidence(a)),
			Verdict: &frontendv1.FooterAccountingCell_Invalid{
				Invalid: &frontendv1.AccountingInvalid{Problems: problems},
			},
		}
	}
	if missing := missingCompleteEvidence(a); len(missing) > 0 {
		return &frontendv1.FooterAccountingCell{
			Summary: "INCOMPLETE ACCOUNTING" + evidenceSuffix(missing),
			Verdict: &frontendv1.FooterAccountingCell_Incomplete{
				Incomplete: &frontendv1.AccountingIncomplete{Missing: missing},
			},
		}
	}
	return &frontendv1.FooterAccountingCell{
		Summary: completeAccountingSummary(a),
		Verdict: &frontendv1.FooterAccountingCell_Complete{Complete: &frontendv1.AccountingComplete{}},
	}
}

// accountingProblems turns the record's typed problems into display-ready
// phrases, in the order the reducer detected them.
//
// A problem whose arm is unset is REPORTED rather than skipped: the record
// said something was wrong, and dropping the entry would leave an invalid
// verdict shorter than the evidence behind it. The empty-list case is covered
// the same way, because "invalid with nothing to say" is exactly the state the
// arms exist to make unrenderable.
func accountingProblems(problems []*frontendv1.TurnAccountingProblem) []string {
	out := make([]string, 0, len(problems))
	for _, p := range problems {
		out = append(out, accountingProblem(p))
	}
	if len(out) == 0 {
		return []string{"the turn was declared invalid without stating a problem"}
	}
	return out
}

func accountingProblem(p *frontendv1.TurnAccountingProblem) string {
	switch {
	case p.GetMissingUsageBoundary() != nil:
		b := p.GetMissingUsageBoundary()
		if b.GetTurnStart() != nil {
			return "usage at turn start was never sampled"
		}
		if b.GetTurnEnd() != nil {
			return "usage at turn end was never sampled"
		}
		return "a usage boundary was never sampled, and which one was not stated"
	case p.GetWindowReset() != nil:
		r := p.GetWindowReset()
		return fmt.Sprintf("the 5h usage window reset mid-turn (start window resets at %d, end window resets at %d)",
			r.GetStartResetsAtMs(), r.GetEndResetsAtMs())
	case p.GetTokenLedgerMismatch() != nil:
		return "the response and result token ledgers disagree at " +
			joinPaths(p.GetTokenLedgerMismatch().GetDifferingFieldPaths())
	case p.GetRuntimeIdentityIncomplete() != nil:
		return "runtime identity evidence is missing at " +
			joinPaths(p.GetRuntimeIdentityIncomplete().GetMissingFieldPaths())
	case p.GetUnmodeledUsageFields() != nil:
		return "the vendor reported usage fields this build has no typed home for: " +
			joinPaths(p.GetUnmodeledUsageFields().GetSourceFieldPaths())
	case p.GetTelemetryRecordMissing() != nil:
		return telemetryRecordMissing(p.GetTelemetryRecordMissing())
	default:
		return "an accounting problem arrived with no stated kind"
	}
}

func telemetryRecordMissing(m *frontendv1.TelemetryRecordMissing) string {
	switch {
	case m.GetQueryLifecycle() != nil:
		return "the query lifecycle record for " + quoted(m.GetQueryLifecycle().GetQueryInstanceId()) + " is missing"
	case m.GetResponseUsage() != nil:
		return "the response usage record for " + quoted(m.GetResponseUsage().GetApiMessageId()) + " is missing"
	case m.GetPersistenceReceipt() != nil:
		return "the durable persistence receipt for turn " + quoted(m.GetPersistenceReceipt().GetTurnId()) + " is missing"
	default:
		return "a required telemetry record is missing, and which one was not stated"
	}
}

func quoted(s string) string {
	if s == "" {
		return "an unnamed record"
	}
	return strconv.Quote(s)
}

// joinPaths writes one problem's field paths as a phrase, folding repeats into
// a single entry carrying its count.
//
// EVERY ONE OF THESE LISTS IS PER-RESPONSE EVIDENCE, and a turn has as many
// responses as it has. One settled turn reported the same unmodeled path once
// per response and the cell read "iterations.0, iterations.0, …" 113 times, a
// sentence long enough to bury the OTHER problem beside it. The count says
// strictly more than the repetition did, in one entry.
//
// THIS IS DISPLAY ONLY. The record's own path list stays complete and
// un-deduplicated: it is the evidence, and folding it there would lose which
// responses were involved. First-appearance order is preserved so the phrase
// still reads in the order the reducer detected the paths.
func joinPaths(paths []string) string {
	if len(paths) == 0 {
		return "field paths the record did not name"
	}
	counts := make(map[string]int, len(paths))
	order := make([]string, 0, len(paths))
	for _, path := range paths {
		if counts[path] == 0 {
			order = append(order, path)
		}
		counts[path]++
	}
	phrases := make([]string, 0, len(order))
	for _, path := range order {
		if counts[path] == 1 {
			phrases = append(phrases, path)
			continue
		}
		phrases = append(phrases, fmt.Sprintf("%s ×%d", path, counts[path]))
	}
	return strings.Join(phrases, ", ")
}

// missingCompleteEvidence names every evidence fragment the full summary needs
// and this record does not carry. An empty result means the summary can be
// drawn from evidence alone.
func missingCompleteEvidence(a *frontendv1.TurnAccounting) []string {
	return absent([]evidence{
		{"runtime identity", a.GetRuntime() != nil},
		{"turn timing", a.GetTiming() != nil},
		{"usage at turn start", usageOutcomeKnown(a.GetUsageAtStart())},
		{"usage at turn end", usageOutcomeKnown(a.GetUsageAtEnd())},
		{"the token reconciliation", a.GetReconciliation().GetResponseAllAgents() != nil},
	})
}

// missingInvalidEvidence is missingCompleteEvidence for a record already
// condemned by its problems. An invalid turn is judged on the PRESENCE of each
// fragment rather than on the readable interior a complete one needs: the
// verdict is settled, and what the note adds is which evidence survived.
func missingInvalidEvidence(a *frontendv1.TurnAccounting) []string {
	return absent([]evidence{
		{"runtime identity", a.GetRuntime() != nil},
		{"turn timing", a.GetTiming() != nil},
		{"usage at turn start", a.GetUsageAtStart() != nil},
		{"usage at turn end", a.GetUsageAtEnd() != nil},
		{"the token reconciliation", a.GetReconciliation() != nil},
	})
}

type evidence struct {
	name    string
	present bool
}

func absent(all []evidence) []string {
	var out []string
	for _, e := range all {
		if !e.present {
			out = append(out, e.name+" is absent")
		}
	}
	return out
}

func evidenceSuffix(missing []string) string {
	if len(missing) == 0 {
		return ""
	}
	return " · " + strings.Join(missing, ", ")
}

// usageOutcomeKnown reports whether a boundary observation SETTLED — either it
// measured the account or it stated why it could not. An observation with
// neither arm is an unfinished sample, not an unavailable one.
func usageOutcomeKnown(o *corev1.AccountUsageObservation) bool {
	if o == nil {
		return false
	}
	return o.GetAvailable() != nil || o.GetUnavailable() != nil
}

// completeAccountingSummary is the sentence a reconciled turn shows: the 5h
// quota move, the turn's duration, the four token dimensions, the cache hit
// rate, the subagent count and the throughput.
//
// Every figure it reads has already been proven present by
// missingCompleteEvidence, except the ones that are legitimately UNAVAILABLE
// on a settled observation — an account-usage sample that failed, or a turn
// with no measurable duration. Those read "unavailable" rather than as a
// fabricated zero.
func completeAccountingSummary(a *frontendv1.TurnAccounting) string {
	total := a.GetReconciliation().GetResponseAllAgents()
	durationMs := a.GetTiming().GetPromptToResultMs()
	return strings.Join([]string{
		"5h " + quotaMove(a.GetUsageAtStart(), a.GetUsageAtEnd()),
		fmt.Sprintf("%ds", roundDivide(durationMs, 1000)),
		"in " + formatTokens(total.GetInputTokens()),
		"out " + formatTokens(total.GetOutputTokens()),
		"read " + formatTokens(total.GetCacheReadInputTokens()),
		"write " + formatTokens(total.GetCacheCreationInputTokens()),
		cacheHit(total.GetCacheRates()),
		subagentCount(a.GetResponses()),
		throughput(total.GetOutputTokens(), durationMs),
	}, " · ")
}

// quotaMove is the five-hour window's utilization at both boundaries and the
// move between them, or "unavailable" when either boundary failed to measure.
// A one-sided move is not reported: the delta is the point, and half of one
// would read as a full account of the turn's cost.
func quotaMove(start, end *corev1.AccountUsageObservation) string {
	from, to := start.GetAvailable(), end.GetAvailable()
	if from == nil || to == nil {
		return "unavailable"
	}
	a, b := from.GetFiveHour().GetUtilizationPercent(), to.GetFiveHour().GetUtilizationPercent()
	return fmt.Sprintf("%.1f%%→%.1f%% (%.1fpp)", a, b, b-a)
}

func cacheHit(rates *frontendv1.TokenCacheRates) string {
	if rates == nil {
		return "hit unavailable"
	}
	return fmt.Sprintf("hit %.1f%%", 100*rates.GetCacheHitRate())
}

func subagentCount(responses []*frontendv1.TokenUtilization) string {
	n := 0
	for _, r := range responses {
		if r.GetSubagent() != nil {
			n++
		}
	}
	if n == 1 {
		return "1 subagent"
	}
	return fmt.Sprintf("%d subagents", n)
}

// throughput is the turn's output rate. A turn with no measurable duration has
// no rate at all, and saying so is the only honest answer — a divide-by-zero
// guarded with a zero would report a silent turn as instantaneous.
func throughput(outputTokens, durationMs int64) string {
	if durationMs <= 0 {
		return "generation unavailable"
	}
	return fmt.Sprintf("%.1f tok/s", 1000*float64(outputTokens)/float64(durationMs))
}

// roundDivide is n/d rounded to nearest, for the whole-second duration. A
// truncating divide reported every sub-second turn as "0s".
func roundDivide(n, d int64) int64 {
	if d == 0 {
		return 0
	}
	if n < 0 {
		return -roundDivide(-n, d)
	}
	return (n + d/2) / d
}

// formatTokens writes a token count the way every other surface writes one:
// grouped in threes ("1,234,567"). It is here rather than borrowed from a
// formatting library because the grouping is part of the rendered string this
// package is now the author of.
func formatTokens(n int64) string {
	if n < 0 {
		return "-" + formatTokens(-n)
	}
	digits := strconv.FormatInt(n, 10)
	lead := len(digits) % 3
	if lead == 0 {
		lead = 3
	}
	var b strings.Builder
	b.WriteString(digits[:lead])
	for i := lead; i < len(digits); i += 3 {
		b.WriteByte(',')
		b.WriteString(digits[i : i+3])
	}
	return b.String()
}
