package progress

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// availableUsage is a boundary observation that measured the account.
func availableUsage(percent float64) *corev1.AccountUsageObservation {
	return &corev1.AccountUsageObservation{
		Outcome: &corev1.AccountUsageObservation_Available{
			Available: &corev1.AccountUsageAvailable{
				FiveHour: &corev1.UsageWindow{UtilizationPercent: percent},
			},
		},
	}
}

// unavailableUsage is a boundary observation that SETTLED by stating why it
// could not measure — a different thing from one that never finished.
func unavailableUsage() *corev1.AccountUsageObservation {
	return &corev1.AccountUsageObservation{
		Outcome: &corev1.AccountUsageObservation_Unavailable{
			Unavailable: &corev1.AccountUsageUnavailable{
				Reason: &corev1.AccountUsageUnavailable_ServiceUnavailable{
					ServiceUnavailable: &corev1.UsageServiceUnavailable{},
				},
			},
		},
	}
}

// completeAccounting is a turn with every fragment the full summary needs.
func completeAccounting() *frontendv1.TurnAccounting {
	return &frontendv1.TurnAccounting{
		TurnId:  "t1",
		Runtime: &corev1.QueryRuntimeIdentity{},
		Timing: &frontendv1.TurnAccountingTiming{
			PromptToResultMs: 10_000,
		},
		UsageAtStart: availableUsage(10),
		UsageAtEnd:   availableUsage(12.5),
		Reconciliation: &frontendv1.TokenUsageReconciliation{
			ResponseAllAgents: &frontendv1.TokenUsageTotals{
				InputTokens:              1234,
				OutputTokens:             500,
				CacheReadInputTokens:     8910,
				CacheCreationInputTokens: 1112,
				CacheRates:               &frontendv1.TokenCacheRates{CacheHitRate: 0.881},
			},
		},
		Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}},
	}
}

func invalidWith(problem *frontendv1.TurnAccountingProblem) *frontendv1.TurnAccounting {
	a := completeAccounting()
	a.Verdict = &frontendv1.TurnAccounting_Invalid{
		Invalid: &frontendv1.TurnAccountingInvalid{Problems: []*frontendv1.TurnAccountingProblem{problem}},
	}
	return a
}

func TestNoSettledTurnHasNoAccountingCell(t *testing.T) {
	// Arrange — an empty cell would claim a reconciliation that has not happened.
	// Act.
	got := AccountingCell(nil)
	// Assert.
	if got != nil {
		t.Fatalf("a nil record produced a cell: %v", got)
	}
}

func TestAReconciledTurnResolvesTheCompleteArm(t *testing.T) {
	// Arrange + Act.
	got := AccountingCell(completeAccounting())
	// Assert.
	if got.GetComplete() == nil {
		t.Fatalf("a reconciled turn did not resolve the complete arm: %v", got)
	}
}

func TestACompleteSummaryStatesTheQuotaMoveAndItsDelta(t *testing.T) {
	// Arrange + Act.
	got := AccountingCell(completeAccounting())
	// Assert.
	if !strings.HasPrefix(got.GetSummary(), "5h 10.0%→12.5% (2.5pp) · ") {
		t.Fatalf("summary = %q, want it to lead with the 5h quota move", got.GetSummary())
	}
}

func TestACompleteSummaryGroupsTokenFiguresInThrees(t *testing.T) {
	// Arrange + Act.
	got := AccountingCell(completeAccounting())
	// Assert.
	if !strings.Contains(got.GetSummary(), "read 8,910") {
		t.Fatalf("summary = %q, want grouped token figures", got.GetSummary())
	}
}

func TestACompleteSummaryStatesTheThroughputFromTheTurnsDuration(t *testing.T) {
	// Arrange — 500 output tokens over 10s.
	// Act.
	got := AccountingCell(completeAccounting())
	// Assert.
	if !strings.HasSuffix(got.GetSummary(), "50.0 tok/s") {
		t.Fatalf("summary = %q, want it to end with the throughput", got.GetSummary())
	}
}

func TestATurnWithNoMeasurableDurationReportsNoThroughput(t *testing.T) {
	// Arrange — a guarded divide-by-zero would report a silent turn as instant.
	a := completeAccounting()
	a.Timing.PromptToResultMs = 0
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.HasSuffix(got.GetSummary(), "generation unavailable") {
		t.Fatalf("summary = %q, want the throughput reported as unavailable", got.GetSummary())
	}
}

func TestASubSecondTurnDoesNotReportZeroSeconds(t *testing.T) {
	// Arrange — a truncating divide read every sub-second turn as "0s".
	a := completeAccounting()
	a.Timing.PromptToResultMs = 600
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.Contains(got.GetSummary(), " · 1s · ") {
		t.Fatalf("summary = %q, want a rounded whole-second duration", got.GetSummary())
	}
}

func TestAnUnmeasuredQuotaBoundaryReportsTheMoveAsUnavailable(t *testing.T) {
	// Arrange — a one-sided move would read as a full account of the cost.
	a := completeAccounting()
	a.UsageAtEnd = unavailableUsage()
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.HasPrefix(got.GetSummary(), "5h unavailable · ") {
		t.Fatalf("summary = %q, want the quota move reported as unavailable", got.GetSummary())
	}
}

func TestASettledButUnavailableBoundaryIsNotAnIncompleteTurn(t *testing.T) {
	// Arrange — the observation stated why it could not measure, which is
	// evidence rather than its absence.
	a := completeAccounting()
	a.UsageAtStart = unavailableUsage()
	// Act.
	got := AccountingCell(a)
	// Assert.
	if got.GetComplete() == nil {
		t.Fatalf("a turn with a settled unavailable boundary resolved %v, want complete", got.GetVerdict())
	}
}

func TestATurnWithNoReconciliationResolvesTheIncompleteArm(t *testing.T) {
	// Arrange.
	a := completeAccounting()
	a.Reconciliation = nil
	// Act.
	got := AccountingCell(a)
	// Assert.
	if got.GetIncomplete() == nil {
		t.Fatalf("a turn with no reconciliation resolved %v, want incomplete", got.GetVerdict())
	}
}

func TestAnIncompleteVerdictNamesWhatIsMissing(t *testing.T) {
	// Arrange — an incompleteness with nothing to name is a daemon fault.
	a := completeAccounting()
	a.Timing = nil
	// Act.
	got := AccountingCell(a)
	// Assert.
	if len(got.GetIncomplete().GetMissing()) == 0 {
		t.Fatalf("an incomplete verdict named nothing missing")
	}
}

func TestAnUnfinishedUsageSampleIsMissingEvidence(t *testing.T) {
	// Arrange — an observation with neither outcome arm never settled.
	a := completeAccounting()
	a.UsageAtEnd = &corev1.AccountUsageObservation{}
	// Act.
	got := AccountingCell(a)
	// Assert.
	if got.GetIncomplete() == nil {
		t.Fatalf("an unfinished usage sample resolved %v, want incomplete", got.GetVerdict())
	}
}

func TestAnInvalidVerdictOutranksMissingEvidence(t *testing.T) {
	// Arrange — the evidence is present and does not add up, which is a
	// different claim from an absence.
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_TokenLedgerMismatch{
			TokenLedgerMismatch: &frontendv1.TokenLedgerMismatch{DifferingFieldPaths: []string{"usage.output_tokens"}},
		},
	})
	a.Timing = nil
	// Act.
	got := AccountingCell(a)
	// Assert.
	if got.GetInvalid() == nil {
		t.Fatalf("an invalid record resolved %v, want invalid", got.GetVerdict())
	}
}

func TestAnInvalidVerdictNamesTheContradictingFieldPaths(t *testing.T) {
	// Arrange.
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_TokenLedgerMismatch{
			TokenLedgerMismatch: &frontendv1.TokenLedgerMismatch{DifferingFieldPaths: []string{"usage.output_tokens"}},
		},
	})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.Contains(got.GetInvalid().GetProblems()[0], "usage.output_tokens") {
		t.Fatalf("problem = %q, want it to name the differing path", got.GetInvalid().GetProblems()[0])
	}
}

func TestAMissingTurnStartBoundaryIsNamedAsSuch(t *testing.T) {
	// Arrange.
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_MissingUsageBoundary{
			MissingUsageBoundary: &frontendv1.MissingUsageBoundary{
				Boundary: &frontendv1.MissingUsageBoundary_TurnStart{TurnStart: &frontendv1.MissingUsageBoundaryTurnStart{}},
			},
		},
	})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if got.GetInvalid().GetProblems()[0] != "usage at turn start was never sampled" {
		t.Fatalf("problem = %q", got.GetInvalid().GetProblems()[0])
	}
}

func TestAMissingPersistenceReceiptNamesItsTurn(t *testing.T) {
	// Arrange.
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_TelemetryRecordMissing{
			TelemetryRecordMissing: &frontendv1.TelemetryRecordMissing{
				Record: &frontendv1.TelemetryRecordMissing_PersistenceReceipt{
					PersistenceReceipt: &frontendv1.TelemetryRecordMissingPersistenceReceipt{TurnId: "t9"},
				},
			},
		},
	})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.Contains(got.GetInvalid().GetProblems()[0], `"t9"`) {
		t.Fatalf("problem = %q, want it to name the turn", got.GetInvalid().GetProblems()[0])
	}
}

func TestAProblemWithNoArmIsReportedRatherThanDropped(t *testing.T) {
	// Arrange — dropping it would leave the verdict shorter than its evidence.
	a := invalidWith(&frontendv1.TurnAccountingProblem{})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if len(got.GetInvalid().GetProblems()) != 1 {
		t.Fatalf("problems = %v, want the armless problem reported", got.GetInvalid().GetProblems())
	}
}

func TestAnInvalidVerdictWithNoProblemsStillSaysSomething(t *testing.T) {
	// Arrange — "invalid with nothing to say" is what the arms make unrenderable.
	a := completeAccounting()
	a.Verdict = &frontendv1.TurnAccounting_Invalid{Invalid: &frontendv1.TurnAccountingInvalid{}}
	// Act.
	got := AccountingCell(a)
	// Assert.
	if len(got.GetInvalid().GetProblems()) == 0 {
		t.Fatalf("an invalid verdict resolved with an empty problems list")
	}
}

func TestASubagentResponseIsCountedInTheSummary(t *testing.T) {
	// Arrange.
	a := completeAccounting()
	a.Responses = []*frontendv1.TokenUtilization{{
		Actor: &frontendv1.TokenUtilization_Subagent{
			Subagent: &frontendv1.TokenUtilizationSubagent{AgentId: "a1"},
		},
	}}
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.Contains(got.GetSummary(), " · 1 subagent · ") {
		t.Fatalf("summary = %q, want a singular subagent count", got.GetSummary())
	}
}

func TestAMainAgentResponseIsNotCountedAsASubagent(t *testing.T) {
	// Arrange.
	a := completeAccounting()
	a.Responses = []*frontendv1.TokenUtilization{{
		Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}},
	}}
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.Contains(got.GetSummary(), " · 0 subagents · ") {
		t.Fatalf("summary = %q, want a zero subagent count", got.GetSummary())
	}
}

func TestAnAbsentCacheRateReportsTheHitAsUnavailable(t *testing.T) {
	// Arrange.
	a := completeAccounting()
	a.Reconciliation.ResponseAllAgents.CacheRates = nil
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.Contains(got.GetSummary(), "hit unavailable") {
		t.Fatalf("summary = %q, want the hit rate reported as unavailable", got.GetSummary())
	}
}

// TestARepeatedUnmodeledPathIsFoldedIntoOneCountedPhrase covers the live shape
// of the problem: one unmodeled usage path reported once per response, which
// printed "iterations.0" 113 times in a single cell.
func TestARepeatedUnmodeledPathIsFoldedIntoOneCountedPhrase(t *testing.T) {
	// Arrange.
	paths := make([]string, 113)
	for i := range paths {
		paths[i] = "iterations.0"
	}
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_UnmodeledUsageFields{
			UnmodeledUsageFields: &frontendv1.UnmodeledUsageFields{SourceFieldPaths: paths},
		},
	})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.HasSuffix(got.GetInvalid().GetProblems()[0], "iterations.0 ×113") {
		t.Fatalf("problem = %q, want one counted phrase for the repeated path", got.GetInvalid().GetProblems()[0])
	}
}

// TestADistinctPathIsNamedWithoutACount keeps the ordinary single-occurrence
// phrase unchanged: a count of one states nothing the path did not.
func TestADistinctPathIsNamedWithoutACount(t *testing.T) {
	// Arrange.
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_UnmodeledUsageFields{
			UnmodeledUsageFields: &frontendv1.UnmodeledUsageFields{SourceFieldPaths: []string{"iterations.0"}},
		},
	})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.HasSuffix(got.GetInvalid().GetProblems()[0], "iterations.0") || strings.Contains(got.GetInvalid().GetProblems()[0], "×") {
		t.Fatalf("problem = %q, want the bare path with no count", got.GetInvalid().GetProblems()[0])
	}
}

// TestFoldedPathsKeepFirstAppearanceOrder proves the fold does not reorder the
// phrase into a map's iteration order.
func TestFoldedPathsKeepFirstAppearanceOrder(t *testing.T) {
	// Arrange.
	a := invalidWith(&frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_TokenLedgerMismatch{
			TokenLedgerMismatch: &frontendv1.TokenLedgerMismatch{DifferingFieldPaths: []string{"zeta", "alpha", "zeta"}},
		},
	})
	// Act.
	got := AccountingCell(a)
	// Assert.
	if !strings.HasSuffix(got.GetInvalid().GetProblems()[0], "zeta ×2, alpha") {
		t.Fatalf("problem = %q, want first-appearance order preserved", got.GetInvalid().GetProblems()[0])
	}
}

// TestTheRecordsOwnPathListIsNotDeduplicated is the other half of the display
// rule: the fold is the CELL's, and the evidence behind it stays complete.
func TestTheRecordsOwnPathListIsNotDeduplicated(t *testing.T) {
	// Arrange.
	problem := &frontendv1.TurnAccountingProblem{
		Problem: &frontendv1.TurnAccountingProblem_UnmodeledUsageFields{
			UnmodeledUsageFields: &frontendv1.UnmodeledUsageFields{SourceFieldPaths: []string{"iterations.0", "iterations.0"}},
		},
	}
	a := invalidWith(problem)
	// Act.
	AccountingCell(a)
	// Assert.
	if got := len(problem.GetUnmodeledUsageFields().GetSourceFieldPaths()); got != 2 {
		t.Fatalf("record path count = %d, want the evidence list left intact", got)
	}
}
