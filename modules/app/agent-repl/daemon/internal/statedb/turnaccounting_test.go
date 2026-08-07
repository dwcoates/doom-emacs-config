package statedb

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
)

func TestTurnAccountingsRejectBlankResponseModelBeforeDurableMutation(t *testing.T) {
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatalf("NewTurnAccountings: %v", err)
	}
	response := completeUtilization("s", "claude", "t", "m")
	response.Model = " \t"
	accounting := &frontendv1.TurnAccounting{TurnId: "t", Responses: []*frontendv1.TokenUtilization{response}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", accounting); err == nil || !strings.Contains(err.Error(), "blank model") {
		t.Fatalf("Record error = %v, want blank model rejection", err)
	}
	for _, table := range []string{"token_utilization", "turn_accounting"} {
		var count int
		if err := store.db.QueryRow("SELECT COUNT(*) FROM " + table).Scan(&count); err != nil {
			t.Fatalf("count %s: %v", table, err)
		}
		if count != 0 {
			t.Fatalf("blank response model partially mutated %s: %d rows", table, count)
		}
	}
}

func TestTurnAccountingsRecordPersistsResponsesAndTerminalEvidence(t *testing.T) {
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatalf("NewTurnAccountings: %v", err)
	}
	response := completeUtilization("s", "claude", "t", "m")
	response.Usage.OutputTokens = 2
	accounting := &frontendv1.TurnAccounting{TurnId: "t", Responses: []*frontendv1.TokenUtilization{response}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", accounting); err != nil {
		t.Fatalf("Record: %v", err)
	}
	got, err := accountings.List("s")
	if err != nil || len(got) != 1 || got[0].GetTurnId() != "t" {
		t.Fatalf("List = %+v, %v", got, err)
	}
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	responses, err := utilizations.List("s")
	if err != nil || len(responses) != 1 || responses[0].GetApiMessageId() != "m" {
		t.Fatalf("responses = %+v, %v", responses, err)
	}
}

func TestTurnAccountingAtomicallyEnrichesCompatibleHistoricalResponse(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatal(err)
	}
	historical := completeUtilization("s", "claude", "", "m")
	if _, err := utilizations.RecordHistorical(historical); err != nil {
		t.Fatal(err)
	}
	live := completeUtilization("s", "claude", "t", "m")
	live.ResponseTiming = &frontendv1.TokenResponseTiming{OutputGenerationDurationMs: int64p(0)}
	accounting := &frontendv1.TurnAccounting{TurnId: "t", Responses: []*frontendv1.TokenUtilization{live}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := accountings.Record("s", accounting); err != nil {
		t.Fatalf("Record terminal enrichment: %v", err)
	}
	got, err := utilizations.List("s")
	if err != nil || len(got) != 1 || !proto.Equal(got[0], live) {
		t.Fatalf("enriched response = %+v, %v", got, err)
	}
}

func TestTurnAccountingsRollsBackResponseWhenTerminalPersistenceFails(t *testing.T) {
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatalf("NewTurnAccountings: %v", err)
	}
	if _, err := store.db.Exec(`CREATE TRIGGER reject_turn_accounting BEFORE INSERT ON turn_accounting BEGIN SELECT RAISE(FAIL, 'forced terminal failure'); END`); err != nil {
		t.Fatal(err)
	}
	accounting := &frontendv1.TurnAccounting{TurnId: "t", Responses: []*frontendv1.TokenUtilization{completeUtilization("s", "claude", "t", "m")}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", accounting); err == nil {
		t.Fatal("Record succeeded")
	}
	var count int
	if err := store.db.QueryRow(`SELECT COUNT(*) FROM token_utilization`).Scan(&count); err != nil {
		t.Fatal(err)
	}
	if count != 0 {
		t.Fatalf("partial response rows = %d", count)
	}
}

// THE PERSISTED ROW WINS ON REPLAY, and everything a fresh bring-up is
// entitled to recompute differently — Timing (wall-clock instants taken at
// settlement time), QueryInstanceId and Runtime (the live query()
// invocation's identity, re-minted every bring-up by design), and Verdict
// (derived from both, so an Invalid verdict's problem paths embed the very
// query id that just changed) — is tolerated rather than compared. This is
// the regression guard for the divergent-replay defect diagnosed against two
// real turns ("ka_26779facc41beb8f1ae45d61" and "fe-4383-0fb7"): both
// recomputed with a brand-new query_instance_id on every bring-up and were
// rejected as "divergent" purely on that ephemeral field.
func TestTurnAccountingsReplayReturnsCanonicalSettlementToleratingEphemeralFields(t *testing.T) {
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatal(err)
	}
	first := &frontendv1.TurnAccounting{TurnId: "t", QueryInstanceId: "query-a", Timing: &frontendv1.TurnAccountingTiming{AccountingSettledAtMs: 100, ResultToSettlementMs: 10}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", first); err != nil {
		t.Fatal(err)
	}
	// A later bring-up recomputes this SAME turn under a brand-new
	// query_instance_id (query-b) and a later settlement instant — both
	// EXPECTED to differ, never a sign of corruption.
	replay := &frontendv1.TurnAccounting{TurnId: "t", QueryInstanceId: "query-b", Timing: &frontendv1.TurnAccountingTiming{AccountingSettledAtMs: 900, ResultToSettlementMs: 810}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	canonical, err := accountings.Record("s", replay)
	if err != nil {
		t.Fatalf("idempotent replay under a fresh query_instance_id: %v", err)
	}
	if canonical.GetQueryInstanceId() != "query-a" {
		t.Fatalf("canonical replay query_instance_id = %q, want the persisted row's %q", canonical.GetQueryInstanceId(), "query-a")
	}
	if canonical.GetTiming().GetAccountingSettledAtMs() != 100 || canonical.GetTiming().GetResultToSettlementMs() != 10 {
		t.Fatalf("canonical replay = %+v", canonical.GetTiming())
	}
}

// A REPLAY WHOSE RESPONSES DISAGREE ON RAW EVIDENCE IS STILL A VIOLATION: the
// api_message_id set and each response's own raw usage are the turn's
// durable identity, not a bring-up-scoped recomputation, so they stay fatal
// even after the ephemeral-field tolerance above.
func TestTurnAccountingsRejectsDivergentResponseEvidence(t *testing.T) {
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatal(err)
	}
	first := &frontendv1.TurnAccounting{TurnId: "t", QueryInstanceId: "query-a", Responses: []*frontendv1.TokenUtilization{completeUtilization("s", "claude", "t", "m")}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", first); err != nil {
		t.Fatal(err)
	}
	divergentResponse := completeUtilization("s", "claude", "t", "m")
	divergentResponse.Usage.OutputTokens = 999
	replay := &frontendv1.TurnAccounting{TurnId: "t", QueryInstanceId: "query-b", Responses: []*frontendv1.TokenUtilization{divergentResponse}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", replay); err == nil {
		t.Fatal("divergent response evidence was accepted")
	}
}

// AN EVIDENCE-FREE CROSS-GENERATION RECOMPUTE STAYS DIVERGENT, and this test
// exists to pin that it does.
//
// This is the exact shape observed live on session s_4418b0d983d8d5b5, turns
// "fe-365-6c53" and "ka_23d27a02b4cc643e46cf1b64", after a full-stack deploy
// stopped every shim and the sessions re-established with stream replay. The
// generation that RAN the turn settled a record carrying account-usage
// boundaries (usage_at_start / usage_at_end, each stamping the retired query's
// query_instance_id plus its own wall-clock observed_at_ms and
// sample_latency_ms), the turn's responses, and a reconciliation built from
// the vendor Result. The replaying generation admits NONE of that: every one
// of those rows names the retired query, so the reducer classifies them
// historical and refuses them as live evidence, and what it recomputes is an
// empty turn.
//
// The store must not paper over that by widening its tolerance any further —
// the canonical comparison already forgives the genuinely ephemeral fields,
// and forgiving the usage boundaries and the response ledger too would leave
// nothing being compared at all. The recompute is simply not a competing
// account of the turn, and the layer that KNOWS that (the session controller,
// which holds the epoch classification) is where the degradation belongs.
func TestTurnAccountingsRejectsEvidenceFreeCrossGenerationRecompute(t *testing.T) {
	// Arrange: the generation that ran the turn settles it with full evidence.
	accountings := newTurnAccountings(t)
	settled := &frontendv1.TurnAccounting{
		TurnId:          "fe-365-6c53",
		QueryInstanceId: "query-a",
		UsageAtStart:    boundaryUsageObservation("query-a", "fe-365-6c53", true),
		UsageAtEnd:      boundaryUsageObservation("query-a", "fe-365-6c53", false),
		Responses:       []*frontendv1.TokenUtilization{completeUtilization("s", "claude", "fe-365-6c53", "m")},
		Reconciliation:  &frontendv1.TokenUsageReconciliation{ResponseRecordCount: 1, ApiMessageIds: []string{"m"}},
		Verdict:         &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}},
	}
	if _, err := accountings.Record("s", settled); err != nil {
		t.Fatalf("first settlement: %v", err)
	}

	// Act: the replaying generation recomputes the same turn with no admitted
	// evidence whatsoever, under a freshly minted query id.
	replay := &frontendv1.TurnAccounting{
		TurnId:          "fe-365-6c53",
		QueryInstanceId: "query-b",
		Reconciliation:  &frontendv1.TokenUsageReconciliation{},
		Verdict:         &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}},
	}
	_, err := accountings.Record("s", replay)

	// Assert.
	if err == nil {
		t.Fatal("an evidence-free cross-generation recompute overwrote a settled turn")
	}
	if !strings.Contains(err.Error(), "divergent replay for turn accounting") {
		t.Fatalf("Record error = %v, want the divergent-replay rejection", err)
	}
}

// boundaryUsageObservation is one account-usage boundary sample of the shape
// the live poller writes: stamped with the query instance that took it and
// with the wall-clock instants of the sampling itself.
func boundaryUsageObservation(queryID, turnID string, start bool) *corev1.AccountUsageObservation {
	observation := &corev1.AccountUsageObservation{
		QueryInstanceId:  queryID,
		TurnId:           turnID,
		BoundaryAtMs:     1786053233408,
		ObservedAtMs:     1786053234013,
		SampleLatencyMs:  605,
		SubscriptionType: "max",
		Outcome: &corev1.AccountUsageObservation_Available{Available: &corev1.AccountUsageAvailable{
			FiveHour: &corev1.UsageWindow{UtilizationPercent: 8, ResetsAtMs: 1786064399579},
		}},
	}
	if start {
		observation.Boundary = &corev1.AccountUsageObservation_TurnStart{TurnStart: &corev1.TurnStartUsageBoundary{}}
	} else {
		observation.Boundary = &corev1.AccountUsageObservation_TurnEnd{TurnEnd: &corev1.TurnEndUsageBoundary{}}
	}
	return observation
}

// newTurnAccountings is a turn accounting store on a fresh state database.
func newTurnAccountings(t *testing.T) *TurnAccountings {
	t.Helper()
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatalf("NewTurnAccountings: %v", err)
	}
	return accountings
}

// THE RECORDED RESULT INSTANT IS THE DURABLE ANSWER to "when did this turn
// end" — the only one that survives the daemon that observed the boundary.
func TestTurnAccountingsEndedAtMsReportsTheRecordedResultInstant(t *testing.T) {
	// Arrange.
	accountings := newTurnAccountings(t)
	if _, err := accountings.Record("s", &frontendv1.TurnAccounting{
		TurnId:  "ka_1",
		Timing:  &frontendv1.TurnAccountingTiming{ResultReceivedAtMs: 2_000},
		Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}},
	}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	got, ok, err := accountings.EndedAtMs("ka_1")

	// Assert.
	if err != nil || !ok || got != 2_000 {
		t.Fatalf("EndedAtMs = (%d, %v, %v), want (2000, true, nil)", got, ok, err)
	}
}

// A TURN THE STORE NEVER SAW reports NOT FOUND rather than a zero that a caller
// could mistake for an instant.
func TestTurnAccountingsEndedAtMsReportsNotFoundForAnUnknownTurn(t *testing.T) {
	// Arrange.
	accountings := newTurnAccountings(t)

	// Act.
	_, ok, err := accountings.EndedAtMs("ka_never_recorded")

	// Assert.
	if err != nil || ok {
		t.Fatalf("EndedAtMs for an unknown turn = (ok=%v, err=%v), want (false, nil)", ok, err)
	}
}

// A RECORD WITH NO RESULT INSTANT is NOT an answer. The row exists but says
// nothing about when the turn ended, and reporting its zero as an instant would
// close a window at the epoch.
func TestTurnAccountingsEndedAtMsReportsNotFoundWithoutAResultInstant(t *testing.T) {
	// Arrange.
	accountings := newTurnAccountings(t)
	if _, err := accountings.Record("s", &frontendv1.TurnAccounting{
		TurnId:  "ka_1",
		Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}},
	}); err != nil {
		t.Fatalf("Record: %v", err)
	}

	// Act.
	_, ok, err := accountings.EndedAtMs("ka_1")

	// Assert.
	if err != nil || ok {
		t.Fatalf("EndedAtMs for a turn with no result instant = (ok=%v, err=%v), want (false, nil)", ok, err)
	}
}

// AN EMPTY TURN ID IS REFUSED rather than scanning the whole table.
func TestTurnAccountingsEndedAtMsRefusesAnEmptyTurnID(t *testing.T) {
	// Arrange.
	accountings := newTurnAccountings(t)

	// Act.
	_, _, err := accountings.EndedAtMs("")

	// Assert.
	if err == nil {
		t.Fatal("EndedAtMs(\"\") = nil, want a refusal")
	}
}
