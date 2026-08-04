package statedb

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"google.golang.org/protobuf/proto"
)

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

func TestTurnAccountingsReplayReturnsCanonicalSettlementAndRejectsOtherDrift(t *testing.T) {
	store, _ := openReceipts(t)
	accountings, err := NewTurnAccountings(store.db)
	if err != nil {
		t.Fatal(err)
	}
	first := &frontendv1.TurnAccounting{TurnId: "t", Timing: &frontendv1.TurnAccountingTiming{AccountingSettledAtMs: 100, ResultToSettlementMs: 10}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	if _, err := accountings.Record("s", first); err != nil {
		t.Fatal(err)
	}
	replay := &frontendv1.TurnAccounting{TurnId: "t", Timing: &frontendv1.TurnAccountingTiming{AccountingSettledAtMs: 900, ResultToSettlementMs: 810}, Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	canonical, err := accountings.Record("s", replay)
	if err != nil {
		t.Fatalf("idempotent replay: %v", err)
	}
	if canonical.GetTiming().GetAccountingSettledAtMs() != 100 || canonical.GetTiming().GetResultToSettlementMs() != 10 {
		t.Fatalf("canonical replay = %+v", canonical.GetTiming())
	}
	divergent := proto.Clone(replay).(*frontendv1.TurnAccounting)
	divergent.QueryInstanceId = "different"
	if _, err := accountings.Record("s", divergent); err == nil {
		t.Fatal("divergent replay was accepted")
	}
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
