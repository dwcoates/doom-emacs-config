package statedb

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

func int64p(v int64) *int64 { return &v }

func TestTokenUtilizationDeduplicatesAndEnrichesTiming(t *testing.T) {
	store, _ := openReceipts(t)
	utilizations, err := NewTokenUtilizations(store.db)
	if err != nil {
		t.Fatalf("NewTokenUtilizations: %v", err)
	}
	base := &frontendv1.TokenUtilization{AgentReplSessionId: "s", ApiMessageId: "m", Usage: &frontendv1.TokenUsage{OutputTokens: 4}, ResponseTiming: &frontendv1.TokenResponseTiming{TimeToFirstTokenMs: int64p(20)}}
	if inserted, err := utilizations.Record(base); err != nil || !inserted {
		t.Fatalf("first Record = %v, %v", inserted, err)
	}
	twin := &frontendv1.TokenUtilization{AgentReplSessionId: "s", ApiMessageId: "m", ResponseTiming: &frontendv1.TokenResponseTiming{OutputGenerationDurationMs: int64p(80)}, Actor: &frontendv1.TokenUtilization_MainAgent{MainAgent: &frontendv1.TokenUtilizationMainAgent{}}}
	if inserted, err := utilizations.Record(twin); err != nil || inserted {
		t.Fatalf("twin Record = %v, %v", inserted, err)
	}
	got, err := utilizations.List("s")
	if err != nil || len(got) != 1 {
		t.Fatalf("List = %v, %v", got, err)
	}
	if got[0].GetResponseTiming().GetTimeToFirstTokenMs() != 20 || got[0].GetResponseTiming().GetOutputGenerationDurationMs() != 80 || got[0].GetMainAgent() == nil {
		t.Fatalf("merged record = %+v", got[0])
	}
}
